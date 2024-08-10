module operation_m

    use value_base_m
    use sequence_value_m
    use iso_fortran_env, only: debug_output => error_unit
    implicit none (type, external)
    private

    type input_key_t
        logical :: has_key = .false.
        character(len=32) :: key = ""
    end type

    public :: input_key_t

    type, abstract :: operation_t
    contains
        procedure(opname_proto), nopass, deferred :: name
        procedure(exec_proto), deferred :: exec
        procedure :: trace => trace_generic
        procedure :: exec_trace
        procedure :: execf
        procedure, nopass :: args_match
        procedure, nopass :: is_elemental
    end type

    public :: operation_t

    abstract interface
        subroutine exec_proto(op, inputs, output)
            import :: value_item_t, operation_t, value_t
            class(operation_t), intent(in) :: op
            type(value_item_t), intent(in) :: inputs(:)
            class(value_t), intent(out), allocatable :: output
        end subroutine
        pure function opname_proto() result(name)
            character(len=32) :: name
        end function
    end interface

    public :: argument_sequence_lengths, sequence_lengths_are_correct, make_sequential_input_vector

contains

    pure function argument_sequence_lengths(inputs) result(sequence_lengths)
        type(value_item_t), intent(in) :: inputs(:) !! operation inputs
        integer :: sequence_lengths(size(inputs))

        integer :: input_index, num_inputs

        num_inputs = size(inputs)

        do input_index = 1, num_inputs
            select type(input_val => inputs(input_index)%value)

              class is (sequence_value_t)
                ! in this case, input is a sequence
                if (.not. allocated(input_val%items)) then
                    error stop "sequence with unallocated values"
                end if
                sequence_lengths(input_index) = size(input_val%items)

              class default
                sequence_lengths(input_index) = 0

            end select
        end do

    end function

    pure function sequence_lengths_are_correct(sequence_lengths)
        integer, intent(in) :: sequence_lengths(:)
        integer, allocatable :: sequence_lengths_nonzero(:)
        logical :: sequence_lengths_are_correct
        logical :: is_sequence(size(sequence_lengths))
        integer :: sequence_length

        sequence_lengths_are_correct = .true.
        is_sequence = sequence_lengths > 0
        if (.not. any(is_sequence)) return
        sequence_lengths_nonzero = pack(sequence_lengths, is_sequence)
        sequence_length = sequence_lengths_nonzero(1)
        sequence_lengths_are_correct = all(sequence_lengths_nonzero == sequence_length)
    end function

    pure subroutine make_sequential_input_vector(inputs, sequence_index, temp_inputs, only_update)
        type(value_item_t), intent(in) :: inputs(:) !! operation inputs
        type(value_item_t), intent(inout) :: temp_inputs(:) !! operation inputs
        integer, intent(in) :: sequence_index
        logical, intent(in) :: only_update
        logical :: is_sequence(size(inputs))
        integer :: input_index, num_inputs

        num_inputs = size(inputs)

        is_sequence = argument_sequence_lengths(inputs) > 0

        if (.not. only_update) then
            ! preallocate non-sequence arguments
            do input_index = 1, num_inputs
                if (.not. is_sequence(input_index)) &
                    temp_inputs(input_index)%value = inputs(input_index)%value
            end do
        end if

        do input_index = 1, num_inputs
            select type(input_val => inputs(input_index)%value)
              class is (sequence_value_t)
                temp_inputs(input_index)%value = input_val%items(sequence_index)%value
            end select
        end do
    end subroutine

    subroutine exec_trace(op, inputs, output)
        !! Execute the operation and handle value tracing
        !! as well as sequence processing.
        !! If your operation explicitly expects sequences
        !! as inputs (for example, to sum all arguments)
        !! you need to override this subroutine.

        class(operation_t), intent(in) :: op !! operation
        type(value_item_t), intent(in) :: inputs(:) !! operation inputs
        class(value_t), allocatable, intent(out) :: output !! output to be allocated

        type(value_item_t) :: temp_inputs(size(inputs))
        integer :: sequence_lengths(size(inputs))
        integer :: input_index, sequence_index, sequence_length, num_inputs
        type(sequence_value_t), allocatable :: sequence_output

        num_inputs = size(inputs)

        if (.not. op % is_elemental()) then
            call op % exec(inputs, output)
            output % trace = op % trace(inputs)
            return
        end if

        sequence_lengths = argument_sequence_lengths(inputs)
        sequence_length = maxval(sequence_lengths)

        ! here we handle a typical case, where none of the inputs is a sequence
        if (sequence_length == 0) then
            call op % exec(inputs, output)
            output % trace = op % trace(inputs)
            return
        end if

        ! by this point we have at least one sequence

        if (.not. sequence_lengths_are_correct(sequence_lengths)) &
            error stop "sequence parameters must all be equal length or scalars"

        ! unfortunately, at the moment sequence processing requires making a copy
        ! but this might be implemented more efficiently in the future using pointers

        ! now cycle through sequence index and only allocate copies for
        ! sequence items

        allocate(sequence_output)
        allocate(sequence_output%items(sequence_length))

        do sequence_index = 1, sequence_length
            call make_sequential_input_vector(inputs, sequence_index, temp_inputs, sequence_index > 1)
            associate (output_item => sequence_output%items(sequence_index))
                call op % exec(temp_inputs, output_item%value)
                output_item%value % trace = op % trace(temp_inputs)
                write (debug_output, *) ' sequence item ', sequence_index, ' ', &
                    output_item%value % get_trace(), ' ---> ',  output_item%value%to_str()
            end associate
        end do

        call move_alloc(sequence_output, output)

    end subroutine

    function execf(op, inputs) result(output)
        class(operation_t), intent(in) :: op
        type(value_item_t), intent(in) :: inputs(:)
        class(value_t), allocatable :: output

        call op % exec_trace(inputs, output)
    end function

    function trace_generic(op, inputs) result(output_trace)
        !! Produces a generic trace for any operation following
        !! the pattern: opname(trace_arg1, trace_arg2, ...)
        !> operation
        class(operation_t), intent(in) :: op
        !> operation inputs
        type(value_item_t), intent(in) :: inputs(:)
        !> string trace
        character(len=:), allocatable :: output_trace
        integer :: i

        output_trace = trim(op % name()) // "("

        do i = 1, size(inputs)
            output_trace = output_trace &
                // trim(inputs(i) % value % get_trace()) &
                // adjustl(trim(merge(" ,", ") ", i < size(inputs))))
        end do
    end function

    pure function args_match(inputs, labels)
        !! return true if the operation is able to handle
        !! the arguments given by the call
        type(value_item_t), intent(in) :: inputs(:)
        type(input_key_t), intent(in) :: labels(:)
        logical :: args_match

        args_match = .true.
    end function

    pure function is_elemental()
        !! return true if the operation should be performed
        !! element-wise on the sequences
        logical :: is_elemental

        is_elemental = .true.
    end function
end module
