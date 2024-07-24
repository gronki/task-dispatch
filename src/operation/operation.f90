module operation_m

    use value_base_m
    use iso_fortran_env, only: debug_output => error_unit
    implicit none (type, external)

    type, abstract :: operation_t
    contains
        procedure(opname_proto), deferred :: name
        procedure(exec_proto), deferred :: exec
        procedure :: trace => trace_generic
        procedure :: exec_trace
        procedure :: execf
    end type

    abstract interface
        subroutine exec_proto(op, inputs, output)
            import :: value_item_t, operation_t, value_t
            class(operation_t), intent(in) :: op
            type(value_item_t), intent(in) :: inputs(:)
            class(value_t), intent(out), allocatable :: output
        end subroutine
        pure function opname_proto(op) result(name)
            import :: operation_t
            class(operation_t), intent(in) :: op
            character(len=32) :: name
        end function
    end interface

contains

    subroutine exec_trace(op, inputs, output)
        !! Execute the operation and handle value tracing 
        !! as well as sequence processing.
        !! If your operation explicitly expects sequences
        !! as inputs (for example, to sum all arguments)
        !! you need to override this subroutine. 
        use sequence_value_m
        !> operation
        class(operation_t), intent(in) :: op
        !> operation inputs
        type(value_item_t), intent(in) :: inputs(:)
        !> output value
        class(value_t), allocatable, intent(out) :: output

        type(value_item_t) :: temp_inputs(size(inputs))
        integer :: sequence_lengths(size(inputs))
        logical :: is_sequence(size(inputs))
        integer :: input_index, sequence_index, sequence_length, num_inputs
        type(sequence_value_t), allocatable :: sequence_output

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

        is_sequence = sequence_lengths > 0

        ! here we handle a typical case, where none of the inputs is a sequence
        if (.not. any(is_sequence)) then
            call op % exec(inputs, output)
            output % trace = op % trace(inputs)
            return
        end if

        ! by this point we have at least one sequence

        block
            integer, allocatable :: sequence_lengths_nonzero(:)
            sequence_lengths_nonzero = pack(sequence_lengths, is_sequence)
            sequence_length = sequence_lengths_nonzero(1)
            if (any(sequence_lengths_nonzero /= sequence_length)) &
                error stop "sequence parameters must all be equal length or scalars"
        end block

        ! unfortunately, at the moment sequence processing requires making a copy
        ! but this might be implemented more efficiently in the future using pointers

        ! preallocate non-sequence arguments
        do input_index = 1, num_inputs
            if (.not. is_sequence(input_index)) &
                temp_inputs(input_index)%value = inputs(input_index)%value
        end do

        ! now cycle through sequence index and only allocate copies for
        ! sequence items

        allocate(sequence_output)
        allocate(sequence_output%items(sequence_length))

        do sequence_index = 1, sequence_length
            do input_index = 1, num_inputs
                select type(input_val => inputs(input_index)%value)
                  class is (sequence_value_t)
                    temp_inputs(input_index)%value = input_val%items(sequence_index)%value
                end select
            end do
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

end module
