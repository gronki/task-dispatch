module operation_item_m

    use value_m, only: value_t, value_trace_t, value_item_t
    use sequence_value_m
    use operation_m, only: operation_t
    use real_value_m
    implicit none (type, external)
    private

    type, extends(operation_t) :: op_item_t
    contains
        procedure, nopass :: name => item_name
        procedure :: exec => exec_item
        ! procedure :: trace => trace_item
        procedure, nopass :: is_elemental
    end type

    public :: op_item_t

contains

    subroutine exec_item(op, inputs, output)
        class(op_item_t), intent(in) :: op
        type(value_item_t), intent(in) :: inputs(:)
        class(value_t), intent(out), allocatable :: output
        integer, allocatable :: indices(:)
        integer :: index_depth, i
        real(kind=real_k) :: index_real

        if (size(inputs) < 1) then
            error stop "item expects at least one argument"
        else if (size(inputs) == 1) then
            allocate(output, source=inputs(1) % value)
            return
        end if

        index_depth = size(inputs) - 1
        allocate(indices(index_depth))

        do i = 1, index_depth
            call parse_int(inputs(i+1) % value, indices(i))
        end do

        call get_item_recursively(inputs(1) % value, indices, 1, output)

    end subroutine

    recursive subroutine get_item_recursively(input, indices, depth, output)
        class(value_t), intent(in) :: input
        integer, intent(in) :: indices(:)
        integer, intent(in) :: depth
        class(value_t), intent(out), allocatable :: output

        integer :: this_index

        ! we could do this differently, but this way is easier to produce
        ! meaningful error messages
        this_index = indices(depth)

        select type (input)
          class is (sequence_value_t)
            if (depth == size(indices)) then
                allocate(output, source=input%items(this_index)%value)
            else
                call get_item_recursively(input%items(this_index)%value, indices, depth + 1, output)
            end if

          class default
            error stop "error retrieving an item: not a sequence"
        end select

    end subroutine

    ! function trace_item(op, input_traces) result(output_trace)
    !     !! Produces a generic trace for any operation following
    !     !! the pattern: opname(trace_arg1, trace_arg2, ...)
    !     !> operation
    !     class(op_item_t), intent(in) :: op
    !     !> operation inputs
    !     type(value_trace_t), intent(in) :: input_traces(:)
    !     !> string trace
    !     type(value_trace_t) :: output_trace
    !     integer :: i, num_inputs

    !     num_inputs = size(input_traces)
    !     output_trace % str = "["

    !     do i = 1, num_inputs
    !         associate (arg_trace => input_traces(i))
    !             output_trace % str = output_trace % str &
    !                 // trim(arg_trace % str) &
    !                 // adjustl(trim(merge(" ,", "] ", i < num_inputs)))
    !         end associate
    !     end do
    ! end function

    pure function item_name()
        character(len=32) :: item_name

        item_name = "item"
    end function

    pure function is_elemental()
        !! return true if the operation should be performed
        !! element-wise on the sequences
        logical :: is_elemental

        is_elemental = .false.
    end function

end module
