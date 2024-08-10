module operation_square_m

    use value_base_m, only: value_item_t
    use operation_m, only: operation_t
    use real_value_m
    implicit none (type, external)

    type, extends(operation_t) :: square_t
    contains
        procedure, nopass :: name => squared_name
        procedure :: exec => exec_square
        procedure :: trace => trace_square
    end type

contains

    subroutine exec_square(op, inputs, output)
        class(square_t), intent(in) :: op
        type(value_item_t), intent(in) :: inputs(:)
        class(value_t), intent(out), allocatable :: output

        if (size(inputs) /= 1) &
            error stop "squared expects only one input"

        select type (val => inputs(1) % value)
          type is (real_value_t)
            output = real_value_t(value=val % value**2)
            return
        end select

        error stop "unexpected input type for square"

    end subroutine

    function trace_square(op, inputs) result(output_trace)
        class(square_t), intent(in) :: op
        type(value_item_t), intent(in) :: inputs(:)
        character(len=:), allocatable :: output_trace

        output_trace = "" // trim(inputs(1) % value % get_trace()) // "%pow(2)"
    end function

    pure function squared_name() result(name)
        character(len=32) :: name

        name = "squared"
    end function

end module
