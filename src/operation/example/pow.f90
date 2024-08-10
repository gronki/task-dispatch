module operation_power_m

    use value_base_m, only: value_item_t
    use operation_m, only: operation_t
    use real_value_m
    implicit none (type, external)

    type, extends(operation_t) :: op_pow_t
    contains
        procedure, nopass :: name => powd_name
        procedure :: exec => exec_pow
        procedure :: trace => trace_pow
    end type

contains

    subroutine exec_pow(op, inputs, output)
        class(op_pow_t), intent(in) :: op
        type(value_item_t), intent(in) :: inputs(:)
        class(value_t), intent(out), allocatable :: output

        if (size(inputs) /= 2) &
            error stop "power requires two inputs"

        select type (val => inputs(1) % value)
          type is (real_value_t)
            select type (expo => inputs(2) % value)
              type is (real_value_t)
                output = real_value(val%value**expo%value)
                return
            end select
        end select

        error stop "unexpected input type for pow"

    end subroutine

    function trace_pow(op, inputs) result(output_trace)
        class(op_pow_t), intent(in) :: op
        type(value_item_t), intent(in) :: inputs(:)
        character(len=:), allocatable :: output_trace

        output_trace = "" // trim(inputs(1) % value % get_trace()) &
            // "%pow(" // trim(inputs(2) % value % get_trace())  // ")"
    end function

    pure function powd_name() result(name)
        character(len=32) :: name

        name = "pow"
    end function

end module
