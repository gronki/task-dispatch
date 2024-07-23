module operation_multiply_m

    use operation_m, only: operation_t, value_item_t
    use real_value_m
    implicit none (type, external)

    type, extends(operation_t) :: multiply_t
    contains
        procedure :: name => multiply_name
        procedure :: exec => exec_multiply
    end type

contains

    subroutine exec_multiply(op, inputs, output)
        class(multiply_t), intent(in) :: op
        type(value_item_t), intent(in) :: inputs(:)
        class(value_t), intent(out), allocatable :: output
        integer :: i
        type(real_value_t) :: result

        if (size(inputs) == 0) &
            error stop "multiply: at least one argument required"

        result % value = 1

        do i = 1, size(inputs)
            select type (val => inputs(i) % value)
              type is (real_value_t)
                result % value = result % value * val % value
              class default
                error stop "multiply: unexpected input type"
            end select
        end do

        output = result

    end subroutine

    function multiply_name(op) result(name)
        class(multiply_t), intent(in) :: op
        character(len=32) :: name

        name = "mul"
    end function

end module
