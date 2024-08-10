module operation_add_m

    use value_base_m, only: value_item_t
    use operation_m, only: operation_t
    use real_value_m
    implicit none (type, external)

    type, extends(operation_t) :: add_t
    contains
        procedure, nopass :: name => add_name
        procedure :: exec => exec_add
    end type

contains

    subroutine exec_add(op, inputs, output)
        class(add_t), intent(in) :: op
        type(value_item_t), intent(in) :: inputs(:)
        class(value_t), intent(out), allocatable :: output
        integer :: i
        type(real_value_t) :: result

        if (size(inputs) == 0) &
            error stop "at least one argument required"

        result % value = 0

        do i = 1, size(inputs)
            select type (val => inputs(i) % value)
              type is (real_value_t)
                result % value = result % value + val % value
              class default
                error stop "add: unexpected input type"
            end select
        end do

        output = result

    end subroutine

    pure function add_name() result(name)
        character(len=32) :: name

        name = "add"
    end function

end module
