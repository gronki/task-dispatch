module operation_sum_m

    use value_m
    use sequence_value_m
    use operation_m, only: operation_t
    use real_value_m
    implicit none (type, external)

    type, extends(operation_t) :: sum_t
    contains
        procedure, nopass :: name => sum_name
        procedure, nopass :: is_elemental => sum_is_elemental
        procedure :: exec => exec_sum
    end type sum_t

contains

    subroutine exec_sum(op, inputs, output)
        class(sum_t), intent(in) :: op
        type(value_ref_t), intent(in) :: inputs(:)
        class(value_t), intent(out), allocatable :: output
        integer :: i
        type(real_value_t), allocatable :: result

        if (size(inputs) /= 1) &
            error stop "sum: exacly one argument required"

        allocate(result)
        result % value = 0

        select type(seq => inputs(1) % value)
          class is (sequence_value_t)
            do i = 1, size(seq % items)
                select type (item_value => seq % items(i) % value)
                  type is (real_value_t)
                    result % value = result % value + item_value % value
                  class default
                    error stop "sum: numeric values expected"
                end select
            end do
          class default
            error stop "sum: sequence expected"
        end select

        call move_alloc(result, output)

    end subroutine exec_sum

    pure function sum_name() result(name)
        character(len=32) :: name

        name = "sum"
    end function sum_name

    pure function sum_is_elemental()
        !! return true if the operation should be performed
        !! element-wise on the sequences
        logical :: sum_is_elemental

        sum_is_elemental = .false.
    end function sum_is_elemental

end module operation_sum_m
