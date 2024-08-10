module operation_mksequence_m

    use value_base_m, only: value_t, value_item_t
    use operation_m, only: operation_t
    use sequence_value_m
    implicit none (type, external)

    type, extends(operation_t) :: op_mkseq_t
    contains
        procedure, nopass :: name => mkseqd_name
        procedure :: exec => exec_mkseq
        procedure :: trace => trace_mkseq
    end type

contains

    subroutine exec_mkseq(op, inputs, output)
        class(op_mkseq_t), intent(in) :: op
        type(value_item_t), intent(in) :: inputs(:)
        class(value_t), intent(out), allocatable :: output

        allocate(sequence_value_t :: output)
        select type (output)
          type is (sequence_value_t)
            allocate(output%items, source=inputs)
        end select

    end subroutine

    function trace_mkseq(op, inputs) result(output_trace)
        class(op_mkseq_t), intent(in) :: op
        type(value_item_t), intent(in) :: inputs(:)
        character(len=:), allocatable :: output_trace
        integer :: i

        output_trace = "["

        do i =1, size(inputs)
            output_trace = output_trace // trim(inputs(i) % value % get_trace()) &
                // adjustl(trim(merge("  ", " ,", i == size(inputs))))
        end do

        output_trace = output_trace // "]"

    end function

    pure function mkseqd_name()
        character(len=32) :: mkseqd_name

        mkseqd_name = "array"
    end function

end module
