module operation_mksequence_m

    use value_m, only: value_t, value_trace_t, value_item_t
    use sequence_value_m
    use operation_m, only: operation_t
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

    function trace_mkseq(op, input_traces) result(output_trace)
        !! Produces a generic trace for any operation following
        !! the pattern: opname(trace_arg1, trace_arg2, ...)
        !> operation
        class(op_mkseq_t), intent(in) :: op
        !> operation inputs
        type(value_trace_t), intent(in) :: input_traces(:)
        !> string trace
        type(value_trace_t) :: output_trace
        integer :: i, num_inputs

        num_inputs = size(input_traces)
        output_trace % str = "["

        do i = 1, num_inputs
            associate (arg_trace => input_traces(i))
                output_trace % str = output_trace % str &
                    // trim(arg_trace % str) &
                    // adjustl(trim(merge(" ,", "] ", i < num_inputs)))
            end associate
        end do
    end function

    pure function mkseqd_name()
        character(len=32) :: mkseqd_name

        mkseqd_name = "array"
    end function

end module
