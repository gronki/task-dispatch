module operation_mksequence_m

use value_m
use sequence_value_m
use operation_m
use error_m
implicit none (type, external)

type, extends(operation_t) :: op_mkseq_t
contains
   procedure, nopass :: name => mkseq_name
   procedure :: exec_one => exec_mkseq
   procedure :: trace => trace_mkseq
end type

contains

subroutine exec_mkseq(op, inputs, keys, output, err)
   class(op_mkseq_t), intent(in) :: op !! operation
   type(value_ref_t), intent(in) :: inputs(:) !! operation inputs
   type(input_key_t), intent(in) :: keys(:) !! input keywords
   class(value_t), intent(out), allocatable :: output !! output/result
   type(err_t), intent(inout) :: err !! error

   allocate(sequence_value_t :: output)
   select type (output)
   type is (sequence_value_t)
      allocate(output%items(size(inputs)))
      call ref_to_item(inputs, output%items)
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

pure function mkseq_name()
   character(len=32) :: mkseq_name

   mkseq_name = "array"
end function

end module
