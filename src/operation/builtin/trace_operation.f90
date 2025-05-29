module operation_trace_m

use value_m, only: value_t, value_trace_t, value_trace_t
use sequence_value_m
use operation_m
use str_value_m
use error_m
implicit none (type, external)
private

type, extends(operation_t) :: op_trace_t
contains
   procedure, nopass :: name
   procedure :: exec_one
   procedure, nopass :: is_elemental
   procedure, nopass :: get_info
end type

public :: op_trace_t

contains

subroutine exec_one(op, inputs, output, err)
   class(op_trace_t), intent(in) :: op !! operation
   type(value_ref_t), intent(in) :: inputs(:) !! operation inputs
   class(value_t), intent(out), allocatable :: output !! output/result
   type(err_t), intent(inout) :: err !! error

   type(str_value_t), allocatable :: result
   type(value_trace_t) :: trace

   if (size(inputs) /= 1) then
      call seterr( err, "trace expects exactly one argument" )
      return
   end if

   allocate(result)
   trace = inputs(1) % value % get_trace()
   result % value = trace % str

   call move_alloc(result, output)

end subroutine

pure function name()
   character(len=32) :: name

   name = "trace"
end function

pure function is_elemental()
   !! return true if the operation should be performed
   !! element-wise on the sequences
   logical :: is_elemental

   is_elemental = .false.
end function

pure subroutine get_info(argspec, help)
   type(arg_entry_t), intent(out), allocatable, optional :: argspec(:)
   character(len=:), intent(out), allocatable, optional :: help

   if (present(help)) &
      help = "Return the trace string of an object."

end subroutine

end module

