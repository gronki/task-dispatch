module operation_range_m

use value_m, only: value_item_t
use sequence_value_m, only: sequence_value_t
use operation_m
use real_value_m
use error_m
implicit none (type, external)

type, extends(operation_t) :: op_range_t
contains
   procedure, nopass :: name => range_name
   procedure :: exec_one => exec_range
end type

contains

subroutine exec_range(op, inputs, output, err)
   class(op_range_t), intent(in) :: op !! operation
   type(value_ref_t), intent(in) :: inputs(:) !! operation inputs
   class(value_t), intent(out), allocatable :: output !! output/result
   type(err_t), intent(inout) :: err !! error

   integer :: i
   type(sequence_value_t) :: result
   real(kind=real_k) :: lo, hi, n_steps_f
   integer :: n_steps

   select case (size(inputs))
   case (1)
      call parse_real(inputs(1) % value, n_steps_f)
      n_steps = int(n_steps_f)
      lo = 1
      hi = n_steps
   case (3)
      call parse_real(inputs(1) % value, lo)
      call parse_real(inputs(2) % value, hi)
      call parse_real(inputs(3) % value, n_steps_f)
      n_steps = int(n_steps_f)
   case default
      call seterr( err, "range arguments: (start, stop, n_steps)" )
      return
   end select

   if (n_steps < 1) then
      call seterr( err, "n_steps must be positive" )
      return
   end if

   allocate(result%items(n_steps))

   if (n_steps == 1) then
      allocate(result % items(1) % value, &
         source=real_value((hi + lo) / 2))
   else
      do i = 1, n_steps
         allocate(result % items(i) % value, &
            source=real_value(lo + (hi - lo) * (i - 1) / (n_steps - 1)))
      end do
   end if

   output = result

end subroutine

pure function range_name() result(name)
   character(len=32) :: name

   name = "range"
end function

end module
