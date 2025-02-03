module operation_multiply_m

use value_m, only: value_item_t
use operation_m
use real_value_m
implicit none (type, external)

type, extends(operation_t) :: multiply_t
contains
   procedure, nopass :: name => multiply_name
   procedure :: exec_one => exec_multiply
end type

contains

subroutine exec_multiply(op, inputs, output, err)
   class(multiply_t), intent(in) :: op !! operation
   type(value_ref_t), intent(in) :: inputs(:) !! operation inputs
   class(value_t), intent(out), allocatable :: output !! output/result
   type(err_t), intent(inout) :: err !! error

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

pure function multiply_name() result(name)
   character(len=32) :: name

   name = "mul"
end function

end module
