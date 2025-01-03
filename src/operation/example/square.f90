module operation_square_m

use value_m, only: value_item_t
use operation_m
use real_value_m
implicit none (type, external)

type, extends(operation_t) :: square_t
contains
   procedure, nopass :: name => squared_name
   procedure :: exec_one => exec_square
end type

contains

subroutine exec_square(op, inputs, keys, output, err)
   class(square_t), intent(in) :: op !! operation
   type(value_ref_t), intent(in) :: inputs(:) !! operation inputs
   type(input_key_t), intent(in) :: keys(:) !! input keywords
   class(value_t), intent(out), allocatable :: output !! output/result
   type(err_t), intent(inout) :: err !! error

   if (size(inputs) /= 1) &
      error stop "squared expects only one input"

   select type (val => inputs(1) % value)
   type is (real_value_t)
      output = real_value_t(value=val % value**2)
      return
   end select

   error stop "unexpected input type for square"

end subroutine

pure function squared_name() result(name)
   character(len=32) :: name

   name = "squared"
end function

end module
