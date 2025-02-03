module operation_add_m

use value_m, only: value_item_t
use operation_m
use real_value_m
use error_m
implicit none (type, external)

type, extends(operation_t) :: add_t
contains
   procedure, nopass :: name => add_name
   procedure :: exec_one => exec_add
end type

contains

subroutine exec_add(op, inputs, output, err)
   class(add_t), intent(in) :: op !! operation
   type(value_ref_t), intent(in) :: inputs(:) !! operation inputs
   class(value_t), intent(out), allocatable :: output !! output/result
   type(err_t), intent(inout) :: err !! error

   integer :: i
   type(real_value_t) :: result

   if (size(inputs) == 0) then
      call seterr( err, "add: at least one argument required")
      return
   end if

   result % value = 0

   do i = 1, size(inputs)
      select type (val => inputs(i) % value)
      type is (real_value_t)
         result % value = result % value + val % value
      class default
         call seterr( err, "add: unexpected input type" )
      end select
   end do

   output = result

end subroutine

pure function add_name() result(name)
   character(len=32) :: name

   name = "add"
end function

end module
