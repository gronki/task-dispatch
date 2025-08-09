module operation_square_m

use value_m, only: value_item_t
use operation_m
use real_value_m
use input_args_m

implicit none (type, external)
private

type, extends(operation_t) :: square_t
contains
   procedure, nopass :: name => squared_name
   procedure :: exec_one => exec_square
   procedure, nopass :: get_info
end type

public :: square_t

contains

subroutine exec_square(op, inputs, output, err)
   class(square_t), intent(in) :: op !! operation
   type(value_ref_t), intent(in) :: inputs(:) !! operation inputs
   class(value_t), intent(out), allocatable :: output !! output/result
   type(err_t), intent(inout) :: err !! error

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

subroutine get_info(argspec, help)
   type(arg_entry_t), intent(out), allocatable, optional :: argspec(:)
   character(len=:), intent(out), allocatable, optional :: help

   if (present(argspec)) then
      argspec = [ &
         arg_entry_t(pos=1, name="x") &
      &]
   end if

   if (present(help)) then
      help = "Raises scalar to the power of 2."
   end if

end subroutine

end module
