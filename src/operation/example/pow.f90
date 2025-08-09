module operation_power_m

use value_m, only: value_item_t
use operation_m
use real_value_m
use input_args_m

implicit none (type, external)
private

type, extends(operation_t) :: op_pow_t
contains
   procedure, nopass :: name => powd_name
   procedure :: exec_one => exec_pow
   procedure, nopass :: get_info
end type

public :: op_pow_t

type(arg_entry_t), target :: pow_argspec(2) = [ &
   arg_entry_t(pos=1, name="x"), &
   arg_entry_t(pos=2, name="exponent") &
&]

contains

subroutine exec_pow(op, inputs, output, err)
   class(op_pow_t), intent(in) :: op !! operation
   type(value_ref_t), intent(in) :: inputs(:) !! operation inputs
   class(value_t), intent(out), allocatable :: output !! output/result
   type(err_t), intent(inout) :: err !! error
   real(kind=f64) :: x, exponent

   call parse_number(inputs(1) % value, to_real=x, err=err)
   call parse_number(inputs(2) % value, to_real=exponent, err=err)

   if (check(err)) return

   output = real_value(x**exponent)

end subroutine

pure function powd_name() result(name)
   character(len=32) :: name

   name = "pow"
end function

subroutine get_info(argspec, help)
   type(arg_entry_t), intent(out), allocatable, optional :: argspec(:)
   character(len=:), intent(out), allocatable, optional :: help

   if (present(argspec)) &
      argspec = pow_argspec

   if (present(help)) &
      help = "Performs the power operation on a scalar."

end subroutine

end module
