module operation_zip_m

use value_m
use sequence_value_m
use operation_m
use error_m
implicit none (type, external)
private

type, extends(operation_t) :: op_zip_t
contains
   procedure, nopass :: name
   procedure :: exec_one
   procedure, nopass :: is_elemental
end type

public :: op_zip_t

contains

subroutine exec_one(op, inputs, output, err)
   class(op_zip_t), intent(in) :: op !! operation
   type(value_ref_t), intent(in) :: inputs(:) !! operation inputs
   class(value_t), intent(out), allocatable :: output !! output/result
   type(err_t), intent(inout) :: err !! error

   allocate(sequence_value_t :: output)
   select type (output)
   type is (sequence_value_t)
      allocate(output%items(size(inputs)))
      call ref_to_item(inputs, output%items)
   end select

end subroutine

pure function name()
   character(len=32) :: name

   name = "zip"
end function

pure function is_elemental()
   !! return true if the operation should be performed
   !! element-wise on the sequences
   logical :: is_elemental

   is_elemental = .true.
end function is_elemental

end module
