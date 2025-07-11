module operation_item_m

use value_m, only: value_t, value_trace_t, value_item_t
use sequence_value_m
use operation_m
use real_value_m
use error_m
implicit none (type, external)
private

type, extends(operation_t) :: op_item_t
contains
   procedure, nopass :: name => item_name
   procedure :: exec_one => exec_item
   ! procedure :: trace => trace_item
   procedure, nopass :: is_elemental
end type

public :: op_item_t

contains

subroutine exec_item(op, inputs, output, err)
   class(op_item_t), intent(in) :: op !! operation
   type(value_ref_t), intent(in) :: inputs(:) !! operation inputs
   class(value_t), intent(out), allocatable :: output !! output/result
   type(err_t), intent(inout) :: err !! error

   integer, allocatable :: indices(:)
   integer :: index_depth, i
   real(kind=real_k) :: index_real

   if (size(inputs) < 1) then
      call seterr( err, "item expects at least one argument" )
   else if (size(inputs) == 1) then
      allocate(output, source=inputs(1) % value)
      return
   end if

   index_depth = size(inputs) - 1
   allocate(indices(index_depth))

   do i = 1, index_depth
      call parse_number(inputs(i+1) % value, to_int=indices(i), err=err)
      if (check(err)) return
   end do

   call get_item_recursively(inputs(1) % value, indices, 1, output)

end subroutine

recursive subroutine get_item_recursively(input, indices, depth, output)
   class(value_t), intent(in) :: input
   integer, intent(in) :: indices(:)
   integer, intent(in) :: depth
   class(value_t), intent(out), allocatable :: output

   integer :: this_index

   ! we could do this differently, but this way is easier to produce
   ! meaningful error messages
   this_index = indices(depth)

   select type (input)
   class is (sequence_value_t)
      if (depth == size(indices)) then
         allocate(output, source=input%items(this_index)%value)
      else
         call get_item_recursively(input%items(this_index)%value, indices, depth + 1, output)
      end if

   class default
      error stop "error retrieving an item: not a sequence"
   end select

end subroutine

pure function item_name()
   character(len=32) :: item_name

   item_name = "item"
end function

pure function is_elemental()
   !! return true if the operation should be performed
   !! element-wise on the sequences
   logical :: is_elemental

   is_elemental = .false.
end function

end module
