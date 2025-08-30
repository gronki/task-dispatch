module sequence_value_m

use value_m

implicit none (type, external)
private

type, extends(value_t) :: sequence_value_t
   type(value_item_t), allocatable :: items(:)
contains
   procedure :: to_str => sequence_to_str
   procedure :: get_trace => sequence_get_trace
end type

public :: sequence_value_t

contains

pure function sequence_to_str(value) result(str)
   class(sequence_value_t), intent(in) :: value
   character(len=64) :: buf
   character(len=:), allocatable :: str

   integer :: i

   str = "["

   if (allocated(value%items)) then
      do i =1, size(value%items)
         str = str // trim(value%items(i) % value % to_str()) &
            // adjustl(trim(merge("  ", " ,", i == size(value%items))))
      end do
   end if

   str = str // "]"
end function

elemental function sequence_get_trace(value) result(trace)
   class(sequence_value_t), intent(in) :: value
   type(value_trace_t) :: trace, item_trace
   integer :: i

   if (allocated(value%trace%str)) then
      trace%str = value%trace%str
      return
   end if

   trace%str = "array("

   if (allocated(value%items)) then
      do i =1, size(value%items)
         item_trace = value % items(i) % value % get_trace()
         trace%str = trace%str // trim(item_trace % str) &
            // adjustl(trim(merge("  ", " ,", i == size(value%items))))
      end do
   end if

   trace%str = trace%str // ")"

end function


end module sequence_value_m
