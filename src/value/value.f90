module value_m
use iso_fortran_env, only: debug_output => error_unit
use error_m
implicit none (type, external)
private

type value_trace_t
   character(len=:), allocatable :: str
contains
   procedure, private :: trace_write_formatted
   generic :: write(Formatted) => trace_write_formatted
   generic :: operator(==) => traces_are_equal
   procedure, private :: traces_are_equal
   generic :: operator(/=) => traces_are_not_equal
   procedure, private :: traces_are_not_equal
end type

public :: value_trace_t

type, abstract :: value_t
   type(value_trace_t) :: trace = value_trace_t()
contains
   procedure :: get_trace
   procedure :: to_str
   procedure :: write => value_write
   procedure :: cleanup => value_cleanup
end type

public :: value_t

type value_item_t
   class(value_t), allocatable :: value
contains
   procedure, non_overridable :: to_str =>  value_item_to_str
   procedure, non_overridable :: get_trace => get_trace_safe_item
end type

public :: value_item_t

type :: value_ref_t
   class(value_t), pointer :: value => null()
   integer, pointer, private :: refcounter => null()
   logical, private :: protect = .false.
contains
   procedure, non_overridable :: to_str => value_ref_to_str
   procedure, non_overridable :: get_trace => get_trace_safe_ref
end type

interface keep
module procedure keep_ref
module procedure keep_val
end interface

public :: value_ref_t
public :: item_to_ref, ref_to_item, keep, free, protect, num_references

contains

subroutine trace_write_formatted(trace,unit,iotype,v_list,iostat,iomsg)
   CLASS(value_trace_t), intent(in) :: trace
   integer, intent(in) :: unit
   character(len=*), intent(in) :: iotype
   integer, intent(in) :: v_list(:)
   integer, intent(out) :: iostat
   character(len=*), intent(inout) :: iomsg

   if (allocated(trace%str)) then
      write(unit, fmt='(a)', iostat=iostat, iomsg=iomsg)  trim(trace % str)
   end if

end subroutine

elemental function get_trace(value) result(trace)
   class(value_t), intent(in) :: value
   type(value_trace_t) :: trace

   if (allocated(value%trace%str)) then
      trace%str = value%trace%str
      return
   end if

   trace%str = value%to_str()

end function

recursive subroutine value_cleanup(val)
   class(value_t), intent(inout) :: val

   write (debug_output, *) "DEL ", val % to_str()
end subroutine

subroutine value_write(val, fn, err)
   class(value_t), intent(in) :: val
   character(len=*), intent(in) :: fn
   type(err_t), intent(out) :: err

   call seterr(err, "writing for this datatype is not implemented")
end subroutine

elemental function traces_are_equal(trace, other_trace)
   CLASS(value_trace_t), intent(in) :: trace, other_trace
   logical :: traces_are_equal

   if (allocated(trace % str) .and. allocated(other_trace % str)) then
      traces_are_equal = (trace % str) == (other_trace % str)
      return
   end if
   traces_are_equal = .false.
end function

elemental function traces_are_not_equal(trace, other_trace)
   CLASS(value_trace_t), intent(in) :: trace, other_trace
   logical :: traces_are_not_equal

   traces_are_not_equal = .not. traces_are_equal(trace, other_trace)
end function

pure function to_str(value) result(str)
   class(value_t), intent(in) :: value
   character(len=:), allocatable :: str

   error stop "no string representation for this value type. override to_str to implement"
end function

pure function get_trace_safe_ref(value_ref) result(trace)
   class(value_ref_t), intent(in) :: value_ref
   type(value_trace_t) :: trace

   if (associated(value_ref % value)) then
      trace = value_ref % value % get_trace()
   else
      trace % str = "(none)"
   end if
end function

pure function get_trace_safe_item(value_item) result(trace)
   class(value_item_t), intent(in) :: value_item
   type(value_trace_t) :: trace

   if (allocated(value_item % value)) then
      trace = value_item % value % get_trace()
   else
      trace % str = "(none)"
   end if
end function

function value_ref_to_str(value_ref) result(str)
   class(value_ref_t), intent(in) :: value_ref
   character(len=:), allocatable :: str
   character(len=12) :: suff

   if (.not. associated(value_ref%value)) then
      str = "null"
      return
   end if

   if (associated(value_ref%refcounter)) then
      write(suff, '(a,i0,a)') "[", value_ref%refcounter, "]"
   else
      suff = '?'
   end if

   if (value_ref%protect) suff = "!"

   str = trim(suff) // value_ref % value % to_str()

end function

function value_item_to_str(value_ref) result(str)
   class(value_item_t), intent(in) :: value_ref
   character(len=:), allocatable :: str
   character(len=12) :: suff

   if (.not. allocated(value_ref%value)) then
      str = "null"
      return
   end if

   str = value_ref % value % to_str()

end function

impure elemental function keep_ref(ref) result(kept)
   type(value_ref_t), intent(in) :: ref
   type(value_ref_t) :: kept

   if ( .not. associated(ref % value) ) then
      error stop "attempting to increase refcount of a null reference"
   end if

   if ( ref % protect ) then
      allocate( kept % value, source=ref % value )
   else
      kept % value => ref % value
   end if

   if (associated(ref % refcounter) ) then
      kept % refcounter => ref % refcounter
   else
      allocate( kept % refcounter )
      kept % refcounter = 0
   end if

   kept % refcounter = kept % refcounter + 1

end function

function keep_val(val) result(kept)
   class(value_t), intent(in), target :: val
   type(value_ref_t) :: kept

   kept % value => val
   allocate( kept % refcounter )
   kept % refcounter = 1

end function


impure elemental function protect(init_val) result(ref)
   type(value_ref_t) :: ref
   class(value_t), intent(in), target :: init_val

   ref % value => init_val
   ref % protect = .true.

end function


impure elemental subroutine free(ref)
   type(value_ref_t), intent(inout) :: ref

   if ( ref % protect ) return

   if ( associated(ref % refcounter) ) then

      ref % refcounter = ref % refcounter - 1

      if ( ref % refcounter >= 1 ) then
         nullify ( ref % value )
         nullify ( ref % refcounter )
         ref % protect = .false.
         return
      end if

      deallocate ( ref % refcounter )

   end if

   if ( .not. associated(ref % value) ) return

   call ref % value % cleanup()
   deallocate ( ref % value )

end subroutine

elemental function num_references(ref)
   type(value_ref_t), intent(in) :: ref
   integer :: num_references

   if (associated(ref % refcounter)) then
      num_references = ref % refcounter
   else
      num_references = -1
   end if
end function

impure elemental function item_to_ref(item) result(ref)
   type(value_item_t), intent(inout), target :: item
   type(value_ref_t) :: ref

   if (allocated(item % value)) then
      ref % value => item % value
      ref % protect = .true.
   end if
end function

impure elemental subroutine ref_to_item(ref, item)
   type(value_ref_t), intent(in) :: ref
   type(value_item_t), intent(out) :: item

   if (associated(ref % value)) then
      allocate(item % value, source=ref % value)
   end if
end subroutine

end module value_m
