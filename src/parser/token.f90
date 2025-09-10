module token_m

implicit none (type, external)
private


integer, parameter, public :: token_none = 0, token_delim = 1, &
   token_ident = 2, token_num_literal = 3, &
   token_str_literal = 4, token_end = 5

type, public :: token_loc_t
   integer :: line = -1, offset = -1
contains
   procedure, pass(loc) :: token_loc_write_fmt
   generic :: write(formatted) => token_loc_write_fmt
   procedure, pass(loc) :: token_loc_read_fmt
   generic :: read(formatted) => token_loc_read_fmt
end type

private :: token_loc_write_fmt, token_loc_read_fmt

type, public :: token_t
   integer :: type = token_none
   character(len=:), allocatable :: value
   type(token_loc_t) :: loc = token_loc_t()
end type


interface operator(==)
module procedure :: tokens_are_equal
end interface

interface operator(/=)
module procedure :: tokens_are_not_equal
end interface

public :: operator(==), operator(/=)

contains


elemental function tokens_are_equal(token1, token2) result(are_equal)
!! Tests for the equality of two tokens.

   type(token_t), intent(in) :: token1 !! lhs
   type(token_t), intent(in) :: token2 !! rhs
   logical :: are_equal !! ``.TRUE.`` if tokens are equal

   are_equal = token1 % type == token2 % type
   if (.not. are_equal) return

   if (token1 % type == token_end) return

   if (.not. allocated(token1 % value)) &
      error stop "one of compared tokens has unallocated value"

   if (.not. allocated(token2 % value)) &
      error stop "one of compared tokens has unallocated value"

   are_equal = are_equal &
      .and. (len(token1 % value) == len(token2 % value)) &
      .and. (token1 % value == token2 % value)
end function

elemental function tokens_are_not_equal(token1, token2) result(are_not_equal)
   type(token_t), intent(in) :: token1
   type(token_t), intent(in) :: token2
   logical :: are_not_equal

   are_not_equal = .not. tokens_are_equal(token1, token2)
end function

subroutine token_loc_write_fmt(loc,unit,iotype,v_list,iostat,iomsg)
   CLASS(token_loc_t), intent(in) :: loc
   integer, intent(in) :: unit
   character(len=*), intent(in) :: iotype
   integer, intent(in) :: v_list(:)
   integer, intent(out) :: iostat
   character(len=*), intent(inout) :: iomsg
   character(len=*), parameter :: token_loc_fmt = '(a1,i5.5,1x,a1,i3.3,a1)'

   write(unit, fmt=token_loc_fmt, iostat=iostat, iomsg=iomsg) &
      "L", max(loc%line, 0), "C", max(loc%offset, 0), ":"
! We add an extra character because gfortran 14 seems to
! have a bug causing the last character to be lost
! on reading by DTIO.
end subroutine

subroutine token_loc_read_fmt(loc,unit,iotype,v_list,iostat,iomsg)
   CLASS(token_loc_t), intent(inout) :: loc
   integer, intent(in) :: unit
   character(len=*), intent(in) :: iotype
   integer, intent(in) :: v_list(:)
   integer, intent(out) :: iostat
   character(len=*), intent(inout) :: iomsg
   character(len=80) :: buffer
   integer :: i
   character(len=*), parameter :: token_loc_fmt = '(a1,i5.5,1x,a1,i3.3,x)'

   character(len=1) :: read_l, read_c

   read(unit, fmt=token_loc_fmt, iostat=iostat, iomsg=iomsg) &
      read_l, loc%line, read_c, loc%offset
   if (iostat /= 0) return

   if (read_l /= "L" .or. read_c /= "C") then
      iostat = 1
      iomsg = "Incorrect format! Expected: L00000 C000"
   end if

end subroutine

end module
