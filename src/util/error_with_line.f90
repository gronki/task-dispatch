module line_error_m

use error_m
use tokenizer_m, only: token_loc_t

implicit none (type, external)
private

interface seterr
module procedure :: seterr_loc
end interface

public :: seterr

type :: error_with_line_t
   type(err_t), allocatable :: err
   character(len=1024) :: line
   logical :: print_line = .true.
   character(len=:), allocatable :: error_prefix
contains
   procedure :: error_with_line_write_formatted
   generic :: write(formatted) => error_with_line_write_formatted
end type

interface error_with_line
module procedure :: new_error_with_line
end interface

public :: error_with_line


contains

pure subroutine seterr_loc(err, message, loc)
   type(err_t), intent(out), optional :: err
   character(len=*), intent(in) :: message
   type(token_loc_t), intent(in) :: loc

   character(len=1024) :: message_with_loc

   if (loc%line == -1 .and. loc%offset == -1) then
      call seterr(err, message)
      return
   end if

   write(message_with_loc, '(dt,2x,a)') loc, trim(message)

   call seterr(err, trim(message_with_loc))
end subroutine

pure function new_error_with_line(err, line, print_line, error_prefix)
   type(err_t), intent(in) :: err
   character(len=*), intent(in) :: line
   logical, intent(in), optional :: print_line
   character(len=*), intent(in), optional :: error_prefix
   type(error_with_line_t) :: new_error_with_line

   new_error_with_line%err = err
   new_error_with_line%line = line

   if (present(print_line)) then
      new_error_with_line%print_line = print_line
   end if

   if (present(error_prefix)) then
      new_error_with_line%error_prefix = error_prefix
   else
      new_error_with_line%error_prefix = "error: "
   end if

end function

subroutine error_with_line_write_formatted(err_w_line,unit,iotype,v_list,iostat,iomsg)
   class(error_with_line_t), intent(in) :: err_w_line
   integer, intent(in) :: unit
   character(len=*), intent(in) :: iotype
   integer, intent(in) :: v_list(:)
   integer, intent(out) :: iostat
   character(len=*), intent(inout) :: iomsg

   integer :: ierr

   if (.not. allocated(err_w_line%err)) return
   if (.not. allocated(err_w_line%err%messages)) return

   do ierr = 1, size(err_w_line%err%messages)
      associate (message_text => err_w_line%err%messages(ierr)%text)
         if (ierr > 1) then
            write(unit, '(/)', iostat=iostat, iomsg=iomsg)
            if (iostat /= 0) return
         end if

         call write_one_error_line(err_w_line, unit, message_text, iostat, iomsg)
         if (iostat /= 0) return
      end associate
   end do
end subroutine

subroutine write_one_error_line(err_w_line, unit, message_text, iostat, iomsg)

   class(error_with_line_t), intent(in) :: err_w_line
   integer, intent(in) :: unit
   character(len=*), intent(in) :: message_text
   integer, intent(out) :: iostat
   character(len=*), intent(inout) :: iomsg

   integer :: ierr, read_iostat
   type(token_loc_t) :: loc
   character(len=1024) :: message_body

   read (message_text, '(dt, 2x, a)', iostat=read_iostat) loc, message_body

   if (read_iostat /= 0) then
      ! no line info
      write(unit, '(a, a)', iostat=iostat, iomsg=iomsg) &
         err_w_line%error_prefix, trim(message_text)
      return
   end if

   if (err_w_line % print_line) then
      write(unit, '(a,a/)', iostat=iostat, iomsg=iomsg) &
         err_w_line%error_prefix, trim(err_w_line%line)
      if (iostat /= 0) return
   end if

   write(unit, '(a, a, a, a)', iostat=iostat, iomsg=iomsg) &
      err_w_line%error_prefix, repeat(" ", loc%offset-1), "^ ", trim(message_body)

end subroutine

end module
