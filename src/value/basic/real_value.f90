module real_value_m
use value_m
use, intrinsic :: iso_fortran_env, only: f64 => real64
use error_m

implicit none (type, external)
public

type, extends(value_t) :: real_value_t
   real(f64) :: value
contains
   procedure :: to_str => real_value_to_str
end type

interface real_value
module procedure :: real_value_from_real
module procedure :: real_value_from_str
end interface

integer, parameter :: real_k = f64

interface parse_number
module procedure :: as_real
module procedure :: as_int
end interface

contains

elemental function real_value_from_real(value, trace) result(value_obj)
   !! build real_value_t object from float
   real(f64), intent(in) :: value
   type(value_trace_t), intent(in), optional :: trace
   type(real_value_t) :: value_obj

   value_obj % value = value

   if (present(trace)) then
      value_obj % trace = trace
   end if

end function

elemental function real_value_from_str(value_str, trace) result(value_obj)
   !! build real_value_t object from string representation
   character(len=*), intent(in) :: value_str
   type(value_trace_t), intent(in), optional :: trace
   type(real_value_t) :: value_obj

   read(value_str, *) value_obj%value

   if (present(trace)) then
      value_obj % trace = trace
   end if

end function

pure function real_value_to_str(value) result(str)
   class(real_value_t), intent(in) :: value
   character(len=64) :: buf
   character(len=:), allocatable :: str

   write (buf, *) real(value%value)
   str = trim(adjustl(buf))
end function

pure subroutine as_int(val, to_int, err, errmsg)
   class(value_t), pointer, intent(in) :: val
   integer, intent(out) :: to_int
   type(err_t), intent(inout), optional :: err
   character(len=*), intent(in), optional :: errmsg

   if (.not. associated(val)) then
      call seterr(err, "null pointer given when numeric value expected")
      return
   end if

   select type(val)
   type is (real_value_t)
      to_int = nint(val%value)
   class default
      if (present(errmsg)) then
         call seterr(err, errmsg)
      else
         call seterr(err, "not an int value")
      end if
   end select
end subroutine

pure subroutine as_real(val, to_real, err, errmsg)
   class(value_t), pointer, intent(in) :: val
   real(kind=real_k), intent(out) :: to_real
   type(err_t), intent(inout), optional :: err
   character(len=*), intent(in), optional :: errmsg

   if (.not. associated(val)) then
      call seterr(err, "null pointer given when numeric value expected")
      return
   end if

   select type(val)
   type is (real_value_t)
      to_real = val%value
   class default
      if (present(errmsg)) then
         call seterr(err, errmsg)
      else
         call seterr(err, "not a real value")
      end if
   end select
end subroutine

end module
