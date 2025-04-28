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

contains

elemental function real_value_from_real(value, trace) result(value_obj)
   !! build real_value_t object from float
   real(f64), intent(in) :: value
   type(value_trace_t), intent(in), optional :: trace
   type(real_value_t) :: value_obj
   character(len=64) :: buf

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

elemental subroutine parse_real(val, r, err)
   class(value_t), intent(in) :: val
   real(kind=real_k), intent(out) :: r
   type(err_t), intent(out), optional :: err

   select type(val)
   type is (real_value_t)
      r = val%value
   class default
      call seterr(err, "not a real value")
   end select
end subroutine

elemental subroutine parse_int(val, i, err)
   class(value_t), intent(in) :: val
   integer, intent(out) :: i
   type(err_t), intent(out), optional :: err

   select type(val)
   type is (real_value_t)
      i = nint(val%value)
   class default
      call seterr(err, "not a real value")
   end select
end subroutine

function real_ptr(val, err)
   class(value_t), pointer :: val
   type(err_t) :: err
   type(real_value_t), pointer :: real_ptr

   if (.not. associated(val)) then
      nullify(real_ptr)
      return
   end if

   select type (val)
   class is (real_value_t)
      real_ptr => val
   class default
      call seterr(err, "expected real value")
   end select
end function

end module
