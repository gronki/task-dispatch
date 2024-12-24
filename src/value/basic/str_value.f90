module str_value_m
use value_m
use, intrinsic :: iso_fortran_env, only: f64 => real64

implicit none (type, external)
public

type, extends(value_t) :: str_value_t
   character(len=:), allocatable :: value
contains
   procedure :: to_str => str_value_to_str
end type

contains

pure function escape_str(str) result (escaped)
   !! returns a string in syntax representation, surronded with
   !! quotation marks and escaped
   character(len=*), intent(in) :: str
   !!      string to be escaped
   character(len=:), allocatable :: escaped
   !!      return value

   escaped =  """" // str // """"
end function

elemental function str_value(value, trace) result(value_obj)
   character(len=*), intent(in) :: value
   character(len=*), intent(in), optional :: trace
   type(str_value_t) :: value_obj

   value_obj % value = value

   if (present(trace)) then
      value_obj % trace % str = trim(adjustl(trace))
   end if
end function

pure function str_value_to_str(value) result(str)
   class(str_value_t), intent(in) :: value
   character(len=:), allocatable :: str

   if (allocated(value%value)) then
      str = escape_str(value%value)
      return
   end if

   str = """"""
end function


end module
