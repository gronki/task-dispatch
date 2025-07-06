module simple_prompt_m

use runner_m

type, extends(console_prompt_t) :: simple_prompt_t
contains
   procedure :: input => simple_prompt
end type

contains

subroutine simple_prompt(prompt, line, eof)
   class(simple_prompt_t) :: prompt
   character(len=*), intent(out) :: line
   logical, intent(out) :: eof
   integer :: iostat

   write (*, '(a)', advance='no') '> '
   read (*, '(a)', iostat=iostat) line
   eof = iostat /= 0
end subroutine

end module
