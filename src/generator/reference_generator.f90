module reference_generator_m

use generator_m
use value_m
use error_m
implicit none(type, external)
private

type, extends(generator_t) :: reference_generator_t
   class(value_t), pointer :: val
contains
   procedure :: yield
   procedure :: yield_ref
   procedure :: trace
end type reference_generator_t

public :: reference_generator_t

contains

recursive subroutine yield(gen, val, err)
   class(reference_generator_t), intent(inout), target :: gen !! must be a target!
   class(value_t), allocatable, intent(out) :: val
   type(err_t), intent(inout), optional :: err

   if (.not. associated(gen % val)) &
      error stop "uninitialized value reference in generator"

   val = gen % val

end subroutine yield

recursive subroutine yield_ref(gen, ref, err)
   class(reference_generator_t), intent(inout), target :: gen
   type(value_ref_t), intent(out) :: ref
   type(err_t), intent(inout), optional :: err

   if (.not. associated(gen % val)) &
      error stop "uninitialized value reference in generator"

   ref = protect(gen % val)

end subroutine yield_ref

recursive function trace(gen)
   class(reference_generator_t), intent(in) :: gen
   type(value_trace_t) :: trace

   if (.not. associated(gen % val)) &
      error stop "uninitialized value reference in generator"

   trace = gen % val % get_trace()

end function trace

end module reference_generator_m
