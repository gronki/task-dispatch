module generator_m

use value_m
use error_m
implicit none(type, external)
private

type, abstract :: generator_t
contains
   procedure(yield), deferred :: yield
   procedure(yield_ref), deferred :: yield_ref
   procedure(trace), deferred :: trace
   procedure :: cleanup
end type generator_t

public :: generator_t

abstract interface
recursive subroutine yield(gen, val, err)
   import :: generator_t, value_t, err_t
   class(generator_t), intent(inout), target :: gen
   class(value_t), allocatable, intent(out) :: val
   type(err_t), intent(inout), optional :: err
end subroutine yield
recursive subroutine yield_ref(gen, ref, err)
   import :: generator_t, value_ref_t, err_t
   class(generator_t), intent(inout), target :: gen
   type(value_ref_t), intent(out) :: ref
   type(err_t), intent(inout), optional :: err
end subroutine yield_ref
recursive function trace(gen)
   import :: generator_t, value_trace_t
   class(generator_t), intent(in) :: gen
   type(value_trace_t) :: trace
end function trace
end interface

contains

impure elemental subroutine cleanup(gen)
   class(generator_t), intent(inout) :: gen
end subroutine

end module generator_m
