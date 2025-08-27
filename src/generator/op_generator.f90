module op_generator_m

use generator_m
use operation_m, only: input_key_t, operation_t
use value_m
use error_m
use input_args_m
implicit none(type, external)
private

type :: op_generator_t_arg_t
   class(generator_t), allocatable :: gen
end type op_generator_t_arg_t

type, extends(generator_t) :: op_generator_t
   type(op_generator_t_arg_t), allocatable :: args(:)
   type(argument_match_t), allocatable :: arg_match(:)
   class(operation_t), allocatable :: op
   type(value_item_t), allocatable :: result
contains
   procedure :: yield
   procedure :: yield_ref
   procedure :: trace
   procedure :: cleanup
end type op_generator_t

public :: op_generator_t, op_generator_t_arg_t

contains

recursive subroutine yield(gen, val, err)
   class(op_generator_t), intent(inout), target :: gen !! must be a target!
   class(value_t), allocatable, intent(out) :: val
   type(err_t), intent(inout), optional :: err

   type(value_item_t), allocatable, target :: evaluated_args(:)
   type(value_ref_t), allocatable :: input_refs(:)
   integer :: i, num_args

   if (.not. allocated(gen % args)) &
      error stop "uninitialized op generator args"

   num_args = size(gen % args)

   allocate (evaluated_args(num_args), input_refs(num_args))

   do i = 1, num_args
      call gen % args(i) % gen % yield_ref(input_refs(i), err)
      if (check(err)) return
   end do

   if (allocated(gen % arg_match)) then
      input_refs = connect_matched_args(input_refs, gen % arg_match)
   end if

   call gen % op % exec_trace(input_refs, val, err)

   do i = 1, num_args
      call gen % args(i) % gen % cleanup
   end do

end subroutine

recursive subroutine yield_ref(gen, ref, err)
   class(op_generator_t), intent(inout), target :: gen
   type(value_ref_t), intent(out) :: ref
   type(err_t), intent(inout), optional :: err

   if (.not. allocated(gen % result)) allocate(gen % result)
   call yield(gen, gen % result % value, err)
   if (check(err)) return

   ref = protect(gen % result % value)
   ref % item => gen % result

end subroutine yield_ref

recursive function trace(gen)
   class(op_generator_t), intent(in) :: gen
   type(value_trace_t) :: trace

   type(value_trace_t), allocatable :: input_traces(:)
   integer :: i, num_args

   if (.not. allocated(gen % args)) &
      error stop "uninitialized op generator args"

   num_args = size(gen % args)

   input_traces = [ (gen % args(i) % gen % trace(), i = 1, num_args) ]

   if (allocated(gen % arg_match)) then
      trace = gen % op % trace(connect_matched_traces(input_traces, gen % arg_match))
   else
      trace = gen % op % trace(input_traces)
   end if

end function


impure elemental subroutine cleanup(gen)
   class(op_generator_t), intent(inout) :: gen

   if (allocated(gen % result)) deallocate(gen % result)
end subroutine

end module
