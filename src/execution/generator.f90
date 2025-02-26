module generator_m

use value_m
use error_m
implicit none(type, external)
private

type, abstract :: generator_t
contains
   procedure(yield), deferred :: yield
   procedure(trace), deferred :: trace
end type generator_t

public :: generator_t

abstract interface
recursive subroutine yield(gen, val, err)
   import :: generator_t, value_t, err_t
   class(generator_t), intent(inout), target :: gen
   class(value_t), allocatable, intent(out) :: val
   type(err_t), intent(inout), optional :: err
end subroutine yield
recursive function trace(gen)
   import :: generator_t, value_trace_t
   class(generator_t), intent(in) :: gen
   type(value_trace_t) :: trace
end function trace
end interface

end module generator_m

!!  sum(square(range(5)))

module value_generator_m

use generator_m
use value_m
use error_m
implicit none (type, external)
private

type, extends(generator_t) :: value_generator_t
   class(value_t), allocatable :: val
contains
   procedure :: yield
   procedure :: trace
end type value_generator_t

public :: value_generator_t

contains

recursive subroutine yield(gen, val, err)
   class(value_generator_t), intent(inout), target :: gen !! must be a target!
   class(value_t), allocatable, intent(out) :: val
   type(err_t), intent(inout), optional :: err

   if (.not. allocated(gen % val)) &
      error stop "uninitialized value in generator"

   val = gen % val

   write (*, *) 'G: fetched: ', val % to_str()
end subroutine yield

recursive function trace(gen)
   class(value_generator_t), intent(in) :: gen
   type(value_trace_t) :: trace

   if (.not. allocated(gen % val)) &
      error stop "uninitialized value in generator"

   trace = gen % val % get_trace()
end function trace

end module value_generator_m

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

   write (*, *) 'G: fetched (by reference): ', val % to_str()
end subroutine yield

recursive function trace(gen)
   class(reference_generator_t), intent(in) :: gen
   type(value_trace_t) :: trace

   if (.not. associated(gen % val)) &
      error stop "uninitialized value reference in generator"

   trace = gen % val % get_trace()
end function trace

end module reference_generator_m

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
   type(input_key_t), allocatable :: arg_keys(:)
   type(argument_match_t), allocatable :: arg_match(:)
   integer :: num_args
   class(operation_t), allocatable :: op
contains
   procedure :: yield
   procedure :: trace
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

   allocate (evaluated_args(gen % num_args))

   do i = 1, gen % num_args
      call gen % args(i) % gen % yield(evaluated_args(i) % value)
   end do

   if (allocated(gen % arg_match)) then
      input_refs = connect_matched_args(item_to_ref(evaluated_args), gen % arg_match)
   else
      input_refs = item_to_ref(evaluated_args)
   end if

   write (*, *) 'G: evaluating ', gen % op % name(), '...'
   ! TODO: executing arrays item-wise should be moved to be performed here
   call gen % op % exec_trace(input_refs, val, err)
   write (*, *) 'G: evaluated  ', gen % op % name(), ' as ', val % to_str()

end subroutine

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

end module

module generator_from_ast_m

use command_m
use ast_m
use generator_m
use op_generator_m
use value_generator_m
use reference_generator_m
use operation_m
use value_m
use input_args_m
use namespace_m
use error_m
use line_error_m
use operation_database_m
use tokenizer_m, only: token_loc_t

implicit none(type, external)
private

public :: build_generator, execute_statement_generator

contains

recursive subroutine build_generator(val_expr, gen, namespace, operation_db, err)
   type(ast_expression_t), intent(in) :: val_expr
   class(generator_t), intent(out), allocatable :: gen
   type(namespace_t), intent(inout), target, optional :: namespace !! must be a target!
   type(operation_db_t), intent(in), optional :: operation_db
   type(err_t), intent(out), optional :: err

   type(op_generator_t), allocatable :: op_gen

   select case (val_expr % argtype)

   case (ARG_CONSTANT)
      gen = value_generator_t(val=val_expr % constant)

   case (ARG_REF)
      if (.not. present(namespace)) then
         error stop "no namespace provided so reference cannot be resolved: "//trim(val_expr % refname)
      end if

      block
         class(value_t), pointer :: fetched_ref
         call namespace % fetch_ptr(val_expr % refname, fetched_ref, err)
         if (check(err)) then
            call seterr(err, "here", loc=val_expr % loc)
            return
         end if
         gen = reference_generator_t(val=fetched_ref)
      end block

   case (ARG_CALL)
      allocate (op_gen)
      call op_generator_from_ast(val_expr, op_gen, namespace, operation_db, err)
      if (check(err)) return
      call move_alloc(op_gen, gen)

   case default
      print *, val_expr%argtype
      error stop "incorrect val_expr%argtype"
   end select

end subroutine

recursive subroutine op_generator_from_ast(val_expr, gen, namespace, operation_db, err)
   type(ast_expression_t), intent(in) :: val_expr
   class(op_generator_t), intent(out) :: gen
   type(namespace_t), intent(inout), target, optional :: namespace !! must be a target!
   type(operation_db_t), intent(in), optional :: operation_db
   type(err_t), intent(out), optional :: err

   class(operation_t), allocatable :: op
   type(op_generator_t_arg_t), allocatable :: args(:)
   integer :: i, nr_args

   nr_args = val_expr % num_args
   gen % num_args = val_expr % num_args
   gen % arg_keys = val_expr % op_arg_keys

   allocate (args(nr_args))
   do i = 1, nr_args
      call build_generator(val_expr % op_args(i), args(i) % gen, namespace, operation_db, err)
      if (check(err)) return
   end do

   if (allocated(val_expr % op)) then
      ! sometimes in testing/debugging we might use allocated operations
      allocate (op, source=val_expr % op)
   else
      if (.not. present(operation_db)) then
         error stop "could not resolve operation since database not provided: "//trim(val_expr % op_name)
      end if
      ! TODO: not sure how to solve this one. perhaps operations could return a dummy
      ! value just for the sake of indentifying the type? this would be much cleaner
      ! wayh of performing gthe operation matching without needing to write
      ! my own type system.
      call fetch_operation(operation_db, val_expr % op_name, op, err)

      if (check(err)) then
         call seterr(err, "here", val_expr % loc)
         return
      end if
   end if

   match_args: block
      type(arg_entry_t), allocatable :: argspec(:)

      call op % get_argspec(argspec)

      if (.not. allocated(argspec)) then
         exit match_args
      end if

      allocate(gen % arg_match(size(argspec)))
      call match_arguments(argspec, val_expr % op_arg_keys, gen % arg_match, err)

      if (check(err)) then
         call seterr(err, "here", val_expr % loc)
         return
      end if

   end block match_args

   call move_alloc(args, gen % args)
   call move_alloc(op, gen % op)

end subroutine

subroutine execute_statement_generator(stmt, namespace, operation_db, gen, result, err)
   type(ast_statement_t), intent(in) :: stmt
   class(value_t), intent(out), allocatable :: result
   type(namespace_t), intent(inout), optional :: namespace
   type(operation_db_t), intent(in), optional :: operation_db
   type(err_t), intent(out), optional :: err
   class(generator_t), allocatable, target :: gen

   call build_generator(stmt % rhs, gen, &
      operation_db=operation_db, &
      namespace=namespace, err=err)

   if (check(err)) return

   call gen % yield(result)
   if (check(err)) return

   if (.not. stmt % is_assignment) return

   if (.not. present(namespace)) then
      error stop "namespace required for assignment statements"
   end if

   call namespace % push(trim(stmt % lhs_refname), result, err)

end subroutine

end module
