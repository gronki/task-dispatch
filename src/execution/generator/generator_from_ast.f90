module generator_from_ast_m

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
   type(namespace_t), intent(inout), target :: namespace !! must be a target!
   type(operation_db_t), intent(in) :: operation_db
   type(err_t), intent(out), optional :: err

   type(op_generator_t), allocatable :: op_gen

   select case (val_expr % argtype)

   case (ARG_CONSTANT)
      gen = value_generator_t(val=val_expr % constant)

   case (ARG_REF)
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
   type(namespace_t), intent(inout), target :: namespace !! must be a target!
   type(operation_db_t), intent(in) :: operation_db
   type(err_t), intent(out), optional :: err

   class(operation_t), allocatable :: op
   type(op_generator_t_arg_t), allocatable :: args(:)
   integer :: i, nr_args

   nr_args = val_expr % num_args

   allocate (args(nr_args))
   do i = 1, nr_args
      call build_generator(val_expr % op_args(i), args(i) % gen, namespace, operation_db, err)
      if (check(err)) return
   end do

   if (allocated(val_expr % op)) then
      ! sometimes in testing/debugging we might use allocated operations
      allocate (op, source=val_expr % op)
   else
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
   type(namespace_t), intent(inout) :: namespace
   type(operation_db_t), intent(in) :: operation_db
   type(err_t), intent(out), optional :: err
   class(generator_t), allocatable, target :: gen

   call build_generator(stmt % rhs, gen, &
      operation_db=operation_db, &
      namespace=namespace, err=err)

   if (check(err)) return

   call gen % yield(result)
   if (check(err)) return

   if (.not. stmt % is_assignment) return

   call namespace % move_in(trim(stmt % lhs_refname), result, err)

end subroutine

end module
