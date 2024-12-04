module command_m

    use operation_m
    use ast_m
    use value_m
    use namespace_m
    use error_m
    use line_error_m
    use operation_database_m
    use tokenizer_m, only: token_loc_t

    implicit none (type, external)
    private

    public :: evaluate_expression, execute_statement

contains

    recursive subroutine evaluate_expression(val_expr, result_value, namespace, operation_db, err)
        type(ast_expression_t), intent(in) :: val_expr
        class(value_t), intent(inout), allocatable :: result_value
        type(namespace_t), intent(in), optional :: namespace
        type(operation_db_t), intent(in), optional :: operation_db
        type(err_t), intent(out), optional :: err

        select case(val_expr % argtype)

          case (ARG_CONSTANT)
            result_value = val_expr % constant

          case (ARG_REF)
            associate (refname => val_expr % refname)
                if (.not. present(namespace)) then
                    error stop "no namespace provided so reference cannot be resolved: " // trim(refname)
                end if
                call namespace % fetch(refname, result_value, err)
                if (check(err)) then
                    call seterr(err, "here", loc=val_expr%loc)
                    return
                end if
            end associate

          case (ARG_CALL)
            call evaluate_operation_call(val_expr, result_value, namespace, operation_db, err)
            if (check(err)) return

          case default
            error stop
        end select

        print *, "evaluated ", result_value % get_trace(), " as ", result_value%to_str()
    end subroutine


    recursive subroutine evaluate_operation_call(val_expr, result_value, namespace, operation_db, err)
        type(ast_expression_t), intent(in) :: val_expr
        class(value_t), intent(inout), allocatable :: result_value
        type(namespace_t), intent(in), optional :: namespace
        type(operation_db_t), intent(in), optional :: operation_db
        type(err_t), intent(out), optional :: err

        class(operation_t), allocatable :: op
        type(value_item_t), allocatable, target :: arg_values(:)
        integer :: i, nr_args

        nr_args = size(val_expr % op_args)

        allocate(arg_values(nr_args))
        do i = 1, nr_args
            call evaluate_expression(val_expr % op_args(i), arg_values(i) % value, namespace, operation_db, err)
            if (check(err)) return
        end do

        if (allocated(val_expr % op)) then
            ! sometimes in testing/debugging we might use allocated operations
            allocate(op, source=val_expr % op)
        else
            if (.not. present(operation_db)) then
                error stop "could not resolve operation since database not provided: " // trim(val_expr%op_name)
            end if
            call fetch_operation(operation_db, val_expr % op_name, op, err)
            if (check(err)) then
                call seterr(err, "here", val_expr % loc)
                return
            end if
        end if

        write (*,*) 'TRACE ', op % trace([(arg_values(i) % value % get_trace(), i = 1, nr_args)])

        ! here should be checked if this call result is already cached
        ! if not, execute the operation and cache the result
        call op % exec_trace(item_to_ref(arg_values), result_value)
        ! call namespace % set_cached(result % value)

    end subroutine

    subroutine execute_statement(stmt, result_value, namespace, operation_db, err)
        type(ast_statement_t), intent(in) :: stmt
        class(value_t), intent(inout), allocatable :: result_value
        type(namespace_t), intent(inout), optional :: namespace
        type(operation_db_t), intent(in), optional :: operation_db
        type(err_t), intent(out), optional :: err

        call evaluate_expression(stmt%rhs, result_value, namespace, operation_db, err)
        if (check(err)) return

        if (.not. stmt%is_assignment) return

        if (.not. present(namespace)) then
            error stop "namespace required for assignment statements"
        end if

        call namespace%push(trim(stmt % lhs_refname), result_value, err)

    end subroutine


end module
