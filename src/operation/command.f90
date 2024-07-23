module command_m

    use operation_m
    use value_base_m
    use namespace_m
    use error_m
    implicit none (type, external)

    integer, parameter :: ARG_UNINITIALIZED = 0, ARG_CONSTANT=1, ARG_REF=2, ARG_CALL=3

    type ast_expression_t
        integer :: argtype = ARG_UNINITIALIZED
        class(value_t), allocatable :: constant
        type(ast_symbol_ref_t), allocatable :: reference
        type(ast_operation_call_t), allocatable :: op_call
    end type

    interface ast_expression
        module procedure ast_expr_const
        module procedure ast_expr_ref
        module procedure ast_expr_op_args
        module procedure ast_expr_call
    end interface

    private :: ast_expr_const, ast_expr_ref, ast_expr_op_args, ast_expr_call

    type ast_symbol_ref_t
        !! stores reference to a symbol
        character(len=32) :: refname
    end type

    type input_key_t
        logical :: has_key = .false.
        character(len=32) :: key = ""
    end type

    type ast_operation_call_t
        class(operation_t), allocatable :: op
        character(len=32) :: opname
        type(ast_expression_t), allocatable :: args(:)
        type(input_key_t), allocatable :: keys(:)
    end type

    type ast_statement_t
        type(ast_expression_t) :: rhs
        logical :: is_assignment = .false.
        type(ast_symbol_ref_t) :: lhs
    end type

    type operation_db_entry_t
        class(operation_t), allocatable :: op
        character(len=32) :: opname
    end type

contains

    function ast_expr_const(value) result(val_expr)
        class(value_t), intent(in) :: value
        type(ast_expression_t) :: val_expr

        val_expr % argtype = ARG_CONSTANT
        allocate(val_expr % constant, source=value)
    end function

    function ast_expr_ref(reference) result(val_expr)
        type(ast_symbol_ref_t), intent(in) :: reference
        type(ast_expression_t) :: val_expr

        val_expr % argtype = ARG_REF
        allocate(val_expr % reference, source=reference)
    end function

    function ast_expr_op_args(op, args) result(val_expr)
        class(operation_t), intent(in) :: op
        type(ast_expression_t), intent(in) :: args(:)
        type(ast_expression_t) :: val_expr

        val_expr % argtype = ARG_CALL
        allocate(val_expr % op_call)
        allocate(val_expr % op_call % op, source=op)
        allocate(val_expr % op_call % args, source=args)
    end function

    function ast_expr_call(callexpr) result(val_expr)
        type(ast_operation_call_t) :: callexpr
        type(ast_expression_t) :: val_expr

        val_expr % argtype = ARG_CALL
        allocate(val_expr % op_call, source=callexpr)
    end function

    subroutine fetch_op(opname, op, err)
        !!      fetches an operation from the operation database based on its name
        use operation_add_m
        use operation_square_m
        use operation_multiply_m
        use operation_power_m
        use operation_mksequence_m
        
        character(len=*), intent(in) :: opname !! operation name
        class(operation_t), intent(out), allocatable :: op !! allocatable operation
        type(err_t), intent(out), optional :: err !! error object

        ! this is only for demonstration, a proper operation database is coming
        select case(opname)
          case ("add")
            allocate(op, source=add_t())
          case ("squared")
            allocate(op, source=square_t())
          case ("mul", "multiply")
            allocate(op, source=multiply_t())
          case ("pow", "power")
            allocate(op, source=op_pow_t())
          case ("array", "arr", "seq")
            allocate(op, source=op_mkseq_t())
          case default
            call seterr(err, "uknown oepration " // opname)
        end select
    end subroutine

    recursive subroutine evaluate_expression(val_expr, result_value, namespace, err)
        type(ast_expression_t), intent(in) :: val_expr
        type(namespace_t), intent(inout) :: namespace
        class(value_t), allocatable :: result_value
        type(err_t), intent(out), optional :: err

        select case(val_expr % argtype)
          case (ARG_CONSTANT)
            result_value = val_expr % constant
          case (ARG_REF)
            associate (refname => val_expr % reference % refname)
                call namespace % fetch(refname, result_value, err)
                if (check(err)) return
            end associate
          case (ARG_CALL)
            associate (op_call => val_expr%op_call)
                block
                    class(operation_t), allocatable :: op
                    type(value_item_t), allocatable, target :: arg_values(:)
                    integer :: i, nr_args

                    if (allocated(op_call%op)) then
                        op = op_call%op
                    else
                        call fetch_op(op_call%opname, op, err)
                        if (check(err)) return
                    end if

                    nr_args = size(op_call % args)

                    allocate(arg_values(nr_args))
                    do i = 1, nr_args
                        call evaluate_expression(op_call % args(i), arg_values(i)%value, namespace, err)
                        if (check(err)) return
                    end do

                    ! here should be checked if this call result is already cached
                    ! if not, execute the operation and cache the result
                    call op % exec_trace(arg_values, result_value)
                    ! call namespace % set_cached(result % value)
                end block
            end associate
        end select

        print *, "evaluated ", result_value % get_trace(), " as ", result_value%to_str()
    end subroutine

end module
