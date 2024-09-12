module ast_m

    use operation_m
    use value_m
    use error_m
    use tokenizer_m, only: token_loc_t
    implicit none (type, external)

    integer, parameter :: ARG_UNINITIALIZED = 0, ARG_CONSTANT=1, ARG_REF=2, ARG_CALL=3

    type ast_expression_t
        integer :: argtype = ARG_UNINITIALIZED
        class(value_t), allocatable :: constant
        type(ast_symbol_ref_t), allocatable :: reference
        type(ast_operation_call_t), allocatable :: op_call
        type(token_loc_t) :: loc
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

contains

    function ast_expr_const(value, loc) result(val_expr)
        class(value_t), intent(in) :: value
        type(token_loc_t), intent(in), optional :: loc
        type(ast_expression_t) :: val_expr

        val_expr % argtype = ARG_CONSTANT
        allocate(val_expr % constant, source=value)
        if (present(loc)) val_expr % loc = loc
    end function

    function ast_expr_ref(reference, loc) result(val_expr)
        type(ast_symbol_ref_t), intent(in) :: reference
        type(token_loc_t), intent(in), optional :: loc
        type(ast_expression_t) :: val_expr

        val_expr % argtype = ARG_REF
        allocate(val_expr % reference, source=reference)
        if (present(loc)) val_expr % loc = loc
    end function

    function ast_expr_op_args(op, args, loc) result(val_expr)
        class(operation_t), intent(in) :: op
        type(ast_expression_t), intent(in) :: args(:)
        type(token_loc_t), intent(in), optional :: loc
        type(ast_expression_t) :: val_expr

        val_expr % argtype = ARG_CALL
        allocate(val_expr % op_call)
        allocate(val_expr % op_call % op, source=op)
        allocate(val_expr % op_call % args, source=args)
        if (present(loc)) val_expr % loc = loc
    end function

    function ast_expr_call(callexpr, loc) result(val_expr)
        type(ast_operation_call_t) :: callexpr
        type(token_loc_t), intent(in), optional :: loc
        type(ast_expression_t) :: val_expr

        val_expr % argtype = ARG_CALL
        allocate(val_expr % op_call, source=callexpr)
        if (present(loc)) val_expr % loc = loc
    end function

end module