module ast_m

    use operation_m
    use value_m
    use error_m
    use tokenizer_m, only: token_loc_t
    implicit none (type, external)
    private

    integer, parameter, public :: ARG_UNINITIALIZED = 0, ARG_CONSTANT=1, ARG_REF=2, ARG_CALL=3

    type :: ast_expression_t
        integer :: argtype = ARG_UNINITIALIZED
        type(token_loc_t) :: loc
        ! -- constant literal --
        class(value_t), allocatable :: constant
        ! -- reference to a symbol --
        character(len=64) :: refname
        ! -- operation call --
        character(len=32) :: op_name
        integer :: num_args
        ! note: if num_args==0, below two arrays may be unallocated
        type(ast_expression_t), allocatable :: op_args(:)
        type(input_key_t), allocatable :: op_arg_keys(:)
        ! sometimes for debugging we might want to provide an operation
        ! object instead of the name
        class(operation_t), allocatable :: op
    end type

    interface ast_expression_t
        module procedure ast_expr_const
        module procedure ast_expr_ref
        module procedure ast_expr_op_args
        module procedure ast_expr_op_name_args
    end interface

    public :: ast_expression_t

    type :: ast_statement_t
        type(ast_expression_t) :: rhs
        logical :: is_assignment = .false.
        character(len=64) :: lhs_refname
    end type

    public :: ast_statement_t
    
contains

    function ast_expr_const(value, loc) result(val_expr)
        class(value_t), intent(in) :: value
        type(token_loc_t), intent(in), optional :: loc
        type(ast_expression_t) :: val_expr

        val_expr % argtype = ARG_CONSTANT
        allocate(val_expr % constant, source=value)
        if (present(loc)) val_expr % loc = loc
    end function

    function ast_expr_ref(refname, loc) result(val_expr)
        character(len=*), intent(in) :: refname
        type(token_loc_t), intent(in), optional :: loc
        type(ast_expression_t) :: val_expr

        val_expr % argtype = ARG_REF
        val_expr % refname = refname
        if (present(loc)) val_expr % loc = loc
    end function

    function ast_expr_op_args(op, num_args, args, keys, loc) result(val_expr)
        class(operation_t), intent(in) :: op
        integer, intent(in) :: num_args
        type(ast_expression_t), intent(in), optional :: args(:)
        type(input_key_t), intent(in), optional :: keys(:)
        type(token_loc_t), intent(in), optional :: loc
        type(ast_expression_t) :: val_expr

        val_expr % argtype = ARG_CALL
        allocate(val_expr % op, source=op)
        val_expr % num_args = num_args
        if (present(loc)) val_expr % loc = loc
        
        if (num_args == 0) return
        if (.not. present(args)) error stop "args required when num_args>0"
        
        allocate(val_expr % op_args, source=args)
        
        if (present(keys)) then
            allocate(val_expr % op_arg_keys, source=keys)
        else
            allocate(val_expr % op_arg_keys(size(args)))
            val_expr % op_arg_keys % has_key = .false.
        end if
    end function
    
    function ast_expr_op_name_args(op_name, num_args, args, keys, loc) result(val_expr)
        character(len=*), intent(in) :: op_name
        integer, intent(in) :: num_args
        type(ast_expression_t), intent(in), optional :: args(:)
        type(input_key_t), intent(in), optional :: keys(:)
        type(token_loc_t), intent(in), optional :: loc
        type(ast_expression_t) :: val_expr

        val_expr % argtype = ARG_CALL
        val_expr % op_name = op_name
        val_expr % num_args = num_args
        if (present(loc)) val_expr % loc = loc
        
        if (num_args == 0) return
        if (.not. present(args)) error stop "args required when num_args>0"
        
        allocate(val_expr % op_args, source=args)
        
        if (present(keys)) then
            allocate(val_expr % op_arg_keys, source=keys)
        else
            allocate(val_expr % op_arg_keys(size(args)))
            val_expr % op_arg_keys(:) % has_key = .false.
        end if
        if (present(loc)) val_expr % loc = loc
    end function

end module
