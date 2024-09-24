module problem2_m

    implicit none (type, external)

    type ast_expression_t
        type(ast_operation_call_t), allocatable :: op_call
    end type

    type ast_operation_call_t
        type(ast_expression_t), allocatable :: args(:)
    end type

end module
