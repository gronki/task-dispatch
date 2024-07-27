module parser_m

    use tokenizer_m
    use command_m
    use str_value_m
    use real_value_m
    use error_m
    use line_error_m
    implicit none (type, external)


contains

    recursive subroutine parse_function_argument(tokens, expr, kw, err)
        type(tok_array_t), intent(inout) :: tokens
        type(ast_expression_t), intent(out) :: expr
        type(input_key_t), intent(out) :: kw
        type(ast_expression_t) :: child_expr
        type(token_t) :: token1, token2
        character(len=:), allocatable :: label_name
        class(err_t), intent(out), optional :: err !! error object

        call peek_token(tokens, 0, token1)
        call peek_token(tokens, 1, token2)

        if (token1%type==token_ident .and. token2 == token_t(type=token_delim, value=kwarg_delim)) then
            label_name = token1%value
            call get_next_token(tokens, token2)
            call get_next_token(tokens, token2)

            kw%has_key = .true.
            kw%key = label_name
        else
            kw%has_key = .false.
        end if

        call parse_expression(tokens, expr, err)
    end subroutine

    recursive subroutine parse_expression(tokens, expr, err)
        type(tok_array_t), intent(inout) :: tokens
        type(ast_expression_t), intent(out) :: expr
        type(ast_expression_t) :: child_expr
        type(token_t) :: token
        class(err_t), intent(out), optional :: err
        integer :: pivot

        call parse_recursive(tokens, expr, err)

        iter_chain: do
            call get_current_token(tokens, token)
            if (token /= token_t(type=token_delim, value=chain_call_delim)) exit iter_chain

            call get_next_token(tokens, token)
            child_expr = expr

            if (child_expr%argtype /= ARG_REF .and. child_expr%argtype /= ARG_CALL) then
                call seterr(err, "chaining only allowed on idents and function call results", child_expr%loc)
                return
            end if

            call parse_recursive(tokens, expr, err)

            if (expr%argtype /= ARG_CALL) then
                call seterr(err, "function call expected in chaining", expr%loc)
                return
            end if

            associate (op_call => expr%op_call)
                block
                    type(ast_expression_t), allocatable :: oldargs(:)
                    type(input_key_t), allocatable :: oldkeys(:)
                    integer :: num_current_args

                    num_current_args = size(op_call%args)
                    call move_alloc(from=op_call%args, to=oldargs)
                    call move_alloc(from=op_call%keys, to=oldkeys)

                    allocate(op_call%args(num_current_args+1))
                    allocate(op_call%keys(num_current_args+1))

                    op_call%args(1) = child_expr
                    op_call%keys(1) = input_key_t(has_key=.false.)
                    if (size(op_call%args) > 1) then
                        op_call%args(2:) = oldargs
                        op_call%keys(2:) = oldkeys
                    end if
                end block
            end associate


        end do iter_chain

    end subroutine

    recursive subroutine parse_recursive(tokens, expr, err)
        type(tok_array_t), intent(inout) :: tokens
        type(ast_expression_t), intent(out) :: expr
        type(token_t) :: token, ident_token
        class(err_t), intent(out), optional :: err !! error object

        call get_current_token(tokens, token)

        if (token%type == token_num_literal) then
            expr = ast_expression(real_value(token%value), loc=token%loc)
            call get_next_token(tokens)
            return
        end if

        if (token%type == token_str_literal) then
            expr = ast_expression(str_value(token%value), loc=token%loc)
            expr % loc = token % loc ! why is this needed?
            call get_next_token(tokens)
            return
        end if

        if (token%type /= token_ident) then
            call seterr(err, "identifier expected", loc=token%loc)
            return
        end if

        ident_token = token

        call get_next_token(tokens, token)
        if (token /= token_t(type=token_delim, value="(")) then
            expr = ast_expression(ast_symbol_ref_t(refname=ident_token%value), loc=ident_token%loc)
            expr % loc = ident_token % loc ! why is this needed?
            return
        end if

        ! function call
        parse_function_call: block
            type(ast_operation_call_t) :: opcall
            integer, parameter :: max_args = 16
            integer :: num_args

            opcall%opname = ident_token%value
            allocate(opcall%args(max_args), opcall%keys(max_args))

            num_args = 0

            iter_args: do
                call get_next_token(tokens, token)

                if (token == token_t(type=token_delim, value=")")) then
                    call get_next_token(tokens, token)
                    exit iter_args
                end if

                num_args = num_args + 1
                if (num_args > max_args) error stop "too many arguments, currently only 16 allowed"

                call parse_function_argument(tokens, opcall%args(num_args), opcall%keys(num_args), err)

                call get_current_token(tokens, token)

                if (token == token_t(type=token_delim, value=")")) then
                    call get_next_token(tokens, token)
                    exit iter_args
                end if

                if (token /= token_t(type=token_delim, value=",")) then
                    call seterr(err, "comma (,) expected", loc=token%loc)
                    return
                end if
            end do iter_args

            opcall%args = opcall%args(:num_args)
            opcall%keys = opcall%keys(:num_args)

            expr = ast_expression(opcall, loc=ident_token%loc)
            expr % loc = ident_token % loc ! why is this needed?
            return

        end block parse_function_call

    end subroutine

    subroutine parse_statement(tokens, statement, err)
        !! Parses an entire statement, that is, a full line of code.
        !! Currently statement can either be:
        !! * an assignment
        !! * an expression (without assignment)

        type(tok_array_t), intent(inout) :: tokens  !! Token array to process
        type(ast_statement_t), intent(out) :: statement !! Statement object
        class(err_t), intent(out), optional :: err !! error object

        type(token_t) :: token1, token2, token3

        call peek_token(tokens, 0, token1)
        call peek_token(tokens, 1, token2)

        if (token1 % type == TOKEN_IDENT .and. token2 == token_t(type=token_delim, value="=")) then
            statement % is_assignment = .true.
            statement % lhs = ast_symbol_ref_t(refname=token1%value)
            call get_next_token(tokens)
            call get_next_token(tokens)
        end if

        call parse_expression(tokens, statement % rhs, err)
        if (check(err)) return

        call get_current_token(tokens, token3)
        if (token3 % type /= TOKEN_END) &
            call seterr(err, "End of line expected", loc=token3%loc)

    end subroutine

end module
