module parser_m

    use tokenizer_m
    use command_m
    use str_value_m
    use operation_m
    use real_value_m
    use error_m
    use line_error_m
    use ast_m
    implicit none (type, external)


contains

    recursive subroutine parse_function_argument(tokens, expr, kw, err)
        type(tok_array_t), intent(inout) :: tokens
        type(ast_expression_t), intent(out) :: expr
        type(input_key_t), intent(out) :: kw
        type(ast_expression_t) :: child_expr
        type(token_t) :: token1, token2
        character(len=:), allocatable :: label_name
        type(err_t), intent(out), optional :: err !! error object

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
        type(ast_expression_t), intent(inout) :: expr
        type(ast_expression_t), allocatable :: child_expr
        type(token_t) :: token
        type(err_t), intent(out), optional :: err
        integer :: pivot

        call parse_recursive(tokens, expr, err)

        iter_chain: do
            call get_current_token(tokens, token)
            if (token /= token_t(type=token_delim, value=chain_call_delim)) exit iter_chain

            call get_next_token(tokens, token)
            child_expr = expr
            child_expr%loc = expr%loc ! why is this needed?

            if (child_expr%argtype /= ARG_REF .and. child_expr%argtype /= ARG_CALL) then
                call seterr(err, "chaining only allowed on idents and function call results", child_expr%loc)
                return
            end if

            call parse_recursive(tokens, expr, err)

            if (expr%argtype /= ARG_CALL) then
                call seterr(err, "function call expected in chaining", expr%loc)
                return
            end if

            block
                type(ast_expression_t), allocatable :: following_expr
                integer :: num_current_args, iarg

                num_current_args = expr % num_args
                following_expr = expr

                ! this whole section could be nicely written using the array
                ! concatenation syntax, but ifx crashes from that :(
                if (allocated(expr % op_args)) deallocate(expr % op_args)
                allocate(expr%op_args(num_current_args+1))
                if (allocated(expr % op_arg_keys)) deallocate(expr % op_arg_keys)
                allocate(expr%op_arg_keys(num_current_args+1))

                expr%op_args(1) = child_expr
                expr%op_arg_keys(1) = input_key_t(has_key=.false.)
                
                do iarg = 2, num_current_args + 1
                    expr % op_args(iarg) = following_expr % op_args(iarg-1)
                    expr % op_arg_keys(iarg) = following_expr % op_arg_keys(iarg-1)
                end do

                deallocate( following_expr )

                expr % num_args = num_current_args + 1

            end block

        end do iter_chain

    end subroutine

    recursive subroutine parse_recursive(tokens, expr, err)
        type(tok_array_t), intent(inout) :: tokens
        type(ast_expression_t), intent(inout) :: expr
        type(token_t) :: token, ident_token
        type(err_t), intent(out), optional :: err !! error object

        call get_current_token(tokens, token)

        if (token%type == token_num_literal) then
            expr = ast_expression_t(value=real_value(token%value), loc=token%loc)
            expr % loc = token % loc ! why is this needed?
            call get_next_token(tokens)
            return
        end if

        if (token%type == token_str_literal) then
            expr = ast_expression_t(value=str_value(token%value), loc=token%loc)
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
            expr = ast_expression_t(refname=ident_token%value, loc=ident_token%loc)
            expr % loc = ident_token % loc ! why is this needed?
            return
        end if

        ! function call
        parse_function_call: block
            integer, parameter :: max_args = 16
            type(ast_expression_t) :: op_args(max_args)
            type(input_key_t) :: op_arg_keys(max_args)
            character(len=64) :: opname
            integer :: num_args

            opname = ident_token%value

            num_args = 0

            iter_args: do
                call get_next_token(tokens, token)

                if (token == token_t(type=token_delim, value=")")) then
                    call get_next_token(tokens, token)
                    exit iter_args
                end if

                num_args = num_args + 1
                if (num_args > max_args) then
                    call seterr(err, "too many arguments, currently only 16 allowed", loc=token%loc)
                    return
                end if

                call parse_function_argument(tokens, op_args(num_args), op_arg_keys(num_args), err)
                if (check(err)) return

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

            if (num_args > 0) then
                expr = ast_expression_t(opname, num_args, args=op_args(:num_args), keys=op_arg_keys(:num_args), loc=ident_token%loc)
            else
                expr = ast_expression_t(opname, num_args, loc=ident_token%loc)
            end if
            expr % loc = ident_token % loc ! why is this needed?
            return

        end block parse_function_call

    end subroutine

    function parsed_expression(line, err)
        character(len=*), intent(in) :: line
        type(tok_array_t) :: tokens
        type(err_t), intent(out), optional :: err
        type(ast_expression_t) :: parsed_expression

        call tokenize_into_array(line, tokens, err)
        if (check(err)) return

        call parse_expression(tokens, parsed_expression, err)

    end function

    subroutine parse_statement(tokens, statement, err)
        !! Parses an entire statement, that is, a full line of code.
        !! Currently statement can either be:
        !! * an assignment
        !! * an expression (without assignment)

        type(tok_array_t), intent(inout) :: tokens  !! Token array to process
        type(ast_statement_t), intent(out) :: statement !! Statement object
        type(err_t), intent(out), optional :: err !! error object

        type(token_t) :: token1, token2, token3

        call peek_token(tokens, 0, token1)
        call peek_token(tokens, 1, token2)

        if (token1 % type == TOKEN_IDENT .and. token2 == token_t(type=token_delim, value="=")) then
            statement % is_assignment = .true.
            statement % lhs_refname = token1%value
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
