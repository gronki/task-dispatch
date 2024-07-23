program parserplay

    use tokenizer_m

    implicit none (type, external)


    character(len=*), parameter :: line = &
        " glob(""test*.fits"", recursive: 0)%file()%sub_bias( file(""bias.fits"") )" // &
        "% levels(std: 2.5, mean: 1.0)  %save(""test2.png"") "
    character(len=:), allocatable :: result
    type(tok_array_t) :: tokens

    call tokenize_into_array(line, tokens)

    result = parse_expression(tokens)

    print *, 'INPUT:  ', line
    print *, 'OUTPUT: ', result

contains



    recursive function parse_function_argument(tokens) result(expr)
        type(tok_array_t), intent(inout) :: tokens
        character(len=:), allocatable :: expr, child_expr
        type(token_t) :: token1, token2
        character(len=:), allocatable :: label_name

        call peek_token(tokens, 0, token1)
        call peek_token(tokens, 1, token2)

        if (token1%type==token_ident .and. token2 == token_t(type=token_delim, value=kwarg_delim)) then
            label_name = token1%value
            call get_next_token(tokens, token2)
            call get_next_token(tokens, token2)

            expr = parse_expression(tokens) // " as " // label_name
            return

        end if

        expr = parse_expression(tokens)
    end function

    recursive function parse_expression(tokens) result(expr)
        type(tok_array_t), intent(inout) :: tokens
        character(len=:), allocatable :: expr, child_expr
        type(token_t) :: token
        integer :: pivot

        expr = parse_recursive(tokens)
        iter_chain: do
            call get_current_token(tokens, token)
            if (token /= token_t(type=token_delim, value=chain_call_delim)) exit iter_chain
            call get_next_token(tokens, token)
            child_expr = expr
            if (child_expr(:5) /= "ident" .and. child_expr(:4) /= "call") &
                error stop "chaining only allowed on idents and function call results"
            expr = parse_recursive(tokens)
            if (expr(:4) /= "call") error stop "function call expected in chaining"
            pivot = index(expr, '[')
            expr = expr(:pivot) // "{" // child_expr // "}" // expr(pivot+1:)
        end do iter_chain

    end function

    recursive function parse_recursive(tokens) result(expr)
        type(tok_array_t), intent(inout) :: tokens
        character(len=:), allocatable :: expr
        type(token_t) :: token
        character(len=:), allocatable :: ident_name

        call get_current_token(tokens, token)
        if (token%type == token_num_literal) then
            expr = "numeric literal: <" // token%value // ">"
            call get_next_token(tokens, token)
            return
        end if
        if (token%type == token_str_literal) then
            expr = "string literal: <" // token%value // ">"
            call get_next_token(tokens, token)
            return
        end if

        if (token%type /= token_ident) error stop "identifier expected"
        ident_name = token%value

        call get_next_token(tokens, token)
        if (token /= token_t(type=token_delim, value="(")) then
            expr = "identifier: <" // ident_name // ">"
            return
        end if

        !function call
        expr = "call of function <" // ident_name // "> on args ["
        iter_args: do
            call get_next_token(tokens, token)

            if (token == token_t(type=token_delim, value=")")) then
                expr = expr // ']'
                call get_next_token(tokens, token)
                exit iter_args
            end if

            expr = expr // "{" // parse_function_argument(tokens) // "}"

            call get_current_token(tokens, token)

            if (token == token_t(type=token_delim, value=")")) then
                expr = expr // ']'
                call get_next_token(tokens, token)
                exit iter_args
            end if
            if (token /= token_t(type=token_delim, value=",")) then
                error stop ", expected"
            end if
        end do iter_args
    end function

end program

