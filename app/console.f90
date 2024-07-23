program test_console

    use tokenizer_m
    use parser_m
    use command_m
    use namespace_m

    character(len=4096) :: line
    type(ast_statement_t) :: stmt
    type(namespace_t) :: ns
    type(value_item_t) :: retval
    type(err_t) :: err
    type(tok_array_t) :: tokens


    do
        write (*, '(a)', advance='no') '> '
        read (*, '(a)') line
        if (adjustl(line) == "exit") exit
        if (line == "") cycle


        call tokenize_into_array(trim(line), tokens)
        call parse_statement(tokens, stmt, err)
        if (check(err)) then
            print *, err
            cycle
        end if 
        call evaluate_expression(stmt%rhs, retval%value, ns, err)
        if (check(err)) then
            print *, err
            cycle
        end if 

        if (stmt%is_assignment) then
            call ns%push(trim(stmt%lhs%refname), retval%value)
        end if

        associate (val=>retval%value)
            print *, "RESULT ::::::::::::::: ", val%get_trace(), " = ", val%to_str()
        end associate

    end do
end program
