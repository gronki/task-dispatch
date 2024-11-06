program test_console

    use tokenizer_m
    use parser_m
    use command_m
    use ast_m
    use namespace_m
    use operation_database_m
    use example_operations_m
    use line_error_m
    use value_m
    use generator_m
    use generator_from_ast_m, only: execute_statement_generator

    character(len=4096) :: line
    type(ast_statement_t) :: stmt
    type(namespace_t), target :: ns
    class(value_t), allocatable :: result
    type(err_t) :: err
    type(tok_array_t) :: tokens
    type(operation_db_t) :: operation_db
    class(generator_t), allocatable, target :: gen

    operation_db = get_example_operation_db()

    do
        write (*, '(a)', advance='no') '> '
        read (*, '(a)') line
        if (adjustl(line) == "exit") exit
        if (line == "") cycle

        call tokenize_into_array(trim(line), tokens)
        call parse_statement(tokens, stmt, err)

        if (check(err)) then
            write(*, '(dt)') error_with_line(err, line)
            cycle
        end if

        call execute_statement_generator(stmt, ns, operation_db, gen, result, err)

        if (check(err)) then
            write(*, '(dt)') error_with_line(err, line)
            cycle
        end if

        print *, "generator trace -> ", gen%trace()
        print *, "result trace -> ", result%get_trace()
        print *, " = ", result%to_str()

    end do
end program
