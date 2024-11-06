module runner_m

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

    implicit none (type, external)
    private

    public :: run_interactive_console, simple_prompt

    abstract interface
        subroutine prompt_iface(line)
            character(len=*), intent(out) :: line
        end subroutine
    end interface

contains

    subroutine simple_prompt(line)
        character(len=*), intent(out) :: line
        write (*, '(a)', advance='no') '> '
        read (*, '(a)') line
    end subroutine

    subroutine run_interactive_console(prompt, operation_db, namespace, err)

        character(len=4096) :: line
        type(namespace_t), target :: namespace
        type(err_t), optional :: err
        type(operation_db_t) :: operation_db
        procedure(prompt_iface) :: prompt

        do
            call prompt(line)
            if (adjustl(line) == "exit") exit
            if (line == "") cycle

            call execute_console_line(line, operation_db, namespace, err)
        end do
    contains

    end subroutine

    subroutine execute_console_line(line, operation_db, namespace, err)
        character(len=4096) :: line
        type(namespace_t), target :: namespace
        type(err_t), optional :: err
        type(operation_db_t) :: operation_db

        type(tok_array_t) :: tokens
        type(ast_statement_t) :: stmt
        class(generator_t), allocatable, target :: gen
        class(value_t), allocatable :: result
        call tokenize_into_array(trim(line), tokens)
        call parse_statement(tokens, stmt, err)

        if (check(err)) then
            write(*, '(dt)') error_with_line(err, line)
            return
        end if

        call execute_statement_generator(stmt, namespace, operation_db, gen, result, err)

        if (check(err)) then
            write(*, '(dt)') error_with_line(err, line)
            return
        end if

        print *, "generator trace -> ", gen%trace()
        print *, "result trace -> ", result%get_trace()
        print *, " = ", result%to_str()
    end subroutine

end module
