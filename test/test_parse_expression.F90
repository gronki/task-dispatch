program test_parser

    use tokenizer_m
    use parser_m
    use command_m
    use ast_m
    use namespace_m
    use real_value_m
    use example_operations_m
    use test_assert_m
#   include "assert_macro.h"

    implicit none (type, external)

    print *, '*** TEST_PARSE_EXPRESSION ***'

    block
        type(ast_expression_t) :: expr
        type(err_t) :: err

        expr = parse("a", err)

        call assertm(expr%argtype == ARG_REF)
        call assertm(expr%refname == 'a')
    end block


    block
        type(ast_expression_t) :: expr
        type(err_t) :: err

        expr = parse("fun(3)", err)

        call assertm(expr%argtype == ARG_CALL)
        call assertm(expr%op_name == 'fun')
        call assertm(size(expr%op_args) == 1)
        call assertm(expr%op_args(1)%argtype == ARG_CONSTANT)

        select type (val=>expr%op_args(1)%constant)
          type is (real_value_t)
            call assertm(abs(val%value-3) < 1e-5)
          class default
            error stop "numeric value expected"
        end select
    end block


    block
        type(ast_expression_t) :: expr
        type(err_t) :: err

        expr = parse("fi(1)%gy(2)", err)

        call assertm(expr%argtype == ARG_CALL)
        call assertm(expr%op_name == 'gy')
        call assertm(expr%num_args == 2)
        associate (first_arg => expr%op_args(1))
            call assertm(first_arg%argtype == ARG_CALL)
            call assertm(first_arg%op_name == 'fi')
            call assertm(first_arg%num_args == 1)
            associate (first_first_arg => first_arg%op_args(1))
                call assertm(first_first_arg%argtype == ARG_CONSTANT)
                select type (val=>first_first_arg%constant)
                  type is (real_value_t)
                    call assertm(abs(val%value - 1) < 1e-5)
                  class default
                    error stop "numeric value expected"
                end select
            end associate
        end associate
        associate (second_arg => expr%op_args(2))
            call assertm(second_arg%argtype == ARG_CONSTANT)
            select type (val=>second_arg%constant)
              type is (real_value_t)
                call assertm(abs(val%value - 2) < 1e-5)
              class default
                error stop "numeric value expected"
            end select
        end associate
    end block


    block
        type(ast_expression_t) :: expr
        type(err_t) :: err

        expr = parse("fun(3", err)

        call assertm(check(err))
        print '(/a/dt/a/)', '---\/ THIS ERROR IS DESIRED \/---', err, '---/\ THIS ERROR IS DESIRED /\---'
        end block

    block
        type(ast_expression_t) :: expr
        type(err_t) :: err

        expr = parse("fun(3 3)", err)

        call assertm(check(err))
        print '(/a/dt/a/)', '---\/ THIS ERROR IS DESIRED \/---', err, '---/\ THIS ERROR IS DESIRED /\---'
        end block


contains

    function parse(line, err) result(expr)
        character(len=*) :: line
        type(tok_array_t) :: tokens
        type(ast_expression_t) :: expr
        type(err_t) :: err

        call tokenize_into_array(line, tokens)
        call parse_expression(tokens, expr, err)
    end function
end program
