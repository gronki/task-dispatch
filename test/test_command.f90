program test_command

    use command_m
    use ast_m
    use namespace_m
    use real_value_m
    use operation_add_m
    implicit none (type, external)

    print *, '*** TEST_COMMAND ***'

    call test_eval_namespace_fetch
    call test_eval_add
    call test_eval_add_fetch

contains

    function get_empty_ns() result(ns)

        type(namespace_t) :: ns

    end function

    function get_test_ns() result(ns)

        type(namespace_t) :: ns

        call ns % push('one', real_value(1.0_f64))
        call ns % push('two', real_value(2.0_f64))

    end function

    subroutine test_eval_namespace_fetch

        type(namespace_t) :: ns
        type(value_item_t) :: result

        ns = get_test_ns()

        call evaluate_expression(ast_expression(ast_symbol_ref_t("one")), result%value, ns)

        select type (value => result % value)
          type is (real_value_t)
            if (value % value == 1.0_f64) return
        end select

        stop 1

    end subroutine

    subroutine test_eval_add

        type(namespace_t) :: ns
        type(value_item_t) :: result

        ns = get_empty_ns()

        call evaluate_expression( &
        &   ast_expression(add_t(), [ &
        &      ast_expression(real_value(1.0_f64)), &
        &      ast_expression(real_value(3.0_f64))  &
        &   ]), result%value, ns)

        select type (value => result % value)
          type is (real_value_t)
            if (value % value == 4.0_f64) return
        end select

        stop 2

    end subroutine

    subroutine test_eval_add_fetch

        type(namespace_t) :: ns
        type(value_item_t) :: result

        ns = get_test_ns()

        call evaluate_expression( &
        &   ast_expression(add_t(), [ &
        &      ast_expression(real_value(1.0_f64)), &
        &      ast_expression(ast_symbol_ref_t("two"))  &
        &   ]), result%value, ns)

        select type (value => result % value)
          type is (real_value_t)
            if (value % value == 3.0_f64) return
        end select

        stop 3

    end subroutine

end program
