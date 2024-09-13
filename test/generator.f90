program test_generator

    use value_m
    use generator_m
    use op_generator_m
    use value_generator_m
    use operation_multiply_m
    use operation_add_m
    use example_operations_m, only: get_example_operation_db
    use generator_from_ast_m
    use parser_m, only: parsed_expression

    call test_gen_1
    call test_gen_2
    call test_gen_3

contains

    subroutine test_gen_1
        class(generator_t), allocatable :: gen
        class(value_t), allocatable :: val

        gen = op_generator_t(op=multiply_t(), args=[ &
            op_generator_t_arg_t(gen=value_generator_t(val=real_value(2.0_real_k))), &
            op_generator_t_arg_t(gen=op_generator_t(op=add_t(), args=[&
            op_generator_t_arg_t(gen=value_generator_t(val=real_value(2.0_real_k))), &
            op_generator_t_arg_t(gen=value_generator_t(val=real_value(2.0_real_k))) &
            ]))])

        call gen % yield(val)

        select type(val)
          type is (real_value_t)
            if (val % value /= 8.0_f64) error stop "expected real result = 8"
          class default
            error stop "expected real value result"
        end select

        print *, val % to_str()
        print *, val % trace
        print *, gen % trace()

        if (val % get_trace() /= gen % trace()) error stop "traces not equal"
    end subroutine

    subroutine test_gen_2
        use example_operations_m

        class(generator_t), allocatable :: gen
        class(value_t), allocatable :: val

        call build_generator(parsed_expression("mul( 2, add(2, 2))"), gen, operation_db=get_example_operation_db())
        call gen % yield(val)

        select type(val)
          type is (real_value_t)
            if (val % value /= 8.0_f64) error stop "expected real result = 8"
          class default
            error stop "expected real value result"
        end select

        print *, val % to_str()
        print *, val % trace
        print *, gen % trace()

        if (val % get_trace() /= gen % trace()) error stop "traces not equal"
    end subroutine

    subroutine test_gen_3
        use example_operations_m

        class(generator_t), allocatable :: gen
        class(value_t), allocatable :: val

        call build_generator(parsed_expression("range(3) % mul(2) % item(2)"), gen, operation_db=get_example_operation_db())
        call gen % yield(val)

        print *, val % to_str()
        print *, val % get_trace()
        print *, gen % trace()

        if (val % get_trace() /= gen % trace()) error stop "traces not equal"
    end subroutine

end program
