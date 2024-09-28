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
    call test_gen_4

contains

    subroutine test_gen_1
        class(generator_t), allocatable, target :: gen
        type(value_ref_t) :: val_ref
        class(value_t), pointer :: val

        gen = op_generator_t(op=multiply_t(), args=[ &
            op_generator_t_arg_t(gen=value_generator_t(val=real_value(2.0_real_k))), &
            op_generator_t_arg_t(gen=op_generator_t(op=add_t(), args=[&
            op_generator_t_arg_t(gen=value_generator_t(val=real_value(1.0_real_k))), &
            op_generator_t_arg_t(gen=value_generator_t(val=real_value(3.0_real_k))) &
            ]))])

        call gen % yield(val_ref)
        val => val_ref % value

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

        call val_ref % dealloc
    end subroutine

    subroutine test_gen_2
        use example_operations_m

        class(generator_t), allocatable, target :: gen
        type(value_ref_t) :: val_ref
        class(value_t), pointer :: val

        call build_generator(parsed_expression("mul( 2, add(1, 3))"), gen, operation_db=get_example_operation_db())

        call gen % yield(val_ref)
        val => val_ref % value

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

        call val_ref % dealloc
    end subroutine

    subroutine test_gen_3
        use example_operations_m

        class(generator_t), allocatable, target :: gen
        type(value_ref_t) :: val_ref
        class(value_t), pointer :: val

        call build_generator(parsed_expression("range(3) % mul(4) % item(2)"), gen, operation_db=get_example_operation_db())

        call gen % yield(val_ref)
        val => val_ref % value

        print *, val % to_str()
        print *, val % get_trace()
        print *, gen % trace()

        if (val % get_trace() /= gen % trace()) error stop "traces not equal"

        call val_ref % dealloc
    end subroutine


    subroutine test_gen_4
        use example_operations_m
        use namespace_m

        class(generator_t), allocatable, target :: gen
        type(value_ref_t) :: val_ref
        class(value_t), pointer :: val
        type(namespace_t), target :: ns

        call ns % push("a", real_value(2.0_f64))
        call build_generator(parsed_expression("mul(a, 5)"), gen, &
            operation_db=get_example_operation_db(), &
            namespace=ns)

        call gen % yield(val_ref)
        val => val_ref % value

        select type(val)
          type is (real_value_t)
            if (val % value /= 10.0_f64) error stop "expected real result = 10"
          class default
            error stop "expected real value result"
        end select

        print *, val % to_str()
        print *, val % trace
        print *, gen % trace()

        if (val % get_trace() /= gen % trace()) error stop "traces not equal"

        call val_ref % dealloc
    end subroutine

end program
