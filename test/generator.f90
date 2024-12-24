program test_generator

   use value_m
   use generator_m
   use op_generator_m
   use ast_m
   use value_generator_m
   use operation_multiply_m
   use operation_add_m
   use example_operations_m, only: get_example_operation_db
   use generator_from_ast_m
   use parser_m, only: parse_expression_str

   call test_gen_1
   call test_gen_2
   call test_gen_3
   call test_gen_4

contains

   subroutine test_gen_1
      class(generator_t), allocatable, target :: gen
      class(value_t), allocatable :: result

      print *, ' *** TEST 1 *** '

      gen = op_generator_t(op=multiply_t(), num_args=2, args=[ &
         op_generator_t_arg_t(gen=value_generator_t(val=real_value(2.0_real_k))), &
         op_generator_t_arg_t(gen=op_generator_t(op=add_t(), num_args=2, args=[&
         op_generator_t_arg_t(gen=value_generator_t(val=real_value(1.0_real_k))), &
         op_generator_t_arg_t(gen=value_generator_t(val=real_value(3.0_real_k))) &
         ]))])

      call gen % yield(result)

      select type(result)
      type is (real_value_t)
         if (result % value /= 8.0_f64) error stop "expected real result = 8"
      class default
         error stop "expected real value result"
      end select

      print *, result % to_str()
      print *, result % trace
      associate (gen_trace => gen % trace())
         print *, gen_trace
      end associate

      if (result % get_trace() /= gen % trace()) error stop "traces not equal"

   end subroutine

   subroutine test_gen_2
      use example_operations_m

      class(generator_t), allocatable, target :: gen
      class(value_t), allocatable :: result
      type(ast_expression_t) :: expr

      print *, ' *** TEST 2 *** '

      call parse_expression_str("mul( 2, add(1, 3))", expr)
      call build_generator(expr, gen, operation_db=get_example_operation_db())

      call gen % yield(result)

      select type(result)
      type is (real_value_t)
         if (result % value /= 8.0_f64) error stop "expected real result = 8"
      class default
         error stop "expected real value result"
      end select

      print *, result % to_str()
      print *, result % trace
      associate (gen_trace => gen % trace())
         print *, gen_trace
      end associate

      if (result % get_trace() /= gen % trace()) error stop "traces not equal"

   end subroutine

   subroutine test_gen_3
      use example_operations_m

      class(generator_t), allocatable, target :: gen
      class(value_t), allocatable :: result
      type(ast_expression_t) :: expr

      print *, ' *** TEST 3 *** '

      call parse_expression_str("range(3) % mul(4) % item(2)", expr)
      call build_generator(expr, gen, operation_db=get_example_operation_db())

      call gen % yield(result)

      print *, result % to_str()
      print *, result % get_trace()
      associate (gen_trace => gen % trace())
         print *, gen_trace
      end associate

      if (result % get_trace() /= gen % trace()) error stop "traces not equal"

   end subroutine


   subroutine test_gen_4
      use example_operations_m
      use namespace_m

      class(generator_t), allocatable, target :: gen
      class(value_t), allocatable :: result
      type(namespace_t), target :: ns
      type(ast_expression_t) :: expr

      print *, ' *** TEST 4 *** '

      call ns % push("a", real_value(2.0_f64))
      call parse_expression_str("mul(a, 5)", expr)
      call build_generator(expr, gen, &
         operation_db=get_example_operation_db(), &
         namespace=ns)

      call gen % yield(result)

      select type(result)
      type is (real_value_t)
         if (result % value /= 10.0_f64) error stop "expected real result = 10"
      class default
         error stop "expected real value result"
      end select

      print *, result % to_str()
      print *, result % trace
      associate (gen_trace => gen % trace())
         print *, gen_trace
      end associate

      if (result % get_trace() /= gen % trace()) error stop "traces not equal"

   end subroutine

end program
