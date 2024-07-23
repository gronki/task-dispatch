program main2

    use operation_add_m
    use operation_square_m
    use real_value_m
    use command_m

    implicit none (type, external)

    character(len=256) :: result1, result2
    type(real_value_t), target :: val1, val2

    val1 = real_value(1.5_f64)
    val2 = real_value(2.0_f64)

    associate (addop => add_t(), squareop => square_t())
        associate(val3 => squareop % execf([ value_item_t(val1) ]))

            print *, addop % name(), squareop % name()

            select type (result => addop % execf([ value_item_t(val3), value_item_t(val2) ]))
              class is (real_value_t)
                write (result1, *) result % value, result % trace
                print *, trim(result1)
            end select
        end associate
    end associate

    block
        type(value_item_t) :: val_item
        type(namespace_t) :: namespace
        type(ast_expression_t) :: val_expr

        val_expr = ast_expression(add_t(), [ &
            ast_expression(square_t(), [ ast_expression(val1) ]), &
            ast_expression(val2)])

        call evaluate_expression(val_expr, val_item%value, namespace)

        select type (result => val_item % value)
          class is (real_value_t)
            write (result2, *) result % value, result % trace
            print *, trim(result2)
        end select
    end block

    print *, result1 == result2


end program main2
