program test_parser

    use tokenizer_m
    use parser_m
    use command_m
    use namespace_m

    implicit none (type, external)


    character(len=*), parameter :: line = &
        " multiply(squared(add(1,1))%pow(3), 0.25) "
    type(ast_expression_t) :: expr
    type(tok_array_t) :: tokens
    type(namespace_t) :: ns
    type(value_item_t) :: retval

    PRINT *, "TRYING TO EVALUATE: ", line

    call tokenize_into_array(line, tokens)
    call parse_expression(tokens, expr)
    call evaluate_expression(expr, retval%value, ns)

    associate (val=>retval%value)
      print *, "RESULT ::::::::::::::: ", val%get_trace(), " = ", val%to_str()
  end associate

end program
