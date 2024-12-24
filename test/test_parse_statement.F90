program test_parse_statement


   use command_m
   use ast_m
   use parser_m
   use namespace_m
   use real_value_m
   use operation_add_m
   use tokenizer_m
   use test_assert_m
   use error_m

   implicit none (type, external)
#   include "assert_macro.h"

   print *, '*** TEST_PARSE_STATEMENT ***'

   block
      character(len=*), parameter :: line = "a = sum(c, 1)"
      type(tok_array_t) :: tokens
      type(ast_statement_t) :: stmt
      type(err_t) :: err

      call tokenize_into_array(line, tokens)
      call parse_statement(tokens, stmt, err)

      call assertm(.not. check(err))
      call assertm(stmt%is_assignment)
      call assertm(stmt%lhs_refname == 'a')
      call assertm(stmt%rhs%argtype == ARG_CALL)
      call assertm(stmt%rhs%op_name == 'sum')

   end block

   block
      character(len=*), parameter :: line = "mul(c, 1)"
      type(tok_array_t) :: tokens
      type(ast_statement_t) :: stmt
      type(err_t) :: err

      call tokenize_into_array(line, tokens)
      call parse_statement(tokens, stmt, err)

      call assertm(.not. check(err))
      call assertm(.not. stmt%is_assignment)
      call assertm(stmt%rhs%argtype == ARG_CALL)
      call assertm(stmt%rhs%op_name == 'mul')

   end block

   block
      character(len=*), parameter :: line = "mul(c, 1) a"
      type(tok_array_t) :: tokens
      type(ast_statement_t) :: stmt
      type(err_t) :: err

      call tokenize_into_array(line, tokens)
      call parse_statement(tokens, stmt, err)

      call assertm(check(err))
      print '(/a/dt/a/)', '---\/ THIS ERROR IS DESIRED \/---', err, '---/\ THIS ERROR IS DESIRED /\---'

   end block


contains


end program
