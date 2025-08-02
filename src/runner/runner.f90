module runner_m

use tokenizer_m
use parser_m
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

public :: run_interactive_console

type, abstract :: console_prompt_t
   logical :: halt_on_error = .false.
contains
   procedure :: init => default_console_init
   procedure(prompt_iface), deferred :: input
end type

abstract interface
subroutine prompt_iface(prompt, line, eof)
   import :: console_prompt_t
   class(console_prompt_t) :: prompt
   character(len=*), intent(out) :: line
   logical, intent(out) :: eof
end subroutine
end interface

public :: console_prompt_t

contains

subroutine default_console_init(prompt)
   class(console_prompt_t), intent(inout) :: prompt
end subroutine

subroutine run_interactive_console(prompt, operation_db, namespace, err)

   character(len=4096) :: line
   type(namespace_t), target :: namespace
   type(operation_db_t) :: operation_db
   class(console_prompt_t) :: prompt
   logical :: eof
   type(err_t), intent(out) :: err

   call prompt % init

   do
      call prompt % input(line, eof)
      line = adjustl(line)
      if (eof) exit
      if (line == "exit") exit
      if (line == "") cycle
      if (line(1:1) == "!") cycle
      
      call clear(err)
      call execute_console_line(line, operation_db, namespace, err)

      if (prompt % halt_on_error .and. check(err)) return
   end do
end subroutine

subroutine execute_console_line(line, operation_db, namespace, err)
   character(len=4096) :: line
   type(namespace_t), target :: namespace
   type(err_t), intent(out), optional :: err
   type(operation_db_t) :: operation_db

   type(tok_array_t) :: tokens
   type(ast_statement_t) :: stmt
   class(generator_t), allocatable, target :: gen
   class(value_t), allocatable :: result

   try: block
      call tokenize_into_array(trim(line), tokens, err)
      if (check(err)) exit try
      call parse_statement(tokens, stmt, err)
      if (check(err)) exit try

      call execute_statement_generator(stmt, namespace, operation_db, gen, result, err=err)
      if (check(err)) exit try

      print *, "TRACE :: ", result%get_trace()
      print *, " = ", result%to_str()
   end block try

   if (check(err))  write(*, '(dt)') error_with_line(err, line)

end subroutine

end module
