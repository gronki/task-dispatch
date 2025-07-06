program test_console

   use error_m
   use namespace_m
   use operation_database_m
   use example_operations_m
   use runner_m
   use simple_prompt_m

   type(namespace_t), target :: namespace
   type(operation_db_t) :: operation_db

   operation_db = get_example_operation_db()

   call run_interactive_console(simple_prompt_t(), operation_db, namespace)

end program
