module example_operations_m

    use operation_m
    use operation_database_m

    implicit none (type, external)

contains

    function get_example_operation_db() result(operation_db)
        use operation_add_m
        use operation_sum_m
        use operation_square_m
        use operation_multiply_m
        use operation_power_m
        use operation_range_m

        type(operation_db_t) :: operation_db !! operation catalog

        call operation_db_init(operation_db)

        call add_operation(operation_db, add_t())
        call add_operation(operation_db, square_t())
        call add_operation(operation_db, sum_t())
        call add_operation(operation_db, multiply_t())
        call add_operation(operation_db, op_pow_t())
        call add_operation(operation_db, op_range_t())
    end function

end module
