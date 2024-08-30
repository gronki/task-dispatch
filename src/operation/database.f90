module operation_database_m

    use value_base_m, only: value_item_t
    use operation_m
    use error_m

    implicit none (type, external)

    type operation_db_entry_t
        character(len=32) :: opname = ""
        class(operation_t), allocatable :: op
    end type

    integer, parameter :: OPERATION_DB_CAPACITY = 128

    type operation_db_t
        integer :: current_size = 0
        type(operation_db_entry_t) :: db(OPERATION_DB_CAPACITY)
    end type

    interface operation_db_t
        module procedure :: operation_db_new
    end interface


contains

    pure subroutine operation_db_init(operation_db)
        use operation_mksequence_m

        type(operation_db_t), intent(inout) :: operation_db !! operation catalog

        call add_operation(operation_db, op_mkseq_t())
    end subroutine

    pure function operation_db_new()
        type(operation_db_t) :: operation_db_new !! operation catalog
        call operation_db_init(operation_db_new)
    end function

    pure function is_operation_matching_sequence_safe(op, inputs, labels) result(matching)
        class(operation_t), intent(in) :: op !! operation
        type(value_item_t), intent(in) :: inputs(:) !! inputs for matching the operation
        type(input_key_t), intent(in) :: labels(:) !! labels to inputs
        logical :: matching
        integer :: input_seq_len(size(inputs))
        type(value_item_t) :: first_item(size(inputs))

        if (.not. op % is_elemental()) then
            matching = op % args_match(inputs, labels)
            return
        end if

        input_seq_len = argument_sequence_lengths(inputs)

        if (maxval(input_seq_len) == 0) then
            matching = op % args_match(inputs, labels)
            return
        end if

        ! here we deal with sequence -- only check first element

        call make_sequential_input_vector(inputs, 1, first_item, .false.)
        matching = op % args_match(first_item, labels)

    end function


    pure subroutine fetch_operation(operation_db, opname, inputs, labels, op, err)
        !! fetches an operation from the operation database based on its name

        type(operation_db_t), intent(in) :: operation_db !! operation catalog
        character(len=*), intent(in) :: opname !! operation name
        type(value_item_t), intent(in) :: inputs(:) !! inputs for matching the operation
        type(input_key_t), intent(in) :: labels(:) !! labels to inputs
        class(operation_t), intent(out), allocatable :: op !! allocatable operation
        class(err_t), intent(out), optional :: err !! error object

        integer :: i
        logical :: is_match(operation_db % current_size)

        is_match = .false.

        do i = 1, operation_db % current_size
            associate (entry => operation_db % db(i))
                if (opname == entry % opname) then
                    if (.not. allocated(entry%op)) continue
                    if (is_operation_matching_sequence_safe(entry%op, inputs, labels)) then
                        is_match(i) = .true.
                    end if
                end if
            end associate
        end do

        associate (num_matches => count(is_match))
            if (num_matches /= 1) then
                if (num_matches == 0) then
                    call seterr(err, "unknown operation " // opname)
                    return
                end if
                block
                    character(len=256) :: errbuf
                    write(errbuf, *) "operation ", opname, " yielded ambigous match (", num_matches, " entries)"
                    call seterr(err, trim(errbuf))
                    return
                end block
            end if
        end associate

        ! exactly one match is expected

        associate(matched_indices => pack([(i, i = 1, operation_db % current_size)], is_match))
            associate (entry => operation_db % db(matched_indices(1)))
                allocate(op, source=entry % op)
            end associate
        end associate

    end subroutine

    pure subroutine add_operation(operation_db, op, err)
        !! fetches an operation from the operation database based on its name

        type(operation_db_t), intent(inout) :: operation_db !! operation catalog
        class(operation_t), intent(in) :: op !! allocatable operation
        class(err_t), intent(out), optional :: err !! error object

        character(len=:), allocatable :: opname
        integer :: i

        opname = op % name()

        if (opname == "") then
            error stop "empty operation name not permitted"
        end if

        do i = 1, size(operation_db % db)
            associate (entry => operation_db % db(i))
                if (entry % opname /= "") cycle
                allocate(entry % op, source=op)
                entry % opname = opname
                operation_db%current_size = max(operation_db%current_size, i)
                return
            end associate
        end do

        error stop "operation database FULL"
    end subroutine
end module
