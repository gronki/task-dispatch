module operation_database_m

    use operation_m
    use error_m

    implicit none (type, external)

    type operation_db_entry_t
        character(len=32) :: opname = ""
        class(operation_t), allocatable :: op
    end type

    integer, parameter :: OPERATION_DB_CAPACITY = 128

    type operation_db_t
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


    pure subroutine fetch_operation(operation_db, opname, op, err)
        !! fetches an operation from the operation database based on its name

        type(operation_db_t), intent(in) :: operation_db !! operation catalog
        character(len=*), intent(in) :: opname !! operation name
        class(operation_t), intent(out), allocatable :: op !! allocatable operation
        class(err_t), intent(out), optional :: err !! error object

        integer :: i

        do i = 1, size(operation_db % db)
            associate (entry => operation_db % db(i))
                if (opname == entry % opname) then
                    allocate(op, source=entry % op)
                    return
                end if
            end associate
        end do

        call seterr(err, "unknown operation " // opname)
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
                if (opname == entry % opname) then
                    call seterr(err, "operation " // opname // " already defined")
                    return
                end if
                if (entry % opname == "") then
                    allocate(entry % op, source=op)
                    entry % opname = opname
                    return
                end if
            end associate
        end do

        error stop "operation database FULL"
    end subroutine
end module
