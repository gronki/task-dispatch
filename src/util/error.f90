module error_m

    implicit none (type, external)

    type err_messg_line_t
        character(len=512) :: text = ""
    end type

    type err_t
        type(err_messg_line_t), allocatable :: messages(:)
        integer :: num_errors = 0
    contains
        procedure, private :: error_write_formatted
        generic :: write(formatted) => error_write_formatted
    end type

    interface seterr
        module procedure seterr_string
    end interface

    private :: seterr_string

    interface clear
        module procedure clear_err
    end interface

    private :: clear_err

contains

    pure function check(err)

        class(err_t), intent(in), optional :: err
        logical :: check

        if (.not. present(err)) then
            check = .false.
            return
        end if

        check = err%num_errors > 0

    end function

    pure subroutine seterr_string(err, message)
        class(err_t), intent(inout), optional :: err
        character(len=*), intent(in) :: message
        type(err_messg_line_t), allocatable :: new_messages(:)

        if (.not. present(err)) &
            error stop "unhandled error: " // message


        err %num_errors = err%num_errors+1

        allocate(new_messages(err%num_errors))
        new_messages(1)%text = message
        if (err%num_errors > 1) then
            new_messages(2:)%text=err%messages%text
        end if
        call move_alloc(from=new_messages, to=err%messages)
    end subroutine

    pure subroutine clear_err(err)
        class(err_t), intent(inout), optional :: err

        if (.not. present(err)) return

        if (allocated(err % messages)) &
            deallocate(err % messages)
        err%num_errors = 0
    end subroutine

    subroutine error_write_formatted(err,unit,iotype,v_list,iostat,iomsg)
        class(err_t), intent(in) :: err
        integer, intent(in) :: unit
        character(len=*), intent(in) :: iotype
        integer, intent(in) :: v_list(:)
        integer, intent(out) :: iostat
        character(len=*), intent(inout) :: iomsg
        character(len=80) :: buffer
        integer :: i

        if (check(err)) then
            write(unit,'(i0,a,*(/"error: ",A,:))',iostat=iostat,iomsg=iomsg) err %num_errors, " error(s)", &
                (trim(err%messages(i)%text), i=1, err%num_errors)
        else
            write(unit,'(A)',iostat=iostat,iomsg=iomsg) "no errors"
        end if
    end subroutine

end module
