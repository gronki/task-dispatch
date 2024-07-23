module value_base_m
    implicit none (type, external)
    public

    type, abstract :: value_t
        character(len=:), allocatable :: trace
    contains
        procedure :: get_trace
        procedure :: to_str
    end type

    type value_item_t
        class(value_t), allocatable :: value
    end type

    ! type value_ref_t
    !     class(value_t), pointer :: value => null()
    !     logical :: owner = .false.
    ! end type

    ! interface dealloc
    !     module procedure value_ref_dealloc
    ! end interface

    ! interface move
    !     module procedure move_value_ref
    ! end interface

    ! interface assignment(=)
    !     module procedure assign_value_ref
    ! end interface

    ! interface has_trace
    !     module procedure has_trace_value
    !     module procedure has_trace_value_item
    ! end interface

contains

    ! impure elemental subroutine value_ref_dealloc(value_ref)
    !     type(value_ref_t), intent(inout) :: value_ref

    !     if (value_ref % owner .and. associated(value_ref % value)) then
    !         deallocate(value_ref % value)
    !         value_ref % owner = .false.
    !     end if
    ! end subroutine

    ! impure elemental subroutine move_value_ref(from, to)
    !     type(value_ref_t), intent(inout) :: from, to

    !     if (associated(to % value) &
    !     &       .and. .not. associated(from % value, to % value) &
    !     &       .and. to % owner) &
    !         deallocate(to % value)
    !     to % value => from % value
    !     to % owner = from % owner
    !     from % owner = .false.
    ! end subroutine

    ! impure elemental subroutine assign_value_ref(to, from)
    !     type(value_ref_t), intent(inout) :: to
    !     type(value_ref_t), intent(in) :: from

    !     if (associated(to % value) &
    !     &       .and. .not. associated(from % value, to % value) &
    !     &       .and. to % owner) &
    !         deallocate(to % value)
    !     to % value => from % value
    !     to % owner = .false.
    ! end subroutine

    ! elemental function has_trace_value(val) result(value_has_trace)
    !     class(value_t), intent(in) :: val
    !     logical :: value_has_trace

    !     value_has_trace = allocated(val % trace)
    ! end function

    ! elemental function has_trace_value_item(val_item) result(value_has_trace)
    !     type(value_item_t), intent(in) :: val_item
    !     logical :: value_has_trace

    !     value_has_trace = .false.
    !     if (.not. allocated(val_item % value)) return
    !     value_has_trace = allocated(val_item % value % trace)
    ! end function

    pure function get_trace(value) result(trace)
        class(value_t), intent(in) :: value
        character(len=:), allocatable :: trace

        if (allocated(value%trace)) then
            trace = value%trace
            return
        end if

        trace = value%to_str()

    end function

    pure function to_str(value) result(str)
        class(value_t), intent(in) :: value
        character(len=:), allocatable :: str

        error stop "no string representation for this value type. override to_str to implement"
    end function

end module value_base_m
