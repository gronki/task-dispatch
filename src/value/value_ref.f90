module value_ref_m

    use value_m
    implicit none (type, external)
    private

    type value_ref_t
        class(value_t), pointer :: ref => null()
        logical :: owner = .false.
    end type

    public :: value_ref_t

    interface assignment(=)
        module procedure assign_value_ref
    end interface

    public :: dealloc_value, move_value, assignment(=)

contains

    subroutine dealloc_value(value_ref)
        type(value_ref_t), intent(inout) :: value_ref

        if (value_ref % owner .and. associated(value_ref % ref)) then
            deallocate(value_ref % ref)
            value_ref % owner = .false.
        else
            nullify(value_ref % ref)
        end if
    end subroutine

    subroutine move_value_ref(from, to)
        type(value_ref_t), intent(inout) :: from, to

        if (associated(to % ref) &
        &       .and. .not. associated(from % ref, to % ref) &
        &       .and. to % owner) &
            call dealloc_value(to)
        to % ref => from % ref
        to % owner = from % owner
        from % owner = .false.
    end subroutine

    subroutine assign_value_ref(to, from)
        type(value_ref_t), intent(inout) :: to
        type(value_ref_t), intent(in) :: from

        if (associated(to % ref) &
        &       .and. .not. associated(from % ref, to % ref) &
        &       .and. to % owner) &
            call dealloc_value(to)
        to % ref => from % ref
        to % owner = .false.
    end subroutine

    subroutine move_value(ptr, value, from, to)
        class(value_t), intent(in), pointer, optional :: ptr
        class(value_t), intent(in), optional :: value
        type(value_ref_t), intent(inout), optional :: from
        type(value_ref_t), intent(inout) :: to

        if (sum(merge(1, 0, [present(ptr), present(value), present(from)])) /= 1) then
            error stop "own_value: exactly one of: (value, ptr, from) must be provided"
        end if

        call dealloc_value(to)

        if (present(ptr)) then
            to % owner = .true.
            to % ref => ptr
        else if (present(value)) then
            to % owner = .true.
            allocate(to % ref, source=value)
        else if (present(from)) then
            call move_value_ref(from=from, to=to)
        end if
    end subroutine

end module
