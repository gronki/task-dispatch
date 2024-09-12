module namespace_m

    use value_m
    use error_m
    implicit none (type, external)
    public

    type namespace_item_t
        logical :: used = .false.
        character(len=32) :: name = ""
        class(value_t), allocatable :: value
    end type

    integer, parameter :: NAMESPACE_CAPACITY = 4096

    type namespace_t
        type(namespace_item_t) :: items(NAMESPACE_CAPACITY)
    contains
        procedure :: push => namespace_push_value
        procedure :: move_in => namespace_move_value_in
        procedure :: fetch => namespace_fetch_value
    end type

contains

    pure subroutine namespace_push_value(namespace, key, value, err)
        class(namespace_t), intent(inout) :: namespace
        character(len=*), intent(in) :: key
        class(value_t), intent(in) :: value
        type(err_t), intent(out), optional :: err

        class(value_t), allocatable :: new_value

        allocate(new_value, source=value)
        call namespace_move_value_in(namespace, key, new_value, err)
    end subroutine

    pure subroutine namespace_move_value_in(namespace, key, value, err)
        class(namespace_t), intent(inout) :: namespace
        character(len=*), intent(in) :: key
        class(value_t), intent(inout), allocatable :: value
        type(err_t), intent(out), optional :: err

        integer :: i

        do i = 1, size(namespace % items)
            associate (item => namespace % items(i))
                if ((.not. item % used) .or. (item % used .and. item % name == key)) then
                    if (.not. item % used) then
                        item % name = trim(key)
                        item % used = .true.
                    end if
                    call move_alloc(from=value, to=item % value)
                    return
                end if
            end associate
        end do

        call seterr(err, "namespace full")

    end subroutine

    pure subroutine namespace_fetch_value(namespace, key, value, err)
        class(namespace_t), intent(in) :: namespace
        character(len=*), intent(in) :: key
        class(value_t), allocatable, intent(inout) :: value
        type(err_t), intent(out), optional :: err

        integer :: i

        do i = 1, size(namespace % items)
            associate (item => namespace % items(i))
                if (.not. item % used) cycle
                if (item % name == key) then
                    value = item%value
                    return
                end if
            end associate
        end do

        call seterr(err, trim(key) // " not found")

    end subroutine


end module
