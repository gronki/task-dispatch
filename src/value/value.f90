module value_m
    use iso_fortran_env, only: debug_output => error_unit
    implicit none (type, external)
    private

    type value_trace_t
        character(len=:), allocatable :: str
    contains
        procedure, private :: trace_write_formatted
        generic :: write(Formatted) => trace_write_formatted
        generic :: operator(==) => traces_are_equal
        procedure, private :: traces_are_equal
        generic :: operator(/=) => traces_are_not_equal
        procedure, private :: traces_are_not_equal
    end type

    public :: value_trace_t

    type, abstract :: value_t
        type(value_trace_t) :: trace
    contains
        procedure :: get_trace
        procedure :: to_str
    end type

    public :: value_t

    type value_item_t
        class(value_t), allocatable :: value
    end type

    public :: value_item_t

    type :: value_ref_t
        class(value_t), pointer :: value => null()
        logical :: owner = .false.
    contains
        procedure :: to_str => value_ref_to_str
        procedure, private :: own_target, own_another_ref
        generic :: own => own_target, own_another_ref
        generic :: refer => refer_target, refer_another_ref
        procedure, private :: refer_target, refer_another_ref
        procedure :: dealloc
    end type

    public :: value_ref_t
    public :: item_to_ref, ref_to_item

contains

    subroutine trace_write_formatted(trace,unit,iotype,v_list,iostat,iomsg)
        CLASS(value_trace_t), intent(in) :: trace
        integer, intent(in) :: unit
        character(len=*), intent(in) :: iotype
        integer, intent(in) :: v_list(:)
        integer, intent(out) :: iostat
        character(len=*), intent(inout) :: iomsg

        if (allocated(trace%str)) then
            write(unit, fmt='(a)', iostat=iostat, iomsg=iomsg)  trace % str
        end if

    end subroutine

    elemental function get_trace(value) result(trace)
        class(value_t), intent(in) :: value
        type(value_trace_t) :: trace

        if (allocated(value%trace%str)) then
            trace%str = value%trace%str
            return
        end if

        trace%str = value%to_str()

    end function

    elemental function traces_are_equal(trace, other_trace)
        CLASS(value_trace_t), intent(in) :: trace, other_trace
        logical :: traces_are_equal

        if (allocated(trace % str) .and. allocated(other_trace % str)) then
            traces_are_equal = (trace % str) == (other_trace % str)
            return
        end if
        traces_are_equal = .false.
    end function

    elemental function traces_are_not_equal(trace, other_trace)
        CLASS(value_trace_t), intent(in) :: trace, other_trace
        logical :: traces_are_not_equal

        traces_are_not_equal = .not. traces_are_equal(trace, other_trace)
    end function

    pure function to_str(value) result(str)
        class(value_t), intent(in) :: value
        character(len=:), allocatable :: str

        error stop "no string representation for this value type. override to_str to implement"
    end function


    elemental function is_owner(ref)
        class(value_ref_t), intent(in) :: ref
        logical :: is_owner
        is_owner = ref % owner
    end function

    function value_ref_to_str(value_ref) result(str)
        class(value_ref_t), intent(in) :: value_ref
        character(len=:), allocatable :: str

        if (.not. associated(value_ref%value)) then
            str = "&(null)"
            return
        end if

        str = "&(" // value_ref % value % to_str() // trim(merge(" owner)", ")      ", value_ref % owner))

    end function

    subroutine dealloc(ref)
        class(value_ref_t), intent(inout) :: ref

        if (ref % owner .and. associated(ref % value)) then
            ! write (debug_output, *) "DEALLOC", ref % to_str()
            deallocate(ref % value)
        end if

        ref % owner = .false.
        nullify(ref % value)
    end subroutine

    pure subroutine own_target(ref, tgt)
        class(value_ref_t), intent(inout) :: ref
        class(value_t), intent(inout), target :: tgt

        ref % value => tgt
        ref % owner = .true.
    end subroutine

    pure subroutine own_another_ref(ref, another)
        class(value_ref_t), intent(inout) :: ref
        type(value_ref_t), intent(inout) :: another

        ref % value => another % value
        ref % owner = another % owner
        another % owner = .false.
    end subroutine

    pure subroutine refer_target(ref, tgt)
        class(value_ref_t), intent(inout) :: ref
        class(value_t), intent(inout), target :: tgt

        ref % value => tgt
        ref % owner = .false.
    end subroutine

    pure subroutine refer_another_ref(ref, another)
        class(value_ref_t), intent(inout) :: ref
        type(value_ref_t), intent(inout) :: another

        ref % value => another % value
        ref % owner = .false.
    end subroutine

    impure elemental function item_to_ref(item) result(ref)
        type(value_item_t), intent(inout), target :: item
        type(value_ref_t) :: ref

        call ref % refer(item % value)
    end function

    impure elemental subroutine ref_to_item(ref, item)
        type(value_ref_t), intent(in) :: ref
        type(value_item_t), intent(out) :: item

        allocate(item % value, source=ref % value)
    end subroutine

end module value_m
