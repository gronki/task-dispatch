module value_m
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

        trace%str = "{" // value%to_str() // "}"

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


end module value_m
