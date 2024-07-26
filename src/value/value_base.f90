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

contains

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
