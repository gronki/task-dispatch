module sequence_value_m
    use value_base_m
    use, intrinsic :: iso_fortran_env, only: f64 => real64

    implicit none (type, external)
    public

    type, extends(value_t) :: sequence_value_t
        type(value_item_t), allocatable :: items(:)
    contains
        procedure :: to_str => sequence_to_str
        procedure :: get_trace => sequence_get_trace
    end type

contains


    pure function sequence_to_str(value) result(str)
        class(sequence_value_t), intent(in) :: value
        character(len=64) :: buf
        character(len=:), allocatable :: str

        integer :: i

        str = "["

        if (allocated(value%items)) then
            do i =1, size(value%items)
                str = str // trim(value%items(i) % value % to_str()) &
                    // adjustl(trim(merge("  ", " ,", i == size(value%items))))
            end do
        end if

        str = str // "]"
    end function

    pure function sequence_get_trace(value) result(trace)
        class(sequence_value_t), intent(in) :: value
        character(len=:), allocatable :: trace
        integer :: i

        if (allocated(value%trace)) then
            trace = value%trace
            return
        end if

        trace = "["

        if (allocated(value%items)) then
            do i =1, size(value%items)
                trace = trace // trim(value%items(i) % value % get_trace()) &
                    // adjustl(trim(merge("  ", " ,", i == size(value%items))))
            end do
        end if

        trace = trace // "]"

    end function

end module
