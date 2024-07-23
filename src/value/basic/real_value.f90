module real_value_m
    use value_base_m
    use, intrinsic :: iso_fortran_env, only: f64 => real64

    implicit none (type, external)
    public

    type, extends(value_t) :: real_value_t
        real(f64) :: value
    contains
        procedure :: to_str => real_value_to_str
    end type

    interface real_value
        module procedure :: real_value_from_real
        module procedure :: real_value_from_str
    end interface

contains

    elemental function real_value_from_real(value, trace) result(value_obj)
        !! build real_value_t object from float
        real(f64), intent(in) :: value
        character(len=*), intent(in), optional :: trace
        type(real_value_t) :: value_obj

        value_obj % value = value
        if (present(trace)) then
            value_obj % trace = trim(adjustl(trace))
        else
            block
                character(len=64) :: buf
                write (buf, '(g10.5)') value_obj % value
                value_obj % trace = trim(adjustl(buf))
            end block
        end if
    end function

    elemental function real_value_from_str(value_str, trace) result(value_obj)
        !! build real_value_t object from string representation
        character(len=*), intent(in) :: value_str
        character(len=*), intent(in), optional :: trace
        type(real_value_t) :: value_obj

        read(value_str, *) value_obj%value

        if (present(trace)) then
            value_obj % trace = trim(adjustl(trace))
        else
            value_obj % trace = value_str
        end if
    end function

    pure function real_value_to_str(value) result(str)
        class(real_value_t), intent(in) :: value
        character(len=64) :: buf
        character(len=:), allocatable :: str

        write (buf, '(g10.5)') value%value
        str = trim(adjustl(buf))
    end function

end module
