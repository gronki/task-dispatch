module problematic1_m
    implicit none (type, external)

    type number_t
        integer :: number
    end type

    type :: value_t
    contains
        procedure :: get_number
    end type

contains

    function get_number(value) result(number)
        class(value_t), intent(in) :: value
        type(number_t) :: number

        number%number = 5
    end function

    subroutine print_trace(value)
        type(value_t), intent(in) :: value

        ! works
        associate(item1 => get_number(value))
            print *, item1 % number
        end associate

        ! no worky
        associate(item2 => value % get_number())
            print *, item2 % number
        end associate
    end subroutine


end module problematic1_m

program test_rpoblematic
    use problematic1_m

    call print_trace(value_t())
end program


