module test_assert_m

    implicit none

contains

    subroutine assert(value, message)
        use iso_fortran_env, only: error_unit
        logical :: value
        character(len=*) :: message

        if (.not. value) then
            write (error_unit, '(A, A)') "Assertion failed: ", message
            error stop
        end if
    end subroutine

    subroutine assert_not(value, message)
        use iso_fortran_env, only: error_unit
        logical :: value
        character(len=*) :: message

        if (value) then
            write (error_unit, '(A, A, A)') "Assertion failed: ", message, " is true"
            error stop
        end if
    end subroutine

end module
