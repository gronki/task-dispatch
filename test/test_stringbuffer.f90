program test_stringbuffer

    use stringbuffer_m, only: stringbuffer_t
    use iso_fortran_env, only: error_unit
    implicit none (type, external)

    type(stringbuffer_t) :: sb
    character(len=:), allocatable :: s

    print *, '*** TEST_STRINGBUFFER ***'

    call sb%append("a")
    s = sb % str()

    if (len(s) /= 1) error stop "len(s) /= 1"
    if (s /= "a") then
        write (error_unit, *) "'", s, "' /= a"
        error stop
    end if

    call sb%append("bc")
    s = sb % str()

    if (len(s) /= 3) error stop "len(s) /= 3"
    if (s /= "abc") then
        write (error_unit, *) "'", s, "' /= abc"
        error stop
    end if

end program
