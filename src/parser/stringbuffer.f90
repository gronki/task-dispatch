module stringbuffer_m

    implicit none (type, external)
    private
    public :: stringbuffer_t, len

    type stringbuffer_t
        character(len=1024), private :: buf
        integer, private :: stored_len = 0
    contains
        procedure :: append => stringbuffer_append
        procedure :: str => stringbuffer_retrieve
        procedure :: len => stringbuffer_get_len
    end type

    interface len
        module procedure stringbuffer_get_len
    end interface

contains

    pure  subroutine stringbuffer_append(sb, str)
        class(stringbuffer_t), intent(inout) :: sb
        character(len=*), intent(in) :: str

        if (sb%stored_len + len(str) > len(sb%buf)) &
            error stop "buffer capacity exceeded"

        sb%buf(1+sb%stored_len:len(str)+sb%stored_len) = str
        sb%stored_len = sb%stored_len + len(str)
    end subroutine

    pure  function stringbuffer_retrieve(sb) result(str)
        class(stringbuffer_t), intent(in) :: sb
        character(len=:), allocatable :: str

        if (sb % stored_len == 0) then
            str = ""
            return
        end if

        str = sb % buf(:sb % stored_len)
    end function

    elemental function stringbuffer_get_len(sb) result(sb_len)
        class(stringbuffer_t), intent(in) :: sb
        integer :: sb_len

        sb_len = sb % stored_len
    end function

end module
