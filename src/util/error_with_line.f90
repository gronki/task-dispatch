module line_error_m

    use error_m
    use tokenizer_m, only: token_loc_t

    implicit none (type, external)

    interface seterr
        module procedure :: seterr_loc
    end interface

contains

    pure subroutine seterr_loc(err, message, loc)
        class(err_t), intent(inout), optional :: err
        character(len=*), intent(in) :: message
        type(token_loc_t), intent(in) :: loc

        character(len=1024) :: message_with_loc

        if (loc%line == -1 .and. loc%offset == -1) then
            call seterr(err, message)
            return
        end if

        write(message_with_loc, '(dt,2x,a)') loc, trim(message)

        call seterr(err, trim(message_with_loc))
    end subroutine

end module
