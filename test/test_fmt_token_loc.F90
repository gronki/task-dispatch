program test_fmt_token_loc

    use tokenizer_m
    use test_assert_m
#   include "assert_macro.h"

    type(token_loc_t) :: a, b
    character(len=128) :: l, s
    integer :: iostat

    print *, '*** test_fmt_token_loc ***'

    a = token_loc_t(line=12, offset=124)

    write(l,*) a
    print *, a
    read(l,*) b
    print *, b

    call assertm(a%line == b%line)
    call assertm(a%offset == b%offset)

    write(l,'(dt, 2x, a)') a, 'ala ma kota'
    print *, a
    read(l,'(dt, 2x, a)') b, s
    print *, b
    print *, s

    call assertm(a%line == b%line)
    call assertm(a%offset == b%offset)

    l = "test nie test"
    read(l, '(dt, 2x, a)', iostat=iostat) b, s
    call assertm(iostat /= 0)

end program