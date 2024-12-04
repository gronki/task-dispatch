program test_fmt_token_loc

    use tokenizer_m
    use test_assert_m
#   include "assert_macro.h"

    type(token_loc_t) :: a, b
    character(len=128) :: l, s, iomsg
    integer :: iostat

    print *, '*** test_fmt_token_loc ***'

    a = token_loc_t(line=36, offset=14)
    print '(2i8)', a%line, a%offset

    write (l, '(dt)') a
    write (*, '(a)') l
    iomsg = ""
    read(l,*,iostat=iostat,iomsg=iomsg) b
    print '(2i8)', b%line, b%offset

    print *, iomsg
    call assertm(iostat == 0)
    call assertm(a%line == b%line)
    call assertm(a%offset == b%offset)

    l(1:1) = 'z'
    iomsg = ""
    read(l,*,iostat=iostat,iomsg=iomsg) b
    print *, iomsg
    call assertm(iostat /= 0)

    write(l,'(dt, 2x, a)') a, 'ala ma kota'
    write (*, '(a)') l
    iomsg = ""
    read(l,'(dt, 2x, a)', iostat=iostat, iomsg=iomsg) b, s
    print '(2i8)', b%line, b%offset
    print '(a)', s

    print *, iomsg
    call assertm(iostat == 0)
    call assertm(a%line == b%line)
    call assertm(a%offset == b%offset)

    l = "test nie test"
    read(l, '(dt, 2x, a)', iostat=iostat) b, s
    call assertm(iostat /= 0)

end program
