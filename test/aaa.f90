module ttt

    type :: a_t
        real :: f
    end type

    type :: b_t
        type(a_t), allocatable :: a
    end type

end module

program ppp

    use ttt
    type(b_t), target :: b
    type(a_t), pointer :: ap

    allocate(b%a, source=a_t(f=1.0))

    ap => b%a

    print *, ap%f

end program
