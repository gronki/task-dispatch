program main

    implicit none

    type :: typ1
    end type

    type, extends(typ1) :: typ1a
    end type

    type :: typ2
        class(typ1), allocatable :: t1
    end type

    type(typ2) :: t2

    ! works
    t2 = typ2(t1=typ1())

    ! crash
    t2 = typ2(t1=typ1a())

end program