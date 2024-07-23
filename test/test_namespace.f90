program test_namespace

    use namespace_m
    use real_value_m
    use error_m
    implicit none (type, external)

    type(namespace_t) :: ns

    call ns % push("one", real_value(1.0_f64))
    call ns % push("two", real_value(2.0_f64))

    select type (val => fetch_ns_value(ns, "two"))
      type is (real_value_t)
        if (val % value == 2.0_f64) then
            print *, "OK"
        else
            stop 11
        end if
      class default
        stop 1
    end select

    block
        class(value_t), allocatable :: val
        type(err_t) :: err

        call ns % fetch("one", val, err)
        if (check(err)) stop 2
        call ns % fetch("three", val, err)
        if (.not. check(err)) stop 3
    end block

contains

    function fetch_ns_value(ns, key) result(val)
        use value_base_m
        type(namespace_t) :: ns
        character(len=*) :: key
        class(value_t), allocatable :: val

        call ns % fetch(key, val)
    end function

end program
