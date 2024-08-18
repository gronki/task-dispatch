program test_refs

    use value_base_m
    use value_ref_m
    use real_value_m

    implicit none (type, external)


    print *, '*** TEST_REFS ***'

    block
        type(value_ref_t) :: ref1, ref2, ref3

        call move_value(value=real_value(1.0_f64), to=ref1)

        if (.not. ref1 % owner) stop -1
        if (.not. associated(ref1 % ref)) stop -2

        call move_value(from=ref1, to=ref2)
        
        if (.not. (associated(ref1 % ref, ref2 % ref))) stop 1
        if (.not. (ref1 % owner .eqv. .false.)) stop 2
        if (.not. (ref2 % owner .eqv. .true.)) stop 2

        ref3 = ref2

        if (.not. associated(ref3 % ref, ref2 % ref)) stop 1
        if (ref3 % owner) stop 2
        
        call dealloc_value(ref1)
        call dealloc_value(ref2)
        call dealloc_value(ref3)

        if (associated(ref2 % ref)) stop 3
    end block



end program
