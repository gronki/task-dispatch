program test_refs

    use value_base_m
    use real_value_m

    implicit none (type, external)

    ! type(value_ref_t) :: ref1, ref2

    ! allocate(ref1 % value, source=real_value(1.0_f64))
    ! ref1 % owner = .true.

    ! call move(from=ref1, to=ref2)

    ! if (.not. (associated(ref1 % value, ref2 % value))) stop 1
    ! if (.not. (ref1 % owner .eqv. .false.)) stop 2
    ! if (.not. (ref2 % owner .eqv. .true.)) stop 2

    ! call dealloc(ref1)
    ! call dealloc(ref2)

    ! if (.not. (.not. associated(ref2 % value))) stop 3

end program
