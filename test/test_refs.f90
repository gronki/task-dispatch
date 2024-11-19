program test_refs

!     use value_m
!     use real_value_m

!     implicit none (type, external)

!     TYPE :: struct_with_ref_t
!         TYPE(value_ref_t) :: ref
!     END TYPE

!     print *, '*** TEST_REFS ***'


!     print *, '*   test 1 '
!     call test_refs_1

!     print *, '*   test 2 '
!     call test_refs_2

!     print *, '*   test 3 '
!     call test_refs_3

! contains

!     subroutine test_refs_1
!         type(value_ref_t) :: ref1, ref2, ref3
!         type(real_value_t), pointer :: val

!         allocate(val, source=real_value(1.0_f64))

!         call ref1 % own(val)
!         print *, ref1%to_str(), ref2%to_str(), ref3%to_str()

!         if (.not. ref1 % owner) stop -1
!         if (.not. associated(ref1 % value)) stop -2

!         call ref2 % own(ref1)
!         print *, ref1%to_str(), ref2%to_str(), ref3%to_str()

!         if (.not. (associated(ref1 % value, ref2 % value))) stop 1
!         if (.not. (ref1 % owner .eqv. .false.)) stop 2
!         if (.not. (ref2 % owner .eqv. .true.)) stop 2

!         call ref3 % refer(ref2)
!         print *, ref1%to_str(), ref2%to_str(), ref3%to_str()

!         if (.not. associated(ref3 % value, ref2 % value)) stop 1
!         if (ref3 % owner) stop 2

!         call ref1 % dealloc
!         print *, ref1%to_str(), ref2%to_str(), ref3%to_str()

!         call ref2 % dealloc
!         call ref3 % dealloc
!         print *, ref1%to_str(), ref2%to_str(), ref3%to_str()

!         if (associated(ref2 % value)) stop 3
!     end subroutine

!     function fun_returns_owned_ref(r) result(ref)
!         real :: r
!         type(value_ref_t) :: ref
!         type(real_value_t), pointer :: ptr
!         allocate(ptr)
!         ptr % value = r
!         call ref % own(ptr)
!     end function

!     function fun_returns_ref() result(ref)
!         type(value_ref_t) :: ref
!         type(real_value_t), allocatable, save, target :: val
!         if (.not. allocated(val)) then
!             allocate(val)
!             val%value = 1.0
!         end if
!         call ref % refer(val)
!     end function

!     subroutine test_refs_2
!         type(value_ref_t) :: ref1, ref2
!         ref1 = fun_returns_owned_ref(1.0)
!         associate (r=>fun_returns_owned_ref(2.0))
!             print *, ref1 % to_str(), r % to_str()
!             call r % dealloc
!         end associate
!         call ref1 % dealloc
!         ref2 = fun_returns_ref()
!         associate (r=>fun_returns_ref())
!             print *, ref2%to_str(), r % to_str()
!             call r % dealloc
!         end associate
!         call ref2 % dealloc
!     end subroutine

!     subroutine test_refs_3
!         TYPE(struct_with_ref_t) :: s1, s2
!         s1 % ref = fun_returns_owned_ref(1.0)
!         s2 = s1
!         print *, s1 % ref % to_str(), s2 % ref % to_str()
!         s2 % ref = fun_returns_owned_ref(2.0)
!         print *, s1 % ref % to_str(), s2 % ref % to_str()
!         call s1 % ref % dealloc
!         call s2 % ref % dealloc
!     end subroutine

end program
