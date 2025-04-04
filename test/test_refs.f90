program test_refs

   use value_m
   use real_value_m
   use ref_sequence_value_m

   implicit none (type, external)


   print *, '*** TEST_REFS ***'


   print *, '*   test 1 '
   call test_refs_1

   print *, '*   test 2 '
   call test_refs_2

   print *, '*   test 3 '
   call test_refs_3

contains


   subroutine test_refs_1
      type(value_ref_t) :: ref1, ref2
      type(real_value_t), pointer :: myval

      allocate (ref1 % value, source=real_value(1.0_f64)); call keep( ref1 )

      print *, ref1 % to_str(), " ", ref2 % to_str()
      if (num_references(ref1) /= 1) error stop 'num_references(ref1) /= 1'

      ref2 = ref1; call keep( ref2 )

      print *, ref1 % to_str(), " ", ref2 % to_str()
      if (num_references(ref1) /= 2) error stop 'num_references(ref1) /= 2'

      call free( ref1 )

      print *, ref1 % to_str(), " ", ref2 % to_str()
      if (num_references(ref2) /= 1) error stop 'num_references(ref2) /= 1'

      call free( ref2 )

      print *, ref1 % to_str(), " ", ref2 % to_str()
      if (num_references(ref2) > 0) error stop 'num_references(ref2) > 0'

   end subroutine


   subroutine test_refs_2
      type(value_item_t) :: item
      type(value_ref_t) :: ref, ref2
      item % value =  real_value(1.0_f64)

      ref = item_to_ref(item); call keep( ref )

      print *, ref % to_str(), " ", ref2 % to_str()
      if (num_references(ref) /= -1) error stop 'num_references(ref) / -1'

      ref2 =  ref; call keep(ref2)

      print *, ref % to_str(), " ", ref2 % to_str()
      if (num_references(ref2) /= -1) error stop 'num_references(ref2) / -1'

      call free( ref )
      call free( ref2 )

      print *, ref % to_str(), " ", ref2 % to_str()
      if (num_references(ref) /= -1) error stop 'num_references(ref) / -1'

   end subroutine


   subroutine test_refs_3

      type(ref_sequence_value_t), pointer :: seq
      type(value_ref_t) :: ref, seq_ref

      integer :: i

      allocate(seq)
      allocate(seq % items(3))
      allocate(seq % items(1) % value, source=real_value(1.0_f64))
      allocate(seq % items(2) % value, source=real_value(2.0_f64))
      allocate(seq % items(3) % value, source=real_value(3.0_f64))
      call keep( seq % items )

      print *, ref % to_str(), " ", (seq % items(i) % to_str(), " ", i = 1, 3)
      if (num_references(seq % items(1)) /= 1 ) error stop "num_references(seq % items(1)) /= 1"
      if (num_references(seq % items(2)) /= 1 ) error stop "num_references(seq % items(2)) /= 1"
      if (num_references(seq % items(3)) /= 1 ) error stop "num_references(seq % items(3)) /= 1"

      seq_ref % value => seq; call keep( seq_ref )

      print *, ref % to_str(), " ", (seq % items(i) % to_str(), " ", i = 1, 3)

      ref = seq % items(2) ; call keep( ref )

      print *, ref % to_str(), " ", (seq % items(i) % to_str(), " ", i = 1, 3)
      if (num_references(ref) /= 2) error stop "num_references(ref) /= 2"

      call free( seq_ref )

      print *, ref % to_str()
      if (num_references(ref) /= 1) error stop "num_references(ref) /= 1"
      if ( associated(seq_ref % value )) error stop "associated(seq_ref % value )"

   end subroutine

end program
