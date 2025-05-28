program test_input_args

   use input_args_m
   use real_value_m
   use value_m
   use error_m
   implicit none (type, external)



   block

      type(arg_entry_t), allocatable :: argspec(:)
      type(argument_match_t), allocatable, target :: match(:)
      type(input_key_t), allocatable :: actual_keys(:)
      type(value_item_t), allocatable, target :: actual_args(:)
      type(err_t) :: err
      type(value_ref_t), allocatable :: actual_args_refs(:)
      integer, parameter :: arg1 = 1, arg2 = 2, arg3 = 3

      argspec = [ &
      & arg_entry_t(pos=arg1, name="arg1"), &
      & arg_entry_t(pos=arg2, name="arg2", default=real_value_t(value=22.0_real_k)), &
      & arg_entry_t(pos=arg3, name="arg3", default=real_value_t(value=33.0_real_k)) ]

      actual_keys = [ input_key_t(has_key=.false.), input_key_t(key="arg3", has_key=.true.) ]

      actual_args = [ &
      & value_item_t(value=real_value_t(value=1.0_real_k)), &
      & value_item_t(value=real_value_t(value=3.0_real_k)) ]

      allocate(match(size(argspec)))

      call match_arguments(argspec, actual_keys, match, err)

      if (check(err)) then
         print *, err
         error stop
      end if

      actual_args_refs = connect_matched_args(item_to_ref(actual_args), match)

      select type (valptr => actual_args_refs(arg1) % value)
      type is (real_value_t)
         if (valptr % value /= 1.0_real_k) error stop "arg1"
         print *, "arg1: ", valptr % value
      class default
         error stop "arg1: incorrect return type!"
      end select

      select type (valptr => actual_args_refs(arg2) % value)
      type is (real_value_t)
         if (valptr % value /= 22.0_real_k) error stop "arg2"
         print *, "arg2: ", valptr % value
      class default
         error stop "arg2: incorrect return type!"
      end select

      select type (valptr => actual_args_refs(arg3) % value)
      type is (real_value_t)
         if (valptr % value /= 3.0_real_k) error stop "arg3"
         print *, "arg3: ", valptr % value
      class default
         error stop "arg3: incorrect return type!"
      end select

      print '(a)', "OK"

   end block
   

   block

      type(arg_entry_t), allocatable :: argspec(:)
      type(argument_match_t), allocatable, target :: match(:)
      type(input_key_t), allocatable :: actual_keys(:)
      type(value_item_t), allocatable, target :: actual_args(:)
      type(err_t) :: err
      type(value_ref_t), allocatable :: actual_args_refs(:)

      argspec = [ &
      & arg_entry_t(pos=1, name="arg1"), &
      & arg_entry_t(pos=2, name="arg2", default=real_value_t(value=22.0_real_k)), &
      & arg_entry_t(pos=3, name="arg3", default=real_value_t(value=33.0_real_k)) ]

      actual_keys = [ input_key_t(has_key=.false.), input_key_t(key="arg4", has_key=.true.) ]

      actual_args = [ &
      & value_item_t(value=real_value_t(value=1.0_real_k)), &
      & value_item_t(value=real_value_t(value=3.0_real_k)) ]

      allocate(match(size(argspec)))

      call match_arguments(argspec, actual_keys, match, err)

      if (check(err)) then
         print *, "OK: ", err
      else
         error stop "error expected but not obtained"
      end if

   end block

   block

      type(arg_entry_t), allocatable :: argspec(:)
      type(argument_match_t), allocatable, target :: match(:)
      type(input_key_t), allocatable :: actual_keys(:)
      type(value_item_t), allocatable, target :: actual_args(:)
      type(err_t) :: err
      type(value_ref_t), allocatable :: actual_args_refs(:)

      argspec = [ &
      & arg_entry_t(pos=1, name="arg1", default=real_value_t(value=11.0_real_k)), &
      & arg_entry_t(pos=2, name="arg2"), &
      & arg_entry_t(pos=3, name="arg3", default=real_value_t(value=33.0_real_k)) ]

      actual_keys = [ input_key_t(has_key=.false.), input_key_t(key="arg3", has_key=.true.) ]

      actual_args = [ &
      & value_item_t(value=real_value_t(value=1.0_real_k)), &
      & value_item_t(value=real_value_t(value=3.0_real_k)) ]

      allocate(match(size(argspec)))

      call match_arguments(argspec, actual_keys, match, err)

      if (check(err)) then
         print *, "OK: ", err
      else
         error stop "error expected but not obtained"
      end if

   end block


   block

      type(arg_entry_t), allocatable :: argspec(:)
      type(argument_match_t), allocatable, target :: match(:)
      type(input_key_t), allocatable :: actual_keys(:)
      type(value_item_t), allocatable, target :: actual_args(:)
      type(err_t) :: err
      type(value_ref_t), allocatable :: actual_args_refs(:)

      argspec = [ &
      & arg_entry_t(pos=1, name="arg1"), &
      & arg_entry_t(pos=2, name="arg2", default=real_value_t(value=22.0_real_k)), &
      & arg_entry_t(pos=3, name="arg3", default=real_value_t(value=33.0_real_k)) ]

      actual_keys = [ input_key_t(key="arg1", has_key=.true.), input_key_t(has_key=.false.) ]

      actual_args = [ &
      & value_item_t(value=real_value_t(value=1.0_real_k)), &
      & value_item_t(value=real_value_t(value=3.0_real_k)) ]

      allocate(match(size(argspec)))

      call match_arguments(argspec, actual_keys, match, err)

      if (check(err)) then
         print *, "OK: ", err
      else
         error stop "error expected but not obtained"
      end if

   end block

   block

      type(arg_entry_t), allocatable :: argspec(:)
      type(argument_match_t), allocatable, target :: match(:)
      type(input_key_t), allocatable :: actual_keys(:)
      type(value_item_t), allocatable, target :: actual_args(:)
      type(err_t) :: err
      type(value_ref_t), allocatable :: actual_args_refs(:)

      argspec = [ &
      & arg_entry_t(pos=1, name="arg1"), &
      & arg_entry_t(pos=2, name="arg2", default=real_value_t(value=22.0_real_k)), &
      & arg_entry_t(pos=3, name="arg3", default=real_value_t(value=33.0_real_k)) ]

      actual_keys = [ input_key_t(has_key=.false.), input_key_t(key="arg1", has_key=.true.) ]

      actual_args = [ &
      & value_item_t(value=real_value_t(value=1.0_real_k)), &
      & value_item_t(value=real_value_t(value=3.0_real_k)) ]

      allocate(match(size(argspec)))

      call match_arguments(argspec, actual_keys, match, err)

      if (check(err)) then
         print *, "OK: ", err
      else
         error stop "error expected but not obtained"
      end if

   end block

   block

      type(arg_entry_t), allocatable :: argspec(:)
      type(argument_match_t), allocatable, target :: match(:)
      type(input_key_t), allocatable :: actual_keys(:)
      type(value_item_t), allocatable, target :: actual_args(:)
      type(err_t) :: err
      type(value_ref_t), allocatable :: actual_args_refs(:)

      argspec = [ &
      & arg_entry_t(pos=1, name="arg1") ]

      actual_keys = [ input_key_t(has_key=.false.)  ]

      actual_args = [ &
      & value_item_t(value=real_value_t(value=1.0_real_k)) ]

      allocate(match(size(argspec)))

      call match_arguments(argspec, actual_keys, match, err)

      if (check(err)) then
         print *, "ERR: ", err
         error stop
      end if

      print *, "OK"

   end block

   block

      type(arg_entry_t), allocatable :: argspec(:)
      type(argument_match_t), allocatable, target :: match(:)
      type(input_key_t), allocatable :: actual_keys(:)
      type(value_item_t), allocatable, target :: actual_args(:)
      type(err_t) :: err
      type(value_ref_t), allocatable :: actual_args_refs(:)

      allocate(argspec(0))
      allocate(actual_keys(0))
      allocate(actual_args(0))

      allocate(match(size(argspec)))

      call match_arguments(argspec, actual_keys, match, err)

      if (check(err)) then
         print *, "ERR: ", err
         error stop
      end if

      print *, "OK"

   end block


   block

      type(arg_entry_t), allocatable :: argspec(:)
      type(argument_match_t), allocatable, target :: match(:)
      type(input_key_t), allocatable :: actual_keys(:)
      type(value_item_t), allocatable, target :: actual_args(:)
      type(err_t) :: err
      type(value_ref_t), allocatable :: actual_args_refs(:)

      allocate(argspec(0))

      actual_keys = [ input_key_t(has_key=.false.)  ]

      actual_args = [ &
      & value_item_t(value=real_value_t(value=1.0_real_k)) ]

      allocate(match(size(argspec)))

      call match_arguments(argspec, actual_keys, match, err)


      if (check(err)) then
         print *, "OK: ", err
      else
         error stop "error expected but not obtained"
      end if

   end block


   block

      type(arg_entry_t), allocatable :: argspec(:)
      type(argument_match_t), allocatable, target :: match(:)
      type(input_key_t), allocatable :: actual_keys(:)
      type(value_item_t), allocatable, target :: actual_args(:)
      type(err_t) :: err
      type(value_ref_t), allocatable :: actual_args_refs(:)

      argspec = [ &
      & arg_entry_t(pos=1, name="arg1", default=real_value_t(value=2.0_real_k)) ]

      allocate(actual_keys(0), actual_args(0))

      allocate(match(size(argspec)))

      call match_arguments(argspec, actual_keys, match, err)

      if (check(err)) then
         print *, "ERR: ", err
         error stop
      end if
      
      actual_args_refs = connect_matched_args(item_to_ref(actual_args), match)

      select type (valptr => actual_args_refs(1) % value)
      type is (real_value_t)
         if (valptr % value /= 2.0_real_k) error stop "arg1"
         print *, "arg1: ", valptr % value
      class default
         error stop "arg1: incorrect return type!"
      end select

      print *, "OK"

   end block


end program
