! module test_operation_add_numbers_m

!     use value_m
!     use sequence_value_m
!     use operation_m
!     use real_value_m
!     implicit none (type, external)
!     private
!     public :: add_numbers_t

!     type, extends(operation_t) :: add_numbers_t
!     contains
!         procedure, nopass :: args_match
!         procedure, nopass :: name
!         procedure :: exec
!     end type

! contains


!     pure function args_match(inputs, labels)
!         !! return true if the operation is able to handle
!         !! the arguments given by the call
!         type(value_ref_t), intent(in) :: inputs(:)
!         type(input_key_t), intent(in) :: labels(:)
!         logical :: args_match

!         integer :: i

!         args_match = .true.

!         do i = 1, size(inputs)
!             select type (val => inputs(i) % value)
!               type is (real_value_t)
!               class default
!                 args_match = .false.
!             end select
!         end do

!     end function

!     subroutine exec(op, inputs, output)
!         class(add_numbers_t), intent(in) :: op
!         type(value_ref_t), intent(in) :: inputs(:)
!         class(value_t), intent(out), allocatable :: output
!         integer :: i
!         type(real_value_t) :: result

!         if (size(inputs) == 0) &
!             error stop "at least one argument required"

!         result % value = 0

!         do i = 1, size(inputs)
!             select type (val => inputs(i) % value)
!               type is (real_value_t)
!                 result % value = result % value + val % value
!               class default
!                 error stop "add: unexpected input type"
!             end select
!         end do

!         output = result

!     end subroutine

!     pure function name()
!         character(len=32) :: name

!         name = "add"
!     end function

! end module

! module test_operation_add_strings_m

!     use value_m
!     use operation_m
!     use str_value_m
!     implicit none (type, external)
!     private
!     public :: add_strings_t

!     type, extends(operation_t) :: add_strings_t
!     contains
!         procedure, nopass :: args_match
!         procedure, nopass :: name
!         procedure :: exec
!     end type

! contains


!     pure function args_match(inputs, labels)
!         !! return true if the operation is able to handle
!         !! the arguments given by the call
!         type(value_ref_t), intent(in) :: inputs(:)
!         type(input_key_t), intent(in) :: labels(:)
!         logical :: args_match

!         integer :: i

!         args_match = .true.

!         do i = 1, size(inputs)
!             select type (val => inputs(i) % value)
!               type is (str_value_t)
!               class default
!                 args_match = .false.
!             end select
!         end do

!     end function

!     subroutine exec(op, inputs, output)
!         class(add_strings_t), intent(in) :: op
!         type(value_ref_t), intent(in) :: inputs(:)
!         class(value_t), intent(out), allocatable :: output
!         integer :: i
!         type(str_value_t) :: result

!         if (size(inputs) == 0) &
!             error stop "at least one argument required"

!         result % value = ""

!         do i = 1, size(inputs)
!             select type (val => inputs(i) % value)
!               type is (str_value_t)
!                 result % value = result % value // val % value
!               class default
!                 error stop "add: unexpected input type"
!             end select
!         end do

!         output = result

!     end subroutine

!     pure function name()
!         character(len=32) :: name

!         name = "add"
!     end function

! end module


! module test_operation_matching_1
!     use real_value_m
!     use sequence_value_m
!     use iso_fortran_env, only: f64 => real64
!     use str_value_m
!     use operation_database_m
!     use operation_m
!     use error_m
!     use test_assert_m
!     use test_operation_add_numbers_m
!     use test_operation_add_strings_m

!     implicit none (type, external)

! contains

!     subroutine init(opdb)
!         type(operation_db_t) :: opdb
!         call operation_db_init(opdb)
!         call add_operation(opdb, add_numbers_t())
!         call add_operation(opdb, add_strings_t())
!     end subroutine

!     subroutine init_bad(opdb)
!         type(operation_db_t) :: opdb
!         call operation_db_init(opdb)
!         call add_operation(opdb, add_numbers_t())
!         call add_operation(opdb, add_numbers_t())
!         call add_operation(opdb, add_strings_t())
!     end subroutine

!     subroutine run_numbers
!         type(operation_db_t) :: opdb
!         class(operation_t), allocatable :: op
!         type(value_item_t), allocatable, target :: inputs(:)
!         class(value_t), allocatable :: result
!         type(err_t) :: err

!         call init(opdb)

!         inputs = [value_item_t(real_value(1.0_f64)), value_item_t(real_value(2.0_f64))]

!         call fetch_operation(opdb, &
!             "add", &
!             item_to_ref(inputs), &
!             [input_key_t(), input_key_t()], &
!             op, &
!             err)
!         call assert(.not. check(err), "error fetching operation")

!         call op%exec(item_to_ref(inputs), result)

!         call assert(same_type_as(op, add_numbers_t()), "same_type_as(op, add_numbers_t())")
!         call assert(same_type_as(result, real_value(1._f64)), "same_type_as(result, real_value_t())")
!         print *, result%to_str()

!     end subroutine

!     subroutine run_strings
!         type(operation_db_t) :: opdb
!         class(operation_t), allocatable :: op
!         type(value_item_t), allocatable :: inputs(:)
!         class(value_t), allocatable, target :: result
!         type(err_t) :: err

!         call init(opdb)

!         inputs = [value_item_t(str_value("ala ")), value_item_t(str_value("ma kota"))]

!         call fetch_operation(opdb, &
!             "add", &
!             item_to_ref(inputs), &
!             [input_key_t(), input_key_t()], &
!             op, &
!             err)
!         call assert(.not. check(err), "error fetching operation")

!         call op%exec(item_to_ref(inputs), result)

!         call assert(same_type_as(op, add_strings_t()), "same_type_as(op, add_strings_t())")
!         call assert(same_type_as(result, str_value("")), "same_type_as(result, str_value_t())")
!         print *, result%to_str()

!     end subroutine

!     subroutine run_mismatch
!         type(operation_db_t) :: opdb
!         class(operation_t), allocatable :: op
!         type(value_item_t), allocatable, target :: inputs(:)
!         class(value_t), allocatable :: result
!         type(err_t) :: err

!         call init(opdb)

!         inputs = [value_item_t(str_value("ala ")), value_item_t(real_value(1.0_f64))]

!         call fetch_operation(opdb, &
!             "add", &
!             item_to_ref(inputs), &
!             [input_key_t(), input_key_t()], &
!             op, &
!             err)

!         call assert(check(err), "error expected on mismatched input types")

!     end subroutine

!     subroutine run_ambiguous
!         type(operation_db_t) :: opdb
!         class(operation_t), allocatable :: op
!         type(value_item_t), allocatable, target :: inputs(:)
!         class(value_t), allocatable :: result
!         type(err_t) :: err

!         call init_bad(opdb)

!         inputs = [value_item_t(real_value(2.0_f64)), value_item_t(real_value(1.0_f64))]

!         call fetch_operation(opdb, &
!             "add", &
!             item_to_ref(inputs), &
!             [input_key_t(), input_key_t()], &
!             op, &
!             err)

!         call assert(check(err), "error expected on ambiguous operation")

!     end subroutine


!     subroutine run_sequence
!         type(operation_db_t) :: opdb
!         class(operation_t), allocatable :: op
!         type(value_item_t), allocatable :: inputs(:)
!         class(value_t), allocatable :: result
!         type(err_t) :: err

!         call init(opdb)

!         inputs = [value_item_t(real_value(1.0_f64)), value_item_t(sequence_value_t(items=[ &
!             value_item_t(real_value(1.0_f64)), value_item_t(real_value(2.0_f64)) &
!             ]))]

!         call fetch_operation(opdb, &
!             "add", &
!             item_to_ref(inputs), &
!             [input_key_t(), input_key_t()], &
!             op, &
!             err)
!         call assert(.not. check(err), "error fetching operation")

!         call op%exec_trace(item_to_ref(inputs), result)

!         call assert(same_type_as(op, add_numbers_t()), "same_type_as(op, add_numbers_t())")
!         call assert(same_type_as(result, sequence_value_t()), "same_type_as(result, sequence_value_t())")
!         print *, result%to_str()

!     end subroutine

!     subroutine run_str_sequence
!         type(operation_db_t) :: opdb
!         class(operation_t), allocatable :: op
!         type(value_item_t), allocatable :: inputs(:)
!         class(value_t), allocatable :: result
!         type(err_t) :: err

!         call init(opdb)

!         inputs = [value_item_t(str_value("ala ")), value_item_t(sequence_value_t(items=[ &
!             value_item_t(str_value("ma kota")), value_item_t(str_value("ma psa")) &
!             ]))]

!         call fetch_operation(opdb, &
!             "add", &
!             item_to_ref(inputs), &
!             [input_key_t(), input_key_t()], &
!             op, &
!             err)
!         call assert(.not. check(err), "error fetching operation")

!         call op%exec_trace(item_to_ref(inputs), result)

!         call assert(same_type_as(op, add_strings_t()), "same_type_as(op, add_numbers_t())")
!         call assert(same_type_as(result, sequence_value_t()), "same_type_as(result, sequence_value_t())")
!         print *, result%to_str()

!     end subroutine

!     subroutine run
!         call run_numbers
!         call run_strings
!         call run_mismatch
!         call run_ambiguous
!         call run_sequence
!         call run_str_sequence
!     end subroutine
! end module

program test_operation_matching

    ! use test_operation_matching_1, only: test1_run => run
    ! implicit none (type, external)

    ! print *, '*** TEST_OPERATION_MATCHING ***'

    ! call test1_run
end program
