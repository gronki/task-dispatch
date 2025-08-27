module operation_m

use value_m
use input_args_m
use error_m
use sequence_value_m
use iso_fortran_env, only: debug_output => error_unit
implicit none (type, external)
private

public :: input_key_t

type, abstract :: operation_t
contains
   procedure(exec_one_proto), deferred :: exec_one
   procedure :: trace => trace_generic
   procedure :: exec_trace
   procedure, nopass :: is_elemental
   procedure, nopass :: is_parallel
   procedure(opname_proto), nopass, deferred :: name
   procedure, nopass :: get_info
end type operation_t

public :: operation_t

abstract interface
subroutine exec_one_proto(op, inputs, output, err)
   import :: operation_t, value_ref_t, input_key_t, value_t, err_t
   class(operation_t), intent(in) :: op !! operation
   type(value_ref_t), intent(in) :: inputs(:) !! operation inputs
   class(value_t), intent(out), allocatable :: output !! output/result
   type(err_t), intent(inout) :: err !! error
end subroutine

pure function opname_proto() result(name)
   character(len=32) :: name
end function
end interface

public :: argument_sequence_lengths, sequence_lengths_are_correct, make_sequential_input_vector

contains

pure function argument_sequence_lengths(inputs) result(sequence_lengths)
   type(value_ref_t), intent(in) :: inputs(:) !! operation inputs
   integer :: sequence_lengths(size(inputs))

   integer :: input_index, num_inputs

   num_inputs = size(inputs)

   do input_index = 1, num_inputs
      select type(input_val => inputs(input_index)%value)

      class is (sequence_value_t)
         ! in this case, input is a sequence
         if (.not. allocated(input_val%items)) then
            error stop "sequence with unallocated values"
         end if
         sequence_lengths(input_index) = size(input_val%items)

      class default
         sequence_lengths(input_index) = 0

      end select
   end do

end function argument_sequence_lengths

pure function sequence_lengths_are_correct(sequence_lengths)
   integer, intent(in) :: sequence_lengths(:)
   logical :: sequence_lengths_are_correct

   integer, allocatable :: sequence_lengths_nonzero(:)
   logical :: is_sequence(size(sequence_lengths))
   integer :: sequence_length

   sequence_lengths_are_correct = .true.
   is_sequence = sequence_lengths > 0
   if (.not. any(is_sequence)) return
   sequence_lengths_nonzero = pack(sequence_lengths, is_sequence)
   sequence_length = sequence_lengths_nonzero(1)
   sequence_lengths_are_correct = all(sequence_lengths_nonzero == sequence_length)
end function sequence_lengths_are_correct

subroutine make_sequential_input_vector(inputs, sequence_index, temp_inputs)
   type(value_ref_t), intent(in) :: inputs(:) !! operation inputs
   type(value_ref_t), intent(inout) :: temp_inputs(:) !! operation inputs
   integer, intent(in) :: sequence_index
   logical :: is_sequence(size(inputs))
   integer :: input_index, num_inputs

   num_inputs = size(inputs)

   do input_index = 1, num_inputs
      select type(input_val => inputs(input_index)%value)
      class is (sequence_value_t)
         temp_inputs(input_index) % value => input_val%items(sequence_index)%value
      class default
         temp_inputs(input_index) % value => input_val
      end select
      nullify(temp_inputs(input_index) % item)
   end do
end subroutine make_sequential_input_vector

recursive subroutine exec_trace(op, inputs, output, err)
   !! Execute the operation and handle value tracing
   !! as well as sequence processing.
   !! If your operation explicitly expects sequences
   !! as inputs (for example, to sum all arguments)
   !! you need to override this subroutine.

   class(operation_t), intent(in) :: op !! operation
   type(value_ref_t), intent(in) :: inputs(:) !! operation inputs
   class(value_t), intent(out), allocatable :: output !! output/result
   type(err_t), intent(inout) :: err !! error

   type(value_ref_t) :: temp_inputs(size(inputs))
   type(sequence_value_t), allocatable :: sequence_output
   integer :: sequence_lengths(size(inputs))
   integer :: input_index, sequence_index, sequence_length, num_inputs
   integer :: i
   logical :: abort, run_parallel
   type(err_t) :: item_err
   type(value_item_t), allocatable :: sequence_output_items(:)
   class(value_t), allocatable :: output_item_value

   num_inputs = size(inputs)
   sequence_lengths = argument_sequence_lengths(inputs)
   sequence_length = maxval(sequence_lengths)

   ! here we handle a typical case, where none of the inputs is a sequence
   if (sequence_length == 0 .or. .not. op % is_elemental()) then
      call op % exec_one(inputs, output, err)

      if (check(err)) return
      if (.not. allocated(output)) then
         call seterr( err, "operation did not yield any output: " // trim(op%name()) )
         return
      end if

      output % trace = op % trace([(inputs(i) % get_trace(), i = 1, num_inputs)])
      return
   end if

   ! by this point we have at least one sequence

   if (.not. sequence_lengths_are_correct(sequence_lengths)) then
      call seterr(err, "sequence parameters must all be equal length or scalars")
      return
   end if

   ! iteratre through arguments, and for sequence items recursively substitute

   allocate(sequence_output)
   allocate(sequence_output_items(sequence_length))
   sequence_output % trace = op % trace([(inputs(i) % get_trace(), i = 1, num_inputs)])

   run_parallel = op % is_parallel()
   abort = .false.

   !$omp parallel do default(none) shared(inputs, sequence_output_items, sequence_length, err, op, num_inputs, abort) private(temp_inputs, output_item_value) firstprivate(item_err) if(run_parallel)
   do sequence_index = 1, sequence_length
      if (abort) cycle

      call make_sequential_input_vector(inputs, sequence_index, temp_inputs)
      call op % exec_one(temp_inputs, output_item_value, item_err)

      if (check(item_err)) then
         !$omp critical
         call seterr(err, item_err)
         abort = .true.
         !$omp end critical
         cycle
      end if

      if (.not. allocated(output_item_value)) then
         !$omp critical
         call seterr( err, "operation did not yield any output: " // trim(op%name()) )
         abort = .true.
         !$omp end critical
         cycle
      end if

      !$omp critical
      write (debug_output, *) ' sequence item ', sequence_index, ' ', &
         output_item_value % get_trace(), ' ---> ',  output_item_value%to_str()
      call move_alloc(output_item_value, sequence_output_items(sequence_index)%value)
      !$omp end critical

   end do
   !$omp end parallel do

   if (check(err)) return

   do sequence_index = 1, sequence_length
      call make_sequential_input_vector(inputs, sequence_index, temp_inputs)
      sequence_output_items(sequence_index) % value % trace &
         = op % trace([(temp_inputs(i) % get_trace(), i = 1, num_inputs)])
   end do

   call move_alloc(sequence_output_items, sequence_output % items)
   call move_alloc(sequence_output, output)

end subroutine exec_trace


function trace_generic(op, input_traces) result(output_trace)
   !! Produces a generic trace for any operation following
   !! the pattern: opname(trace_arg1, trace_arg2, ...)
   !> operation
   class(operation_t), intent(in) :: op
   !> operation inputs
   type(value_trace_t), intent(in) :: input_traces(:)
   !> string trace
   type(value_trace_t) :: output_trace
   character(len=16384) :: buf
   integer :: i, num_inputs

   num_inputs = size(input_traces)

   ! TODO: with Fortran 2023 standard, the variable can be automatically allocated
   write (buf, '(a, *(a))') "@" // trim(op % name()) // "{", &
      ( trim(input_traces(i)%str) // adjustl(trim(merge(" ,", "} ", i < num_inputs))), &
      i = 1, num_inputs )

   ! TODO: update to print prettier traces when argspec is present

   output_trace % str = trim(buf)

end function trace_generic

pure function is_elemental()
   !! return true if the operation should be performed
   !! element-wise on the sequences
   logical :: is_elemental

   is_elemental = .true.
end function

pure function is_parallel()
   !! return true if the operation should be performed
   !! parallel
   logical :: is_parallel

   is_parallel = .true.
end function

subroutine get_info(argspec, help)
   type(arg_entry_t), intent(out), allocatable, optional :: argspec(:)
   character(len=:), intent(out), allocatable, optional :: help
end subroutine

end module operation_m
