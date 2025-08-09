module input_args_m

use value_m
use characters_m
use error_m
use yaftree_m
use tokenizer_m, only: token_loc_t
use line_error_m

implicit none (type, external)
private

integer, parameter :: key_max_len = 32

type input_key_t
   logical :: has_key = .false.
   character(len=key_max_len) :: key = ""
   type(token_loc_t) :: loc = token_loc_t()
end type input_key_t

public :: input_key_t


type :: arg_entry_t
   integer :: pos
   character(len=key_max_len) :: name
   class(value_t), allocatable :: default
   logical :: required = .true.
end type

public :: arg_entry_t

type :: argument_match_t
   integer :: matched_pos = -1
   class(value_t), allocatable :: default
end type

public :: argument_match_t

public :: match_arguments, connect_matched_args, connect_matched_traces

contains

!> Checks if a given string can be considered a correct ident.
pure function is_correct_ident(key)
   character(len=*), intent(in) :: key
   logical :: is_correct_ident

   integer :: i

   is_correct_ident = .false.

   do i = 1, len(key)
      if (i > 1) then
         is_correct_ident = is_ident_ch(key(i:i))
      else
         is_correct_ident = is_ident_start_ch(key(i:i))
      end if

      if (.not. is_correct_ident) return
   end do

end function

!> Check if arguments specification adheres to the rules.
subroutine check_argspec_integrity(argspec, err)
   !> Argument specification.
   type(arg_entry_t), intent(in) :: argspec(:)
   !> Error.
   type(err_t), intent(inout) :: err

   logical :: current_is_keyword, keyword_section
   integer :: iarg

   keyword_section = .false.

   do iarg = 1, size(argspec)
      if (argspec(iarg) % pos /= iarg) then
         call seterr( err, "argspec%pos must be subsequent numbers starting from 1")
         return
      end if

      current_is_keyword = allocated(argspec(iarg) % default) .or. .not. argspec(iarg) % required
      if (current_is_keyword) keyword_section = .true.

      if (.not. is_correct_ident(trim(argspec(iarg) % name))) then
         call seterr( err, "argspec: not a correct identifier: " // trim(argspec(iarg) % name) )
         return
      end if

      if (keyword_section) then
         if (.not. current_is_keyword) then
            call seterr(err, "argspec: arguments with defaults must go after required arguments")
            return
         end if
      end if
   end do

end subroutine

!> Fill they keys which were not explicitly given in the function call.
!> Also check for correctness of the keys as well as other possible errors.
subroutine collect_keys(argspec, actual_keys, key_positions, err)
   !> Argument specification.
   type(arg_entry_t), intent(in) :: argspec(:)
   !> Keys given in operation call.
   type(input_key_t), intent(in) :: actual_keys(:)
   !> Dictionary containing key positions.
   type(dict_t) :: key_positions
   !> Error.
   type(err_t), intent(inout) :: err

   integer :: ikey
   logical :: keyword_section
   character(len=:), allocatable :: current_key
   type(set_t) :: valid_keys

   valid_keys%hasher => fnv_hash

   keyword_section = .false.

   if (size(actual_keys) > size(argspec)) then
      call seterr(err, "Argument list too long.")
      return
   end if

   do ikey = 1, size(argspec)
      call insert(valid_keys, trim(argspec(ikey) % name))
   end do

   do ikey = 1, size(actual_keys)
      if (.not. actual_keys(ikey) % has_key) then
         ! ensure we are in positional argument half
         if (keyword_section) then
            call seterr( err, "Positional arguments are not allowed to follow keyword arguments.", &
               actual_keys(ikey) % loc )
            return
         end if

         current_key = trim(argspec(ikey) % name)
      else ! keyword argument
         keyword_section = .true.
         current_key = trim(actual_keys(ikey) % key)

         if (current_key .notin. valid_keys) then
            call seterr( err, "Unknown argument: " // current_key // ".",  actual_keys(ikey) % loc )
            return
         end if

      end if

      if (current_key .in. key_positions) then
         call seterr( err, "Duplicate key: " // current_key // ".",  actual_keys(ikey) % loc )
         return
      end if

      call insert(key_positions, current_key, ikey)
   end do

end subroutine

!> Matches provided actual arguments with args given in the
!> arg specification. Produces a match which then can be
!> used to create a list where args have fixed positions.
!> This allows for this (slow) matching to be performed
!> once at the parsing stage, and subsequent argument
!> accesses will be very fast (for example, for elemental
!> operations that are executed for many items).
subroutine match_arguments(argspec, actual_keys, match, err)
   !> Argument specification.
   type(arg_entry_t), intent(in) :: argspec(:)
   !> Actual keys provided during the operation call.
   type(input_key_t), intent(in) :: actual_keys(:)
   !> Match information. Must be same dimension as argspec.
   type(argument_match_t), intent(out) :: match(:)
   !> Error for argument matching. Recommend handling since
   !> errors may be common.
   type(err_t), intent(inout) :: err

   type(dict_t) :: key_positions
   integer :: iarg

   key_positions % hasher => fnv_hash

   if (size(match) /= size(argspec)) then
      error stop "match_arguments: Dimensions of argspec and match must be the same."
   end if

   call check_argspec_integrity(argspec, err)
   if (check(err)) return

   call collect_keys(argspec, actual_keys, key_positions, err)
   if (check(err)) return

   do iarg = 1, size(argspec)
      select type(arg_pos => get(key_positions, trim(argspec(iarg) % name)))
      type is (integer)
         ! this argument was given, save position
         match(iarg) % matched_pos = arg_pos

      type is (key_not_found_t)
         ! this argument was not given
         match(iarg) % matched_pos = -1

         if (.not. argspec(iarg) % required) cycle

         if (.not. allocated(argspec(iarg) % default)) then
            call seterr( err, "Argument " // trim(argspec(iarg) % name) // " required." )
            return
         end if

         allocate( match(iarg) % default, source = argspec(iarg) % default )
      end select
   end do

end subroutine

!> Generates positional argument list, based on provided actual
!> arguments and result of argument matching.
function connect_matched_args(refs, match) result(matched_refs)
   !> References to values passed as actual arguments.
   type(value_ref_t), intent(in) :: refs(:)
   !> Result of ``match_arguments()``.
   type(argument_match_t), target, intent(in) :: match(:)
   !> References with fixed positions same as argument specification (argspec).
   type(value_ref_t) :: matched_refs(size(match))

   integer :: iarg

   do iarg = 1, size(match)
      if (allocated(match(iarg) % default)) then
         ! default
         matched_refs(iarg) = protect(match(iarg) % default)
      else
         if (match(iarg) % matched_pos == -1) then
            nullify(matched_refs(iarg) % value)
            cycle
         end if
         matched_refs(iarg) = refs(match(iarg) % matched_pos) 
      end if
   end do

end function


pure function connect_matched_traces(input_traces, match) result(expanded_traces)
   type(value_trace_t), intent(in) :: input_traces(:)
   type(argument_match_t), intent(in) :: match(:)
   type(value_trace_t) :: expanded_traces(size(match))

   integer :: iarg

   do iarg = 1, size(match)
      if (allocated(match(iarg) % default)) then
         ! default
         expanded_traces(iarg) = match(iarg) % default % get_trace()
      else
         if (match(iarg) % matched_pos == -1) then
            expanded_traces(iarg) % str = "(none)"
            cycle
         end if
         expanded_traces(iarg) = input_traces(match(iarg) % matched_pos)
      end if
   end do

end function

end module
