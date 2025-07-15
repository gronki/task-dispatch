module tokenizer_m

use iso_fortran_env, only: real64, debug_unit => error_unit, error_unit => error_unit
use stringbuffer_m
use characters_m
use error_m
use line_error_m
use token_m

implicit none (type, external)

type :: tok_array_t
   type(token_t), allocatable :: tokens(:)
   integer :: icur = 0
end type

type tokenizer_t
   private
   character(len=:), allocatable :: str
   integer :: pos = 1
   integer :: line_number = 0
end type

interface tokenizer_t
module procedure :: new_tokenizer
end interface

contains

pure function new_tokenizer(line)
   character(len=*), intent(in) :: line
   type(tokenizer_t) :: new_tokenizer

   new_tokenizer % str = line
end function

pure function is_end(tokenizer)
   type(tokenizer_t), intent(in) :: tokenizer
   logical :: is_end

   is_end = .true.

   if (.not. allocated(tokenizer%str)) return
   if (tokenizer%pos > len(tokenizer%str)) return

   is_end = .false.

end function

subroutine next_char(tokenizer, ch)
   type(tokenizer_t), intent(inout) :: tokenizer
   character(len=1), intent(out) :: ch

   ch = " "
   tokenizer%pos = tokenizer%pos + 1
   if (is_end(tokenizer)) return
   associate (pos => tokenizer%pos)
      ch = tokenizer%str(pos:pos)
      call log_state(tokenizer)
   end associate
end subroutine

subroutine get_char(tokenizer, ch)
   type(tokenizer_t), intent(in) :: tokenizer
   character(len=1), intent(out) :: ch

   ch = " "
   if (is_end(tokenizer)) return
   associate (pos => tokenizer%pos)
      ch = tokenizer%str(pos:pos)
   end associate
end subroutine

subroutine skip_whitespace(tokenizer)
   type(tokenizer_t), intent(inout) :: tokenizer
   character(len=1) :: ch

   call get_char(tokenizer, ch)
   do while (.not. is_end(tokenizer))
      if (.not. is_whitespace(ch)) return
      call next_char(tokenizer, ch)
      ! write (debug_unit, *) "skipping whitespace "
   end do
end subroutine

subroutine consume_until(tokenizer, buffer, condition)
   type(tokenizer_t), intent(inout) :: tokenizer
   character(len=1) :: ch
   character(len=:), allocatable, intent(out) :: buffer
   type(stringbuffer_t) :: sb

   interface
   pure function condition(ch)
      character(len=1), intent(in) :: ch
      logical :: condition
   end function
   end interface

   consume_chars: block
      call get_char(tokenizer, ch)
      do while (.not. is_end(tokenizer))
         if (.not. condition(ch)) exit consume_chars
         ! write (debug_unit, '(a,a)') "consuming character: ", ch
         call sb%append(ch)
         call next_char(tokenizer, ch)
      end do
   end block consume_chars

   buffer = sb%str()
   ! write (debug_unit, '(a,a)') "consumed: ", buffer

end subroutine

subroutine log_state(tokenizer)
   type(tokenizer_t), intent(in) :: tokenizer
   character(len=1) :: ch
   call get_char(tokenizer, ch)
   ! write (debug_unit, '(a,a,a,i0, a)') "tokenizer state: [", ch, "] @ ", &
   !     tokenizer%pos, merge(' E', '  ', is_end(tokenizer))
end subroutine

subroutine next_token(tokenizer, token, err)
   type(tokenizer_t), intent(inout) :: tokenizer
   type(token_t), intent(out) :: token
   type(err_t), intent(out), optional :: err
   character(len=1) :: ch

   if (is_end(tokenizer)) then
      token%type = token_end
      return
   end if

   call skip_whitespace(tokenizer)
   call get_char(tokenizer, ch)

   token%loc%line = tokenizer%line_number
   token%loc%offset = tokenizer%pos

   if (is_ident_start_ch(ch)) then
      token%type = token_ident
      call consume_until(tokenizer, token%value, is_ident_ch)
      ! write (debug_unit, *) "identifier: ", token%value
      return
   end if

   if (is_str_literal_start(ch)) then
      token%type = token_str_literal
      call next_char(tokenizer, ch)
      call consume_until(tokenizer, token%value, is_not_str_literal_end)
      ! write (debug_unit, *) "character literal: ", token%value
      if (is_end(tokenizer)) then
         call seterr( err, "parser warning: string literal not closed", token%loc )
         return
      end if
      call next_char(tokenizer, ch)
      return
   end if

   if (is_number_start(ch)) then
      token%type = token_num_literal
      call consume_until(tokenizer, token%value, is_number_body)
      ! write (debug_unit, *) "number literal: ", token%value
      return
   end if

   if (is_delim(ch)) then
      token%type = token_Delim
      token%value = ch
      ! write (debug_unit, *) "delimiter: ", ch
      call next_char(tokenizer, ch)
      return
   end if

   if (is_end(tokenizer)) then
      token%type = token_end
      return
   end if

   ! TODO: this must be handled as a proper err_t
   call seterr( err, "error: unknown character " // ch, token_loc_t(offset=tokenizer%pos) )

end subroutine

subroutine tokenize_into_array(line, tok_array, err)
   !! Tokenize line into token array, which allows token
   !! peeking, opposite to sequential tokenizer.

   character(len=*), intent(in) :: line !! line to be tokenized
   type(tok_array_t), intent(out) :: tok_array !! token array
   type(err_t), intent(out), optional :: err !! optional error output

   type(tokenizer_t) :: tokenizer
   integer :: i
   INTEGER, PARAMETER :: TOKEN_ARRAY_MAX_SIZE = 4096

   tokenizer = tokenizer_t(line)
   allocate(tok_array%tokens(TOKEN_ARRAY_MAX_SIZE))

   do i = 1, size(tok_array%tokens)
      associate (token => tok_array%tokens(i))
         call next_token(tokenizer, token, err)
         if (check(err)) return
         if (token%type == token_end) then
            tok_array%tokens = tok_array%tokens(:i)
            return
         end if
      end associate
   end do

   call seterr(err, "Whatever you wrote here was too long for me to comprehend.")

end subroutine

function tokenized_into_array(line, err) result(tok_array)
   !! convenience function wrapper over tokenize_into_array

   character(len=*), intent(in) :: line !! line to be tokenized
   type(err_t), intent(out), optional :: err !! optional error output
   type(tok_array_t) :: tok_array !! token array

   call tokenize_into_array(line, tok_array, err)
end function

subroutine get_next_token(tok_array, token)
   !! Move the token array pointer by one. If token is given,
   !! the new token will be copied there.

   type(tok_array_t), intent(inout) :: tok_array !! token array
   type(token_t), intent(out), optional :: token !! output token

   tok_array%icur = tok_array%icur + 1
   if (present(token)) &
      call get_current_token(tok_array, token)
end subroutine

subroutine get_current_token(tok_array, token)
   !! Return the current token without advancing the cursor.
   !! See documentation for ``peek_token`` for more details.

   type(tok_array_t), intent(inout) :: tok_array !! token array
   type(token_t), intent(out) :: token !! current token

   call peek_token(tok_array, 0, token)
end subroutine

subroutine peek_token(tok_array, offset, token)
   !! Get a token located at a given offset without progressing
   !! the cursor, with the exception of array being used for the
   !! first time. In that case, the current token will be set to
   !! the first token. If peeking before the first token, ``TOKEN_NONE``
   !! will be returned. Opposite, peeking after the last token
   !! will yield ``TOKEN_END`` type of token.

   type(tok_array_t), intent(inout) :: tok_array !! token array to peek
   integer, intent(in) :: offset !! offset to peek at
   type(token_t), intent(out) :: token !! result token

   if (tok_array%icur == 0) tok_array%icur = 1

   if (tok_array%icur+offset >= size(tok_array%tokens)) then
      token = token_t(type=token_end)
      return
   end if

   if (tok_array%icur+offset <= 0) then
      token = token_t(type=token_none)
      return
   end if

   token = tok_array%tokens(tok_array%icur+offset)
   
end subroutine

end module
