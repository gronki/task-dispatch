program test_tokenizer

   use tokenizer_m
   use token_m
   use test_assert_m

   implicit none (type, external)

   print *, '*** TEST_TOKENIZER ***'

   call test_token_equality

   call test1
   call test2
   call test3

#   include "assert_macro.h"

contains


   subroutine test_token_equality

      call assert(token_t(type=token_end) == token_t(type=token_end), &
         'token_t(type=token_end) == token_t(type=token_end)')

      call assert(token_t(type=token_end) /= token_t(type=token_ident, value="a"), &
         'token_t(type=token_end) /= token_t(type=token_ident, value="a")')

      call assert_not  (token_t(type=token_ident, value="b") == token_t(type=token_ident, value="a"), &
         'token_t(type=token_ident, value="b") == token_t(type=token_ident, value="a")')

      call assert (token_t(type=token_ident, value="b") /= token_t(type=token_ident, value="a"), &
         'token_t(type=token_ident, value="b") /= token_t(type=token_ident, value="a")')

   end subroutine

   subroutine test1
      type(tokenizer_t) :: tokenizer
      type(token_t) :: token

      tokenizer = tokenizer_t("  y= ff(a, b) ")

      call next_token(tokenizer, token)
      call assert (token == token_t(type=token_ident, value="y"), &
         '[0] token == token_t(type=token_ident, value="y")')

      call next_token(tokenizer, token)
      call assert (token == token_t(type=token_delim, value="="), &
         '[1] token == token_t(type=token_delim, value="=")')

      call next_token(tokenizer, token)
      call assert (token == token_t(type=token_ident, value="ff"), &
         '[2] token == token_t(type=token_ident, value="ff")')

      call next_token(tokenizer, token)
      call assert (token == token_t(type=token_delim, value="("), &
         '[3] token == token_t(type=token_delim, value="(")')

      call next_token(tokenizer, token)
      call assert (token == token_t(type=token_ident, value="a"), &
         '[4] token == token_t(type=token_ident, value="a")')

      call next_token(tokenizer, token)
      call assert (token == token_t(type=token_delim, value=","), &
         '[5] token == token_t(type=token_delim, value=",")')

      call next_token(tokenizer, token)
      call assert (token == token_t(type=token_ident, value="b"), &
         '[6] token == token_t(type=token_ident, value="b")')

      call next_token(tokenizer, token)
      call assert (token == token_t(type=token_delim, value=")"), &
         '[7] token == token_t(type=token_delim, value=")")')

      call next_token(tokenizer, token)
      call assert (token % type == token_end, &
         '[8] token % type == token_end')

   end subroutine

   subroutine test2
      type(tokenizer_t) :: tokenizer
      type(token_t) :: token

      tokenizer = tokenizer_t('  f(3.0, 4, image( "file.fits" )) ')

      ! i hate using apostrophes as quotation marks, but ifort
      ! seems not to be able to escape quotation marks " in macros

      call next_token(tokenizer, token)
      call assertm (token == token_t(type=token_ident, value='f'))

      call next_token(tokenizer, token)
      call assertm (token == token_t(type=token_delim, value='('))

      call next_token(tokenizer, token)
      call assertm (token == token_t(type=token_num_literal, value='3.0'))

      call next_token(tokenizer, token)
      call assertm (token == token_t(type=token_delim, value=','))

      call next_token(tokenizer, token)
      call assertm (token == token_t(type=token_num_literal, value='4'))

      call next_token(tokenizer, token)
      call assertm (token == token_t(type=token_delim, value=','))

      call next_token(tokenizer, token)
      call assertm (token == token_t(type=token_ident, value='image'))

      call next_token(tokenizer, token)
      call assertm (token == token_t(type=token_delim, value='('))

      call next_token(tokenizer, token)
      call assertm (token == token_t(type=token_str_literal, value='file.fits'))

      call next_token(tokenizer, token)
      call assertm (token == token_t(type=token_delim, value=')'))

      call next_token(tokenizer, token)
      call assertm (token == token_t(type=token_delim, value=')'))

      call next_token(tokenizer, token)
      call assertm (token % type == token_end)
   end subroutine

   subroutine test3
      type(tokenizer_t) :: tokenizer
      type(token_t) :: token

      tokenizer = tokenizer_t('  ')

      call next_token(tokenizer, token)
      call assertm (token % type == token_end)
   end subroutine

end program
