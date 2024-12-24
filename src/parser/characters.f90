module characters_m

implicit none (type, external)

character(len=*), parameter :: chain_call_delim = "%"
character(len=*), parameter :: kwarg_delim = ":"

contains


pure function is_whitespace(ch)
   character(len=1), intent(in) :: ch
   logical :: is_whitespace
   character(len=1), parameter :: char_tab = achar(9)

   is_whitespace = (ch == ' ') .or. (ch == char_tab)
end function

pure function is_delim(ch)
   character(len=1), intent(in) :: ch
   logical :: is_delim

   is_delim = (ch == '(') .or. (ch == ')') .or. (ch == ',') .or. (ch == '=') &
      .or. (ch == chain_call_delim) .or. (ch == kwarg_delim)
end function

pure function is_str_literal_start(ch)
   character(len=1), intent(in) :: ch
   logical :: is_str_literal_start

   is_str_literal_start = (ch == '"')
end function

pure function is_not_str_literal_end(ch)
   character(len=1), intent(in) :: ch
   logical :: is_not_str_literal_end

   is_not_str_literal_end = (ch /= '"')
end function

pure function is_ident_start_ch(ch)
   character(len=1), intent(in) :: ch
   logical :: is_ident_start_ch

   is_ident_start_ch = .false.
   is_ident_start_ch = is_ident_start_ch .or. (ichar(ch) >= ichar('a') .and. ichar(ch) <= ichar('z'))
   is_ident_start_ch = is_ident_start_ch .or. (ichar(ch) >= ichar('A') .and. ichar(ch) <= ichar('Z'))
   is_ident_start_ch = is_ident_start_ch .or. ch == '_'

end function


pure function is_digit(ch)
   character(len=1), intent(in) :: ch
   logical :: is_digit

   is_digit = ichar(ch) >= ichar('0') .and. ichar(ch) <= ichar('9')

end function

pure function is_number_start(ch)
   character(len=1), intent(in) :: ch
   logical :: is_number_start
   is_number_start = ch == '-' .or. is_digit(ch)
end function

pure function is_number_body(ch)
   character(len=1), intent(in) :: ch
   logical :: is_number_body
   is_number_body = ch == '.' .or. ch == 'e' .or. ch == 'E' &
      .or. ch == '+' .or. is_number_start(ch)
end function

pure function is_ident_ch(ch)
   character(len=1), intent(in) :: ch
   logical :: is_ident_ch

   is_ident_ch = is_ident_start_ch(ch) .or. is_digit(ch)

end function


pure function is_valid_char(ch)
   character(len=1), intent(in) :: ch
   logical :: is_valid_char

   is_valid_char = is_whitespace(ch) .or. is_delim(ch) .or. is_ident_ch(ch)
end function
end module
