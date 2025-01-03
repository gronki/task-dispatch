module input_args_m

use value_m
implicit none (type, external)
private

integer, parameter :: key_max_len = 32

type input_key_t
   logical :: has_key = .false.
   character(len=key_max_len) :: key = ""
end type input_key_t

public :: input_key_t


end module
