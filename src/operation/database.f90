module operation_database_m

use value_m
use operation_m
use error_m

implicit none (type, external)
private

type operation_db_entry_t
   character(len=32) :: opname = ""
   class(operation_t), allocatable :: op
end type

integer, parameter :: OPERATION_DB_CAPACITY = 128

type operation_db_t
   integer :: current_size = 0
   type(operation_db_entry_t) :: db(OPERATION_DB_CAPACITY)
end type

interface operation_db_t
module procedure :: operation_db_new
end interface

public :: operation_db_t

public :: operation_db_init, fetch_operation, add_operation

contains

pure subroutine operation_db_init(operation_db)
   use operation_mksequence_m
   use operation_item_m
   use operation_trace_m

   type(operation_db_t), intent(inout) :: operation_db !! operation catalog

   call add_operation(operation_db, op_mkseq_t())
   call add_operation(operation_db, op_item_t())
   call add_operation(operation_db, op_trace_t())
end subroutine

pure function operation_db_new()
   type(operation_db_t) :: operation_db_new !! operation catalog
   call operation_db_init(operation_db_new)
end function

pure subroutine fetch_operation(operation_db, opname, op, err)
   !! fetches an operation from the operation database based on its name

   type(operation_db_t), intent(in) :: operation_db !! operation catalog
   character(len=*), intent(in) :: opname !! operation name
   class(operation_t), intent(inout), allocatable :: op !! allocatable operation
   type(err_t), intent(out), optional :: err !! error object

   integer :: i
   logical :: is_match(operation_db % current_size)

   is_match = .false.

   do i = 1, operation_db % current_size
      associate (entry => operation_db % db(i))
         if (opname == entry % opname) then
            if (.not. allocated(entry%op)) continue
            is_match(i) = .true.
         end if
      end associate
   end do

   associate (num_matches => count(is_match))
      if (num_matches /= 1) then
         if (num_matches == 0) then
            call seterr(err, "unknown operation " // opname)
            return
         end if
         block
            character(len=256) :: errbuf
            write(errbuf, *) "operation ", opname, " yielded ambigous match (", num_matches, " entries)"
            call seterr(err, trim(errbuf))
            return
         end block
      end if
   end associate

   ! exactly one match is expected

   associate(matched_indices => pack([(i, i = 1, operation_db % current_size)], is_match))
      associate (entry => operation_db % db(matched_indices(1)))
         allocate(op, source=entry % op)
      end associate
   end associate

end subroutine

pure subroutine add_operation(operation_db, op, err)
   !! fetches an operation from the operation database based on its name

   type(operation_db_t), intent(inout) :: operation_db !! operation catalog
   class(operation_t), intent(in) :: op !! allocatable operation
   type(err_t), intent(out), optional :: err !! error object

   character(len=:), allocatable :: opname
   integer :: i

   opname = op % name()

   if (opname == "") then
      error stop "empty operation name not permitted"
   end if

   do i = 1, size(operation_db % db)
      associate (entry => operation_db % db(i))
         if (entry % opname /= "") cycle
         allocate(entry % op, source=op)
         entry % opname = opname
         operation_db%current_size = max(operation_db%current_size, i)
         return
      end associate
   end do

   error stop "operation database FULL"
end subroutine
end module
