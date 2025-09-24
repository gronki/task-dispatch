module namespace_m

use value_m
use error_m
use yaftree_m
implicit none (type, external)
public

type namespace_item_t
   logical :: used = .false.
   character(len=32) :: name = ""
   class(value_t), allocatable :: value
end type

integer, parameter :: NAMESPACE_CAPACITY = 4096

type namespace_t
   !! suggestion: always create namespace with pointer/target attribute.
   type(dict_t) :: vars
contains
   procedure :: push => namespace_push_value
   procedure :: move_in => namespace_move_value_in
   procedure :: fetch => namespace_fetch_value
   procedure :: fetch_ptr => namespace_fetch_ptr
end type

contains

subroutine namespace_push_value(namespace, key, value, err)
   class(namespace_t), intent(inout), target :: namespace
   character(len=*), intent(in) :: key
   class(value_t), intent(in) :: value
   type(err_t), intent(out), optional :: err

   class(value_t), allocatable :: new_value

   allocate(new_value, source=value)
   call namespace_move_value_in(namespace, key, new_value, err)
end subroutine

subroutine namespace_move_value_in(namespace, key, value, err)
   class(namespace_t), intent(inout), target :: namespace
   character(len=*), intent(in) :: key
   class(value_t), intent(inout), allocatable :: value
   type(err_t), intent(out), optional :: err
   type(binary_tree_node_t), pointer :: node

   type(value_item_t), allocatable :: item

   print *, "pushing key <", key, ">"

   if (.not. associated(namespace%vars%hasher)) then
      namespace%vars%hasher => fnv_hash
   end if

   node => get_node(namespace%vars, key)
   if (.not. associated(node)) error stop

   allocate(item)
   call move_alloc( from=value, to=item%value )
   call move_alloc( from=item, to=node%value )

end subroutine

subroutine namespace_fetch_value(namespace, key, value, err)
   class(namespace_t), intent(inout) :: namespace
   character(len=*), intent(in) :: key
   class(value_t), allocatable, intent(inout) :: value
   type(err_t), intent(out), optional :: err

   if (.not. associated(namespace%vars%hasher)) then
      namespace%vars%hasher => fnv_hash
   end if

   select type (item => get(namespace%vars, key))
   type is (value_item_t)
      value = item % value
   type is (key_not_found_t)
      call seterr(err, trim(key) // " not found")
   class default
      error stop
   end select

end subroutine

subroutine namespace_fetch_ptr(namespace, key, ptr, err)
   !! Fetches pointer to value of item contained in a namespace.
   class(namespace_t), intent(inout), target :: namespace
   !! Namespace - must be target!
   character(len=*), intent(in) :: key
   !! Key
   class(value_t), intent(inout), pointer :: ptr
   !! Pointer to be associated with key value
   type(err_t), intent(out), optional :: err
   !! Optional error object

   if (.not. associated(namespace%vars%hasher)) then
      namespace%vars%hasher => fnv_hash
   end if

   select type (item_ptr => get_ptr(namespace%vars, key))
   type is (value_item_t)
      ptr => item_ptr % value
   type is (key_not_found_t)
      call seterr(err, trim(key) // " not found")
   class default
      error stop
   end select

end subroutine


end module
