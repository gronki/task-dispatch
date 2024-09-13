module generator_m

    use value_m, only: value_t, value_trace_t
    implicit none (type, external)
    private

    type :: generator_t
    contains
        procedure :: yield
        procedure :: trace
    end type

    public :: generator_t

    abstract interface
        subroutine yield(gen, val)
            import :: generator_t, value_t
            class(generator_t), intent(inout) :: gen
            class(value_t), intent(out), allocatable :: val
        end subroutine
        function trace(gen)
            import :: generator_t, value_trace_t
            class(generator_t), intent(in) :: gen
            type(value_trace_t) :: trace
        end function
    end interface

end module

module value_generator_m

    use generator_m
    use value_m, only: value_t, value_trace_t
    implicit none (type, external)
    private

    ! note to self: this way we can also make pointer-sourced generator
    ! involving a copy but one less
    type, extends(generator_t) :: value_generator_t
        class(value_t), allocatable :: val
    contains
        procedure :: yield
        procedure :: trace
    end type

    public :: value_generator_t

contains

    subroutine yield(gen, val)
        class(value_generator_t), intent(inout) :: gen
        class(value_t), intent(out) , allocatable:: val

        if (.not. allocated(gen % val)) &
            error stop "uninitialized value generator"
        val = gen % val
    end subroutine

    function trace(gen)
        class(value_generator_t), intent(in) :: gen
        type(value_trace_t) :: trace

        trace = gen % val % get_trace()
    end function

end module

module op_generator_m

    use generator_m
    use operation_m, only: input_key_t, operation_t
    use value_m, only: value_t, value_item_t, value_trace_t
    implicit none (type, external)
    private

    type :: op_generator_t_arg_t
        class(generator_t), allocatable :: gen
        type(input_key_t) :: key
    end type

    type, extends(generator_t) :: op_generator_t
        type(op_generator_t_arg_t), allocatable :: args(:)
        class(operation_t), allocatable :: op
    contains
        procedure :: yield
        procedure :: trace
    end type

    public :: op_generator_t, op_generator_t_arg_t

contains

    subroutine yield(gen, val)
        class(op_generator_t), intent(inout) :: gen
        class(value_t), intent(out), allocatable :: val

        class(value_item_t), allocatable :: evaluated_args(:)
        integer :: i, num_args

        if (.not. allocated(gen % args)) &
            error stop "uninitialized op generator args"

        num_args = size(gen % args)
        allocate(evaluated_args(num_args))

        do i = 1, num_args
            call gen % args(i) % gen % yield(evaluated_args(i) % value)
        end do

        call gen%op%exec_trace(evaluated_args, val)

    end subroutine

    function trace(gen)
        class(op_generator_t), intent(in) :: gen
        type(value_trace_t) :: trace

        type(value_trace_t), allocatable :: input_traces(:)
        integer :: i, num_args

        if (.not. allocated(gen % args)) &
            error stop "uninitialized op generator args"

        num_args = size(gen % args)
        allocate(input_traces(num_args))

        do i = 1, num_args
            input_traces(i) = gen % args(i) % gen % trace()
        end do

        trace = gen % op % trace(input_traces)

    end function

end module

module generator_from_ast_m

    use command_m
    use ast_m
    use generator_m
    use op_generator_m
    use value_generator_m
    use operation_m
    use value_m
    use namespace_m
    use error_m
    use line_error_m
    use operation_database_m
    use tokenizer_m, only: token_loc_t

    implicit none (type, external)
    private

    public :: build_generator

contains


    recursive subroutine build_generator(val_expr, gen, namespace, operation_db, err)
        type(ast_expression_t), intent(in) :: val_expr
        class(generator_t), intent(out), allocatable :: gen
        type(namespace_t), intent(in), optional :: namespace
        type(operation_db_t), intent(in), optional :: operation_db
        type(err_t), intent(out), optional :: err

        select case(val_expr % argtype)

          case (ARG_CONSTANT)
            gen = value_generator_t(val=val_expr%constant)

          case (ARG_REF)
            associate (refname => val_expr % reference % refname)
                if (.not. present(namespace)) then
                    error stop "no namespace provided so reference cannot be resolved: " // trim(refname)
                end if
                gen = value_generator_t()
                select type (gen)
                  class is (value_generator_t)
                    call namespace % fetch(refname, gen%val, err)
                    if (check(err)) then
                        call seterr(err, "here", loc=val_expr%loc)
                        return
                    end if
                end select
            end associate

          case (ARG_CALL)
            gen = op_generator_t()
            select type (gen)
              class is (op_generator_t)
                call op_generator_from_ast(val_expr, gen, namespace, operation_db, err)
                if (check(err)) return
            end select

          case default
            error stop
        end select

    end subroutine


    recursive subroutine op_generator_from_ast(val_expr, gen, namespace, operation_db, err)
        type(ast_expression_t), intent(in) :: val_expr
        class(op_generator_t), intent(out) :: gen
        type(namespace_t), intent(in), optional :: namespace
        type(operation_db_t), intent(in), optional :: operation_db
        type(err_t), intent(out), optional :: err

        class(operation_t), allocatable :: op
        type(op_generator_t_arg_t), allocatable :: args(:)
        integer :: i, nr_args

        associate(op_call => val_expr%op_call)

            nr_args = size(op_call % args)

            allocate(args(nr_args))
            do i = 1, nr_args
                args(i) % key = op_call % keys(i)
                call build_generator(op_call % args(i), args(i)%gen, namespace, operation_db, err)
                if (check(err)) return
            end do

            if (allocated(op_call%op)) then
                ! sometimes in testing/debugging we might use allocated operations
                allocate(op, source=op_call%op)
            else
                if (.not. present(operation_db)) then
                    error stop "could not resolve operation since database not provided: " // trim(op_call%opname)
                end if
                ! TODO: not sure how to solve this one. perhaps operations could return a dummy
                ! value just for the sake of indentifying the type? this would be much cleaner
                ! wayh of performing gthe operation matching without needing to write
                ! my own type system.
                call fetch_operation(operation_db, op_call%opname, op, err)

                if (check(err)) then
                    call seterr(err, "here", val_expr%loc)
                    return
                end if
            end if

            call move_alloc(args, gen % args)
            call move_alloc(op, gen % op)

        end associate
    end subroutine

end module