module generator_m

    use value_m
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
            import :: generator_t, value_ref_t
            class(generator_t), intent(inout), target :: gen
            class(value_ref_t), intent(out) :: val
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
    use value_m
    implicit none (type, external)
    private

    type, extends(generator_t) :: value_generator_t
        class(value_t), allocatable :: val
    contains
        procedure :: yield
        procedure :: trace
    end type

    public :: value_generator_t

contains

    subroutine yield(gen, val)
        class(value_generator_t), intent(inout), target :: gen !! must be a target!
        class(value_ref_t), intent(out) :: val

        if (.not. allocated(gen % val)) &
            error stop "uninitialized value in generator"

        call val % refer(gen % val)
    end subroutine

    function trace(gen)
        class(value_generator_t), intent(in) :: gen
        type(value_trace_t) :: trace

        if (.not. allocated(gen % val)) &
            error stop "uninitialized value in generator"

        trace = gen % val % get_trace()
    end function

end module

module reference_generator_m

    use generator_m
    use value_m
    implicit none (type, external)
    private

    type, extends(generator_t) :: reference_generator_t
        class(value_t), pointer :: val
    contains
        procedure :: yield
        procedure :: trace
    end type

    public :: reference_generator_t

contains

    subroutine yield(gen, val)
        class(reference_generator_t), intent(inout), target :: gen !! must be a target!
        class(value_ref_t), intent(out) :: val

        if (.not. associated(gen % val)) &
            error stop "uninitialized value reference in generator"

        call val % refer(gen % val)
    end subroutine

    function trace(gen)
        class(reference_generator_t), intent(in) :: gen
        type(value_trace_t) :: trace

        if (.not. associated(gen % val)) &
            error stop "uninitialized value reference in generator"

        trace = gen % val % get_trace()
    end function

end module

module op_generator_m

    use generator_m
    use operation_m, only: input_key_t, operation_t
    use value_m
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
        class(op_generator_t), intent(inout), target :: gen !! must be a target!
        class(value_ref_t), intent(out) :: val

        class(value_t), allocatable :: output_value

        class(value_ref_t), allocatable :: evaluated_args(:)
        integer :: i, num_args

        if (.not. allocated(gen % args)) &
            error stop "uninitialized op generator args"

        num_args = size(gen % args)
        allocate(evaluated_args(num_args))

        do i = 1, num_args
            call gen % args(i) % gen % yield(evaluated_args(i))
        end do

        call gen%op%exec_trace(evaluated_args, output_value)
        call val % alloc(output_value)

        do i = 1, num_args
            call evaluated_args(i) % dealloc
        end do

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
    use reference_generator_m
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
        type(namespace_t), intent(inout), target, optional :: namespace !! must be a target!
        type(operation_db_t), intent(in), optional :: operation_db
        type(err_t), intent(out), optional :: err

        type(op_generator_t), allocatable :: op_gen
        type(value_generator_t), allocatable :: val_gen
        type(reference_generator_t), allocatable :: ref_gen

        select case(val_expr % argtype)

          case (ARG_CONSTANT)
            allocate(val_gen)
            val_gen%val = val_expr%constant
            call move_alloc(val_gen, gen)

          case (ARG_REF)
            associate (refname => val_expr % reference % refname)
                if (.not. present(namespace)) then
                    error stop "no namespace provided so reference cannot be resolved: " // trim(refname)
                end if
                allocate(ref_gen)
                call namespace % fetch_ptr(refname, ref_gen%val, err)
                if (check(err)) then
                    call seterr(err, "here", loc=val_expr%loc)
                    return
                end if
                call move_alloc(ref_gen, gen)
            end associate

          case (ARG_CALL)
            allocate(op_gen)
            call op_generator_from_ast(val_expr, op_gen, namespace, operation_db, err)
            if (check(err)) return
            call move_alloc(op_gen, gen)

          case default
            error stop
        end select

    end subroutine


    recursive subroutine op_generator_from_ast(val_expr, gen, namespace, operation_db, err)
        type(ast_expression_t), intent(in) :: val_expr
        class(op_generator_t), intent(out) :: gen
        type(namespace_t), intent(inout), target, optional :: namespace !! must be a target!
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
