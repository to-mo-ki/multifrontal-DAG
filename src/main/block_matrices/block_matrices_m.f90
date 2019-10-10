module block_matrices_m
  use contiguous_sets_m
  use jagged_array_m
  use matrix_controller_m
  implicit none
  private
  type, public :: block_matrices_c
    private
    double precision, pointer, contiguous :: val(:)
    type(contiguous_sets_c), pointer :: ptr
    type(contiguous_sets_c), pointer :: node_sets
    type(jagged_array_c), pointer :: ccs
    class(matrix_controller_c), pointer :: controller
    integer :: nb
  contains
    procedure :: get_ptr
  end type

  public :: create_block_matrices

contains
  function create_block_matrices(nb, node_sets, ccs, controller) result(this)
    type(block_matrices_c), pointer :: this
    integer, intent(in) :: nb
    type(contiguous_sets_c), pointer, intent(in) :: node_sets
    type(jagged_array_c), pointer, intent(in) :: ccs
    integer, pointer, contiguous :: matrix_size(:)
    class(matrix_controller_c), pointer, intent(in) :: controller
    integer :: i, nc, nr
    type(contiguous_sets_c), pointer :: tmp

    allocate(this)
    allocate(matrix_size(node_sets%get_num_sets()))
    do i=1, node_sets%get_num_sets()
      nc = node_sets%get_length(i)
      nr = ccs%get_array_length(i)
      matrix_size(i) = controller%estimate_size(nb, nc, nr)
    enddo
    this%ptr => create_contiguous_sets(matrix_size)
    deallocate(matrix_size)
    allocate(this%val(this%ptr%get_num_elements()))
    this%controller => controller
    this%node_sets => node_sets
    this%ccs => ccs
    this%nb = nb

  end function

  function get_ptr(this, node, i, j) result(ptr)
    double precision, pointer, contiguous :: ptr(:)
    class(block_matrices_c) :: this
    integer, intent(in) :: node, i, j
    integer :: nb, nc, nr
    double precision, pointer, contiguous :: array(:)

    nb = this%nb
    nc = this%node_sets%get_length(node)
    nr = this%ccs%get_array_length(node)
    array => this%val(this%ptr%get_first(node):this%ptr%get_last(node))
    ptr => this%controller%get_ptr(array, nb, nc, nr, i, j)

  end function  
end module