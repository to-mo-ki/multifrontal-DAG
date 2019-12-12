module block_matrices_m
  use contiguous_sets_m
  use matrix_extractor_m
  implicit none
  private
  type, public :: block_matrices_c
    private
    double precision, pointer, contiguous :: val(:)
    type(contiguous_sets_c), pointer :: ptr
    integer, pointer, contiguous :: supernode_size(:), work_size(:)
    class(extractor_c), pointer :: controller
    integer :: nb
  contains
    procedure :: get_ptr
  end type

  public :: create_block_matrices

contains
  function create_block_matrices(nb, supernode_size, work_size, controller) result(this)
    type(block_matrices_c), pointer :: this
    integer, intent(in) :: nb
    integer, contiguous, target :: supernode_size(:), work_size(:)
    integer, pointer, contiguous :: matrix_size(:)
    class(extractor_c), pointer, intent(in) :: controller
    integer :: i, nc, nr

    allocate(this)
    allocate(matrix_size(size(supernode_size)))
    do i=1, size(supernode_size)
      nc = supernode_size(i)
      nr = work_size(i)
      matrix_size(i) = controller%estimate_size(nb, nc, nr)
    enddo
    this%ptr => create_contiguous_sets(matrix_size)
    deallocate(matrix_size)
    allocate(this%val(this%ptr%get_num_elements()))
    this%controller => controller
    this%supernode_size => supernode_size
    this%work_size => work_size
    this%nb = nb

  end function

  function get_ptr(this, node, i, j) result(ptr)
    double precision, pointer, contiguous :: ptr(:)
    class(block_matrices_c) :: this
    integer, intent(in) :: node, i, j
    integer :: nb, nc, nr
    double precision, pointer, contiguous :: array(:)

    nb = this%nb
    nc = this%supernode_size(node)
    nr = this%work_size(node)
    array => this%val(this%ptr%get_first(node):this%ptr%get_last(node))
    ptr => this%controller%get_ptr(array, nb, nc, nr, i, j)

  end function  
end module