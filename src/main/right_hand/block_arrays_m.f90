module block_arrays_m
  use contiguous_sets_m
  use jagged_array_m
  use rh_controller_m
  implicit none
  private
  type, public :: block_arrays_c
    private
    double precision, pointer, contiguous :: val(:)
    type(contiguous_sets_c), pointer :: ptr
    type(contiguous_sets_c), pointer :: node_sets
    type(jagged_array_c), pointer :: ccs
    class(rh_controller_c), pointer :: controller
    integer :: nb
  contains
    procedure :: get_ptr
  end type

  public :: create_block_arrays

contains
  function create_block_arrays(nb, node_sets, ccs, controller) result(this)
    type(block_arrays_c), pointer :: this
    integer, intent(in) :: nb
    type(contiguous_sets_c), pointer, intent(in) :: node_sets
    type(jagged_array_c), pointer, intent(in) :: ccs
    integer, pointer, contiguous :: array_size(:)
    class(rh_controller_c), pointer, intent(in) :: controller
    integer :: i, nc, nr

    allocate(this)
    allocate(array_size(node_sets%get_num_sets()))
    do i=1, node_sets%get_num_sets()
      nc = node_sets%get_length(i)
      nr = ccs%get_array_length(i)
      array_size(i) = controller%estimate_size(nb, nc, nr)
    enddo
    this%ptr => create_contiguous_sets(array_size)
    deallocate(array_size)
    allocate(this%val(this%ptr%get_num_elements()))
    this%controller => controller
    this%node_sets => node_sets
    this%ccs => ccs
    this%nb = nb

  end function

  function get_ptr(this, node, idx) result(ptr)
    double precision, pointer, contiguous :: ptr(:)
    class(block_arrays_c) :: this
    integer, intent(in) :: node, idx
    integer :: nb, nc, nr
    double precision, pointer, contiguous :: array(:)

    nb = this%nb
    nc = this%node_sets%get_length(node)
    nr = this%ccs%get_array_length(node)
    array => this%val(this%ptr%get_first(node):this%ptr%get_last(node))
    ptr => this%controller%get_ptr(array, nb, nc, nr, idx)

  end function  
end module