module compute_tree_m
  use jagged_array_m
  use disjoint_set_m
  implicit none
  private

  public :: compute_tree
  
contains
  function compute_tree(crs) result(parent)
    ! OPTIMIZE: 素集合データ構造を用いたものに変更する必要あり
    integer, pointer, contiguous :: parent(:)
    type(jagged_array_c), pointer, intent(in) :: crs
    integer :: i, k, t, n
    integer, pointer, contiguous :: cols(:)
    type(disjoint_set_c), pointer :: disjoint_set

    n = crs%get_num_arrays()
    allocate(parent(n))

    disjoint_set => create_disjoint_set(n)
    
    do i=1, n
      parent(i) = 0
      cols => crs%get_array(i)
      do k=1, size(cols)-1
        t = disjoint_set%find(cols(k))
        if(parent(t) == 0 .and. t /= i)then
          parent(t) = i
          call disjoint_set%link(t, i)
        endif
      enddo
    enddo

  end function

end module