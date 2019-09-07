module compute_tree_m
  use jagged_array_m
  implicit none
  private

  public :: compute_tree
  
contains
  function compute_tree(crs) result(parent)
    ! OPTIMIZE: 素集合データ構造を用いたものに変更する必要あり
    integer, pointer, contiguous :: parent(:)
    type(jagged_array_c), intent(in) :: crs
    integer :: j, i, r, t, n
    integer, pointer, contiguous :: rows(:)
    integer, allocatable :: virtual(:), pos_parent(:)

    n = crs%get_num_arrays()
    allocate(parent(n))
    allocate(virtual(n), pos_parent(n))

    do j = 1, n
      parent(j) = 0
      virtual(j) = n+1
      rows => crs%get_array(j)
      do i = 1, size(rows) - 1
        r = rows(i)
        do while(virtual(r) < j)
          t = virtual(r)
          virtual(r) = j
          r = t
        enddo
        if(virtual(r) == n+1)then
          virtual(r) = j
          parent(r) = j
        endif
      enddo
    enddo
  end function

end module