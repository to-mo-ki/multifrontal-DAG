module finding_leaves_m
  use jagged_array_m
  implicit none
  
contains
  function finding_leaves(subtree_size, ccs) result(isleaf)
    integer, pointer, contiguous :: isleaf(:)
    integer, pointer, contiguous, intent(in) :: subtree_size(:)
    integer, pointer, contiguous :: rows(:)
    integer, allocatable :: prev_rownz(:)
    type(jagged_array_c), pointer, intent(in) :: ccs
    integer :: n, j, i

    n = ccs%get_num_arrays()
    allocate(isleaf(n), prev_rownz(n))
    isleaf = 0
    prev_rownz = 0

    do j=1, n
      rows => ccs%get_array(j)
      do i=2, size(rows)
        if(prev_rownz(rows(i)) < j - subtree_size(j) + 1)then
          isleaf(j) = 1
        endif
        prev_rownz(rows(i)) = j
      enddo
    enddo
    
  end function
end module