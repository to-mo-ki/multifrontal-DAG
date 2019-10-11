module extract_matrix_m
  use factors_m
  implicit none
  
  interface
    function extract_matrix_i(factors, node, i, j) result(matrix)
      import factors_c
      type(factors_c), pointer :: factors
      double precision, pointer, contiguous :: matrix(:)
      integer, intent(in) :: node, i, j
    end function
  end interface

contains

  function extract_supernode(factors, node, i, j) result(matrix)
    type(factors_c), pointer :: factors
    double precision, pointer, contiguous :: matrix(:)
    integer, intent(in) :: node, i, j
    matrix => factors%get_supernode_ptr(node, i, j)
  end function

  function extract_border(factors, node, i, j) result(matrix)
    type(factors_c), pointer :: factors
    double precision, pointer, contiguous :: matrix(:)
    integer, intent(in) :: node, i, j
    matrix => factors%get_border_ptr(node, i, j)
  end function

  function extract_work(factors, node, i, j) result(matrix)
    type(factors_c), pointer :: factors
    double precision, pointer, contiguous :: matrix(:)
    integer, intent(in) :: node, i, j
    matrix => factors%get_work_ptr(node, i, j)
  end function

  function extract_matrix(factors, node, i, j) result(matrix)
    type(factors_c), pointer :: factors
    double precision, pointer, contiguous :: matrix(:)
    integer, intent(in) :: node, i, j
    matrix => factors%get_matrix_ptr(node, i, j)
  end function

end module