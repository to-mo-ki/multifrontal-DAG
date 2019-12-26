module starpu_matrix_extractor_m
  use node_data_m
  use iso_c_binding, only: c_ptr
  implicit none
  private
  type, abstract, public :: extractor_c
    type(node_data_c), pointer :: node_data
  contains
    procedure(get_pos), deferred :: get_pos
    procedure(estimate_size), deferred :: estimate_size
  end type
  interface
    integer function get_pos(this, node, i, j)
      import extractor_c
      class(extractor_c) :: this
      integer, intent(in) :: node, i, j
    end function

    integer function estimate_size(this, node)
      import extractor_c
      class(extractor_c) :: this
      integer, intent(in) :: node
    end function
  end interface

end module