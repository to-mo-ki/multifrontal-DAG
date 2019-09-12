module contiguous_sets_m
  implicit none
  private
  type, public :: contiguous_sets_c
    integer, pointer, contiguous :: ptr(:)
  contains
    procedure :: get_first
    procedure :: get_last
    procedure :: get_length
    procedure :: get_num_sets
    procedure :: get_num_elements
  end type

  public :: create_contiguous_sets

contains
  type(contiguous_sets_c) function create_contiguous_sets(set_length) result(this)
    integer :: set_length(:)
    integer :: n, i

    n = size(set_length)
    allocate(this%ptr(n+1))
    this%ptr(1) = 1
    do i=1, n
      this%ptr(i+1) = this%ptr(i) + set_length(i)
    enddo

  end function

  integer function get_first(this, idx)
    class(contiguous_sets_c) :: this
    integer, intent(in) :: idx

    get_first = this%ptr(idx)
    
  end function

  integer function get_last(this, idx)
    class(contiguous_sets_c) :: this
    integer, intent(in) :: idx

    get_last = this%ptr(idx+1)-1
    
  end function

  integer function get_length(this, idx)
    class(contiguous_sets_c) :: this
    integer, intent(in) :: idx

    get_length = this%ptr(idx+1) - this%ptr(idx)
    
  end function

  integer function get_num_sets(this)
    class(contiguous_sets_c) :: this

    get_num_sets = size(this%ptr) - 1

  end function

  integer function get_num_elements(this)
    class(contiguous_sets_c) :: this

    get_num_elements = this%get_last(this%get_num_sets())

  end function

end module