module contiguous_set_m
  implicit none
  private
  type, public :: contiguous_set_c
    integer, pointer, contiguous :: ptr(:)
  contains
    procedure :: get_first
    procedure :: get_last
    procedure :: get_length
  end type

  public :: create_contiguous_set
  
contains
  type(contiguous_set_c) function create_contiguous_set(set_length) result(this)
    integer, pointer, contiguous :: set_length(:)
    integer :: n, i

    n = size(set_length)
    allocate(this%ptr(n+1))
    this%ptr(1) = 1
    do i=1, n
      this%ptr(i+1) = this%ptr(i) + set_length(i)
    enddo

  end function

  integer function get_first(this, idx)
    class(contiguous_set_c) :: this
    integer, intent(in) :: idx

    get_first = this%ptr(idx)
    
  end function

  integer function get_last(this, idx)
    class(contiguous_set_c) :: this
    integer, intent(in) :: idx

    get_last = this%ptr(idx+1)-1
    
  end function

  integer function get_length(this, idx)
    class(contiguous_set_c) :: this
    integer, intent(in) :: idx

    get_length = this%ptr(idx+1) - this%ptr(idx)
    
  end function

end module