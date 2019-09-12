module perm_m
  implicit none

  interface permutate
    procedure :: permutate_DP
    procedure :: permutate_int
  end interface

contains

  subroutine set_iperm(perm, iperm)
    integer, pointer, intent(in) :: perm(:)
    integer, pointer, intent(out) :: iperm(:)
    integer :: i, n

    n = size(perm)

    allocate(iperm(n))
    do i=1,n
      iperm(perm(i)) = i
    enddo

  end subroutine set_iperm

  subroutine perm_product(first_perm, second_perm, out_perm)
    integer, pointer, intent(in) :: first_perm(:), second_perm(:)
    integer, pointer, intent(out) :: out_perm(:)
    integer :: i, n

    n = size(first_perm)
    allocate(out_perm(n))
    do i=1,n
      out_perm(i) = first_perm(second_perm(i))
    enddo

  end subroutine

  subroutine permutate_DP(perm, in_data, out_data)
    integer, pointer, intent(in) :: perm(:)
    double precision, pointer, intent(in) :: in_data(:)
    double precision, pointer, intent(out) :: out_data(:)
    integer :: i, n

    n = size(perm)
    allocate(out_data(n))
    do i=1,n
      out_data(i) = in_data(perm(i))
    enddo

  end subroutine

  subroutine permutate_int(perm, in_data, out_data)
    integer, pointer, intent(in) :: perm(:)
    integer, pointer, intent(in) :: in_data(:)
    integer, pointer, intent(out) :: out_data(:)
    integer :: i, n

    n = size(perm)
    allocate(out_data(n))
    do i=1,n
      out_data(i) = in_data(perm(i))
    enddo

  end subroutine

  subroutine inverse_permutate(perm, in_data, out_data)
    integer, pointer, intent(in) :: perm(:)
    double precision, pointer, intent(in) :: in_data(:)
    double precision, pointer, intent(out) :: out_data(:)
    integer :: i, n

    n = size(perm)
    allocate(out_data(n))
    do i=1,n
      out_data(perm(i)) = in_data(i)
    enddo

  end subroutine inverse_permutate

end module perm_m
