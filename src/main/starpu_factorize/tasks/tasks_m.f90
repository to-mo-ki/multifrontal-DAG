module tasks_m
  use factorize_tasks_m, factorize_init => init, factorize_finalize => finalize
  use border_tasks_m, border_init => init, border_finalize => finalize
  use extend_add_tasks_m, extend_add_init => init, extend_add_finalize => finalize
  use rearrange_tasks_m, rearrange_init => init, rearrange_finalize => finalize
  implicit none
  private
  
  public :: init, finalize

contains

  subroutine init
    call factorize_init
    call border_init
    call extend_add_init
    call rearrange_init
  end subroutine

  subroutine finalize
    call factorize_finalize
    call border_finalize
    call extend_add_finalize
    call rearrange_finalize
  end subroutine
  
end module