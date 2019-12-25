module border_tasks_m
  use task_m
  implicit none
  private
  type(task_c), public :: border_factorize_task, border_solve_task

  public :: init, finalize

contains
  subroutine init()
    use starpu_border_kernel_m
    border_factorize_task = create_task((/MODE_RW, MODE_RW/), 2, border_potrf)
    border_solve_task = create_task((/MODE_R, MODE_RW, MODE_RW/), 3, border_trsm)
  end subroutine

  subroutine finalize()
    call border_factorize_task%free()
    call border_solve_task%free()
  end subroutine

end module