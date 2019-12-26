module factorize_tasks_m
  use task_m
  implicit none
  private
  type(task_c), public :: factorize_task, solve_task, update_task, sym_update_task

  public :: init, finalize

contains
  subroutine init()
    use starpu_factorize_kernel_m
    factorize_task = create_task((/MODE_RW/), 1, mydpotrf)
    solve_task = create_task((/MODE_R, MODE_RW/), 2, mydtrsm)
    update_task = create_task((/MODE_R, MODE_R, MODE_RWC/), 3, mydgemm)
    sym_update_task = create_task((/MODE_R, MODE_RWC/), 2, mydsyrk)
  end subroutine

  subroutine finalize()
    call factorize_task%free()
    call solve_task%free()
    call update_task%free()
    call sym_update_task%free()
  end subroutine

end module