module extend_add_tasks_m
  use task_m
  implicit none
  private
  type(task_c), public :: extend_add_tri_task, extend_add_rect_task

  public :: init, finalize

contains
  subroutine init()
    use starpu_extend_add_kernel_m
    extend_add_tri_task = create_task_type((/MODE_R, MODE_RW, MODE_R/), 2, extend_add_tri)
    extend_add_rect_task = create_task_type((/MODE_R, MODE_RW, MODE_R, MODE_R/), 2, extend_add_rect)
  end subroutine

  subroutine finalize()
    call extend_add_tri_task%free()
    call extend_add_rect_task%free()
  end subroutine

end module