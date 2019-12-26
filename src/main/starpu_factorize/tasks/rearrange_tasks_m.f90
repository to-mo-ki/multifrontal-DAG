module rearrange_tasks_m
  use task_m
  implicit none
  private
  type(task_c), public :: split_tri_matrix_task, split_rect_matrix_task

  public :: init, finalize

contains
  subroutine init()
    use starpu_matrix_splitter_m
    split_tri_matrix_task = create_task((/MODE_R, MODE_RW, MODE_RW/), 2, split_tri_matrix)
    split_rect_matrix_task = create_task((/MODE_R, MODE_RW, MODE_RW/), 3, split_rect_matrix)
  end subroutine

  subroutine finalize()
    call split_tri_matrix_task%free()
    call split_rect_matrix_task%free()
  end subroutine

end module