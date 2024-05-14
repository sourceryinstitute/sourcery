module parallelism_m
  !! Use compile-time polymophism to select wrappers for native or alternative parallel progromming models
  implicit none

  private
  public :: mpi_t ! alternative programming models

  public :: error_stop_   ! execute error stop or print stop code, invoke MPI_Finalize, and invoke MPI_Abort
  !public :: co_broadcast_ ! call co_broadcast or MPI_Bcast
  !public :: co_sum_       ! call co_sum or MPI_Reduce
  !public :: co_min_       ! call co_min or MPI_Reduce
  !public :: co_max_       ! call co_max or MPI_Reduce
  !public :: co_reduce_    ! call co_reduce or MPI_Reduce
  public :: init_         ! do nothing or invoke MPI_Init
  public :: finalize_     ! do nothing or a invoke MPI_Finalize
  public :: num_images_   ! invoke num_images() or call MPI_Comm_Size
  !public :: sync_all_     ! execute sync all or invoke MPI_Barrier
  !public :: stop_         ! execute stop or print stop code, invoke MPI_Finalize, and then execute stop
  public :: this_image_   ! invoke this_image() or call MPI_Comm_Rank

  type mpi_t
  end type

  interface error_stop_

    module subroutine error_stop_native_integer(code)
      implicit none
      integer, intent(in) :: code
    end subroutine 

    module subroutine error_stop_mpi_integer(mpi, code)
      implicit none
      type(mpi_t) mpi
      integer, intent(in) :: code
    end subroutine 

    module subroutine error_stop_native_character(code)
      implicit none
      character(len=*), intent(in) :: code
    end subroutine 

    module subroutine error_stop_mpi_character(mpi, code)
      implicit none
      type(mpi_t) mpi
      character(len=*), intent(in) :: code
    end subroutine 

  end interface

  interface init_

    module subroutine init_native()
      implicit none
    end subroutine 

    module subroutine init_mpi(mpi)
      implicit none
      type(mpi_t) mpi
    end subroutine 

  end interface

  interface finalize_

    module subroutine finalize_native()
      implicit none
    end subroutine 

    module subroutine finalize_mpi(mpi)
      implicit none
      type(mpi_t) mpi
    end subroutine 

  end interface

  interface this_image_

    integer module function this_image_native()
      implicit none
    end function 

    integer module function this_image_mpi(mpi)
      implicit none
      type(mpi_t) mpi
    end function 

  end interface

  interface num_images_

    integer module function num_images_native()
      implicit none
    end function 

    integer module function num_images_mpi(mpi)
      implicit none
      type(mpi_t) mpi
    end function 

  end interface

  ! ...

end module parallelism_m
