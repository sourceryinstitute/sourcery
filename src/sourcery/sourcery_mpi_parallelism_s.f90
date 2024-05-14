submodule(parallelism_m) mpi_parallelism_s
  !! Define wrappers for Message Passing Interface (MPI) procedures
  use mpi_f08
  use iso_fortran_env, only : error_unit
  implicit none

contains

  module procedure error_stop_mpi_integer
    call MPI_Abort(mpi_comm_world, code)
  end procedure 

  module procedure error_stop_mpi_character
    write(error_unit,*) code
    call MPI_Abort(mpi_comm_world, errorcode=1)
  end procedure 

  module procedure init_mpi
    integer ierr
    call mpi_init(ierr)
  end procedure 

  module procedure finalize_mpi
    call mpi_finalize()
  end procedure 

  module procedure this_image_mpi
    integer rank, ierr
    call mpi_comm_rank(mpi_comm_world, rank, ierr)
    this_image_mpi = rank + 1
  end procedure 

  module procedure num_images_mpi
    integer ierr
    call mpi_comm_size(mpi_comm_world, num_images_mpi, ierr)
  end procedure 

end submodule mpi_parallelism_s
