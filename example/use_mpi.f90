program mpi_hello_world
  !! Use MPI wrappers analogous to Fortran's native parallel features
  use parallelism_m, only : mpi_t, init_, finalize_, this_image_, num_images_
  implicit none
  
  type(mpi_t) mpi

  call init_(mpi)
  print *,"Hello from image ",this_image_()," of ",num_images_()
  call finalize_(mpi)
end program
