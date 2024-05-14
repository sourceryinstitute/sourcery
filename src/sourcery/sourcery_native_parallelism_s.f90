submodule(parallelism_m) native_parallelism_s
  !! Define wrappers for Fortan's native parallel programming model
  implicit none

contains

  module procedure error_stop_native_integer
    error stop code
  end procedure 

  module procedure error_stop_native_character
    error stop code
  end procedure 

  module procedure init_native
  end procedure 

  module procedure finalize_native
  end procedure 

  module procedure this_image_native
    this_image_native = this_image()
  end procedure 

  module procedure num_images_native
    num_images_native = num_images()
  end procedure 

end submodule native_parallelism_s
