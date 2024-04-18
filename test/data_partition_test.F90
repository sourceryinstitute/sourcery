module data_partition_test_m
  !! check data partitioning across images and data gathering
  use sourcery_m, only : &
    data_partition_t, test_t, test_result_t, test_description_substring, test_description_t, string_t
#ifdef __GFORTRAN__
  use sourcery_m, only : test_function_i
#endif
    
  use iso_fortran_env, only : real64
  implicit none

  private
  public :: data_partition_test_t

  type, extends(test_t) :: data_partition_test_t
  contains
    procedure, nopass :: subject
    procedure, nopass :: results
  end type

  type(data_partition_t) partition
  integer, parameter :: num_particles=31, gatherer=1, num_steps=9, dummy=0

contains

  pure function subject() result(specimen)
    character(len=:), allocatable :: specimen
    specimen = "The data_partition_t type" 
  end function

  function results() result(test_results)
    type(test_result_t), allocatable :: test_results(:)
    type(test_description_t), allocatable :: test_descriptions(:)
#ifndef __GFORTRAN__
    test_descriptions = [ & 
      test_description_t(string_t("partitioning data in nearly even blocks"), check_block_partitioning), &
      test_description_t(string_t("default image_number is this_image()"), check_default_image_number), &
      test_description_t(string_t("partitioning all data across all images without data loss"), check_all_particles_partitioned), &
      test_description_t(string_t("gathering a 1D real array onto all images"), check_all_gather_1D_real_array), &
      test_description_t(string_t("gathering dimension 1 of 2D real array onto all images witout dim argument"), &
        check_all_gather_2D_real_array), &
      test_description_t(string_t("gathering dimension 1 of 2D real array onton all images with dim argument"), &
        check_all_gather_2D_real_array_dim1), &
      test_description_t(string_t("gathering dimension 1 of 2D real array onto result_image with dim argument"), &
        check_gather_2D_real_array_dim1) &
    ]   
#else
    ! Work around missing Fortran 2008 feature: associating a procedure actual argument with a procedure pointer dummy argument:
    procedure(test_function_i), pointer :: &
      check_block_ptr, check_default_ptr, check_all_particles_ptr, check_all_gather_ptr, check_all_gather_2D_ptr, &
      check_all_gather_2D_real_ptr, check_gather_2D_real_array_ptr

    check_block_ptr => check_block_partitioning
    check_default_ptr => check_default_image_number
    check_all_particles_ptr => check_all_particles_partitioned
    check_all_gather_ptr => check_all_gather_1D_real_array
    check_all_gather_2D_ptr => check_all_gather_2D_real_array
    check_all_gather_2D_real_ptr => check_all_gather_2D_real_array_dim1
    check_gather_2D_real_array_ptr => check_gather_2D_real_array_dim1

    test_descriptions = [ & 
      test_description_t(string_t("partitioning data in nearly even blocks"), check_block_ptr), &
      test_description_t(string_t("default image_number is this_image()"), check_default_ptr), &
      test_description_t(string_t("partitioning all data across all images without data loss"), check_all_particles_ptr), &
      test_description_t(string_t("gathering a 1D real array onto all images"), check_all_gather_ptr), &
      test_description_t( &
        string_t("gathering dimension 1 of 2D real array onto all images witout dim argument"), check_all_gather_ptr), &
      test_description_t( &
        string_t("gathering dimension 1 of 2D real array onton all images with dim argument"), check_all_gather_2D_ptr), &
      test_description_t( &
        string_t("gathering dimension 1 of 2D real array onto result_image with dim argument"), check_gather_2D_real_array_ptr) &
    ]   
#endif
    test_descriptions = pack(test_descriptions, &
      index(subject(), test_description_substring) /= 0 .or. &
      test_descriptions%contains_text(string_t(test_description_substring)))
    test_results = test_descriptions%run()
  end function

  function check_block_partitioning() result(test_passes)
    !! check that the data is partitioned across images evenly to
    !! within a difference of one datum between any two images.
    logical test_passes

#ifndef _CRAYFTN
    associate( me=>this_image(), partition => data_partition_t(cardinality=num_particles))
      associate( my_first=>partition%first(me), my_last=>partition%last(me) )
        associate( ni=>num_images(), my_particles => my_last - my_first + 1)
          associate( quotient=>num_particles/ni, remainder=>mod(num_particles,ni)  )
            test_passes = quotient + merge(1, 0, me<=remainder) == my_particles
          end associate
        end associate
      end associate
    end associate
#else
    type(data_partition_t) partition

    associate(me=>this_image())
      partition = data_partition_t(cardinality=num_particles)
      associate( my_first=>partition%first(me), my_last=>partition%last(me) )
        associate( ni=>num_images(), my_particles => my_last - my_first + 1)
          associate( quotient=>num_particles/ni, remainder=>mod(num_particles,ni)  )
            test_passes = quotient + merge(1, 0, me<=remainder) == my_particles
          end associate
        end associate
      end associate
    end associate
#endif

  end function

  function check_default_image_number() result(test_passes)
    !! check that the first and last functions assume image_number == this_image() if image_number is not  present
    logical test_passes

#ifndef _CRAYFTN
    associate( me=>this_image(), partition => data_partition_t(cardinality=num_particles))
      test_passes = partition%first() == partition%first(me) .and.partition%last() == partition%last(me)
    end associate
#else
    type(data_partition_t) partition

    partition = data_partition_t(cardinality=num_particles)
    associate( me=>this_image())
      test_passes = partition%first() == partition%first(me) .and. partition%last() == partition%last(me)
    end associate
#endif
  end function

  function check_all_particles_partitioned() result(test_passes)
    !! check that the number of particles on each image sums to the
    !! total number of particles distributed.
    logical test_passes
    integer particles

#ifndef _CRAYFTN
    associate(me => this_image(), partition => data_partition_t(cardinality=num_particles))
      associate(my_first=>partition%first(me), my_last=>partition%last(me))
        particles = my_last - my_first + 1
        call co_sum(particles)
        test_passes = num_particles == particles
      end associate
    end associate
#else
    type(data_partition_t) partition

    partition = data_partition_t(cardinality=num_particles)
    associate(me=>this_image())
      associate(my_first=>partition%first(me), my_last=>partition%last(me))
        particles = my_last - my_first + 1
        call co_sum(particles)
        test_passes = num_particles == particles
      end associate
    end associate
#endif
  end function

 function check_all_gather_1D_real_array() result(test_passes)
   logical test_passes
   real(real64) :: particle_scalar(num_particles)
   real(real64), parameter :: junk=-12345._real64, expected=1._real64

#ifndef _CRAYFTN
   associate( me=>this_image(), partition => data_partition_t(cardinality=num_particles))
     associate( first=>partition%first(me), last=>partition%last(me) )
       particle_scalar(first:last) = expected !! values to be gathered
       particle_scalar(1:first-1)  = junk !! values to be overwritten by the gather
       particle_scalar(last+1:)  = junk !! values to be overwritten by the gather
       call partition%gather(particle_scalar)
       test_passes = all(particle_scalar==expected)
     end associate
   end associate
#else
   type(data_partition_t) partition

   associate( me=>this_image())
     partition = data_partition_t(cardinality=num_particles)
     associate( first=>partition%first(me), last=>partition%last(me) )
       particle_scalar(first:last) = expected !! values to be gathered
       particle_scalar(1:first-1)  = junk !! values to be overwritten by the gather
       particle_scalar(last+1:)  = junk !! values to be overwritten by the gather
       call partition%gather(particle_scalar)
       test_passes = all(particle_scalar==expected)
     end associate
   end associate
#endif
 end function

 function check_all_gather_2D_real_array() result(test_passes)
   logical test_passes
   integer, parameter :: vec_space_dim=3
   real(real64) particle_vector(vec_space_dim, num_particles)
   real(real64), parameter :: junk=-12345._real64, expected=1._real64

#ifndef _CRAYFTN
   associate( me=>this_image(), partition => data_partition_t(cardinality=num_particles))
     associate( first=>partition%first(me), last=>partition%last(me) )
       particle_vector(:, first:last) = expected !! values to be gathered
       particle_vector(:, 1:first-1)  = junk !! values to be overwritten by the gather
       particle_vector(:, last+1:)  = junk !! values to be overwritten by the gather
       call partition%gather(particle_vector)
       test_passes = all(particle_vector==expected)
     end associate
   end associate
#else
   type(data_partition_t) partition

   associate( me=>this_image())
     partition = data_partition_t(cardinality=num_particles)
     associate( first=>partition%first(me), last=>partition%last(me) )
       particle_vector(:, first:last) = expected !! values to be gathered
       particle_vector(:, 1:first-1)  = junk !! values to be overwritten by the gather
       particle_vector(:, last+1:)  = junk !! values to be overwritten by the gather
       call partition%gather(particle_vector)
       test_passes = all(particle_vector==expected)
     end associate
   end associate
#endif
 end function

 function check_all_gather_2D_real_array_dim1() result(test_passes)
   logical test_passes
   integer, parameter :: vec_space_dim=3
   real(real64) :: vector_transpose(num_particles, vec_space_dim)
   real(real64), parameter :: junk=-12345._real64, expected=1._real64

#ifndef _CRAYFTN
   associate( me=>this_image(), partition => data_partition_t(cardinality=num_particles))
     associate( first=>partition%first(me), last=>partition%last(me) )
       vector_transpose(first:last, :) = expected !! values to be gathered
       vector_transpose(1:first-1, :)  = junk !! values to be overwritten by the gather
       vector_transpose(last+1:, :)  = junk !! values to be overwritten by the gather
       call partition%gather( vector_transpose, dim=1)
       test_passes= all(vector_transpose==expected)
      end associate
    end associate
#else
   type(data_partition_t) partition

   associate(me=>this_image())
     partition = data_partition_t(cardinality=num_particles)
     associate( first=>partition%first(me), last=>partition%last(me) )
       vector_transpose(first:last, :) = expected !! values to be gathered
       vector_transpose(1:first-1, :)  = junk !! values to be overwritten by the gather
       vector_transpose(last+1:, :)  = junk !! values to be overwritten by the gather
       call partition%gather( vector_transpose, dim=1)
       test_passes= all(vector_transpose==expected)
      end associate
    end associate
#endif
 end function

 function check_gather_2D_real_array_dim1() result(test_passes)
   logical test_passes
   integer, parameter :: vec_space_dim=3
   real(real64) :: vector_transpose(num_particles, vec_space_dim)
   real(real64), parameter :: junk=-12345._real64, expected=1._real64

#ifndef _CRAYFTN
   associate( me=>this_image(), partition => data_partition_t(cardinality=num_particles))
     associate( first=>partition%first(me), last=>partition%last(me) )

       vector_transpose(first:last, :) = expected !! values to be gathered
       vector_transpose(1:first-1, :)  = junk !! values to be overwritten by the gather
       vector_transpose(last+1:, :)  = junk !! values to be overwritten by the gather

       call partition%gather( vector_transpose, result_image=gatherer, dim=1)

       if (me==gatherer) then
         test_passes = all(vector_transpose==expected)
       else
         test_passes = &
           all(vector_transpose(1:first-1,:)==junk) .and. &
           all(vector_transpose(first:last,:)==expected) .and. &
           all(vector_transpose(last+1:,:)==junk)
       end if

     end associate
   end associate
#else
   type(data_partition_t) partition

   associate(me=>this_image())
     partition = data_partition_t(cardinality=num_particles)
     associate( first=>partition%first(me), last=>partition%last(me) )

       vector_transpose(first:last, :) = expected !! values to be gathered
       vector_transpose(1:first-1, :)  = junk !! values to be overwritten by the gather
       vector_transpose(last+1:, :)  = junk !! values to be overwritten by the gather

       call partition%gather( vector_transpose, result_image=gatherer, dim=1)

       if (me==gatherer) then
         test_passes = all(vector_transpose==expected)
       else
         test_passes = &
           all(vector_transpose(1:first-1,:)==junk) .and. &
           all(vector_transpose(first:last,:)==expected) .and. &
           all(vector_transpose(last+1:,:)==junk)
       end if

     end associate
   end associate
#endif
 end function

end module data_partition_test_m
