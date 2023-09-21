module data_partition_test_m
  !! verify data partitioning across images and data gathering
  use sourcery_m, only : data_partition_t, test_t, test_result_t
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

    test_results = [ &
      test_result_t("partitioning data in nearly even blocks", verify_block_partitioning()), &
      test_result_t("default image_number is this_image()", verify_default_image_number()), &
      test_result_t("partitioning all data across all images without data loss", verify_all_particles_partitioned()), &
      test_result_t("gathering a 1D real array onto all images", verify_all_gather_1D_real_array()), &
      test_result_t("gathering dimension 1 of 2D real array onto all images witout dim argument", &
        verify_all_gather_2D_real_array()), &
      test_result_t("gathering dimension 1 of 2D real array onton all images with dim argument", &
        verify_all_gather_2D_real_array_dim1()), &
      test_result_t("gathering dimension 1 of 2D real array onto result_image with dim argument", &
        verify_gather_2D_real_array_dim1()) &
    ]
  end function

  function verify_block_partitioning() result(test_passes)
    !! Verify that the data is partitioned across images evenly to
    !! within a difference of one datum between any two images.
    type(data_partition_t) partition
    logical test_passes
    integer my_particles

    call partition%define_partitions(cardinality=num_particles)

    associate( me=>this_image() )
      associate( my_first=>partition%first(me), my_last=>partition%last(me) )
        my_particles = my_last - my_first + 1
        associate( ni=>num_images() )
          associate( quotient=>num_particles/ni, remainder=>mod(num_particles,ni)  )
            test_passes = quotient + merge(1, 0, me<=remainder) == my_particles
          end associate
        end associate
      end associate
    end associate

  end function

  function verify_default_image_number() result(test_passes)
    !! Verify that the first and last functions assume image_number == this_image() if image_number is not  present
    type(data_partition_t) partition
    logical test_passes

    call partition%define_partitions(cardinality=num_particles)

    associate( me=>this_image() )
      test_passes = partition%first() == partition%first(me) .and.partition%last() == partition%last(me)
    end associate
  end function

  function verify_all_particles_partitioned() result(test_passes)
    !! Verify that the number of particles on each image sums to the
    !! total number of particles distributed.
    type(data_partition_t) partition
    logical test_passes
    integer particles

    call partition%define_partitions(cardinality=num_particles)

    associate(me => this_image())
      associate( my_first=>partition%first(me), my_last=>partition%last(me) )
        particles = my_last - my_first + 1
        call co_sum(particles)
        test_passes = num_particles == particles
      end associate
    end associate
  end function

 function verify_all_gather_1D_real_array() result(test_passes)
   type(data_partition_t) partition
   logical test_passes
   real(real64) :: particle_scalar(num_particles)
   real(real64), parameter :: junk=-12345._real64, expected=1._real64

   call partition%define_partitions(cardinality=num_particles)

   associate(me => this_image())
     associate( first=>partition%first(me), last=>partition%last(me) )
       particle_scalar(first:last) = expected !! values to be gathered
       particle_scalar(1:first-1)  = junk !! values to be overwritten by the gather
       particle_scalar(last+1:)  = junk !! values to be overwritten by the gather
       call partition%gather(particle_scalar)
       test_passes = all(particle_scalar==expected)
     end associate
   end associate
 end function

 function verify_all_gather_2D_real_array() result(test_passes)
   type(data_partition_t) partition
   logical test_passes
   integer, parameter :: vec_space_dim=3
   real(real64) particle_vector(vec_space_dim, num_particles)
   real(real64), parameter :: junk=-12345._real64, expected=1._real64

   call partition%define_partitions(cardinality=num_particles)

   associate(me => this_image())
     associate( first=>partition%first(me), last=>partition%last(me) )

       particle_vector(:, first:last) = expected !! values to be gathered
       particle_vector(:, 1:first-1)  = junk !! values to be overwritten by the gather
       particle_vector(:, last+1:)  = junk !! values to be overwritten by the gather
       call partition%gather(particle_vector)
       test_passes = all(particle_vector==expected)
     end associate
   end associate
 end function

 function verify_all_gather_2D_real_array_dim1() result(test_passes)
   type(data_partition_t) partition
   logical test_passes
   integer, parameter :: vec_space_dim=3
   real(real64) :: vector_transpose(num_particles, vec_space_dim)
   real(real64), parameter :: junk=-12345._real64, expected=1._real64

   call partition%define_partitions(cardinality=num_particles)

   associate(me => this_image())
     associate( first=>partition%first(me), last=>partition%last(me) )

       vector_transpose(first:last, :) = expected !! values to be gathered
       vector_transpose(1:first-1, :)  = junk !! values to be overwritten by the gather
       vector_transpose(last+1:, :)  = junk !! values to be overwritten by the gather

       call partition%gather( vector_transpose, dim=1)

       test_passes= all(vector_transpose==expected)

      end associate
    end associate
 end function

 function verify_gather_2D_real_array_dim1() result(test_passes)
   type(data_partition_t) partition
   logical test_passes
   integer, parameter :: vec_space_dim=3
   real(real64) :: vector_transpose(num_particles, vec_space_dim)
   real(real64), parameter :: junk=-12345._real64, expected=1._real64

   call partition%define_partitions(cardinality=num_particles)

   associate(me => this_image())
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
 end function

end module data_partition_test_m
