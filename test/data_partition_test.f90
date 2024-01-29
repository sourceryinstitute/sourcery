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
      test_result_t("partitioning data in nearly even blocks", verify_block_partitioning(num_particles)), &
      test_result_t("partitioning data in nearly even blocks when some blocks are singletons", &
       verify_block_partitioning(num_images()+1)), &
      test_result_t("bins of size 1 when set cardinality == num_images()", verify_block_partitioning_with_singletons()), &
      test_result_t("default image_number is this_image()", verify_default_image_number()), &
      test_result_t("partitioning data into contiguous bins without overlap", &
        verify_partitions_are_contiguous_without_overlap(num_particles)), &
      test_result_t("contiguous non overlapping partitions with singletons", &
        verify_partitions_are_contiguous_without_overlap(num_images())), &
      test_result_t("contiguous non overlapping partitions with some singletons", &
        verify_partitions_are_contiguous_without_overlap(num_images()+1)), &
      test_result_t("partitioning all data across all images without data loss", verify_all_particles_partitioned()), &
      test_result_t("no data is lost when singleton bins are used", verify_all_particles_partitioned_on_singletons()), &
      test_result_t("gathering a 1D real array onto all images", verify_all_gather_1D_real_array()), &
      test_result_t("gathering dimension 1 of 2D real array onto all images witout dim argument", &
        verify_all_gather_2D_real_array()), &
      test_result_t("gathering dimension 1 of 2D real array onton all images with dim argument", &
        verify_all_gather_2D_real_array_dim1()), &
      test_result_t("gathering dimension 1 of 2D real array onto result_image with dim argument", &
        verify_gather_2D_real_array_dim1()) &
    ]
  end function

  function verify_testing_in_parallel() result(test_passes)
    !! Verify that the test is being run in parallel
    logical test_passes
    test_passes = num_images() > 1
  end function

  function verify_block_partitioning(cardinality) result(test_passes)
    !! Verify that the data is partitioned across images evenly to
    !! within a difference of one datum between any two images.
    integer, intent(in) :: cardinality
    type(data_partition_t) partition
    logical test_passes
    integer my_particles

    associate( me=>this_image(), partition => data_partition_t(cardinality=cardinality))
      associate( my_first=>partition%first(me), my_last=>partition%last(me) )
        my_particles = my_last - my_first + 1
        associate( ni=>num_images() )
          associate( quotient=>cardinality/ni, remainder=>mod(cardinality,ni)  )
            test_passes = quotient + merge(1, 0, me<=remainder) == my_particles
          end associate
        end associate
      end associate
    end associate

  end function

  function verify_block_partitioning_with_singletons() result(test_passes)
    !! Verify that the data is partitioned so that each image has a bin of
    !! size 1.
    type(data_partition_t) partition, another_partition
    logical test_passes
    integer my_particles
    integer num_particles
        
    num_particles = num_images()
    another_partition = data_partition_t(cardinality=num_particles)

    associate( me=>this_image(), partition => data_partition_t(cardinality=num_particles))
      associate( my_first=>partition%first(me), my_last=>partition%last(me) )
        my_particles = my_last - my_first + 1
        associate( ni=>num_images() )
          associate( quotient=>num_particles/ni, remainder=>mod(num_particles,ni)  )
            test_passes = quotient == 1 &
              .and. remainder == 0 &
              .and. my_particles == 1 &
              .and. my_last == my_first &
              .and. my_first == me
          end associate
        end associate
      end associate
    end associate

  end function

  function verify_partitions_are_contiguous_without_overlap(cardinality) result(test_passes)
    !! Verify that the data is partitioned across images into contiguous bins without overlap
    integer, intent(in) :: cardinality
    logical test_passes
    type(data_partition_t) partition

    associate( me=>this_image(), partition => data_partition_t(cardinality=cardinality))
      associate( my_first=>partition%first(me), my_last=>partition%last(me) )
        associate(ni => num_images())
          if (me > 1) then
            associate( your_first=>partition%first(me-1), your_last=>partition%last(me-1) )
                test_passes = my_first <= my_last &
                .and. your_first <= your_last &
                .and. my_first >= 1 &
                .and. my_last <= cardinality &
                .and. my_first == your_last + 1
            end associate
          else if (me == 1 .and. ni > 1) then
            associate( your_first=>partition%first(me+1), your_last=>partition%last(me+1) )
                test_passes = my_first <= my_last &
                .and. your_first <= your_last &
                .and. my_first >= 1 &
                .and. my_last <= cardinality &
                .and. my_last == your_first - 1
            end associate
          else if (ni == 1) then
            test_passes = my_first <= my_last &
            .and. my_first >= 1 &
            .and. my_last <= cardinality
          end if
        end associate
      end associate
    end associate
  end function

  function verify_default_image_number() result(test_passes)
    !! Verify that the first and last functions assume image_number == this_image() if image_number is not  present
    type(data_partition_t) partition
    logical test_passes

    associate( me=>this_image(), partition => data_partition_t(cardinality=num_particles))
      test_passes = partition%first() == partition%first(me) .and.partition%last() == partition%last(me)
    end associate
  end function

  function verify_all_particles_partitioned() result(test_passes)
    !! Verify that the number of particles on each image sums to the
    !! total number of particles distributed.
    type(data_partition_t) partition
    logical test_passes
    integer particles

    associate( me=>this_image(), partition => data_partition_t(cardinality=num_particles))
      associate( my_first=>partition%first(me), my_last=>partition%last(me) )
        particles = my_last - my_first + 1
        call co_sum(particles)
        test_passes = num_particles == particles
      end associate
    end associate
  end function

  function verify_all_particles_partitioned_on_singletons() result(test_passes)
    !! Verify that the number of particles on each image sums to the
    !! total number of particles distributed when the cardinality of the 
    !! partitioned set is equal to num_images()
    type(data_partition_t) partition
    logical test_passes
    integer particles
    integer num_particles
    num_particles = num_images()

    associate( me=>this_image(), partition => data_partition_t(cardinality=num_particles))
      associate( my_first=>partition%first(me), my_last=>partition%last(me) )
        particles = my_last - my_first + 1
        test_passes = particles == 1
        call co_sum(particles)
        test_passes = test_passes .and. num_particles == particles
      end associate
    end associate
  end function

 function verify_all_gather_1D_real_array() result(test_passes)
   type(data_partition_t) partition
   logical test_passes
   real(real64) :: particle_scalar(num_particles)
   real(real64), parameter :: junk=-12345._real64, expected=1._real64

   associate( me=>this_image(), partition => data_partition_t(cardinality=num_particles))
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

   associate( me=>this_image(), partition => data_partition_t(cardinality=num_particles))
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

   associate( me=>this_image(), partition => data_partition_t(cardinality=num_particles))
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
 end function

end module data_partition_test_m
