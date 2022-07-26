module data_partition_test
   !! author: Damian Rouson
   !!
   !! summary: verify data partitioning across images and data gathering
   use vegetables, only: &
     result_t, example_t, input_t, integer_input_t, test_item_t, & ! types
     describe, it, assert_equals, assert_that  ! functions
   use data_partition_m, only : data_partition_t
   use iso_fortran_env, only : real64
   
#ifdef USE_CAFFEINE
   use caffeine_m, only : this_image => caf_this_image, num_images => caf_num_images, co_sum => caf_co_sum
#endif
   
   implicit none

   private
   public :: test_data_partition

   type(data_partition_t) partition
   integer, parameter :: num_particles=31, gatherer=1, num_steps=9, dummy=0

contains

  function test_data_partition() result(tests)
    type(test_item_t) tests

    call partition%define_partitions( cardinality=num_particles)

    associate( me=>this_image() )
      associate( my_first=>partition%first(me), my_last=>partition%last(me) )
        tests = describe( &
         "data_partition class", &
         [it( &
           "partitions data in nearly even blocks", &
           verify_block_partitioning), &
          it( &
           "all data partitioned across all images without data loss", &
           verify_all_particles_partitioned), &
          it( &
           "1D real array gathered on all images", &
           [example_t(integer_input_t(dummy)), example_t(integer_input_t(dummy))], &
           verify_all_gather_1D_real_array), &
          it( &
           "dimension 1 of 2D real array gathered on all images witout dim argument", &
           [example_t(integer_input_t(dummy)), example_t(integer_input_t(dummy))], &
           verify_all_gather_2D_real_array), &
          it( &
           "dimension 1 of 2D real array gathered on all images with dim argument", &
           [example_t(integer_input_t(dummy)), example_t(integer_input_t(dummy))], &
           verify_all_gather_2D_real_array_dim1), &
          it( &
           "dimension 1 of 2D real array gathered onto result_image with dim argument", &
           [example_t(integer_input_t(dummy)), example_t(integer_input_t(dummy))], &
           verify_gather_2D_real_array_dim1)])

      end associate
    end associate
  end function

  function verify_block_partitioning() result(result_)
    !! Verify that the data is partitioned across images evenly to
    !! within a difference of one datum between any two images.
    type(data_partition_t) partition
    type(result_t) result_
    integer my_particles

    associate( me=>this_image() )
      associate( my_first=>partition%first(me), my_last=>partition%last(me) )
        my_particles = my_last - my_first + 1
        associate( ni=>num_images() )
          associate( quotient=>num_particles/ni, remainder=>mod(num_particles,ni)  )
            result_ = assert_equals( quotient + merge(1, 0, me<=remainder), my_particles, "block distribution" )
          end associate
        end associate
      end associate
    end associate
  end function

  function verify_all_particles_partitioned() result(result_)
    !! Verify that the number of particles on each image sums to the
    !! total number of particles distributed.
    type(data_partition_t) partition
    type(result_t) result_
    integer particles

    associate(me => this_image())
      associate( my_first=>partition%first(me), my_last=>partition%last(me) )
        particles = my_last - my_first + 1
        call co_sum(particles)
        result_ = assert_equals(num_particles, particles, "all particles distributed" )
      end associate
    end associate
  end function

 function verify_all_gather_1D_real_array(unused) result(result_)
   type(data_partition_t) partition
   class(input_t), intent(in) :: unused
   type(result_t) result_
   real(real64) :: particle_scalar(num_particles)
   real(real64), parameter :: junk=-12345._real64, expected=1._real64

   associate( no_op => unused) ! eliminate unused-variable warning
   end associate

   associate(me => this_image())
     associate( first=>partition%first(me), last=>partition%last(me) )

       particle_scalar(first:last) = expected !! values to be gathered
       particle_scalar(1:first-1)  = junk !! values to be overwritten by the gather
       particle_scalar(last+1:)  = junk !! values to be overwritten by the gather

       call partition%gather(particle_scalar)

       result_ = assert_that( all(particle_scalar==expected), "real 1D array all-gathered" )

     end associate
   end associate
 end function

 function verify_all_gather_2D_real_array(unused) result(result_)
   class(input_t), intent(in) :: unused
   type(data_partition_t) partition
   type(result_t) result_
   integer, parameter :: vec_space_dim=3
   real(real64) particle_vector(vec_space_dim, num_particles)
   real(real64), parameter :: junk=-12345._real64, expected=1._real64

   associate( no_op => unused) ! eliminate unused-variable warning
   end associate

   associate(me => this_image())
     associate( first=>partition%first(me), last=>partition%last(me) )

       particle_vector(:, first:last) = expected !! values to be gathered
       particle_vector(:, 1:first-1)  = junk !! values to be overwritten by the gather
       particle_vector(:, last+1:)  = junk !! values to be overwritten by the gather

       call partition%gather(particle_vector)

       result_ = assert_that(all(particle_vector==expected), "real 2D array all-gathered implicitly along dimension 1" )

     end associate
   end associate
 end function

 function verify_all_gather_2D_real_array_dim1(unused) result(result_)
   class(input_t), intent(in) :: unused
   type(data_partition_t) partition
   type(result_t) result_
   integer, parameter :: vec_space_dim=3
   real(real64) :: vector_transpose(num_particles, vec_space_dim)
   real(real64), parameter :: junk=-12345._real64, expected=1._real64

   associate( no_op => unused) ! eliminate unused-variable warning
   end associate

   associate(me => this_image())
     associate( first=>partition%first(me), last=>partition%last(me) )

       vector_transpose(first:last, :) = expected !! values to be gathered
       vector_transpose(1:first-1, :)  = junk !! values to be overwritten by the gather
       vector_transpose(last+1:, :)  = junk !! values to be overwritten by the gather

       call partition%gather( vector_transpose, dim=1)

       result_ = assert_that(all(vector_transpose==expected), "vector_transpose gathered explicitly along dimension 1" )

      end associate
    end associate
 end function

 function verify_gather_2D_real_array_dim1(unused) result(result_)
   class(input_t), intent(in) :: unused
   type(data_partition_t) partition
   type(result_t) result_
   integer, parameter :: vec_space_dim=3
   real(real64) :: vector_transpose(num_particles, vec_space_dim)
   real(real64), parameter :: junk=-12345._real64, expected=1._real64

   associate( no_op => unused) ! eliminate unused-variable warning
   end associate

   associate(me => this_image())
     associate( first=>partition%first(me), last=>partition%last(me) )

       vector_transpose(first:last, :) = expected !! values to be gathered
       vector_transpose(1:first-1, :)  = junk !! values to be overwritten by the gather
       vector_transpose(last+1:, :)  = junk !! values to be overwritten by the gather

       call partition%gather( vector_transpose, result_image=gatherer, dim=1)

       if (me==gatherer) then
         result_ = assert_that(all(vector_transpose==expected), "all( particle_vector==expected)")
       else
         result_ = &
           assert_that(all(vector_transpose(1:first-1,:)==junk), "lower transpose data unchanged)") .and. &
           assert_that(all(vector_transpose(first:last,:)==expected), "expected transpose data gathered") .and. &
           assert_that(all(vector_transpose(last+1:,:)==junk), "upper transpose data unchanged)" )
       end if

     end associate
   end associate
 end function

end module data_partition_test
