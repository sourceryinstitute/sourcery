module sourcery_data_partition_m
  !! distribute data identification numbers across images such that the number of
  !! items differs by at most 1 between any two images.
  use iso_fortran_env, only : real32, real64
  use sourcery_bin_m, only : bin_t
  implicit none

  private
  public :: data_partition_t

  type data_partition_t
    !! encapsulate a description of the data subset the executing image owns
    private
    type(bin_t), allocatable :: bin(:)
  contains
    procedure :: define_partitions
    procedure :: first
    procedure :: last
    procedure, private :: gather_real32_2D_array, gather_real64_2D_array,  gather_real32_1D_array, gather_real64_1D_array
    generic :: gather => gather_real32_2D_array, gather_real64_2D_array,  gather_real32_1D_array, gather_real64_1D_array
  end type

  interface data_partition_t

    pure module function construct(cardinality) result(data_partition)
      implicit none
      type(data_partition_t) data_partition
      integer, intent(in) :: cardinality
    end function

  end interface

  interface

    pure module subroutine define_partitions(self, cardinality)
      !! define the range of data identification numbers owned by the executing image
      implicit none
      class(data_partition_t), intent(inout) :: self
      integer, intent(in) :: cardinality
    end subroutine

    pure module function first(self, image_number) result(first_index)
      !! the result is the first identification number owned by the executing image
      implicit none
      class(data_partition_t), intent(in) :: self
      integer, intent(in), optional :: image_number
      integer first_index
    end function

    pure module function last(self, image_number) result(last_index)
      !! the result is the last identification number owned by the executing image
      implicit none
      class(data_partition_t), intent(in) :: self
      integer, intent(in), optional :: image_number
      integer last_index
    end function

    !! Gathers are inherently expensive and are best used either
    !! 1. Near the beginning/end of execution to amortize costs across an entire run or
    !! 2. Temporarily while developing/debugging code.

    module subroutine gather_real32_1D_array(self, a, result_image, dim )
      !! Gather the elements of an 1D array distributed along dimension dim onto result_image
      implicit none
      class(data_partition_t), intent(in) :: self
      real(real32), intent(inout) :: a(:)
      integer, intent(in), optional :: result_image
      integer, intent(in), optional :: dim
    end subroutine

    module subroutine gather_real64_1D_array(self, a, result_image, dim )
      !! Gather the elements of an 1D array distributed along dimension dim onto result_image
      implicit none
      class(data_partition_t), intent(in) :: self
      real(real64), intent(inout) :: a(:)
      integer, intent(in), optional :: result_image
      integer, intent(in), optional :: dim
    end subroutine

    module subroutine gather_real32_2D_array(self, a, result_image, dim )
      !! Gather the elements of an 2D array distributed along dimension dim onto result_image
      implicit none
      class(data_partition_t), intent(in) :: self
      real(real32), intent(inout) :: a(:,:)
      integer, intent(in), optional :: result_image
      integer, intent(in), optional :: dim
    end subroutine

    module subroutine gather_real64_2D_array(self, a, result_image, dim )
      !! Gather the elements of an 2D array distributed along dimension dim onto result_image
      implicit none
      class(data_partition_t), intent(in) :: self
      real(real64), intent(inout) :: a(:,:)
      integer, intent(in), optional :: result_image
      integer, intent(in), optional :: dim
    end subroutine

  end interface

end module sourcery_data_partition_m
