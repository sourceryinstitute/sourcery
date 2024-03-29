submodule(sourcery_data_partition_m) sourcery_data_partition_s
  use assert_m, only : assert
  implicit none

  logical, parameter :: verbose=.false.

contains

  module procedure define_partitions
    integer image
    associate(ni => num_images())
      self%bin = [( bin_t(num_items=cardinality, num_bins=ni, bin_number=image), image=1,ni )]
    end associate
  end procedure

  module procedure construct
    call data_partition%define_partitions(cardinality)
  end procedure

  module procedure first
    integer image

    call assert( allocated(self%bin), "data_partition_s(first): allocated(self%bin)")

    if (present(image_number)) then
      image = image_number
    else
      image = this_image()
    end if
    first_index = self%bin(image)%first()
  end procedure

  module procedure last
    integer image

    call assert( allocated(self%bin), "data_partition_s(last): allocated(self%in)")

    if (present(image_number)) then
      image = image_number
    else
      image = this_image()
    end if
    last_index = self%bin(image)%last()
  end procedure

  module procedure gather_real32_1D_array

    if (present(dim)) call assert (dim==1, "dimensioned partitioned == 1")

    associate( me => this_image() )
      if (verbose) then
        write(6,*) 'gather_real_1D_array(): executing on image', me
        flush(6)
      end if
      associate(first=>self%first(me), last=>self%last(me))
        if (.not. present(result_image)) then
          a(1:first-1)  = 0.
          a(last+1:)  = 0.
          call co_sum(a)
        else
          block
            real(real32), allocatable, dimension(:) :: a_lower, a_upper
            a_lower = a(1:first-1)
            a_upper = a(last+1:)
            a(1:first-1)  = 0.
            a(last+1:)  = 0.
            call co_sum(a, result_image=result_image)
            if (result_image /= me) then
              a(1:first-1) = a_lower
              a(last+1:) = a_upper
            end if
          end block
        end if
      end associate
    end associate
  end procedure

  module procedure gather_real64_1D_array

    if (present(dim)) call assert (dim==1, "dimensioned partitioned == 1")

    associate( me => this_image() )
      if (verbose) then
        write(6,*) 'gather_real_1D_array(): executing on image', me
        flush(6)
      end if
      associate(first=>self%first(me), last=>self%last(me))
        if (.not. present(result_image)) then
          a(1:first-1)  = 0.
          a(last+1:)  = 0.
          call co_sum(a)
        else
          block
            real(real64), allocatable, dimension(:) :: a_lower, a_upper
            a_lower = a(1:first-1)
            a_upper = a(last+1:)
            a(1:first-1)  = 0.
            a(last+1:)  = 0.
            call co_sum(a, result_image=result_image)
            if (result_image /= me) then
              a(1:first-1) = a_lower
              a(last+1:) = a_upper
            end if
          end block
        end if
      end associate
    end associate
  end procedure

  module procedure gather_real32_2D_array

    integer dim_
    if (present(dim)) then
      dim_ = dim
    else
      dim_ = 2
    end if

    associate( me => this_image() )
      if (verbose) then
        write(6,*) 'gather_real32_2D_array(): executing on image', me
        flush(6)
      end if
      associate(first=>self%first(me), last=>self%last(me))
        if (.not. present(result_image)) then
          select case(dim_)
            case(1)
              a(1:first-1, :) = 0.
              a(last+1:, :) = 0.
            case(2)
              a(:, 1:first-1) = 0.
              a(:, last+1:) = 0.
            case default
              error stop "gather_real32_2D_array: invalid dim argument"
          end select
          call co_sum(a)
        else
          block
            real(real32), allocatable, dimension(:,:) :: a_lower, a_upper
            select case(dim_)
              case(1)
                a_lower = a(1:first-1, :)
                a_upper = a(last+1:, :)
                a(1:first-1, :) = 0.
                a(last+1:, :) = 0.
              case(2)
                a_lower = a(:, 1:first-1)
                a_upper = a(:, last+1:)
                a(:, 1:first-1) = 0.
                a(:, last+1:) = 0.
              case default
                error stop "gather_real32_2D_array: invalid dim argument"
            end select

            call co_sum(a, result_image=result_image)

            if (result_image /= me) then
              select case(dim_)
                case(1)
                  a(1:first-1, :) = a_lower
                  a(last+1:, :) = a_upper
                case(2)
                  a(:, 1:first-1) = a_lower
                  a(:, last+1:) = a_upper
                case default
                  error stop "gather_real32_2D_array: invalid dim argument"
              end select
            end if
          end block
        end if
      end associate
    end associate
  end procedure

  module procedure gather_real64_2D_array

    integer dim_
    if (present(dim)) then
      dim_ = dim
    else
      dim_ = 2
    end if

    associate( me => this_image() )
      if (verbose) then
        write(6,*) 'gather_real64_2D_array(): executing on image', me
        flush(6)
      end if
      associate(first => self%first(me), last => self%last(me))
        if (.not. present(result_image)) then
          select case(dim_)
            case(1)
              a(1:first-1, :) = 0.
              a(last+1:, :) = 0.
            case(2)
              a(:, 1:first-1) = 0.
              a(:, last+1:) = 0.
            case default
              error stop "gather_real64_2D_array: invalid dim argument"
          end select
          call co_sum(a)
        else
          block
            real(real64), allocatable, dimension(:,:) :: a_lower, a_upper
            select case(dim_)
              case(1)
                a_lower = a(1:first-1, :)
                a_upper = a(last+1:, :)
                a(1:first-1, :) = 0.
                a(last+1:, :) = 0.
              case(2)
                a_lower = a(:, 1:first-1)
                a_upper = a(:, last+1:)
                a(:, 1:first-1) = 0.
                a(:, last+1:) = 0.
              case default
                error stop "gather_real64_2D_array: invalid dim argument"
            end select

            call co_sum(a, result_image=result_image)

            if (result_image /= me) then
              select case(dim_)
                case(1)
                  a(1:first-1, :) = a_lower
                  a(last+1:, :) = a_upper
                case(2)
                  a(:, 1:first-1) = a_lower
                  a(:, last+1:) = a_upper
                case default
                  error stop "gather_real64_2D_array: invalid dim argument"
              end select
            end if
          end block
        end if
      end associate
    end associate
  end procedure

end submodule sourcery_data_partition_s
