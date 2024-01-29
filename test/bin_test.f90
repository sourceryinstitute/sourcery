module bin_test_m
  !! verify data partitioning across bins
  use sourcery_m, only : bin_t, test_t, test_result_t
  use assert_m, only : assert
  implicit none

  private
  public :: bin_test_t

  type, extends(test_t) :: bin_test_t
  contains
    procedure, nopass :: subject
    procedure, nopass :: results
  end type

contains

  pure function subject() result(specimen)
    character(len=:), allocatable :: specimen
    specimen = "An array of bin_t objects (bins)" 
  end function

  function results() result(test_results)
    type(test_result_t), allocatable :: test_results(:)
    character(len=*), parameter :: longest_description = &
          "partitioning all item across all bins without item loss"

    associate( &
      descriptions => &
        [ character(len=len(longest_description)) :: &
          "partitioning items nearly evenly across bins", &
          "partitioning all item across all bins without item loss", &
          "partitioning items such that each bin only has one works", &
          "partitioning items such that some bins only have one works" &
        ], &
      outcomes => &
         [ verify_block_partitioning(), &
           verify_all_items_partitioned(), &
           verify_all_singleton_bins_are_ok(), &
           verify_some_singleton_bins_are_ok() &
         ] &
     )
       call assert(size(descriptions) == size(outcomes), "bin_test_m(results): size(descriptions) == size(outcomes)")
       test_results = test_result_t(descriptions, outcomes)
     end associate

  end function

  function verify_block_partitioning() result(test_passes)
    !! Verify that the items are partitioned across bins evenly to within a difference of one item per bin
    logical test_passes

    type(bin_t), allocatable :: bins(:)
    integer, parameter :: n_items=11, n_bins=7
    integer b

    bins = [( bin_t(num_items=n_items, num_bins=n_bins, bin_number=b), b = 1,n_bins )]
    associate(in_bin => [(bins(b)%last() - bins(b)%first() + 1, b = 1, n_bins)])
      associate(remainder => mod(n_items, n_bins), items_per_bin => n_items/n_bins)
        test_passes = all([(in_bin(1:remainder) == items_per_bin + 1)]) .and. all([(in_bin(remainder+1:) == items_per_bin)])
      end associate
    end associate

  end function

  function verify_all_items_partitioned() result(test_passes)
    !! Verify that the number of items in each bin sums to the total number of items
    type(bin_t) partition
    logical test_passes

    type(bin_t), allocatable :: bins(:)
    integer, parameter :: n_items=11, n_bins=6
    integer b

    bins = [( bin_t(num_items=n_items, num_bins=n_bins, bin_number=b), b = 1,n_bins )]
    test_passes = sum([(bins(b)%last() - bins(b)%first() + 1, b = 1, n_bins)]) == n_items

  end function

  function verify_all_singleton_bins_are_ok() result(test_passes)
    !! Verify that all singleton bins can be created and that the number of items equals the number of bins
    type(bin_t) partition
    logical test_passes

    type(bin_t), allocatable :: bins(:)
    integer, parameter :: n_items=11, n_bins=11
    integer b

    bins = [( bin_t(num_items=n_items, num_bins=n_bins, bin_number=b), b = 1,n_bins )]
    test_passes = sum([(bins(b)%last() - bins(b)%first() + 1, b = 1, n_bins)]) == n_items

  end function

  function verify_some_singleton_bins_are_ok() result(test_passes)
    !! Verify that some singleton bins can be created and that the number of items equals the number of bins
    type(bin_t) partition
    logical test_passes

    type(bin_t), allocatable :: bins(:), more_bins(:)
    integer, parameter :: n_bins=11, n_items=(n_bins + (n_bins/2))
    integer b

    bins = [( bin_t(num_items=n_items, num_bins=n_bins, bin_number=b), b = 1,n_bins )]
    test_passes = sum([(bins(b)%last() - bins(b)%first() + 1, b = 1, n_bins)]) == n_items

    more_bins = [( bin_t(num_items=n_items, num_bins=n_bins, bin_number=b), b = 1,n_bins )]
    associate(in_bin => [(more_bins(b)%last() - more_bins(b)%first() + 1, b = 1, n_bins)])
      associate(remainder => mod(n_items, n_bins), items_per_bin => n_items/n_bins)
        test_passes = test_passes .and. &
          all([(in_bin(1:remainder) == items_per_bin + 1)]) .and. all([(in_bin(remainder+1:) == items_per_bin)])
      end associate
    end associate

  end function
end module bin_test_m
