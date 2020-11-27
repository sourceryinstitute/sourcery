!
!     (c) 2019-2020 Guide Star Engineering, LLC
!     This Software was developed for the US Nuclear Regulatory Commission (US NRC) under contract
!     "Multi-Dimensional Physics Implementation into Fuel Analysis under Steady-state and Transients (FAST)",
!     contract # NRC-HQ-60-17-C-0007
!
#ifndef USE_ASSERTIONS
# define USE_ASSERTIONS .true.
#endif
module assertions_interface
  !! summary: Utility for runtime checking of logical assertions.
  !! usage: error-terminate if the assertion fails:
  !!
  !!    use assertions_interface, only : assert
  !!    call assert( 2 > 1, "2 > 1")
  !!
  !! Turn off assertions in production code by setting USE_ASSERTIONS to .false. via the preprocessor:
  !!
  !!    caf -cpp -DUSE_ASSERTIONS=.false. -c assertions_interface.f90
  !!
  !! Doing so may eliminate any associated runtime overhead by enabling optimizing compilers to ignore
  !! the assertion procedure body during a dead-code-removal phase of optimization.
  implicit none
  private
  public :: assert, assertions, max_errmsg_len

  logical, parameter :: assertions=USE_ASSERTIONS
  integer, parameter :: max_errmsg_len = len( &
  "warning (183): FASTMEM allocation is requested but the libmemkind library is not linked in, so using the default allocator.")
  !! longest Intel compiler error message (see https://intel.ly/35x84yr).

  interface

    elemental module subroutine assert(assertion, description, diagnostic_data)
      !! If assertion is .false., error-terminate with optional, variable stop code containing diagnostic_data
      implicit none
      logical, intent(in) :: assertion
        !! Most assertions will be expressions, e.g., call assert( i>0, "positive i")
      character(len=*), intent(in) :: description
        !! Brief statement of what is being asserted
      class(*), intent(in), optional :: diagnostic_data
        !! Optional error stop code, which may be of intrinsic type or object class
    end subroutine

  end interface

end module
