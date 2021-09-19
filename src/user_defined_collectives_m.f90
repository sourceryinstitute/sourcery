!
!     (c) 2019-2020 Guide Star Engineering, LLC
!     This Software was developed for the US Nuclear Regulatory Commission (US NRC) under contract
!     "Multi-Dimensional Physics Implementation into Fuel Analysis under Steady-state and Transients (FAST)",
!     contract # NRC-HQ-60-17-C-0007
!
module user_defined_collectives_m
  !! author: Damian Rouson
  !! 
  !! This module contains user-defined collective subroutines.
  implicit none

  interface

    module subroutine co_all(boolean)
      !! If any image in a team calls this subroutine, then this procedure must
      !! in the corresponding team.  The subroutine sets the "boolean" argument
      !! .true. if it is true in all participating images upon entry and .false.
      !! otherwise.
      implicit none
      logical, intent(inout) :: boolean
    end subroutine

  end interface

end module user_defined_collectives_m
