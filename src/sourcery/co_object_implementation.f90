!
!     (c) 2019-2020 Guide Star Engineering, LLC
!     This Software was developed for the US Nuclear Regulatory Commission (US NRC) under contract
!     "Multi-Dimensional Physics Implementation into Fuel Analysis under Steady-state and Transients (FAST)",
!     contract # NRC-HQ-60-17-C-0007
!
submodule(sourcery_co_object_m) co_object_s
  implicit none

contains

    module procedure mark_as_defined
      self%defined=.true.
    end procedure

    module procedure user_defined
      is_defined = self%defined
    end procedure

end submodule
