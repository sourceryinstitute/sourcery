!
!     (c) 2019-2020 Guide Star Engineering, LLC
!     This Software was developed for the US Nuclear Regulatory Commission (US NRC) under contract
!     "Multi-Dimensional Physics Implementation into Fuel Analysis under Steady-state and Transients (FAST)",
!     contract # NRC-HQ-60-17-C-0007
!
submodule(assertions_interface) assertions_implementation
  implicit none

contains

  module procedure assert
    use iso_fortran_env, only : error_unit
    use string_functions_interface, only : string
    use object_interface, only : object_t

    character(len=:), allocatable :: header, trailer
    integer, parameter :: max_this_image_digits=9

    if (assertions) then

      if (.not. assertion) then

        associate(assertion_failed_on => 'Assertion "' // description // '" failed on image')
          header = repeat(" ", ncopies = len(assertion_failed_on) + max_this_image_digits)
          write(header, *) assertion_failed_on, this_image()
        end associate

        if (.not. present(diagnostic_data)) then

          trailer = ""

        else

          block
            character(len=*), parameter :: prefix = "with diagnostic data"
            integer, parameter :: max_data_length = 1024

            select type(diagnostic_data)
              type is(character(len=*))
                trailer =  prefix // diagnostic_data
              type is(integer)
                trailer = prefix // string(diagnostic_data)
              class is(object_t)
                trailer = repeat(" ", ncopies =  max_data_length)
                write(trailer,*) diagnostic_data
              class default
                trailer = prefix // 'of unsupported type'
            end select
          end block

        end if

        error stop header // trim(trailer)

      end if

    end if

  end procedure

end submodule
