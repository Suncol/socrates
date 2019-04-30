
      module SIM_CONTROLS

      real, parameter :: missval = -1.e29  ! the missing value
      character(len=16) :: model_name      ! Global attribute for NetCDF archive(s)
      character(len=16) :: label_short     ! Global attribute for NetCDF archive(s)
      character(len=64) :: label_long      ! Global attribute for NetCDF archive(s)

      end module SIM_CONTROLS

!=======================================================================

      module GRID_DIMS           ! model dimensions in lat/alt

      implicit none

      integer, parameter :: lmax = 35
      integer, parameter :: niz = 121

      end module GRID_DIMS


!=======================================================================

      module ALLCO
      
      use GRID_DIMS, only : lmax, niz
      
      real, dimension(lmax) :: phi
      real, dimension(niz) :: zkm
      
      end module ALLCO

!=======================================================================

      module TYPE_DEF
      
      use GRID_DIMS, only : lmax, niz

      type TIMING
         integer :: year, month, day, days0
         real ::    cal_day
      end type TIMING

      type VAR2D_TYPE
         character(len=21) :: name
         character(len=12) :: units
         real, dimension(lmax,niz) :: vals
      end type VAR2D_TYPE

      end module TYPE_DEF

!=======================================================================

      module ARCH_WHAT

      integer, parameter :: max_nldesc = 50
                     
      character(len=4)  :: vdims(3) = (/ 'lat ', 'lev ', 'time' /)
      integer :: nldesc                                     ! set in...
      character(len=80), dimension(max_nldesc) :: desc_text !   DESCRIBE_SIM

      end module ARCH_WHAT

!=======================================================================
