! subversion Id for THIS file : $Id: heat.mod.f 8506 2016-01-28 10:09:58Z simonc $
! $URL: http://svn-ae/ae/COMMON/stand-alone/atmos_models/socrates/tags/07.18.01/src/heat.mod.f $
!-----------------------------------------------------------------------

      module TAU0  ! qsum to archive only, qcorr is diags for now

      use GRID_DIMS, only : lmax, niz
      use SIM_CONTROLS, only : missval
 
         real, dimension(lmax,niz) :: q = 0., qsum = 0., qcorr

      end module TAU0

!=======================================================================


      module TAU1

      use GRID_DIMS, only : lmax, niz
      use SIM_CONTROLS, only : missval
 
         real, dimension(lmax,niz) :: pch = missval   ! Total solar heating (K/day)

      end module TAU1

!=======================================================================

      module TAU3

      use GRID_DIMS, only : lmax, niz
      use SIM_CONTROLS, only : missval
 
         real, dimension(lmax,niz) :: qcool = missval ! Total IR cooling (K/day)

      end module TAU3

!=======================================================================

      module TAU4

      use GRID_DIMS, only : lmax, niz
      use SIM_CONTROLS, only : missval
 
         real, dimension(lmax,niz) :: 
     $                 q2 = missval   ! H2O radiative cooling at ?? um (K/day)
     $,                q3 = missval   ! O3 radiative cooling at 9.6 um (K/day)
     $,                q4 = missval   ! CO2 radiative cooling at 15 um (K/day)
     $,                q5 = missval   ! NO radiative cooling at 5.3 um (K/day)

      end module TAU4
      
!=======================================================================

      module HEAT_TERMS
      
      use GRID_DIMS, only : lmax, niz
      use SIM_CONTROLS, only : missval
 
         integer, parameter :: nch = 7    ! number of exothermic chem reactions
         integer, parameter :: nsh = 3    ! number of solar heating processes
         
         real, dimension(lmax,niz,nsh) :: solheat  = missval       ! K/day
         real, dimension(lmax,niz,nch) :: chemheat = missval       ! K/day

         character(len=6), dimension(nsh) ::  ! check order in HEAT_RATES
     $      heatsol  = (/ 'sol-o2', 'sol-o3', 'q-aero' /)
         character(len=6), dimension(nch) ::  ! check order in HEAT_RATES
     $      heatreac = (/ ' O-O-M', 'O-O2-M', '  O-O3', '  H-O3', 
     $                    '  O-OH', ' O-HO2', 'H-O2-M' /)

         real, dimension(lmax,niz) :: 
     $           dh = missval     ! divergence of eddy diffusive flux of heat (K/s)
     $,          dxt = missval    ! divergence of molecular diffusive flux of heat (K/s)
     $,          gwheat = 0.      ! Heating by Gravity Wave breaking (HINES97) (K/day)

      end module HEAT_TERMS

!=======================================================================

      module SOLTEST

      use GRID_DIMS, only : lmax, niz
      use SIM_CONTROLS, only : missval
 
         real, dimension(lmax,niz) :: 
     $             srheat1 = missval        ! Heat, O3 abs solar UV (K/day)
     $,            srheat2 = missval        ! Heat, O2+aero abs solar UV (K/day)
     $,            srheat3 = missval        ! total chemical heating (K/day)

      end module SOLTEST

!=======================================================================

      module AEROR

      use GRID_DIMS, only : lmax, niz
 
         real :: aerorad(niz,lmax) = 0.

      end module AEROR

