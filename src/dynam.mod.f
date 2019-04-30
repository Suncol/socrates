! subversion Id for THIS file : $Id: dynam.mod.f 8506 2016-01-28 10:09:58Z simonc $
! $URL: http://svn-ae/ae/COMMON/stand-alone/atmos_models/socrates/tags/07.18.01/src/dynam.mod.f $
!-----------------------------------------------------------------------
      module TROPOPAUSE

      use GRID_DIMS, only : lmax

      integer, parameter :: izmMax = 17 ! level of max tropopause height in izm 

      integer :: izm(lmax)           ! tropopause vertical index
      data izm / 2*9, 2*10, 2*11, 12, 13, 14, 15, 2*16, 11*17, 2*16,
     $           15, 14, 13, 12, 2*11, 2*10, 2*9 /

      end module TROPOPAUSE

!=======================================================================

      module ALLCO

         use GRID_DIMS, only :  lmax, niz

         real, parameter :: dlat = 5.
         integer, dimension(lmax) :: omp_lats = -99
         real, dimension(lmax) :: phi, phir
         real :: zkm(niz), dlatr

      end module ALLCO

!=======================================================================

      module ZGRID

         use GRID_DIMS, only :  lmax, niz

         real, dimension(niz) :: pmb       ! fixed pressure grid, in mb=hPa
         real, dimension(lmax,niz) :: Hair, zgeo

      end module ZGRID
      

!=======================================================================

      module CONC

      use GRID_DIMS, only : lmax, niz
      use SIM_CONTROLS, only : missval

         real, dimension(niz) :: ro_s                ! tot mass dens, kg/m3
         real, dimension(lmax,niz) :: hm2d = missval ! tot nb dens, molec/cm3

      end module CONC

!=======================================================================

      module VEN1

      use GRID_DIMS, only :  lmax, niz
      use SIM_CONTROLS, only : missval

         real, dimension(lmax,niz) :: t2d = missval

      end module VEN1

!=======================================================================

      module VEN2

      use GRID_DIMS, only :  lmax, niz
      use SIM_CONTROLS, only : missval

         real, dimension(lmax,niz) :: u = missval

      end module VEN2

!=======================================================================

      module VEN3

      use GRID_DIMS, only :  lmax, niz

         real, dimension(lmax) :: ff, fl
         real, dimension(lmax,niz) :: ul, uz, uzz
         real, dimension(lmax,niz) :: tl, tll, tz, tzz

      end module VEN3

!=======================================================================

      module VEN5

      use GRID_DIMS, only :  lmax, niz

         real, dimension(lmax,niz) :: cf, fx = 0., ftot = 0.
         real, dimension(lmax,niz) :: f_tropic=0.

      end module VEN5

!=======================================================================

      module VEN6

      use GRID_DIMS, only :  lmax, niz
      use SIM_CONTROLS, only : missval

         real, dimension(lmax,niz) :: x = 0., v = missval, w = missval

      end module VEN6

!=======================================================================

      module VEN7

         use GRID_DIMS, only :  lmax, niz

         real, dimension(lmax,niz) :: xa, xb, xc, xd, xe, xf, xg

      end module VEN7

!=======================================================================

      module VEN8

         use GRID_DIMS, only :  lmax, niz

         real, dimension(niz) :: p             ! p is the pressure grid in Pa
         real, dimension(lmax,niz) :: bv, wd1

      end module VEN8

!=======================================================================

      module VEN9

         use GRID_DIMS, only :  lmax, niz

         real, dimension(lmax,niz) :: xky, xkyl, xkz, xkzz

      end module VEN9

!=======================================================================

      module VEN10

         use GRID_DIMS, only :  lmax, niz

         real, dimension(lmax,niz) :: xtz, xtzz, mu

      end module VEN10

!=======================================================================

      module VEN12

         use GRID_DIMS, only :  lmax, niz

         real, dimension(lmax,niz) :: ftrop, dyytrop, dzztrop

      end module VEN12

!=======================================================================

      module VEN13

         use GRID_DIMS, only :  lmax, niz

         real :: fbnd(lmax,niz), fmvisc(lmax,niz)

      end module VEN13

!=======================================================================

      module VEN14

         use GRID_DIMS, only :  lmax, niz

         real, dimension(lmax,niz) :: t2dqprt = 0., uqprt = 0.
         real, dimension(lmax,niz) :: f_e = 0.

      end module VEN14

!=======================================================================

      module ROSS

         use GRID_DIMS, only :  lmax, niz

         real, dimension(lmax,niz) :: a = 0., ayy = 0., fr = 0.

      end module ROSS

!=======================================================================

      module TIDE1

         use GRID_DIMS, only :  lmax, niz

         real, dimension(lmax,niz) :: dkt = 0., ftx, dktz = 0.

      end module TIDE1

!=======================================================================

      module TRELAX

         use GRID_DIMS, only :  lmax, niz

         real :: trx(lmax,niz)

      end module TRELAX

!=======================================================================

      module ALP

         use GRID_DIMS, only :  lmax, niz

         real :: alpha(niz)

      end module ALP

!=======================================================================

      module UPPER

         use GRID_DIMS, only :  lmax, niz

         real, dimension(lmax) :: wu, xu

      end module UPPER

!=======================================================================

      module GRAVT

         use GRID_DIMS, only :  lmax, niz

         real :: gumc(5,lmax,niz)
         real, dimension(lmax,niz) :: xktgr, xktgrz

      end module GRAVT

!=======================================================================

      module TROPIC

         use GRID_DIMS, only :  lmax, niz

         real :: clh(lmax,niz)

      end module TROPIC

!=======================================================================

      module TEMP_HIST

         use GRID_DIMS, only :  lmax, niz

	 integer :: perm(3) = (/ 1, 2, 3 /)
	 real    :: this(lmax,niz,3)

      end module TEMP_HIST
