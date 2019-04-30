! subversion Id for THIS file : $Id: wave2.mod.f 8506 2016-01-28 10:09:58Z simonc $
! $URL: http://svn-ae/ae/COMMON/stand-alone/atmos_models/socrates/tags/07.18.01/src/wave2.mod.f $
!-----------------------------------------------------------------------
      module WAVE_PARMS
           integer, parameter :: Nwaves = 2

!!         integer, parameter :: ny = 35, nz = 53  ! bottom at 16 km
!!         integer, parameter :: ny = 35, nz = 56  ! bottom at 10 km
         integer, parameter :: ny = 35, nz = 58  ! bottom at 6 km
      end module WAVE_PARMS


      module BOUND1
         use WAVE_PARMS, only : ny
         complex :: bdy(ny)
      end module BOUND1


      module DAMPS
         use WAVE_PARMS, only : nz
         real, dimension(nz) :: ac, rfr, dacdz
      end module DAMPS


      module DISSPA
         use WAVE_PARMS
         real, dimension(ny,nz) :: deltmax, phspd, ampl = 0.
         real, dimension(ny,nz,Nwaves) :: delta = 0.
      end module DISSPA


      module GEOM
         use WAVE_PARMS
         real am
         real, dimension(nz) :: z, exzhalf
         real, dimension(ny) :: alat, coslat, sinlat,
     $                          tanlat, f,
     $                          sinsq, cossq, sincos, 
     $                          dmterm, aecosi, sinsqi, cossqi
      end module GEOM


      module TRNS
         use WAVE_PARMS
         complex :: tr(ny,nz,Nwaves)
      end module TRNS


      module WAV_AMP
         use GRID_DIMS, only : lmax, niz
         real, dimension(lmax,niz) :: ampl1, ampl2
      end module WAV_AMP


      module WBASIC
         use WAVE_PARMS
         complex, dimension(ny,nz) :: alpha, beta, dalphz
      end module WBASIC


      module WCONST
         use WAVE_PARMS
         real :: delt
         real, parameter :: ae = 6.37e06
         real, parameter :: aesq = ae**2
         real, parameter :: conv = 3.14159/180.
         real, parameter :: hs = 7.e03
         real, parameter :: omega = 7.292e-05
         real, parameter :: r = 287.
         real, parameter :: g = 9.80665
!!         real, parameter :: bottom = 6.0e03
         real, parameter :: bottom = (121. - nz*2. + 1.) * 1.e3
         real, parameter :: dz = 2.e3
         real, parameter :: dzsq = dz**2
         real, parameter :: dtheta = 5.*conv
         real, parameter :: dthsq = dtheta**2
         real, parameter :: bvsq = (.02)**2
         real, parameter :: ss = bvsq / (2.*omega*ae)**2
         complex, parameter :: aim = (0.,1.)
      end module WCONST


      module WROSS
         use WAVE_PARMS
         real, dimension(ny,nz) ::  wfr, wdyy
      end module WROSS


      module WUCOEF
         use WAVE_PARMS
         complex, dimension(ny,nz) :: aa, bb, cc, dd, qq
      end module WUCOEF


      module WVEN2
         use WAVE_PARMS
         real, dimension(ny,nz) :: uzonal, dudy, dudz, dqdy
      end module WVEN2


      module WW_VARS
         use WAVE_PARMS
         complex, dimension(ny,nz) :: pw = (1.e-15,1.e-15)
         complex, dimension(ny,nz,Nwaves) :: pwm
      end module WW_VARS
