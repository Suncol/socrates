! subversion Id for THIS file : $Id: sun_chem.f 8506 2016-01-28 10:09:58Z simonc $
! $URL: http://svn-ae/ae/COMMON/stand-alone/atmos_models/socrates/tags/07.18.01/src/sun_chem.f $
!-----------------------------------------------------------------------

      subroutine SUN_CHEM( lat, slt, cal_day, phid, 
     $                     sza, sunlight, scsza )
!-----------------------------------------------------------------------
!    ... Calculate the Solar Zenith Angle 'sza', the alt-dependent boolean
!        'sunlight' & the secant/Chapman 'scsza' for CHEM module.
!        Extracted from SETPHT
!                                v7s04 - simonc@oma.be, March 2002
!-----------------------------------------------------------------------
      use GRID_DIMS, only : lmax, niz
      use PHYS_CST, only : d2r
      use ZGRID, only : Hair, zgeo
      use SIM_CONTROLS, only : missval 
      use DIAG_CONTROLS, only : debug, diags, ldiag, zdiag
      use SUN_UTILS, only : SUN_COSZA, SUN_CHAPMAN, SUN_LIGHT

      implicit none      

!-----------------------------------------------------------------------
!    ... Dummy args
!-----------------------------------------------------------------------
      integer, intent(in) :: lat        ! latitude index
      real, intent(in)    :: slt        ! ... of current timestep (hours)
      real, intent(in)    :: cal_day    ! day of year, 1.->365. or more
      real, intent(in)    :: phid       ! latitude (degrees)
      real, intent(out)   :: sza        ! solar zenith angle (degrees)
      logical, intent(out), dimension(niz)     :: sunlight
      real, intent(out), dimension(niz)        :: scsza ! secant/chapman of sza

!-----------------------------------------------------------------------
!    ... Local variables
!-----------------------------------------------------------------------
      integer :: k, iz, ier, i
      real :: cosza
      
      scsza(:) = missval
      ier = 0

!-----------------------------------------------------------------------
!    ... Compute cosine of Solar zenith angle
!-----------------------------------------------------------------------
      cosza = SUN_COSZA( phid, cal_day, slt )
      sza = ACOS( cosza ) / d2r
      
      zloop: do iz = 1, niz

!-----------------------------------------------------------------------
!    ... Day/night tests
!-----------------------------------------------------------------------
         sunlight(iz) = SUN_LIGHT( phid, zgeo(lat,iz), cal_day, slt )
         if( .not. sunlight(iz) ) cycle zloop

!-----------------------------------------------------------------------
!    ... Compute secant/Chapman of SZA (scsza)
!-----------------------------------------------------------------------
         scsza(iz) = SUN_CHAPMAN(zgeo(lat,iz), Hair(lat,iz), cosza, ier)
         if( ier /= 0 ) exit zloop   ! SUN_CHAPMAN fails -> ier=1,2 or 3

      end do zloop

!-----------------------------------------------------------------------
!    ... Everything below is diagnostics and error reporting
!-----------------------------------------------------------------------
      if( ( diags .and. lat==ldiag ) .or. ( ier /= 0 ) ) then
         if( ier == 0 ) then
            iz = zdiag
          else
            write(*,*)' !!!! SUN_CHEM ERROR: ier= ',ier
         end if
         write(*,'(2(a,i3),4(a,f8.4))')'SUN_CHEM @ (',lat,',',iz,
     $      ') ; cal_day= ',cal_day,' ; slt= ',slt,' ; sza= ',sza
         write(*,*)'   sunlight= ',sunlight(iz)
!         if( sunlight(iz) ) 
         write(*,'(2(a,f12.6),2(a,es12.5))') 
     $    '    zgeo= ',zgeo(lat,iz),' ; Hair= ',Hair(lat,iz),
     $    ' ; cosza= ',cosza,' ; scsza= ',scsza(iz)
         if( ier /= 0 .and. debug ) stop 'SUN_CHEM: Fatal Error' 
      end if

      end subroutine SUN_CHEM
