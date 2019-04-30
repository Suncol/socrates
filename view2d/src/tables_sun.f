! subversion Id for THIS file : $Id: tables_sun.f 8506 2016-01-28 10:09:58Z simonc $
! $URL: http://svn-ae/ae/COMMON/stand-alone/atmos_models/socrates/tags/07.18.01/src/tables_sun.f $
!-----------------------------------------------------------------------

      subroutine TABLES_SUN( cal_day, lat, polar_night, scsza )
!-----------------------------------------------------------------------
!    	... Calculates secant/Chapman of solar zenith angles in a table.
!           See PHO_PARMS (pho.mod.f): for nd2=2, ki=1,nk corresponds to
!   ki =        1     2     3     4     5       6
!                          |--nd2--|
!                    |-------njpd--------|
!              |--------------nk-----------------|
!       ... where the vals go from a sun close to zenith to sun below horizon:
!   ki= 1: fictitious value for sun higher than highest sun on that day
!   ki= 2: value for highest sun on that day, i.e. at noon
!   ki= 3: value for 1st intermediate sun position (closer to noon)
!   ki= 4: val for last intermediate sun pos (closer to lowest visible sun)
!   ki= 5: val for lowest visible sun, i.e. sunrise *or* midnight if polar day
!   ki= 6: fictitious val for sun lower than lowest visible sun, i.e. at night
!                                                 simonc@oma.be, March 2002
!-----------------------------------------------------------------------
      use PHYS_CST, only : pi, d2r
      use GRID_DIMS, only : niz
      use PHO_PARMS, only : nd2, njpd, nk    ! njpd=nd2+2, nk=njpd+2
      use ALLCO, only : phi, phir
      use ZGRID, only : zgeo, Hair
      use SIM_CONTROLS, only : missval
      use DIAG_CONTROLS, only : debug, diags, ldiag, zdiag
      use SUN_UTILS, only : SUN_DAYLENGTH, SUN_CHAPMAN, SUN_DECLIN
      
      implicit none
       
!-----------------------------------------------------------------------
!	... Dummy args
!-----------------------------------------------------------------------
      real, intent(in)  :: cal_day      ! day of year, 1.->365. or more
      integer, intent(in)  :: lat       ! latitude index
      logical, intent(out), dimension(niz) :: polar_night
      real, intent(out), dimension(niz,nk) :: scsza

!-----------------------------------------------------------------------
!	... Local variables
!-----------------------------------------------------------------------
      integer :: iz, ki, ier = 0
      real :: phid, td, cosza_losun, cosza_hisun, h, hh, cosza, declinr
      real, dimension(nk) :: sza          ! for diags

      ier = 0
      sza(:) = missval
      phid = phi(lat)
      declinr = SUN_DECLIN( cal_day ) * d2r
      
      zloop: do iz = 1, niz
         td = SUN_DAYLENGTH( zgeo(lat,iz), phid, cal_day, 
     $                       cosza_losun, cosza_hisun )

!-----------------------------------------------------------------------
!	... Polar night case
!-----------------------------------------------------------------------
         polar_night(iz) = .false.
         if( td == 0. ) then
            polar_night(iz) = .true.
            scsza(iz,1) = 1.e12
            do ki = 2, nk
               scsza(iz,ki) = 10. * scsza(iz,ki-1)
            end do
            cycle zloop
         end if

!-----------------------------------------------------------------------
!	... Highest sun (ki=2, noon)
!-----------------------------------------------------------------------
         sza(1+1) = ACOS( cosza_hisun ) / d2r
         scsza(iz,1+1) = SUN_CHAPMAN( zgeo(lat,iz), Hair(lat,iz), 
     $                                cosza_hisun, ier )
         if( ier /= 0 ) then
            write(*,*) 'TABLES_SUN: error calling SUN_CHAPMAN at hisun'
            goto 100
         end if

!-----------------------------------------------------------------------
!	... Intermediate positions of the sun
!-----------------------------------------------------------------------
         h = pi * td / 24.
         do ki = 2+1, 2+nd2
            hh = h * REAL(1+2*(ki-3)) / REAL(2*nd2)    ! if nd2=2 -> hh=h/4,3h/4
            cosza = SIN(phir(lat)) * SIN(declinr) 
     $            + COS(phir(lat)) * COS(declinr) * COS( hh )
            sza(ki) = ACOS( cosza ) / d2r
            scsza(iz,ki) = SUN_CHAPMAN( zgeo(lat,iz), Hair(lat,iz), 
     $                                  cosza, ier )
            if( ier /= 0 ) then
               write(*,*) 'TABLES_SUN: error calling SUN_CHAPMAN,ki=',ki
               goto 100
            end if
         end do

!-----------------------------------------------------------------------
!       ... Lowest visible sun ( ki=1+njpd, sunrise/sunset *or* 
!                               midnight if in polar day)
!-----------------------------------------------------------------------
         sza(1+njpd) = ACOS( cosza_losun ) / d2r
         scsza(iz,1+njpd) = SUN_CHAPMAN( zgeo(lat,iz), Hair(lat,iz), 
     $                                   cosza_losun, ier )
         if( ier /= 0 ) then
            write(*,*) 'TABLES_SUN: error calling SUN_CHAPMAN at losun'
            goto 100
         end if

!-----------------------------------------------------------------------
!	... fictitious small value for sun above its noon position (ki=1)
!	... fictitious big value for sun below its lowest position (ki=nk)
!-----------------------------------------------------------------------
         scsza(iz,1) = 0.1 * scsza(iz,2)
         scsza(iz,nk) = 10. * scsza(iz,1+njpd)
      
!-----------------------------------------------------------------------
!	... Diagnostics and error reporting
!-----------------------------------------------------------------------
  100    if( ier/=0 .or. (diags .and. lat==ldiag .and. iz==zdiag) ) then
            if( ier /= 0 ) write(*,*)'!!!!! TABLES_SUN ERROR: ier= ',ier
            write(*,'(a,f6.1)') 'TABLES_SUN, cal_day= ',cal_day
            write(*,'(2(a,i3),2(a,f8.5))') '   lat= ',lat,' ; iz= ',iz,
     $           ' ; td = ',td
            write(*,'(3(a,f12.6))')'   phid= ',phid,' ; zgeo= ',
     $              zgeo(lat,iz),' ; Hair= ',Hair(lat,iz)
            write(*,*) '   polar_night= ',polar_night(iz)
            do ki = 1, nk
               write(*,'(a,i2,a,f12.7,a,es12.5)')'  ki= ',ki,' ; sza= ',
     $                          sza(ki),' ; scsza= ',scsza(iz,ki)
            end do
            if( ier /= 0 .and. debug ) stop 'TABLES_SUN: fatal error'
         end if

      end do zloop

      end subroutine TABLES_SUN
