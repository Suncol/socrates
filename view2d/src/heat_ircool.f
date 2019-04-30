! subversion Id for THIS file : $Id: heat_ircool.f 8506 2016-01-28 10:09:58Z simonc $
! $URL: http://svn-ae/ae/COMMON/stand-alone/atmos_models/socrates/tags/07.18.01/src/heat_ircool.f $
!-----------------------------------------------------------------------
      subroutine HEAT_IRCOOL( )
!-----------------------------------------------------------------------
!    ... Calculates various IR radiative cooling rates
!-----------------------------------------------------------------------
      use SPECIES_DIMS, only : nbcon
      use GRID_DIMS,    only : lmax, niz
      use SPC_NAMES,    only : vid_o3p, vid_no, vid_o2, vid_o3, vid_h2o
      use CONC,         only : hm2d
      use BACKATM,      only : Xmsis, N2, wmole, Cp
      use SPECIES,      only : qn2da
      use VEN1,         only : t2d
      use TAU4,         only : q2, q3, q4, q5
      use PHYS_CST,     only : clight, hPl, k, Nav
      use TRANSFORM,    only : SMOOTHV, SMOOTHL
!      use NETCDF_UTILS   ! for sensitivity test

      implicit none
!-----------------------------------------------------------------------
!    ... Parameters
!-----------------------------------------------------------------------
      real, parameter :: wl_NO = 5.3e-6          ! wavelength of emission (m)
      real, parameter :: hvNO = hPl*clight/wl_NO ! energy of emission line (J)
      real, parameter :: A10NO = 13.3            ! transition probablility (1/s)
      real, parameter :: kOqNO = 6.5e-11         ! quenching rate of NO* by O(3P) (cm3/s)
      real, parameter :: kO2qNO = 2.4e-14        ! quenching rate of NO* by O2 (cm3/s)
      real, parameter :: kN2qNO = 6.5e-11        ! quenching rate of NO* by N2 (cm3/s)
      real, parameter :: wl_O3 = 9.597e-6        ! wavelength of emission (m)
      real, parameter :: hvO3 = hPl*clight/wl_O3 ! energy of emission line (J)
      real, parameter :: A10O3 = 11.086          ! transition probablility (1/s)
      real, parameter :: kMqO3 = 4.3e-14         ! quenching rate of O3(001) by M (cm3/s)
      
!-----------------------------------------------------------------------
!    ... Local variables
!-----------------------------------------------------------------------
      integer :: lat, iz
      real, dimension(niz) :: wO, wO2, wN2, LNO, wM_O3, LO3
      real, dimension(lmax,niz) :: h2ovmr, tau, logtau, ltlt, LH2O
      real, dimension(lmax,niz) :: q4ref, q4tmax, q4omax, q4co2max,  ! for sensitivity test
     $                             tref, oref, co2ref 
      
!-----------------------------------------------------------------------
!    ... IR cool rate from CO2 at 15 micrometer (NLTE, radiative 
!           transfer param following Fomichev/et-al:1998) for all altitudes
!           and from O3 at 9.6 um (LTE, rad trsf) below ~75km. Outputs
!           are q3 (for O3) and q4 (for CO2) in K/day, positive when cool      
!-----------------------------------------------------------------------
      call FOMICHEV98( )
      call SMOOTHV( q4, 41, niz, 1 )     
      call SMOOTHL( q4, 41, niz, 1 )

!-----------------------------------------------------------------------
!    ... This is a special sensitivity test - REMOVE from here to the stop
!-----------------------------------------------------------------------
!      tref = t2d
!      oref = qn2da(:,:,vid_o3p)
!      co2ref = qn2da(:,:,vid_co2)
!      q4ref = q4
!      call OPEN_FILE( '/home/simonc/socrates/saved/ts1_2002-9-28.nc', 
!     $                NF_NOWRITE )
!      call NETCDF_READ( 'temperature', matrix = t2d )
!      call FOMICHEV98( )
!      call SMOOTHV( q4, 41, niz, 1 )     
!      call SMOOTHL( q4, 41, niz, 1 )
!      q4tmax = q4
!      t2d = tref
!      call NETCDF_READ( 'o3p-da', matrix = qn2da(:,:,vid_o3p) )
!      call FOMICHEV98( )
!      call SMOOTHV( q4, 41, niz, 1 )     
!      call SMOOTHL( q4, 41, niz, 1 )
!      q4omax = q4
!      qn2da(:,:,vid_o3p) = oref
!      call NETCDF_READ( 'co2-da', matrix = qn2da(:,:,vid_co2) )
!      call FOMICHEV98( )
!      call SMOOTHV( q4, 41, niz, 1 )     
!      call SMOOTHL( q4, 41, niz, 1 )
!      q4co2max = q4
!      qn2da(:,:,vid_co2) = co2ref
!      q4 = q4ref
!      call CLOSE_FILE
!      write(33,*) 'q4 (i.e. qcool_co2, K/day)computed by FOMICHEV98'
!      write(33,*) '       in SOCRATES v6s28'
!      write(33,*) 'ref case is IC in t0a_2007-9-28.nc (SOLMIN)'
!      write(33,*) 'T, O & CO2 perturb read in ts1_2002-9-28.nc (SOLMAX)'
!      write(33,*)
!      write(33,'(5a12)') 'z* (km)', 'q4ref','q4tmax','q4omax','q4co2max'
!      write(33,*)
!      do iz = 1, niz
!         write(33,'(5f12.3)') REAL(iz-1), q4ref(18,iz),q4tmax(18,iz),
!     $         q4omax(18,iz),q4co2max(18,iz)
!      end do
!      stop 'HEAT_RCOOL: sensitivity test - see fort.33'

      
      

      do lat = 1, lmax
!-----------------------------------------------------------------------
!    ... IR cool rate from NO at 5.3 micrometer using cool-to-space
!           approx described in Kockarts, GRL, Vol 7, p. 137, 1980
!           Output q5 in K/day, positive when cools         
!-----------------------------------------------------------------------
         wO = kOqNO * qn2da(lat,:,vid_o3p) * hm2d(lat,:) 
     $      / ( kOqNO * qn2da(lat,:,vid_o3p) * hm2d(lat,:) + A10NO )
         wO2 = kO2qNO * qn2da(lat,:,vid_o2) * hm2d(lat,:)
     $       / ( kO2qNO * qn2da(lat,:,vid_o2) * hm2d(lat,:) + A10NO )
         wN2 = kN2qNO * Xmsis(lat,:,N2) * hm2d(lat,:)
     $       / ( kN2qNO * Xmsis(lat,:,N2) * hm2d(lat,:) + A10NO )
      
         LNO = hvNO * qn2da(lat,:,vid_no) * hm2d(lat,:) * A10NO
     $       * ( wO + wO2 + wN2 ) * EXP( - hvNO / ( k*t2d(lat,:) ) )
      
         q5(lat,:) = 86400. * LNO * Nav
     $             / ( hm2d(lat,:) * wmole(lat,:) * Cp(lat,:) )

!         if( lat == 1 ) then
!            write(*,'(a,es11.3,a,f8.3)')'HEAT_IRCOOL @ (1,113): hm2d= ',
!     $              hm2d(1,113),' ; t2d= ',t2d(1,113)
!            write(*,'(3(a,es11.3))') '    qn2da(o3p)= ',
!     $        qn2da(1,113,vid_o3p),' ; qn2da(no)= ',qn2da(1,113,vid_no),
!     $           ' ; qn2da(o2)= ',qn2da(1,113,vid_o2)
!            write(*,'(3(a,es11.3))') '    Cp= ',Cp(1,113),
!     $                 ' ; wmole= ',wmole(1,113),' ; q5= ',q5(1,113)
!        end if
     
!-----------------------------------------------------------------------
!       ... Below is a new, VERY crude approx of cooling rate from O3 
!           We consider only the fundamental band O3(001)->O3(000)
!           at 9.6 um above 80km using cool-to-space approx
!           The quenching rate kMqO3 from Mlynczak/drayson:1990a, table 2
!           is very uncertain (Mlynczak/Zhou:1998).
!           The Einstein's coeff A10O3 is from HITRAN, 1986
!           and seems well known (Mlynczak/Zhou:1998).    *SC - dec 1999
!-----------------------------------------------------------------------
         wM_O3 = kMqO3 * hm2d(lat,:) / ( kMqO3 * hm2d(lat,:) + A10O3 )
         
         LO3 = hvO3 * qn2da(lat,:,vid_o3) * hm2d(lat,:) * A10O3
     $       * wM_O3 * EXP( - hvO3 / ( k*t2d(lat,:) ) )
     
         q3(lat,81:niz) = 86400. * LO3(81:niz) * Nav
     $       / ( hm2d(lat,81:niz) * wmole(lat,81:niz) * Cp(lat,81:niz) )
         
      end do

!-----------------------------------------------------------------------
!  ... IR cool rate from rotational H2O band (LTE, cool-to-space approx)
!      described in appendix of Fomichev et al, JATP, 1986, vol48, p 529
!      NOTICE: this param was developed with H2O=3ppm MAX, should NOT
!              be used with greater mix ratios (which happen in 
!              polar night). Fixed at v6s27, testing 10ppm max at v7s11
!  q2=0 in tropo where cool-to-space approx can not work, because 
!          so much H2O must imply radiative transfer
!-----------------------------------------------------------------------
      h2ovmr(:,:) = MIN( qn2da(:,:,vid_h2o), 10.e-6 ) ! 10ppm, v7s11
      tau(:,:) = 2.72e-6 * h2ovmr * hm2d / t2d
      logtau(:,:) = LOG(tau)
      where( logtau > 1. )
         ltlt(:,:) = LOG( tau * logtau )
         LH2O(:,:) = 1.39e-7 * t2d**(-0.25) * SQRT( hm2d/tau )
     $             + 1.2 * ( 1.2 + 0.5*ltlt + 1.2/ltlt ) / tau
       else where
         LH2O(:,:) = 1.
      end where
      LH2O = MIN( LH2O, 1. )
      q2(:,:) = 1250. * h2ovmr * t2d*t2d * LH2O             ! erg/g/s = cm2/s3
      q2(:,:) = 86400. * 1.e-7 * q2 / Cp                    ! K/day
      q2(:,1:17) = 0.                                       ! troposphere
      
      end subroutine HEAT_IRCOOL

