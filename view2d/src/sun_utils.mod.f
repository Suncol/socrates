! subversion Id for THIS file : $Id: sun_utils.mod.f 8506 2016-01-28 10:09:58Z simonc $
! $URL: http://svn-ae/ae/COMMON/stand-alone/atmos_models/socrates/tags/07.18.01/src/sun_utils.mod.f $
!-----------------------------------------------------------------------
      module SUN_UTILS
!-----------------------------------------------------------------------
!         ... Compute the sun geometry parameters, taking in account
!             the altitude dependence of sunrise & sunset zenith angles
!                          SOCRATES v7s02 - simonc@oma.be, Feb 2002
!-----------------------------------------------------------------------
      implicit none
      
      PRIVATE
      PUBLIC :: SUN_ALT_TERM, SUN_SZASR, SUN_DECLIN, SUN_COSZA, 
     $          SUN_LIGHT, SUN_DAYLENGTH, SUN_CHAPMAN

      CONTAINS  

!=======================================================================

      real function SUN_ALT_TERM( cosza )
!-----------------------------------------------------------------------
!	... Calculate the terminator altitude (km)
!-----------------------------------------------------------------------
      use PHYS_CST, only : R0             ! R0 is Earth radius (km); d2r=pi/180

      real, intent(in)  :: cosza          ! Cosine of Solar Zenith Angle
      
      real :: sinza
!-----------------------------------------------------------------------

      if( ABS( cosza ) >= 1. ) then
         write(*,*) 'SUN_ALT_TERM: input cosza= ',cosza,' must be < 1.'
         stop 'SUN_ALT_TERM: fatal error, cosza >=1'
      end if
      
      sinza = SQRT( 1. - cosza*cosza )
      SUN_ALT_TERM = R0 * ( 1./sinza - 1. )
            
      end function SUN_ALT_TERM

!=======================================================================

      real function SUN_SZASR( z )
!-----------------------------------------------------------------------
!	... Calculate the Solar Zenith Angle (degrees) at sunrise/sunset.
!           This fct is inverted from the terminator altitude formula
!           in SUN_ALT_TERM
!-----------------------------------------------------------------------
      use PHYS_CST, only : R0, pi, d2r  ! R0 is Earth radius (km); d2r=pi/180

      real, intent(in)  :: z            ! Altitude (km) at point P
      
      real :: sinszasr, szar
!-----------------------------------------------------------------------
      
      sinszasr = 1. / ( z/R0 + 1. )
      szar = ASIN( sinszasr )
      if( szar < 0.5*pi ) szar = pi - szar
      SUN_SZASR = szar / d2r
            
      end function SUN_SZASR

!=======================================================================

      real function SUN_DECLIN( cal_day )
!-----------------------------------------------------------------------
!	... Calculate Solar declination angle - 3rd order sin/cos dvpt
!           from Iqbal, "An Introduction to solar radiation", 1983
!           Output is in degrees
!-----------------------------------------------------------------------
      use PHYS_CST, only : d2r             ! d2r=pi/180
      use SIM_CONTROLS, only : missval     ! conventional missing value

      real, intent(in)  :: cal_day         ! Day of year (0.->365.)

      real :: gma, declinr
      
      gma = d2r * 360. * (cal_day-1.) / 365.
      declinr = 0.006918 - 0.399912*COS(gma) + 0.070257*SIN(gma) 
     $        - 0.006758*COS(2.*gma) + 0.000907*SIN(2.*gma)
     $        - 0.002697*COS(3.*gma) + 0.00148*SIN(3.*gma)
      SUN_DECLIN = declinr / d2r

      end function SUN_DECLIN

!=======================================================================

      real function SUN_COSZA( phid, cal_day, slt )
!-----------------------------------------------------------------------
!    ... Calculate the COSINE of the solar zenith angle
!-----------------------------------------------------------------------
      use PHYS_CST, only : d2r             ! d2r=pi/180
      use SIM_CONTROLS, only : missval     ! conventional missing value

      real, intent(in) :: phid,            ! latitude (angular degrees)
     $                    cal_day,         ! Day of year (0.->365.)
     $                    slt              ! Solar local time (hours)

      real :: declin, sindecl, cosdecl, sinphi, cosphi, cos_h
      
      declin = d2r * SUN_DECLIN( cal_day )        ! radians
      sindecl = SIN( declin )
      cosdecl = SQRT( 1. - sindecl*sindecl )      ! declin never > pi/2
      sinphi = SIN( phid * d2r )
      cosphi = SQRT( 1. - sinphi * sinphi )       ! phi never > pi/2
      cos_h = COS( d2r * (slt - 12.) * 15. )      ! 15 = 360degrees / 24hours
      SUN_COSZA = cos_h * cosphi * cosdecl +  sinphi * sindecl 

      end function SUN_COSZA

!=======================================================================

      real function SUN_DAYLENGTH( z, phid, cal_day,
     $                             cosza_losun, cosza_hisun )
!-----------------------------------------------------------------------
!    ... Calculate the number of hours of daylight
!                       (when point outside of shadow of Earth) per day.
!        Optional output arguments:
!          cosza_losun -> cosine of Zenith angle when sun is at the 
!                         visible lowest point, i.e. at sunrise/sunset if
!                         normal day/night; at midnight if polar day ;
!                         missing value if polar night
!          cosza_hisun -> cosine of Zenith angle when sun is at the 
!                         visible highest point, i.e. at noon; missing 
!                         value if polar night
!-----------------------------------------------------------------------
      use PHYS_CST, only : d2r                      ! d2r=pi/180
      use SIM_CONTROLS, only : missval              ! conventional missing value

      real, intent(in) :: z,                        ! altitude (km) 
     $                    phid,                     ! latitude (angular degrees)
     $                    cal_day                   ! Day of year (0.->365.)

      real, intent(out), optional :: cosza_losun, cosza_hisun
     
      real :: szasr, declind, cosza_noon, cosza_midnight
      real :: coszasr, sindecl, cosdecl, sinphi, cosphi, cos_h
!-----------------------------------------------------------------------
      
      szasr = SUN_SZASR( z )
      declind = SUN_DECLIN( cal_day )
      
      coszasr = COS( szasr * d2r )
      sindecl = SIN( declind * d2r )
      cosdecl = SQRT( 1. - sindecl*sindecl )               ! declin never > pi/2
      sinphi = SIN( phid * d2r )
      cosphi = SQRT( 1. - sinphi * sinphi )                ! phi never > pi/2
         
      cos_h = ( coszasr - sinphi*sindecl ) / ( cosphi*cosdecl )
      cosza_noon = cosphi * cosdecl +  sinphi * sindecl 
      cosza_midnight = - cosphi * cosdecl +  sinphi * sindecl
  
      if( ABS( cos_h ) <= 1. ) then                        ! normal day/night
         SUN_DAYLENGTH = 2. * ACOS(cos_h) / ( 15.*d2r )    !    15 = 360/24
       else if( cosza_noon < coszasr )  then               ! polar night
         SUN_DAYLENGTH = 0.
       else if( cosza_midnight > coszasr )  then           ! polar day
         SUN_DAYLENGTH = 24.
       else
         goto 50
      end if
      
      if( PRESENT( cosza_losun ) ) then
         if( SUN_DAYLENGTH == 24. ) then
            cosza_losun = cosza_midnight
          else if( SUN_DAYLENGTH == 0. ) then
            cosza_losun = missval
          else
            cosza_losun = coszasr
         end if
      end if

      if( PRESENT( cosza_hisun ) ) then
         if( SUN_DAYLENGTH == 0. ) then
            cosza_hisun = missval
          else
            cosza_hisun = cosza_noon
         end if
      end if
      
      return
      
   50 write(*,*) 'SUN_DAYLENGTH: algorithm error'
      write(*,100)'    z= ',z,' ; phid= ',phid,' ; cal_day= ',cal_day
      write(*,100)'    szasr= ',szasr,' ; declind= ',declind
      write(*,110)'    coszasr= ',coszasr,' ; cos_h= ',cos_h
      write(*,110)'    cosza_noon= ',cosza_noon,
     $             ' ; cosza_midnight= ',cosza_midnight
      stop 'SUN_DAYLENGTH: fatal error'
   
  100 format(3(a,f12.5))
  110 format(3(a,es12.3))
  
      end function SUN_DAYLENGTH

!=======================================================================

      real function SUN_CHAPMAN( z, h, cosza, ier )
!---------------------------------------------------------------------
!    ...Calculates Chapman fct for large zenith angles (khi>65 deg)
!       The input is cosza, the COSINE of Solar Zenith Angle. 
!       Output = 1/input if SZA is <65 deg, or Chapman function if SZA is
!         > 65 deg. Hence, the Chapman fct is a "corrected" secant of SZA,
!      with caveat: isothermal atmosphere with constant scale height
!      From Smith & Smith, J.G.R., Vol.77, pp.3592-3597, 1972.
!             New version for SOCRATES v6s07, simonc@oma.be, sept 2000
!---------------------------------------------------------------------
      use PHYS_CST, only : R0, pi         ! R0 is Earth radius (km)
      use SIM_CONTROLS, only : missval    ! conventional missing value

      real, intent(in)  :: z,             ! Altitude (km) at point P
     $                     h,             ! Scale height (km) of const. at P
     $                     cosza          ! COSINE of solar Zenith angle at P
      integer, intent(out) :: ier         ! ier /= 0 if SUN_CHAPMAN failed

      real    :: xp, y, sinza
!---------------------------------------------------------------------

      ier = 0
      SUN_CHAPMAN = missval
      
      if( ABS(cosza) > 1. ) then
         write(*,'(3(a,es9.3),a)') ' SUN_CHAPMAN: fct called with wrong'
     $      // ' cosza= ',cosza,' should be between -1 and 1'
         write(*,*) ' Input of SUN_CHAPMAN is COS(sza), not SECANT(sza)'
         ier = 1
         return
       else if( cosza >= .422 ) then   ! zenith angle < 65 degrees
         SUN_CHAPMAN = 1. / cosza
         return
      end if
         
      xp = (R0 + z) / h
      if( xp < 50. .or. xp > 1.e9 ) then
         write(*,'(3(a,es9.3),a)')
     $          ' SUN_CHAPMAN: function not valid : z=',
     $              z,' ; h=',h,' => xp=(R0+z)/h=',xp,' < 50. or > 1.e9'
         ier = 2
         return
      end if

      y = SQRT( .5*xp ) * ABS( cosza )
      if ( y > 100. ) then
         write(*,'(3(a,es12.3))') 
     $           ' SUN_CHAPMAN: function not valid : z=',
     $                 z,' ; h=',h,' => xp=(R0+z)/h=',xp
         write(*,*) '        cosza=',cosza,' => y = ',y,' > 100.'
         ier = 3
         return
      end if
      
      if ( cosza >= 0.) then
         SUN_CHAPMAN = SQRT( pi*xp*.5 ) * ERFCEXP( )
      else
         sinza = SQRT( 1. - cosza*cosza )
         SUN_CHAPMAN = SQRT( 2.*pi*xp )
     $               * (SQRT(sinza) * EXP( xp*(1. - sinza) )
     $                                         - .5*ERFCEXP( ) )
      end if

      CONTAINS

      real function ERFCEXP( )
!---------------------------------------------------------------------
!    ... Complementary error function
!---------------------------------------------------------------------

      implicit none
      
      if ( y < 8 ) then
         ERFCEXP = (1.0606963 + .55643831*y)
     $             / (1.0619896 + y*(1.7245609 + y))
      else if ( y <= 100. ) then
         ERFCEXP = .56498823 / (.06651874 + y)
      end if
      
      end function ERFCEXP

      end function SUN_CHAPMAN

!=======================================================================

      logical function SUN_LIGHT( phid, z, cal_day, slt )
!-----------------------------------------------------------------------
!    ... Tells if there is sunlight or not. Based on code provided by
!         Jan Kazil ( jan.kazil@phim.unibe.ch )
!-----------------------------------------------------------------------
      real, intent(in) :: phid,        ! latitude (angular degrees)
     $                    z,           ! Geometric altitude (km)
     $                    cal_day,     ! Day of year (0.->365.)
     $                    slt          ! Solar local time (hours)

      real    :: cosza
!-----------------------------------------------------------------------    

      cosza = SUN_COSZA( phid, cal_day, slt )
 
      if( cosza >= 0. ) then               ! sza <= pi/2
         SUN_LIGHT = .true.
       else if( cosza == -1. ) then        ! sza = pi
         SUN_LIGHT = .false.
       else                                ! pi/2 < sza < pi
         if( z >= SUN_ALT_TERM( cosza ) ) then
            SUN_LIGHT = .true.
          else
            SUN_LIGHT = .false.
         end if
      endif
      
      end function SUN_LIGHT

!=======================================================================

      end module SUN_UTILS
