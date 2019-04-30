! subversion Id for THIS file : $Id: solcycles.f 8506 2016-01-28 10:09:58Z simonc $
! $URL: http://svn-ae/ae/COMMON/stand-alone/atmos_models/socrates/tags/07.18.01/src/solcycles.f $
!-----------------------------------------------------------------------

      subroutine SOLCYCLES( time )
!-----------------------------------------------------------------------
!    ... Modify variables according to solar activity level set by mlt_sw(6)
!        Takes in account SIN fit of 11-year solcycle (see varsolflux.xls)
!        and earth-sun distance factor.
!        Input variables: solflux(:,0) at solmin, solflux(:,2) at solmax
!        Output variables: 
!           f107, solar radio flux (1e-22 W m-2 Hz-1) used by MSIS & HWM
!           fbeamr, the solar flux used in PHODIS
!        pcst_no(91:niz), phve_no & flxub(:,vid_no) are influenced in
!        SPECIAL_NO_PROD                 v6s17, Feb 2001 - simonc@oma.be
!-----------------------------------------------------------------------
      use PHYS_CST, only : pi
      use PHO_PARMS, only : mxwvn
      use SIM_CONTROLS, only : missval, mlt_sw
      use SUN_VARS, only : solflux, days0_s2k, e107_s2k, flya_s2k  ! input from PHO_INIT
     $                   , solfactor, f107 ! output
      use PHO_VARS, only : fbeamr          ! output: time-dep solar spectrum
      use DIAG_CONTROLS, only : diags
      use TIME_CONTROLS, only : TIMING
      use DATE_UTILS, only : DATE2TIME

      implicit none

!-----------------------------------------------------------------------
!    ... Parameters
!-----------------------------------------------------------------------
      integer, parameter    :: lya_iv = 8
      real, parameter :: e107min = 67.5, e107max = 220.
      real, parameter :: flyamin = 3.5e11, flyamax = 6.e11
      real, parameter :: p11 = 10.8   ! period of the "11" year solar cycle

!----------------------------------------------------------------------
!    ... Dummy args
!----------------------------------------------------------------------
      type( TIMING ), intent(in) :: time

!----------------------------------------------------------------------
!    ... Local variables
!----------------------------------------------------------------------
      real :: dist_factor, diffdays0, solfactor_flya
      type(TIMING), save :: date_solavg
      integer, save :: id = 1
      logical, save :: entered = .false.

      if( .not. entered ) then
         date_solavg = TIMING( 1999, 8, 21, 0, 0 )
         call DATE2TIME( date_solavg )
      end if

      fbeamr(:) = solflux(:,1)
      if( mlt_sw(6) == 0 ) then          ! solmin
         solfactor = 0.  
         solfactor_flya = solfactor
       else if( mlt_sw(6) == 1 ) then    ! solmax
         solfactor = 1.
         solfactor_flya = solfactor
         
!----------------------------------------------------------------------
!    ... sin cycle between 0. & 1.
!----------------------------------------------------------------------
       else if( mlt_sw(6)==2 .or. e107_s2k(id)==missval ) then 
         diffdays0 = REAL( time%days0 - date_solavg%days0 )
         solfactor = 0.5 * ( 1. + SIN( 2.*pi*diffdays0 / 365. / p11 ) )
         solfactor_flya = solfactor
         
!----------------------------------------------------------------------
!    ... Use daily SOLAR2000
!----------------------------------------------------------------------
       else
          if( days0_s2k(id) /= time%days0 ) then
             write(*,*) 'SOLCYCLES error: days0_s2k /= time%days0'
             write(*,*) 'SOLCYCLES id= ',id,' ; days0_s2k= '
     $          ,days0_s2k(id),' ; time%days0 = ',time%days0 
             stop 'SOLCYCLES error: days0_s2k and time%days0 mismatch'
          end if
          solfactor = ( e107_s2k(id) - e107min ) / ( e107max - e107min )
          solfactor_flya = ( flya_s2k(id) - flyamin ) 
     $                   / ( flyamax - flyamin )
      end if
      if( entered ) id = id + 1
      entered = .true.

!----------------------------------------------------------------------
!    ... F10.7 for MSIS & HWM replaced by E10.7 given by SOLAR2000 
!           empirical model, see Tobiska/et-al-2000
!----------------------------------------------------------------------
      if( mlt_sw(7) == 0 ) then
         f107 = e107min                                  ! solmin value
       else if( mlt_sw(7) == 1 ) then
         f107 = e107min + (e107max-e107min) * solfactor  ! f107 = 220. at solmax
      end if      

!-----------------------------------------------------------------------
!    ... Solar flux, solflux read by PHO_INIT
!-----------------------------------------------------------------------
      if( mlt_sw(8) == 0 ) then    
         fbeamr(:) = solflux(:,0)        ! solmin values
       else if( mlt_sw(8) == 1 ) then
         fbeamr(:) = solflux(:,0) 
     $             + ( solflux(:,2) - solflux(:,0) ) * solfactor
      end if

!-----------------------------------------------------------------------
!   ... Solar flux at Lyman-alpha line
!-----------------------------------------------------------------------
      if( mlt_sw(9) == 0 ) then
         fbeamr(lya_iv) = flyamin
       else if( mlt_sw(9) == 1 ) then
         fbeamr(lya_iv) = flyamin + (flyamax-flyamin) * solfactor_flya ! 6.e11 at solmax
      end if

!-----------------------------------------------------------------------
!       ... Compute the earth-sun distance factor
!            and factor solar flux input
!-----------------------------------------------------------------------
      dist_factor = 1. + .0342* COS(2.*pi*time%cal_day/365.)
      fbeamr(:) = dist_factor * fbeamr(:)
      
!-----------------------------------------------------------------------
!   ... Everything below is diagnostics
!-----------------------------------------------------------------------
      if( diags ) then
         write(*,'(a,i3,''/'',i2,''/'',i4,a,f8.5,a,f8.2,a,es12.3)') 
     $    'SOLCYCLES, date= ',time%month,time%day,time%year
     $     ,' ; solfactor= ',solfactor,' ; f107= ',f107
     $     ,' ; fbeamr(lya_iv)= ',fbeamr(lya_iv)
!         if( id == 3 ) stop 'testing SOLCYCLES'
      end if

      end subroutine SOLCYCLES
