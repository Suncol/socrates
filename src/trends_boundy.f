! subversion Id for THIS file : $Id: trends_boundy.f 8506 2016-01-28 10:09:58Z simonc $
! $URL: http://svn-ae/ae/COMMON/stand-alone/atmos_models/socrates/tags/07.18.01/src/trends_boundy.f $
!-----------------------------------------------------------------------

      subroutine TRENDS_BOUNDY( time )
!-----------------------------------------------------------------------
!    ... Long-term trends of chemical composition are set here, as a 
!        function of the current year. 
!        The Boundary Conditions (BC) are adjusted by default to the 
!        initial simulation year, or to the current simulation year
!        if relevant gcc_sw are set. The Initial Conditions (IC) are 
!        adjusted at first call only if gcc_sw(1)==1 .
!        (see describe_sim.f)                          simonc@oma.be
!-----------------------------------------------------------------------
      use SIM_CONTROLS, only : sim_start_time, sim_stop_time, gcc_sw
      use DIAG_CONTROLS, only : debug
      use BOUNDARIES, only : lbc, ibc        ! output
     $                     , ibc_vmr_h2o     ! input
      use SPECIES, only : qn2da, qn2noon     ! modified at 1st call only
      use SPC_NAMES, only : vid_co2, vid_ch4, vid_h2o
      use TIME_CONTROLS, only : TIMING
      use FIT_UTILS, only : FIT_EXP_TREND, FIT_ASYMSIG

      implicit none
!-----------------------------------------------------------------------
!	... parameters
!-----------------------------------------------------------------------
      integer, parameter :: nyears = 161

!-----------------------------------------------------------------------
!    ... Dummy args
!-----------------------------------------------------------------------
      type( TIMING ), intent(in) ::  time

!-----------------------------------------------------------------------
!    ... Local variables
!-----------------------------------------------------------------------
      integer :: i
      real :: init_year, current_year, last_year, year
     $      , co2ratio, ch4ratio, h2oratio, co2lbmr, ch4lbmr
      logical, save :: entered = .false.

!-----------------------------------------------------------------------
!    ... Set current simulation year and initial simulation year
!-----------------------------------------------------------------------
      current_year = REAL( time%year )     + time%cal_day / 365.
      init_year = REAL(sim_start_time%year)+ sim_start_time%cal_day/365.
      last_year = REAL(sim_stop_time%year) + sim_stop_time%cal_day/365.
   
!-----------------------------------------------------------------------
!    ... Change IC & LBC for CO2. 1800-2100: fitting fct from 1954-2000 data
!        of IPCC2001:fig32, fitted using TableCurve2D on pandora.oma.be, see
!        hercules://home/simonc/digitscan/data/CO2_surf_1960-2000.dat
!-----------------------------------------------------------------------
      if( .not. lbc(vid_co2)%is_vmr ) 
     $   stop 'TRENDS_BOUNDY: lbc(vid_co2) is .not. vmr'

      year = init_year
      if( gcc_sw(2) == 1 ) year = current_year
      if( gcc_sw(2) == 2 ) year = last_year
      
      if( year < 1800. ) then       ! preindustrial , see IPCC?? 
         co2lbmr = 250.e-6
       else if( year < 2100. ) then
         co2lbmr = 1.e-6 * FIT_EXP_TREND( year, 288.19, 34.848
     $                                  , 2.7538e-2, 1967.684 )
       else
         co2lbmr = 715.e-6
      end if
      
      if( co2lbmr < 154.e-6 .or. co2lbmr > 716.e-6 ) goto 100
      co2ratio = co2lbmr / lbc(vid_co2)%val(18)
      
      lbc(vid_co2)%val(:) = co2ratio * lbc(vid_co2)%val(:)
      if( .not. entered .and. gcc_sw(1) == 1 ) then
         qn2da(:,:,vid_co2) = co2ratio * qn2da(:,:,vid_co2)
         qn2noon(:,:,vid_co2) = co2ratio * qn2noon(:,:,vid_co2)
      end if
       
!-----------------------------------------------------------------------
!    ... Change IC & LBC for CH4. Post-1800: fitting fct from 1954-2000 data
!        of IPCC2001:fig41 using TableCurve2D on pandora.oma.be, see
!        hercules://home/simonc/digitscan/data/CH4_surf_1000-2000.dat
!-----------------------------------------------------------------------
      if( .not. lbc(vid_ch4)%is_vmr ) 
     $   stop 'TRENDS_BOUNDY: lbc(vid_ch4) is .not. vmr'

      year = init_year
      if( gcc_sw(3) == 1 ) year = current_year
      if( gcc_sw(3) == 2 ) year = last_year
      
      if( year < 1800. ) then ! preindustrial , see Stauffer/et-al-1985
         ch4lbmr = 0.65e-6
       else 
         ch4lbmr = 1.e-9 * FIT_ASYMSIG( year, 700.48, 1049.6
     $                                , 1954.5, 3.4872, 0.06568 )
      end if

      if( ch4lbmr < 0. .or. ch4lbmr > 5.e-6 ) goto 100
      ch4ratio = ch4lbmr / lbc(vid_ch4)%val(18)
      
      lbc(vid_ch4)%val(:) = ch4ratio * lbc(vid_ch4)%val(:)
      if( .not. entered .and. gcc_sw(1) == 1 ) then
         qn2noon(:,:,vid_ch4) = ch4ratio * qn2noon(:,:,vid_ch4)
      end if
 
!-----------------------------------------------------------------------
!    ... Change IC & intermediate BC for H2O. We suppose a 1% increase 
!        in lower strato (Rosenlof/et-al-2001) to modify the intermediate 
!        vmr BC for H2O at 17km, typical of 1994 and set in BOUNDY
!-----------------------------------------------------------------------
      if( .not. ibc(vid_h2o)%is_vmr ) 
     $   stop 'TRENDS_BOUNDY: ibc(vid_h2o) is .not. vmr'

      year = init_year
      if( gcc_sw(4) == 1 ) year = current_year
      if( gcc_sw(4) == 2 ) year = last_year

      h2oratio =  1.01**( year - 1994. )
      if( h2oratio < 0.1 .or. h2oratio > 2. ) goto 100

      ibc(vid_h2o)%val(:) = h2oratio * ibc_vmr_h2o(time%month,:) 
      if( .not. entered .and. gcc_sw(1) == 1 ) then
         qn2da(:,:,vid_h2o) = h2oratio * qn2da(:,:,vid_h2o)
         qn2noon(:,:,vid_h2o) = h2oratio * qn2noon(:,:,vid_h2o)
      end if
      
      entered = .true.
      return

!-----------------------------------------------------------------------
!    ... Diagnostics and error reporting
!-----------------------------------------------------------------------
  100 write(*,*) 'TRENDS_BOUNDY warning, year= ',year
      write(*,*) '     co2lbmr= ',co2lbmr
      write(*,*) '     ch4lbmr= ',ch4lbmr
      write(*,*)'     h2oratio= ',h2oratio
      if( debug ) stop 'TRENDS_BOUNDY: error fitting CO2/CH4/H2O BC'
      write(*,*) '   lbmr(:,co2 or ch4) was not modified'

      end subroutine TRENDS_BOUNDY
