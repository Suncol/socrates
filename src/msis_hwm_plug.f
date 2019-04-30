! subversion Id for THIS file : $Id: msis_hwm_plug.f 8506 2016-01-28 10:09:58Z simonc $
! $URL: http://svn-ae/ae/COMMON/stand-alone/atmos_models/socrates/tags/07.18.01/src/msis_hwm_plug.f $
!-----------------------------------------------------------------------
      subroutine MSIS_HWM_PLUG( slt, time )
!----------------------------------------------------------------------
!       ... call MSIS model (NWGHP6_F90) in log-p altitude coords.
!           and HWM wind model (NWGWS5_F90) in corresponding zgeo coords
!       ... slt is the solar lacal time. A negative value means that 
!           this routine is called from outside of chemistry, diurnal 
!           averages values are then used
!                                       Apr 2001, simonc@oma.be
!----------------------------------------------------------------------
      use GRID_DIMS, only : lmax, niz
      use ALLCO, only : phi, zkm
      use ZGRID, only : pmb
      use TROPOPAUSE, only : izmMax, izm
      use SUN_VARS, only : f107
      use BACKATM, only : tot, Tmsis, Dmsis, Xmsis, u_hwm, v_hwm  ! output
     $                  , O1, O2                                  ! for diags
      use SIM_CONTROLS, only : model_type
      use DIAG_CONTROLS, only : diags, ldiag, zdiag
      use TIME_CONTROLS, only : TIMING
      use MSIS_CODE, only : NWGHP6_F90
      use HWM_CODE, only : NWGWS5_F90

      implicit none
!----------------------------------------------------------------------
!    ... Parameters
!----------------------------------------------------------------------
      real, parameter :: tinc = 3.4   ! increase of surf T for 2*CO2 (K)

!----------------------------------------------------------------------
!    ... Dummy arguments
!----------------------------------------------------------------------
      type( TIMING ), intent(in) ::  time
      real, intent(in) :: slt
      
!----------------------------------------------------------------------
!    ... Local variables
!----------------------------------------------------------------------
      real :: GLONG = 0., F107A, ntotmsis
      real, dimension(7) :: AP = (/ 4., 0., 0., 0., 0., 0., 0. /)
      real :: T(2), v_u_hwm(2), STL, ALT
      integer  ::  l, iz, iSW(26), iw_hwm(25)
      
      F107A = f107
      iSW(1:25) = 1
      iSW(26) = 1
      STL = slt
      if( slt < 0. ) then
         STL = 0.
         iSW(7:8) = 0
         iSW(10:14) = 0
      end if
      iw_hwm(1:14) = iSW(1:14)
      iw_hwm(15:25) = 1
      
      do l = 1,lmax

         if( model_type /= 'two_d' .and. l /= ldiag ) cycle
         do iz = niz, 1, -1

!-----------------------------------------------------------------------
!     ... Call MSIS
!-----------------------------------------------------------------------
            call NWGHP6_F90( INT(time%cal_day), ALT, phi(l),
     $                       GLONG, STL, F107A, f107, AP,
     $                       Dmsis(l,iz,:), T,
     $                       pmb(iz), iSW                      )

            Tmsis(l,iz) = T(2)
            Dmsis(l,iz,tot) = SUM( Dmsis(l,iz,1:7) )
            Xmsis(l,iz,:) = Dmsis(l,iz,:) / Dmsis(l,iz,tot)
 
!-----------------------------------------------------------------------
!     ... Call HWM
!-----------------------------------------------------------------------
            call NWGWS5_F90( INT(time%cal_day), ALT, phi(l), 
     $                       GLONG, STL, F107A, F107, AP(1:2), 
     $                       v_u_hwm, iw_hwm )

            u_hwm(l,iz) = v_u_hwm(2)
            v_hwm(l,iz) = v_u_hwm(1)
                          
         end do
      end do
      
      if( model_type /= 'two_d' ) then
         do l = 1, lmax
            Tmsis(l,:) = Tmsis(ldiag,:)
            Dmsis(l,:,:) = Dmsis(ldiag,:,:)
            Xmsis(l,:,:) = Xmsis(ldiag,:,:)
         end do
      end if

!-------------------------------------------------------------------
!    ... Everything below is error reporting and diagnostics
!-------------------------------------------------------------------
      if( diags ) then
         write(*,'(2(a,i3),2(a,f7.3),a,es9.3)') 'MSIS_HWM_PLUG @ ('
     $     ,ldiag,',', zdiag,'): T= ',Tmsis(ldiag,zdiag),' ; O2= '
     $     ,Xmsis(ldiag,zdiag,O2),' ; O= ',Xmsis(ldiag,zdiag,O1)
      end if
      
      end subroutine MSIS_HWM_PLUG
