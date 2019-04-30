! subversion Id for THIS file : $Id: total_heat_rate.f 8506 2016-01-28 10:09:58Z simonc $
! $URL: http://svn-ae/ae/COMMON/stand-alone/atmos_models/socrates/tags/07.18.01/src/total_heat_rate.f $
!-----------------------------------------------------------------------

      subroutine TOTAL_HEAT_RATE( daynum, q )
!-----------------------------------------------------------------------
!     Calculate heating and cooling rates, their respecive sums 
!     (pch and qcool) and the total heating rate q, in K/s
!-----------------------------------------------------------------------
      use GRID_DIMS,    only  : lmax, niz
      use SOLTEST,      only  : srheat1, srheat2, srheat3
      use TAU1,         only  : pch
      use TAU4,         only  : q2, q3, q4, q5
      use TROPIC,       only  : clh
      use TAU3,         only  : qcool
      use HEAT_TERMS,   only  : solheat, chemheat, gwheat
      use TROPOPAUSE,   only  : izm
      use TAU0,         only  : qsum                         ! for archiving
      use SIM_CONTROLS, only  : jump_chem, het_sw, ntstep
      use DIAG_CONTROLS, only : diags,ldiag, zdiag  
      use VEN1,         only  : t2d                          ! for diags
      use TRANSFORM,    only  : SMOOTHV, SMOOTHL

      implicit none

!------------------------------------------------------------------
!	... Dummy args
!------------------------------------------------------------------
      real, intent(in) ::  daynum
      real, intent(out), dimension(lmax,niz) :: q

!-----------------------------------------------------------------------
!	... Local variables
!-----------------------------------------------------------------------
      logical, save :: entered = .false.

!-----------------------------------------------------------------------
!     	... Calculate the total heatings ( not at 1st call when read )
!-----------------------------------------------------------------------
      if( entered .and. .not. jump_chem ) then
         srheat1(:,:) = solheat(:,:,2)                   ! O3 heating, K/day
         srheat2(:,:) = solheat(:,:,1)                   ! O2 heating, K/day
         if( het_sw(7) == 1 ) srheat2 = srheat2 + solheat(:,:,3)  ! aerosol heating, K/day
         srheat3(:,:) = SUM( chemheat(:,:,:), DIM=3 )    ! chem heating, K/day
      end if
      pch = srheat1 + srheat2

!-----------------------------------------------------------------------
!     	... Calculate the IR radiative coolings q2 (H2O, rotational bands),
!           q3 (O3, 9.6um), q4 (CO2, NLTE, 15um) and q5 (NO, 5.3 um)
!-----------------------------------------------------------------------
      call HEAT_IRCOOL( )
      qcool = q2 + q3 + q4 + q5      

!-----------------------------------------------------------------------
!    ... Specify the time-indep heating rate in troposphere (output clh)
!-----------------------------------------------------------------------
      if( .not. entered ) then
         call TROPO_HEAT( )
         entered = .true.
      end if
  
!-----------------------------------------------------------------------
!  	... Form net heating q (K/day) ; clh is fixed tropospheric heating
!-----------------------------------------------------------------------
      q = pch + srheat3 + clh - qcool + gwheat

      if( diags ) then
         write(*,*)
         write(*,'(a,i2,a,i3,6(a12,es11.3))')'TOTAL_HEAT_RATE @(',ldiag,
     $ ',',zdiag,'): q2: ',q2(ldiag,zdiag),' ; q3: ',q3(ldiag,zdiag)
     $  ,' ; q4: ',q4(ldiag,zdiag),' ; q5: ',q5(ldiag,zdiag)
         write(*,'(23x,6(a12,es11.3))')'srheat1: ',srheat1(ldiag,zdiag),
     $ ' ; srheat2: ',srheat2(ldiag,zdiag),
     $ ' ; srheat3: ',srheat3(ldiag,zdiag)
         write(*,'(23x,6(a12,es11.3))') 'qcool ',qcool(ldiag,zdiag),
     $ ' ; q: ',q(ldiag,zdiag)
      end if

!-----------------------------------------------------------------------
!     	... Smooth q in z and y direction to avoid spurious layering
!-----------------------------------------------------------------------
      call SMOOTHV( q, 1, niz, 5 )
      call SMOOTHL( q, 1, niz, 2 )
      qsum = q                           ! for archiving

!-----------------------------------------------------------------------
!     	... Convert q from K/day to K/second
!-----------------------------------------------------------------------
      q(:,:) = q(:,:) / 86400.
      
      end subroutine TOTAL_HEAT_RATE
