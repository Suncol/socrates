! subversion Id for THIS file : $Id: minsch93_pho.f 8506 2016-01-28 10:09:58Z simonc $
! $URL: http://svn-ae/ae/COMMON/stand-alone/atmos_models/socrates/tags/07.18.01/src/minsch93_pho.f $
!-----------------------------------------------------------------------

      subroutine MINSCH93_PHO( levlo, zs, colo2, uavg, secchap, dens, 
     $                         jjno )
!-----------------------------------------------------------------------
!	... Compute NO photolysis rate
!           param by Minschwaner & Siskind, 1993, JGR, p. 20401
!-----------------------------------------------------------------------
      use PHYS_CST, only : pi
      use PHO_PARMS, only : mxcly, mxwvn, maxden

      implicit none
!-----------------------------------------------------------------------
!	... Parameters
!-----------------------------------------------------------------------
      real, parameter :: cs250(6) = (/ 1.117e-23, 2.447e-23, 7.188e-23,
     $                            3.042e-22, 1.748e-21, 1.112e-20 /)
      real, parameter :: cs290(6) = (/ 1.350e-22, 2.991e-22, 7.334e-22,
     $                            3.074e-21, 1.689e-20, 1.658e-19 /)
      real, parameter :: cs2100(6) = (/ 2.968e-22, 5.831e-22, 2.053e-21,
     $                             8.192e-21, 4.802e-20, 2.655e-19 /)

!-----------------------------------------------------------------------
!   	... 6 sub-intervals for O2 5-0 at 265K,
!	    2 sub-sub-intervals for NO 0-0 at 250K
!-----------------------------------------------------------------------
      real, parameter :: a(24) = (/ 0., 0., 0., 0.0,
     $             5.12E-02, 5.68E-03, 1.32E-18, 4.41E-17,
     $             1.36E-01, 1.83E-02, 6.35E-19, 4.45E-17,
     $             1.65E-01, 1.52E-02, 7.09E-19, 4.50E-17,
     $             1.41E-01, 1.57E-02, 2.18E-19, 2.94E-17,
     $             4.50E-02, 5.00E-03, 4.67E-19, 4.35E-17 /)

!-----------------------------------------------------------------------
!   	... Sub-intervals for O2 9-0 band,
!	    2 sub-sub-intervals for NO 1-0 at 250 K
!-----------------------------------------------------------------------
      real, parameter :: b(24) = (/ 0., 0., 0., 0.,
     $           0.00E+00, 0.00E+00, 0.00E+00, 0.00E+00,
     $           1.93E-03, 2.14E-04, 3.05E-21, 3.20E-21,
     $           9.73E-02, 1.08E-02, 5.76E-19, 5.71E-17,
     $           9.75E-02, 1.08E-02, 2.29E-18, 9.09E-17,
     $           3.48E-02, 3.86E-03, 2.21E-18, 6.00E-17 /)

!-----------------------------------------------------------------------
! 	... Sub-intervals for O2 10-0 band,
!	    2 sub-sub-intervals for NO 1-0 at 250 K
!-----------------------------------------------------------------------
      real, parameter :: c(24) = (/
     $           4.50E-02, 5.00E-03, 1.80E-18, 1.40E-16,
     $           1.80E-01, 2.00E-02, 1.50E-18, 1.52E-16,
     $           2.25E-01, 2.50E-02, 5.01E-19, 7.00E-17,
     $           2.25E-01, 2.50E-02, 7.20E-20, 2.83E-17,
     $           1.80E-01, 2.00E-02, 6.72E-20, 2.73E-17,
     $           4.50E-02, 5.00E-03, 1.49E-21, 6.57E-18 /)

!-----------------------------------------------------------------------
!    	... Note: first 2 sub-intervals in 10-0 band
!           slightly modified for accuracy
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
!	... Dummy args
!-----------------------------------------------------------------------
      integer, intent(in) :: levlo
      real, dimension(0:mxcly), intent(in)  :: zs, colo2, secchap
      real, intent(in)  :: uavg(mxwvn,0:mxcly)
      real, intent(in)  :: dens(0:mxcly,maxden)
      real, intent(out) :: jjno(0:mxcly)

!-----------------------------------------------------------------------
!	... Local variables
!-----------------------------------------------------------------------
      integer :: lev
      real  ::  dz, jno50, jno90, jno100, solid_angle
      real, dimension(6,2)  ::  wtno50, csno50, wtno90, csno90,
     $                                wtno100, csno100
      real :: nodz(0:mxcly)

         wtno50(1:6,1) = a(1:24:4)
         wtno50(1:6,2) = a(2:24:4)
         csno50(1:6,1) = a(3:24:4)
         csno50(1:6,2) = a(4:24:4)
         wtno90(1:6,1) = b(1:24:4)
         wtno90(1:6,2) = b(2:24:4)
         csno90(1:6,1) = b(3:24:4)
         csno90(1:6,2) = b(4:24:4)
         wtno100(1:6,1) = c(1:24:4)
         wtno100(1:6,2) = c(2:24:4)
         csno100(1:6,1) = c(3:24:4)
         csno100(1:6,2) = c(4:24:4)
         solid_angle = 4. * pi

      nodz(0) = 3.e13
      do lev = 1, levlo
         dz = 1.e5 * (zs(lev-1) - zs(lev))
         nodz(lev) = nodz(lev-1)
     $             + .5*(dens(lev-1,4) + dens(lev,4))*dz
      end do

      do lev = 0, levlo
         jno50 = PJNO( 55, cs250, wtno50, csno50 )
         jno90 = PJNO( 51, cs290, wtno90, csno90 )
         jno100 = PJNO( 50, cs2100, wtno100, csno100 )
         jjno(lev) = jno50 + jno90 + jno100
      end do

      CONTAINS

      real function PJNO( w, cs2, wtno, csno )
!----------------------------------------------------------------
!   	... Uses xsec at center of g subinterval for O2
!           uses mean values for NO
!----------------------------------------------------------------
      
      implicit none

!----------------------------------------------------------------
!	... Parameters
!----------------------------------------------------------------
      integer, parameter :: ngint = 6, nno = 2

!----------------------------------------------------------------
!	... Dummy args
!----------------------------------------------------------------
      integer, intent(in) :: w
      real, intent(in) :: cs2(ngint)
      real, dimension(ngint,nno), intent(in) :: csno, wtno

!----------------------------------------------------------------
!	... Local variables
!----------------------------------------------------------------
      integer ::  jj, i, k
      real :: tauno, trano, trans, tau
      real :: jno, jno1

      jno = 0.
      do k = 1,ngint
         tau = colo2(lev) * cs2(k) * secchap(lev)  
	 if( tau < 50. ) then
	    trans = EXP( -tau )
	 else
	    trans = 0.
	 end if
         jno1 = 0.    
         do jj = 1,nno
            tauno = csno(k,jj) * nodz(lev) * secchap(lev)
	    if( tauno < 50. ) then
	       trano = EXP( -tauno )
	    else
	       trano = 0.
	    end if
            jno1 = jno1 + csno(k,jj) * wtno(k,jj) * trano     
         end do
         jno = jno + jno1*trans
      end do

      PJNO = jno * uavg(w,lev) * solid_angle

      if( w == 55 ) then
         PJNO = 1.65e9/(5.1e7 + 1.65e9 + 1.5e-9*.79*dens(lev,1))*PJNO
      end if

      end function PJNO

      end subroutine MINSCH93_PHO
