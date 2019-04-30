! subversion Id for THIS file : $Id: dyn_wup.f 8506 2016-01-28 10:09:58Z simonc $
! $URL: http://svn-ae/ae/COMMON/stand-alone/atmos_models/socrates/tags/07.18.01/src/dyn_wup.f $
!-----------------------------------------------------------------------

      subroutine DYN_WUP( )
!-----------------------------------------------------------------------
!   	... Set Upper boundary conditions wu on vert wind w as 
!                                  wd1=g*q/T/bv/bv (calc in DYN_BVFREQ). 
!           Correct wu as a fct of latitude for global mass balance. 
!           Set xu, the UBC on streamfct, as the the lat integral of wu
!-----------------------------------------------------------------------
      use GRID_DIMS, only : lmax, niz
      use PHYS_CST, only : R0
      use UPPER, only : wu, xu          ! output
      use ALLCO, only : phir, dlatr
      use VEN8,  only : wd1

      implicit none

!-----------------------------------------------------------------------
!   	... Local variables
!-----------------------------------------------------------------------
      integer :: i
      real    :: a, b
      real, save :: dy
      real, dimension(lmax) :: side, side2
      logical, save :: entered = .false.

      if( .not. entered ) then
         dy = dlatr * R0 * 1.e3       ! latitude step, meters
         entered = .true.
      end if
      
      wu(:) = wd1(:,niz)     ! was calculated in DYN_BVFREQ
      side(:) =  wu(:) * COS( phir(:) )
      side2(1:lmax-1) = 0.5*(side(1:lmax-1) + side(2:35))
      b =  dlatr * SUM( side2(1:lmax-1) )
      wu(:) = wu(:) - .5*b  
      
      xu(1) = wu(1)*COS( phir(1) )*dy 
      a = xu(1)
      do i = 2, lmax
         a = a + .5*(wu(i-1)*COS( phir(i-1) )+ wu(i)*cos(phir(i)) )*dy
         xu(i) = a
      end do

      end subroutine DYN_WUP
