! subversion Id for THIS file : $Id: dyn_adam.f 8506 2016-01-28 10:09:58Z simonc $
! $URL: http://svn-ae/ae/COMMON/stand-alone/atmos_models/socrates/tags/07.18.01/src/dyn_adam.f $
!-----------------------------------------------------------------------

      subroutine DYN_ADAM()
!-----------------------------------------------------------------------
!     	... Calculate coefficients :a,b,c,d,e,f,g
!           involved in the streamfunction equation
!-----------------------------------------------------------------------
      use PHYS_CST, only : R0
      use GRID_DIMS, only : lmax
      use ALLCO, only : phir, dlatr
      use VEN2, only : u
      use VEN3, only : ff, fl, ul, uz
      use VEN7, only : xa, xb, xc, xd, xe, xf, xg               ! output
      use VEN8, only : bv

      implicit none

!-----------------------------------------------------------------------
!	... Parameters
!-----------------------------------------------------------------------
      real, parameter :: hi = 1. / 7.0e03
      real, parameter :: s1 = 1.e-3 / R0
      real, parameter :: rg = 2.87e02
      real, parameter :: dzi = 1.e-3
      real, parameter :: dz2i = .5 * dzi
      real, parameter :: dzsqi = dzi * dzi

!-----------------------------------------------------------------------
!	... Local variables
!-----------------------------------------------------------------------
      integer :: l
      real    :: const0
      real, dimension(121) :: eps, s5, s6, s7, s8
      real, dimension(121) :: cz, czz, czy, cy, cyy
      real, dimension(35), save  :: s3, s4
      real, save :: dyi, dy2i, dysqi
      logical, save :: entered = .false.
      
      if( .not. entered ) then
         dyi = 1.e-3 / (R0*dlatr)
         dy2i = .5 * dyi
         dysqi = dyi * dyi
         s3(:) = TAN( phir(:) ) * s1
         s4(:) = ff(:) * hi
         entered = .true.
      end if
      
      do l = 1, lmax
         s5(:) = ff(l) + u(l,:) * s3(l) - s1 * ul(l,:)
         s6(:) = bv(l,:) * bv(l,:)
         czz(:) = ff(l) * s5(:)
	 where( czz(:) <= 0. )
	    czz(:) = 1.e-12
	 end where
         cz(:)  = -s4(l) * s5(:) + uz(l,:) * (2.*ff(l)*s3(l) + s1*fl(l))
         czy(:) = 2. * ff(l) * uz(l,:)
         cy(:)  = s6(:) * s3(l) - s4(l) * uz(l,:)
         cyy(:) = s6(:)
         eps(:) = (4.*cyy(:)*czz(:)) - (czy(:)*czy(:))
	 where( eps(:) <= 0. )
	    czy(:) = 0.
	 end where
         s7(:) = czz(:) * dzsqi
         s8(:) = cyy(:) * dysqi
         xa(l,:) = -2.*(s7(:) + s8(:))
         xb(l,:) = s7(:) + cz(:) * dz2i
         xc(l,:) = s7(:) - cz(:) * dz2i
         xd(l,:) = s8(:) + cy(:) * dy2i
         xe(l,:) = s8(:) - cy(:) * dy2i
         xf(l,:) = czy(:) * dz2i * dy2i
         xg(l,:) = -xf(l,:)
      end do

      do l = 15,21
         const0 = .125 * REAL(l-14)
         xa(l,:) = xa(14,:) + const0*(xa(22,:) - xa(14,:))
         xb(l,:) = xb(14,:) + const0*(xb(22,:) - xb(14,:))
         xc(l,:) = xc(14,:) + const0*(xc(22,:) - xc(14,:))
         xd(l,:) = xd(14,:) + const0*(xd(22,:) - xd(14,:))
         xe(l,:) = xe(14,:) + const0*(xe(22,:) - xe(14,:))
         xf(l,:) = xf(14,:) + const0*(xf(22,:) - xf(14,:))
         xg(l,:) = xg(14,:) + const0*(xg(22,:) - xg(14,:))
      end do

      end subroutine DYN_ADAM
