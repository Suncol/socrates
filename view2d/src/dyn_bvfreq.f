! subversion Id for THIS file : $Id: dyn_bvfreq.f 8506 2016-01-28 10:09:58Z simonc $
! $URL: http://svn-ae/ae/COMMON/stand-alone/atmos_models/socrates/tags/07.18.01/src/dyn_bvfreq.f $
!-----------------------------------------------------------------------
      subroutine DYN_BVFREQ( ok )
!-------------------------------------------------------------
!	... Compute the Brunt-Vaisala frequency bv
!-------------------------------------------------------------
      use GRID_DIMS, only : lmax, niz
      use PHYS_CST, only : g0
      use VEN1,     only : t2d
      use TAU0,     only : q
      use VEN3,     only : tz
      use VEN8,     only : bv, wd1

      implicit none

!-----------------------------------------------------------------------
!	... Dummy args
!-----------------------------------------------------------------------
      logical, intent(out) :: ok

!-----------------------------------------------------------------------
!	... Local variables
!-----------------------------------------------------------------------
      integer :: l, k, neg_cnt
      integer :: inds(2)
      real    :: bb(niz)

!-----------------------------------------------------------------------
!	... Check temperature
!-----------------------------------------------------------------------
      ok = .false.
      neg_cnt = COUNT( t2d(:,:) <= 0. )
      if( neg_cnt /= 0 ) then
	 inds(:) = MINLOC( t2d(:,:) )
	 write(*,*) 'DYN_BVFREQ: There are ',neg_cnt,' temps <= 0.'
	 write(*,*) '        Min value = ',MINVAL( t2d(:,:) )
	 write(*,*) '        Min indicies = ',inds
	 return
      end if

      ok = .true.
      do l = 1, lmax
         tz(l,:niz) = MAX( -6.5e-3,tz(l,:niz) )
         bb(:) = g0 / t2d(l,:niz) * (tz(l,:niz) + g0/1005.)
	 do k = 1,niz
             if( bb(k) < 0. ) then
               bv(l,k) = SQRT( 7.5e-3*g0/t2d(l,k) )
            else
               bv(l,k) = SQRT( bb(k) )
            end if
         end do
         wd1(l,:niz) = (q(l,:niz)*g0)
     $                / (t2d(l,:niz)*bv(l,:niz)*bv(l,:niz))
      end do

      end subroutine DYN_BVFREQ
