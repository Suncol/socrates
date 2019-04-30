! subversion Id for THIS file : $Id: dyn_diffus_heat.f 8506 2016-01-28 10:09:58Z simonc $
! $URL: http://svn-ae/ae/COMMON/stand-alone/atmos_models/socrates/tags/07.18.01/src/dyn_diffus_heat.f $
!-----------------------------------------------------------------------
      subroutine DYN_DIFFUS_HEAT( )
!-----------------------------------------------------------------------
!     	... Calculates forcing term for streamfunction equation
!-----------------------------------------------------------------------
      use GRID_DIMS, only : lmax, niz
      use SIM_CONTROLS, only : mlt_sw
      use SPC_NAMES, only : vid_o2, vid_o3p
      use SPECIES, only : qn2da
      use ALLCO, only : zkm
      use CONC, only  : hm2d
      use VEN1, only  : t2d
      use VEN3, only  : tz, tzz
      use VEN9, only  : xkz, xkzz
      use VEN10, only : xtz, xtzz
      use HEAT_TERMS, only : dh, dxt

      implicit none

!-----------------------------------------------------------------------
!     	... Parameters
!-----------------------------------------------------------------------
      real, parameter :: hi = 1. / 7.00e03
      real, parameter :: kappa = 0.285         ! Pot temp = temp * (p0/p)**kappa

!-----------------------------------------------------------------------
!     	... Local variables
!-----------------------------------------------------------------------
      integer :: k, l, iz
      real, dimension(lmax,niz) :: ptz, ptzz, ro, t69
      real, dimension(niz), save :: t2pt
      logical, save :: entered = .false.
      
      if( .not. entered ) then
         t2pt(:) = EXP( kappa * zkm(:) * hi )
         entered = .true.
      end if

!-----------------------------------------------------------------------
!     	... divergence of eddy diffusive flux of heat: see 2d term
!           of RHS of eq (7) in G/S:1983, and eq (19) in 
!           Brasseur/et-al:1990 . ptz=dteta/dz where teta=pot temp
!           dh was calculated in DIFTRA
!-----------------------------------------------------------------------
      do l = 1, lmax
         ptz(l,:)  = t2pt(:) * ( kappa*t2d(l,:)*hi + tz(l,:) )
         ptzz(l,:) = t2pt(:) *
     $   ( tzz(l,:) + 2.*kappa*tz(l,:)*hi + kappa*kappa*t2d(l,:)*hi*hi )
      end do
      dh(:,:) = - ptz * xkzz - xkz * ptzz + xkz * ptz * hi          ! K/s

      if( mlt_sw(3) == 1 ) then
!-------------------------------------------------------------------
! 	... Molecular diffusive heat flux gradient - as dh but using xtz
!-------------------------------------------------------------------
         dxt =  ptz*xtzz + xtz*ptzz - xtz * ptz * hi                  ! K/s
      else
	xtz = 0.
	xtzz = 0.
	dxt = 0.
      end if

!      write(*,'(6(a,es11.3))') 'DYNF_DIFFUS_HEAT at (60,115): ptz: ',
!     $   ptz(60,115),' ; ptzz= ',ptzz(30,116),' ; xkz= ',xkz(30,116),
!     $   ' ; xkzz= ',xkzz(30,116),' ; dh: ',dh(30,116)
!      write(*,'(9x,6(a,es11.3))') 'qn2da(O2): ',qn2da(30,116,vid_o2),
!     $ ' ; hm2d: ',hm2d(30,116),' ; wmole: ',wmole(30,116),
!     $ ' ; ro: ',ro(30,116),' ; Cp: ',Cp(30,116)
!      write(*,'(9x,6(a,es11.3))') ' ; xtz= ',xtz(30,116),
!     $   ' ; xtzz= ',xtzz(30,116),' ; dxt: ',dxt(30,116)

      end subroutine DYN_DIFFUS_HEAT
