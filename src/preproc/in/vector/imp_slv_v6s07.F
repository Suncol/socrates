! subversion Id for THIS file : $Id: imp_slv_v6s07.F 8506 2016-01-28 10:09:58Z simonc $
! $URL: http://svn-ae/ae/COMMON/stand-alone/atmos_models/socrates/tags/07.18.01/src/preproc/in/vector/imp_slv_v6s07.F $
!-----------------------------------------------------------------------

!=======================================================================

      module IMP_SLV0

      implicit none

!-----------------------------------------------------------------------      
!        Newton-Raphson iteration limits
!-----------------------------------------------------------------------      
      integer, parameter :: iterdim      = 192
      integer, parameter :: factor_limit = 13
      real, parameter    :: epsilon = 1.e-3

      real :: eps(CLSCNT4)
      logical ::   factor(iterdim)

      CONTAINS

      subroutine IMP_SLV_INTI()

      use SPC_NAMES
      use CHEM_MODS
      use SIM_CONTROLS, only : chem_itermax

      implicit none

      integer :: mc, m
      real :: epsil(PCNST) = epsilon

      if( chem_itermax > iterdim ) then
         write(*,*) 'IMP_SLV_INTI, fatal error: chem_itermax=',
     $       chem_itermax,' must be < iterdim= ',iterdim
         write(*,*)'Decrease chem_itermax (namelist control_parms)'
         write(*,*)'  or increase iterdim (preproc/in/vector/imp_slv.F)'
         stop 'IMP_SLV_INTI: chem_itermax > iterdim, fatal error'
      end if
      epsil(vid_o3) = 1.e-4
      epsil(vid_oh) = 1.e-4
      epsil(vid_ho2) = 1.e-4
      epsil(vid_no2) = 1.e-4
      epsil(vid_no) = 1.e-4
      do m = 1,CLSCNT4
         mc = clsmap(m,4)
	 eps(m) = epsil(mc)
      end do
      factor(:chem_itermax) = .true.

      end subroutine IMP_SLV_INTI

      end module IMP_SLV0

!=======================================================================

      subroutine IMP_SLV( base_dens
     $,                   dens_n
     $,                   reaction_rates
     $,                   het_rates
     $,                   extfrc
     $,                   prod_no
     $,                   pcv
     $,                   lcv
     $,                   pfr
     $,                   lfr
     $,                   loss_out
     $,                   dts
     $,                   lat
     $,                   it
     $,                   slt
     $,                   date
     $,                   times )
!-----------------------------------------------------------------------
!      	... Imp_sol advances the volumetric mixing ratio
!           forward one time step via the fully implicit
!           Euler scheme
!-----------------------------------------------------------------------
      use CHEM_MODS
      use BOUNDARIES, only : lbc, ubc
      use TIME_CONTROLS, only : TIMING
      use IMP_SLV0
      use SPC_NAMES, only : vid_no          
      use SIM_CONTROLS, only : chem_itermax, diags_lat

      implicit none

!-----------------------------------------------------------------------
!     	... Dummy args
!-----------------------------------------------------------------------
      integer, intent(in) ::   lat
      real, intent(in)    ::   dts             ! time step in seconds
      real, intent(in)    ::   reaction_rates(PLEV,RXNCNT)
     $,                        het_rates(PLEV,HETCNT)
     $,                        extfrc(PLEV,EXTCNT)
      real, dimension(PLEV), intent(in) :: prod_no
      real, dimension(PLEV,PCNST), intent(in) :: pcv, pfr, lcv, lfr
      real, intent(in)    ::   dens_n(PLEV,PCNST)    ! input densities at timestep n
      real, intent(inout) ::   base_dens(PLEV,PCNST) ! output densities at timestep n+1
      real, intent(out)   ::   loss_out(PLEV,PCNST)  ! output loss rates (for diags)
      real, intent(inout) ::   times(*)
      real, intent(in) :: slt                  ! for diags
      integer, intent(in) ::   it              ! for diags
      type( TIMING ), intent(in) :: date       ! for diags

!-----------------------------------------------------------------------
!     	... The local variables and arrays
!-----------------------------------------------------------------------
      integer, parameter :: specified = 0

      integer ::   nr_iter, j, m, mc, mp, con_cnt
      real :: dti                                   ! 1/dts
      real, dimension(PLEV,NZCNT) ::
     $             sys_jac
     $,            lin_jac
      real, dimension(PLEV,CLSCNT4) :: 
     $             cls_dens
     $,            forcing
     $,            ind_prd
     $,            iter_invariant
     $,            prod
     $,            loss
     $,            wrk
      real, dimension(PLEV,PCNST) :: ldens_n
      real, dimension(CLSCNT4) :: l1_norm
      real    ::   timer 

!-----------------------------------------------------------------------
!       ... Function declarations
!-----------------------------------------------------------------------
      real, external :: SECOND

!-----------------------------------------------------------------------      
!        ... If there is "independent" production put it in the forcing
!        ... Set the iteration invariant part of the function F(y)
!-----------------------------------------------------------------------      
      timer = SECOND( )
      call INDPRD( 4
     $,            ind_prd
     $,            base_dens
     $,            extfrc
     $,            reaction_rates )
!-----------------------------------------------------------------------
!   	... Special section to handle convection and fronts
!-----------------------------------------------------------------------
      do m = 1,CLSCNT4
         mc = clsmap(m,4)
         mp = permute(m,4)
         ind_prd(:,mp) = ind_prd(:,mp) + pcv(:,mc) + pfr(:,mc)
      end do
!-----------------------------------------------------------------------
!   	... Special section to handle extraneous no production
!-----------------------------------------------------------------------
      m = base2cls(vid_no,4)
      mp = permute(m,4)
      ind_prd(:,mp) = ind_prd(:,mp) + prod_no(:)
      times(8) = times(8) + SECOND() - timer

      ldens_n(:,:) = dens_n(:,:)
      dti = 1. / dts

!-----------------------------------------------------------------------      
!        ... Transfer from base to class array
!-----------------------------------------------------------------------      
      do m = 1,CLSCNT4
         mc = clsmap(m,4)
         mp = permute(m,4)
         cls_dens(:,mp) = base_dens(:,mc)
      end do
      do m = 1,CLSCNT4
         mp = permute_orig(m,4)
         mc = clsmap(mp,4)
         iter_invariant(:,m) = dti * ldens_n(:,mc) + ind_prd(:,m)
      end do
!-----------------------------------------------------------------------      
!        ... The linear component
!-----------------------------------------------------------------------      
      timer = SECOND( )
      lin_jac(:,:) = 0.
       call LINMAT( lin_jac
     $,             base_dens
     $,             reaction_rates
     $,             het_rates )
      do j = 1,CLSCNT4
         m = diag_map(j)
         mc = clsmap(j,4)   ! was 'permute_orig(j,4)" up to v5s19, probably a bug with no consequence
         lin_jac(:,m) = lin_jac(:,m) - (dti + lcv(:,mc) + lfr(:,mc))
      end do
      times(4) = times(4) + SECOND() - timer

!=======================================================================
!        The Newton-Raphson iteration for F(y) = 0
!=======================================================================
      do nr_iter = 1, chem_itermax
!-----------------------------------------------------------------------      
!        ... The non-linear component
!-----------------------------------------------------------------------      
        if( factor(nr_iter) ) then
            timer = SECOND( )
            sys_jac(:,:) = 0.
            call NLNMAT( sys_jac
     $,                  base_dens
     $,                  reaction_rates )
            sys_jac(:,:) = sys_jac(:,:) + lin_jac(:,:)
            times(5) = times(5) + SECOND() - timer
            timer    = SECOND( )
!-----------------------------------------------------------------------      
!         ... Factor the "system" matrix
!-----------------------------------------------------------------------      
            call LU_FAC( sys_jac )
            times(2) = times(2) + SECOND( ) - timer
         end if      
!-----------------------------------------------------------------------
!   	... Form F(y)
!-----------------------------------------------------------------------
         timer = SECOND( )
         call IMP_PROD_LOSS( prod
     $,                      loss
     $,                      base_dens
     $,                      reaction_rates
     $,                      het_rates )

!-----------------------------------------------------------------------
!   	... Special section to handle convection and fronts
!-----------------------------------------------------------------------
         do m = 1,CLSCNT4
            mc = clsmap(m,4)
            mp = permute(m,4)
            loss(:,mp) = loss(:,mp) 
     $                 + (lcv(:,mc) + lfr(:,mc))*base_dens(:,mc)
         end do
         do m = 1,CLSCNT4
            forcing(:,m) = cls_dens(:,m)*dti 
     $                   - (iter_invariant(:,m) + prod(:,m) - loss(:,m))
         end do
         times(8) = times(8) + SECOND() - timer

!-----------------------------------------------------------------------      
!         ... Solve for the mixing ratio at t(n+1)
!-----------------------------------------------------------------------      
         timer = SECOND( )
	 call LU_SLV( sys_jac, forcing )
         times(3) = times(3) + SECOND( ) - timer
	 do m = 1,CLSCNT4
            cls_dens(:,m) = cls_dens(:,m) + forcing(:,m)
         end do
!-----------------------------------------------------------------------      
!         Transfer latest cls_dens back to "base" array
!-----------------------------------------------------------------------      
         do m = 1,CLSCNT4
            mc = clsmap(m,4)
            mp = permute(m,4)
	    where( cls_dens(:,mp) < 0. )
	       cls_dens(:,mp) = .01*dens_n(:,mc)
	    end where
	    base_dens(:,mc) = cls_dens(:,mp)
	    where( cls_dens(:,mp) /= 0. )
	       wrk(:,m) = forcing(:,mp) / cls_dens(:,mp)
	    elsewhere
	       wrk(:,m) = 0.
	    end where
	    if( lbc(mc) == specified ) then
	       wrk(1,m) = 0.
	    end if
	    if( ubc(mc) == specified ) then
	       wrk(PLEV,m) = 0.
	    end if
	    l1_norm(m) = MAXVAL( ABS( wrk(:,m) ) )
         end do
	 con_cnt = COUNT( l1_norm(:) <= eps(:) )
         if( diags_lat .or. 
     $           (nr_iter==chem_itermax .and. con_cnt/=CLSCNT4) ) then
            call DIAGS_IMP_SLV( lat, date, slt, it, 
     $                          nr_iter, con_cnt, l1_norm, 
     $                          dens_n, base_dens )
         end if
         if( con_cnt == CLSCNT4 ) exit

      end do              ! nr_iter loop

      do m = 1,CLSCNT4
         mc = clsmap(m,4)
         mp = permute(m,4)
         loss_out(:,mc) = loss(:,mp)
      end do

      end subroutine IMP_SLV

!=======================================================================

