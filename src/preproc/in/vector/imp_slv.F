! subversion Id for THIS file : $Id: imp_slv.F 8506 2016-01-28 10:09:58Z simonc $
! $URL: http://svn-ae/ae/COMMON/stand-alone/atmos_models/socrates/tags/07.18.01/src/preproc/in/vector/imp_slv.F $
!-----------------------------------------------------------------------

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
      use SPC_NAMES
      use SIM_CONTROLS, only : chem_itermax, diags_lat, zdiag

      implicit none

!-----------------------------------------------------------------------      
!        ... Parameters
!-----------------------------------------------------------------------      
      real, parameter    :: epsilon = 1.e-3
      real, parameter    :: epsilon_strong = 1.e-4

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
     $,            lcls_dens      ! for diags
     $,            forcing
     $,            ind_prd
     $,            iter_invariant
     $,            prod
     $,            loss
     $,            wrk
      real, dimension(PCNST) :: epsil = epsilon
      real, dimension(PLEV,PCNST) :: ldens_n
      real, dimension(CLSCNT4) :: l1_norm
      real    ::   timer 
      real, dimension(CLSCNT4), save :: eps
      logical, save :: entered = .false.
      real, external :: SECOND         !   Function declaration

      if( .not. entered ) then
         epsil(vid_o3) = epsilon_strong
         epsil(vid_oh) = epsilon_strong
         epsil(vid_h) = epsilon_strong
         epsil(vid_ho2) = epsilon_strong
         epsil(vid_no2) = epsilon_strong
         epsil(vid_no) = epsilon_strong
         do m = 1,CLSCNT4
            mc = clsmap(m,4)
	    eps(m) = epsil(mc)
         end do
         entered = .true.
      end if

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
!   	... Special section to handle extraneous NO production
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
      lcls_dens(:,:) = cls_dens(:,:)        ! for diags
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
         timer = SECOND( )
         sys_jac(:,:) = 0.
         call NLNMAT( sys_jac
     $,               base_dens
     $,               reaction_rates )
         sys_jac(:,:) = sys_jac(:,:) + lin_jac(:,:)
         times(5) = times(5) + SECOND() - timer
         timer    = SECOND( )
!-----------------------------------------------------------------------      
!         ... Factor the "system" matrix
!-----------------------------------------------------------------------      
         call LU_FAC( sys_jac )
         times(2) = times(2) + SECOND( ) - timer

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
	    where( cls_dens(:,mp) < 1.e-99 )
	       cls_dens(:,mp) = 0.
	    end where
	    base_dens(:,mc) = cls_dens(:,mp)
!-----------------------------------------------------------------------      
!        Calc the relative difference from one iteration to the next.
!        The criteria for low values *must* be easier than above, or else
!        numerical oscillations can occur.
!-----------------------------------------------------------------------      
	    where( cls_dens(:,mp) > 1.e-9 )  
	       wrk(:,m) = forcing(:,mp) / cls_dens(:,mp)
	    elsewhere
	       wrk(:,m) = 0.
	    end where
	    if( lbc(mc)%is_vmr ) wrk(1,m) = 0.
	    if( ubc(mc)%is_vmr ) wrk(PLEV,m) = 0.
	    l1_norm(m) = MAXVAL( ABS( wrk(:,m) ) )
         end do
	 con_cnt = COUNT( l1_norm(:) <= eps(:) )
         if(                            !         if( diags_lat .or. 
     $           (nr_iter==chem_itermax .and. con_cnt/=CLSCNT4) ) then
            call IMP_SLV_DIAGS( )
         end if
         if( con_cnt == CLSCNT4 ) exit
         lcls_dens(:,:) = cls_dens(:,:)        ! for diags
      end do              ! nr_iter loop

      do m = 1,CLSCNT4
         mc = clsmap(m,4)
         mp = permute(m,4)
         loss_out(:,mc) = loss(:,mp)
      end do

      if( diags_lat ) write(*,'(4(a,i4),2(a,f8.4),a)') ' IMP_SLV@ (',
     $ lat,',',zdiag,'): it= ',it,' ; nr_iter= ',nr_iter,' ; cal_day= ',
     $       date%cal_day,' ; slt= ',slt,' converged'
     
      contains

!=======================================================================

      subroutine IMP_SLV_DIAGS( )
!-----------------------------------------------------------------------
!         ... Diagnostics utility for chem solver IMP_SLV (preprocessed.f)
!-----------------------------------------------------------------------
      use SPC_NAMES          
      use TRACNM, only : solsym
        
      implicit none

      real :: maxreldiff
      integer :: m, mc, mp, iz(1)

      write(*,'(4(a,i4),2(a,f8.4),2(a,es9.3))') ' IMP_SLV@ (',
     $ lat,',',zdiag,'): it= ',it,' ; nr_iter= ',nr_iter,' ; cal_day= ',
     $ date%cal_day,' ; slt= ',slt,
     $ ' ; new_dens(OH)= ',base_dens(zdiag,vid_oh),
     $ ' ; new_dens(ClO)= ',base_dens(zdiag,vid_clo)
     
      if( nr_iter == chem_itermax .and. con_cnt /= CLSCNT4 ) then
         write(*,*) ' IMP_SLV: ',CLSCNT4-con_cnt,
     $                    ' species failed to converge @ lat = ',lat
         write(*,'(a,i2,''/'',i2,''/'',i4,a,i4)') '   Sim date ',
     $      date%month,date%day,date%year, ' ; time step ',it
         write(*,*)
         write(*,'(7a16)') 'species', 'eps', 'MAX(reldiff)', 'at iz',
     $                          'ni_t','ni_t+1_m', 'ni_t+1_m+1'
         write(*,*)
         do m = 1, nbcon
            if( l1_norm(m) > eps(m) ) then
               mp = permute(m,4)
               mc = clsmap(m,4)
               iz = MAXLOC( ABS( wrk(:,m) ) )
               write(*,'(a16,2es16.4,i16,3es16.4)') solsym(mc), 
     $          eps(m), l1_norm(m), iz(1), dens_n(iz(1),mc), 
     $          lcls_dens(iz(1),mp), base_dens(iz(1),mc)
            end if
         end do
         stop 'IMP_SLV_DIAGS: species failed to converge, see stdout'
      end if

      end subroutine IMP_SLV_DIAGS

      end subroutine IMP_SLV

!=======================================================================
