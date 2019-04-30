! subversion Id for THIS file : $Id: imp_slv.F 8506 2016-01-28 10:09:58Z simonc $
! $URL: http://svn-ae/ae/COMMON/stand-alone/atmos_models/socrates/tags/07.18.01/src/preproc/in/cache/imp_slv.F $
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
      use SPC_NAMES
      use BOUNDARIES, only : lbc, ubc
      use SIM_CONTROLS, only : chem_itermax
      use DIAG_CONTROLS, only : diags_lat, zdiag
      use TIME_CONTROLS, only : TIMING

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

      integer ::   nr_iter, i, j, m, mc, mp, con_cnt
     $,            k, kl, ku, ofl, ofu
      real :: dti                            ! 1/delt
      real, dimension(4,NZCNT) ::
     $             sys_jac
     $,            lin_jac
      real, dimension(4,RXNCNT) :: 
     $             lrates
      real, dimension(4,HETCNT) :: 
     $             lhet
      real, dimension(4,CLSCNT4) :: 
     $             cls_dens
     $,            forcing
     $,            iter_invariant
     $,            prod
     $,            loss
     $,            wrk
      real, dimension(PCNST) :: epsil = epsilon
      real, dimension(4,PCNST) :: 
     $             lbase = 0.
     $,            base_sav = 0.
     $,            ldens_n
     $,            lcvl
     $,            lfrl
      real, dimension(PLEV,CLSCNT4) :: ind_prd
      real, dimension(CLSCNT4) :: l1_norm
      real    ::   timer 
      real    ::   maxerr
      logical ::   alt_con(4)
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
      call INDPRD( 4,
     $             ind_prd,
     $             base_dens,
     $             extfrc,
     $             reaction_rates )
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

      dti = 1. / dts

!-----------------------------------------------------------------------
!   	... Overall spatial (altitude) loop
!-----------------------------------------------------------------------
      ofu = 0
      do
	 ofl = ofu + 1
	 ofu = MIN( PLEV, ofl+3 )
	 kl = 1
	 ku = ofu - ofl + 1
         do m = 1,RXNCNT
	    lrates(kl:ku,m) = reaction_rates(ofl:ofu,m)
         end do
         do m = 1,HETCNT
	    lhet(kl:ku,m) = het_rates(ofl:ofu,m)
         end do
         do m = 1,PCNST
	    ldens_n(kl:ku,m) = dens_n(ofl:ofu,m)
	    lcvl(kl:ku,m) = lcv(ofl:ofu,m)
	    lfrl(kl:ku,m) = lfr(ofl:ofu,m)
         end do

!-----------------------------------------------------------------------      
!        ... Transfer from base to class array
!-----------------------------------------------------------------------      
         do m = 1,CLSCNT4
            mc = clsmap(m,4)
            mp = permute(m,4)
            cls_dens(kl:ku,mp) = base_dens(ofl:ofu,mc)
	    lbase(kl:ku,m) = base_dens(ofl:ofu,m)
	    base_sav(kl:ku,m) = lbase(kl:ku,m)
         end do
         do m = 1,CLSCNT4
	    mp = permute_orig(m,4)
	    mc = clsmap(mp,4)
            iter_invariant(kl:ku,m) = dti * ldens_n(kl:ku,mc)
     $                              + ind_prd(ofl:ofu,m)
         end do
!-----------------------------------------------------------------------      
!        ... The linear component
!-----------------------------------------------------------------------      
         timer = SECOND( )
         lin_jac(kl:ku,1:NZCNT) = 0.
         call LINMAT( lin_jac,
     $                lbase,
     $                lrates,
     $                lhet,
     $                kl, ku )
         do j = 1,CLSCNT4
            m = diag_map(j)
            mc = clsmap(j,4)   ! was "permute_orig(j,4)" in Stacy's, probably a bug with no consequence
            lin_jac(kl:ku,m) = lin_jac(kl:ku,m)
     $                       - (dti + lcvl(kl:ku,mc) +lfrl(kl:ku,mc))
         end do
         times(4) = times(4) + SECOND() - timer

!=======================================================================
!        The Newton-Raphson iteration for F(y) = 0
!=======================================================================
         alt_con(kl:ku) = .false.
         do nr_iter = 1, chem_itermax
!-----------------------------------------------------------------------      
!        ... The non-linear component
!-----------------------------------------------------------------------      
            timer = SECOND( )
            sys_jac(kl:ku,1:NZCNT) = 0.
            call NLNMAT( sys_jac
     $,                  lbase
     $,                  lrates
     $,                  alt_con
     $,                  kl, ku )
            sys_jac(kl:ku,1:NZCNT) = sys_jac(kl:ku,1:NZCNT) 
     $                             + lin_jac(kl:ku,1:NZCNT)
            times(5) = times(5) + SECOND() - timer
            timer    = SECOND( )
!-----------------------------------------------------------------------      
!         ... Factor the "system" matrix
!-----------------------------------------------------------------------      
            call LU_FAC( sys_jac, kl, ku, alt_con )
            times(2) = times(2) + SECOND( ) - timer

!-----------------------------------------------------------------------
!   	... Form F(y)
!-----------------------------------------------------------------------
            timer = SECOND( )
            call IMP_PROD_LOSS( prod
     $,                         loss
     $,                         lbase
     $,                         lrates
     $,                         lhet
     $,                         alt_con
     $,                         kl, ku )

!-----------------------------------------------------------------------
!   	... Special section to handle convection and fronts
!-----------------------------------------------------------------------
	    do m = 1,CLSCNT4
	       mc = clsmap(m,4)
               mp = permute(m,4)
	       where( .not. alt_con(kl:ku) )
	          loss(kl:ku,mp) = loss(kl:ku,mp)
     $           + ( lcvl(kl:ku,mc) + lfrl(kl:ku,mc) ) * lbase(kl:ku,mc)
	       end where
	    end do
            do k = kl, ku
	       if( .not. alt_con(k) ) then
                  forcing(k,:) = cls_dens(k,:)*dti
     $              - ( iter_invariant(k,:) + prod(k,:) - loss(k,:) )
               end if
            end do
            times(8) = times(8) + SECOND() - timer

!-----------------------------------------------------------------------      
!         ... Solve for the mixing ratio at t(n+1)
!-----------------------------------------------------------------------      
            timer = SECOND( )
	    call LU_SLV( sys_jac, forcing, kl, ku, alt_con )
            times(3) = times(3) + SECOND( ) - timer
            do k = kl, ku
               wrk(k,:) = 0.
               if( .not. alt_con(k) ) then
                  cls_dens(k,:) = cls_dens(k,:) + forcing(k,:)

!-----------------------------------------------------------------------      
!         Transfer latest cls_dens back to "base" array
!-----------------------------------------------------------------------      
                  do m = 1,CLSCNT4
                     mc = clsmap(m,4)
                     mp = permute(m,4)
                     if( cls_dens(k,mp) < 1.e-99 ) cls_dens(k,mp) = 0.
                     lbase(k,mc) = cls_dens(k,mp)

!-----------------------------------------------------------------------      
!        Calc the relative difference from one iteration to the next.
!        The criteria for low values *must* be easier than above, or else
!        numerical oscillations can occur.
!-----------------------------------------------------------------------      
                     if( lbase(k,mc) > 1.e-9 ) wrk(k,m) = forcing(k,mp)
     $                                                  / lbase(k,mc)
                  end do
               end if
            end do
            
            do m = 1, CLSCNT4
               mc = clsmap(m,4)
               if( ofl == 1 .and. lbc(mc)%is_vmr ) wrk(kl,m) = 0.
	       if( ofu==PLEV .and. ubc(mc)%is_vmr ) wrk(ku,m) = 0.
               l1_norm(m) = MAXVAL( ABS( wrk(kl:ku,m) )
     $                             ,mask=.not. alt_con(kl:ku) )
            end do
            do k = kl,ku
	       alt_con(k) = alt_con(k) .or. ALL(ABS(wrk(k,:)) <= eps(:))
	    end do
	    con_cnt = COUNT( l1_norm(:) <= eps(:) )
            if(                            !         if( diags_lat .or. 
     $           (nr_iter==chem_itermax .and. con_cnt/=CLSCNT4) ) then
               call IMP_SLV_DIAGS( )
            end if
            if( con_cnt == CLSCNT4 ) exit                ! nr_iter loop
	    base_sav(kl:ku,:) = lbase(kl:ku,:)           ! for IMP_SLV_DIAGS
         end do                                          ! nr_iter loop
            
         base_dens(ofl:ofu,1:PCNST) = lbase(kl:ku,1:PCNST)

         do m = 1,CLSCNT4
	    mc = clsmap(m,4)
	    mp = permute(m,4)
            loss_out(ofl:ofu,mc) = loss(kl:ku,mp)
         end do

         if( diags_lat .and. zdiag >= ofl .and. zdiag <= ofu ) 
     $      write(*,'(4(a,i4),2(a,f8.4),a)') ' IMP_SLV@ (',lat,',',
     $       zdiag,'): it= ',it,' ; nr_iter= ',nr_iter,' ; cal_day= ',
     $       date%cal_day,' ; slt= ',slt,' converged'

	 if( ofu >= PLEV ) exit                          ! altitude loop
      end do                                             ! altitude loop

      contains

!=======================================================================

      subroutine IMP_SLV_DIAGS( )
!-----------------------------------------------------------------------
!         ... Diagnostics utility for chem solver IMP_SLV (preprocessed.f)
!-----------------------------------------------------------------------
      use SPC_NAMES          
      use TRACNM, only : solsym
      use DIAG_CONTROLS, only : debug
        
      implicit none

      real :: maxreldiff
      integer :: m, mc, mp, iz(1)

      write(*,'(4(a,i4),2(a,f8.4),2(a,es9.3))') ' IMP_SLV@ (',
     $ lat,',',zdiag,'): it= ',it,' ; nr_iter= ',nr_iter,' ; cal_day= ',
     $ date%cal_day,' ; slt= ',slt,
     $ ' ; new_dens(OH)= ',lbase(zdiag-ofl+1,vid_oh),
     $ ' ; new_dens(ClO)= ',lbase(zdiag-ofl+1,vid_clo)
     
      if( nr_iter == chem_itermax .and. con_cnt /= CLSCNT4 ) then
         write(*,*) ' IMP_SLV warning: ',CLSCNT4-con_cnt,
     $                    ' species failed to converge @ lat = ',lat
         write(*,'(a,i2,''/'',i2,''/'',i4,a,i4)') '   Sim date ',
     $      date%month,date%day,date%year, ' ; time step ',it
     
         if( debug ) then
            write(*,*)
            write(*,'(7a16)') 'species', 'eps', 'MAX(reldiff)', 'at iz',
     $                          'ni_t','ni_t+1_m', 'ni_t+1_m+1'
            write(*,*)
            do m = 1, CLSCNT4
               if( l1_norm(m) > eps(m) ) then
                  mp = permute(m,4)
                  mc = clsmap(m,4)
                  iz = MAXLOC( ABS( wrk(:,m) ) )
                  write(*,'(a16,2es16.4,i16,3es16.4)') solsym(mc), 
     $             eps(m), l1_norm(m), ofl+iz(1)-1, ldens_n(iz(1),mc), 
     $             base_sav(iz(1),mc), lbase(iz(1),mc)
               end if
            end do
            stop 'IMP_SLV_DIAGS: species failed to converge, see stdout'
         end if
      end if

      end subroutine IMP_SLV_DIAGS

      end subroutine IMP_SLV

!=======================================================================

