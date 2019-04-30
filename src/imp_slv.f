! subversion Id for THIS file : $Id: imp_slv.f 8506 2016-01-28 10:09:58Z simonc $
! $URL: http://svn-ae/ae/COMMON/stand-alone/atmos_models/socrates/tags/07.18.01/src/imp_slv.f $
!-----------------------------------------------------------------------

      subroutine IMP_SLV( base_dens
     $,                   dens_n
     $,                   reaction_rates
     $,                   het_rates
     $,                   prod_no
     $,                   loss_out
     $,                   dts
     $,                   lat
     $,                   it
     $,                   slt
     $,                   date )
!-----------------------------------------------------------------------
!      	... Imp_sol advances the volumetric mixing ratio
!           forward one time step via the fully implicit
!           Euler scheme
!-----------------------------------------------------------------------
      use CHEM_MODS
      use SPC_NAMES
      use RXT_NAMES   ! for diags
      use BOUNDARIES, only : lbc, ubc
      use SIM_CONTROLS, only : chem_itermax
      use TIME_CONTROLS, only : TIMING

      implicit none

!-----------------------------------------------------------------------
!     	... Dummy args
!-----------------------------------------------------------------------
      integer, intent(in) ::   lat             ! latitude index
      real, intent(in)    ::   dts             ! time step in seconds
      real, intent(in)    ::   slt             ! for diags
      integer, intent(in) ::   it              ! for diags
      type( TIMING ), intent(in) :: date       ! for diags
      real, intent(in)    ::   reaction_rates(121,202)
     $,                        het_rates(121,7)
      real, dimension(121), intent(in) :: prod_no
      real, dimension(121,56), intent(in)  ::   dens_n  ! input densities at tstep n
      real, dimension(121,56), intent(out) :: base_dens ! output densities at tstep n+1
     $                                      , loss_out  ! output loss rates (for diags)

!-----------------------------------------------------------------------
!     	... The local variables and arrays
!-----------------------------------------------------------------------
      integer ::   nr_iter, i, j, m, mc, mp, con_cnt
     $,            k, kl, ku, ofl, ofu
      real :: epsilon, epsilon_strong, dti           ! dti=1/dts
      real, dimension(4,573) ::
     $             sys_jac
     $,            lin_jac
      real, dimension(4,202) ::
     $             lrates
      real, dimension(4,7) ::
     $             lhet
      real, dimension(4,56) ::
     $             cls_dens
     $,            forcing
     $,            iter_invariant
     $,            prod
     $,            loss
     $,            wrk
      real, dimension(56) :: epsil
      real, dimension(4,56) ::
     $             lbase
     $,            base_sav 
     $,            ldens_n
      real, dimension(121,56) :: ind_prd
      real, dimension(56) :: l1_norm
      real    ::   timer
      real    ::   maxerr
      logical ::   alt_con(4)
      real, dimension(56) :: eps
      
!-----------------------------------------------------------------------
!        ... Initialization
!-----------------------------------------------------------------------
!      if( .not. entered ) then
         if( dts > 1860. ) then           ! chemdtm > 31 min
            epsilon = 5.e-3
            epsilon_strong = 1.e-3
          else
            epsilon = 1.e-3
            epsilon_strong = 1.e-4
         end if
         epsil(:) = epsilon
         epsil(vid_o3) = epsilon_strong
         epsil(vid_oh) = epsilon_strong
         epsil(vid_h) = epsilon_strong
         epsil(vid_ho2) = epsilon_strong
         epsil(vid_no2) = epsilon_strong
         epsil(vid_no) = epsilon_strong
         do m = 1,56
            mc = clsmap(m,4)
	    eps(m) = epsil(mc)
         end do
!         entered = .true.
!      end if
      base_dens(:,:) = dens_n(:,:)
      
!-----------------------------------------------------------------------
!   	... Special section to handle extraneous NO production
!-----------------------------------------------------------------------
      ind_prd(:,:) = 0.
      m = base2cls(vid_no,4)
      mp = permute(m,4)
      ind_prd(:,mp) = ind_prd(:,mp) + prod_no(:)

      dti = 1. / dts

!-----------------------------------------------------------------------
!   	... Overall spatial (altitude) loop
!-----------------------------------------------------------------------
      ofu = 0
      do
	 ofl = ofu + 1
	 ofu = MIN( 121, ofl+3 )
	 kl = 1
	 ku = ofu - ofl + 1
         do m = 1,202
	    lrates(kl:ku,m) = reaction_rates(ofl:ofu,m)
         end do
         do m = 1,7
	    lhet(kl:ku,m) = het_rates(ofl:ofu,m)
         end do
         do m = 1,56
	    ldens_n(kl:ku,m) = dens_n(ofl:ofu,m)
	    lbase(kl:ku,m) = base_dens(ofl:ofu,m)
	    base_sav(kl:ku,m) = lbase(kl:ku,m)
         end do

!-----------------------------------------------------------------------
!        ... Transfer from base to class array
!-----------------------------------------------------------------------
         do m = 1,56
            mc = clsmap(m,4)
            mp = permute(m,4)
            cls_dens(kl:ku,mp) = base_dens(ofl:ofu,mc)
         end do
         do m = 1,56
	    mp = permute_orig(m,4)
	    mc = clsmap(mp,4)
            iter_invariant(kl:ku,m) = dti * ldens_n(kl:ku,mc)
     $                              + ind_prd(ofl:ofu,m)
         end do
!-----------------------------------------------------------------------
!        ... The linear component
!-----------------------------------------------------------------------
         lin_jac(kl:ku,1:573) = 0.
         call LINMAT( lin_jac,
     $                lbase,
     $                lrates,
     $                lhet,
     $                kl, ku )
         do j = 1,56
            m = diag_map(j)
            lin_jac(kl:ku,m) = lin_jac(kl:ku,m) - dti 
         end do

!=======================================================================
!        The Newton-Raphson iteration for F(y) = 0
!=======================================================================
         alt_con(kl:ku) = .false.
         do nr_iter = 1, chem_itermax
!-----------------------------------------------------------------------
!        ... The non-linear component
!-----------------------------------------------------------------------
            sys_jac(kl:ku,1:573) = 0.
            
            call NLNMAT( sys_jac
     $,                  lbase
     $,                  lrates
     $,                  alt_con
     $,                  kl, ku )
            sys_jac(kl:ku,1:573) = sys_jac(kl:ku,1:573)
     $                             + lin_jac(kl:ku,1:573)
!-----------------------------------------------------------------------
!         ... Factor the "system" matrix
!-----------------------------------------------------------------------
            call LU_FAC( sys_jac, kl, ku, alt_con )

!-----------------------------------------------------------------------
!   	... Form F(y)
!-----------------------------------------------------------------------
            call IMP_PROD_LOSS( prod
     $,                         loss
     $,                         lbase
     $,                         lrates
     $,                         lhet
     $,                         alt_con
     $,                         kl, ku )

            do k = kl, ku
	       if( .not. alt_con(k) ) then
                  forcing(k,:) = cls_dens(k,:)*dti
     $              - ( iter_invariant(k,:) + prod(k,:) - loss(k,:) )
               end if
            end do

!-----------------------------------------------------------------------
!         ... Solve for the mixing ratio at t(n+1)
!-----------------------------------------------------------------------
	    call LU_SLV( sys_jac, forcing, kl, ku, alt_con )
            do k = kl, ku
               wrk(k,:) = 0.
               if( .not. alt_con(k) ) then
                  cls_dens(k,:) = cls_dens(k,:) + forcing(k,:)
                  
!-----------------------------------------------------------------------
!         Transfer latest cls_dens back to "base" array
!-----------------------------------------------------------------------
                  do m = 1,56
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

            do m = 1, 56
               mc = clsmap(m,4)
               if( ofl == 1 .and. lbc(mc)%is_vmr ) wrk(kl,m) = 0.
	       if( ofu==121 .and. ubc(mc)%is_vmr ) wrk(ku,m) = 0.
               l1_norm(m) = MAXVAL( ABS( wrk(kl:ku,m) )
     $                             ,mask=.not. alt_con(kl:ku) )
            end do
            do k = kl,ku
	       alt_con(k) = alt_con(k) .or. ALL(ABS(wrk(k,:)) <= eps(:))
	    end do
	    con_cnt = COUNT( l1_norm(:) <= eps(:) )
            if( nr_iter==chem_itermax .and. con_cnt/=56) then
               call IMP_SLV_DIAGS( )
            end if
            if( con_cnt == 56 ) exit                ! nr_iter loop
	    base_sav(kl:ku,:) = lbase(kl:ku,:)           ! for IMP_SLV_DIAGS

         end do                                          ! nr_iter loop

         base_dens(ofl:ofu,1:56) = lbase(kl:ku,1:56)

         do m = 1,56
	    mc = clsmap(m,4)
	    mp = permute(m,4)
            loss_out(ofl:ofu,mc) = loss(kl:ku,mp)
         end do

	 if( ofu >= 121 ) exit                          ! altitude loop
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

      write(lat,'(3(a,i4),2(a,f8.4),2(a,es9.3))') ' IMP_SLV, lat= ',
     $ lat,' ; it= ',it,' ; nr_iter= ',nr_iter,' ; cal_day= ',
     $ date%cal_day,' ; slt= ',slt

      if( nr_iter == chem_itermax .and. con_cnt /= 56 ) then
         write(lat,*) ' IMP_SLV warning: ',56-con_cnt,
     $                    ' species failed to converge @ lat = ',lat
         write(lat,'(a,i2,''/'',i2,''/'',i4,a,i4)') '   Sim date ',
     $      date%month,date%day,date%year, ' ; time step ',it

         if( debug ) then
            write(lat,*)
            write(lat,'(7a16)') 'species', 'eps', 'MAX(reldiff)', 
     $                          'at iz', 'ni_t','ni_t+1_m', 'ni_t+1_m+1'
            write(lat,*)
            do m = 1, 56
               if( l1_norm(m) > eps(m) ) then
                  mp = permute(m,4)
                  mc = clsmap(m,4)
                  iz = MAXLOC( ABS( wrk(:,m) ) )
                  write(lat,'(a16,2es16.4,i16,3es16.4)') solsym(mc),
     $             eps(m), l1_norm(m), ofl+iz(1)-1, ldens_n(iz(1),mc),
     $             base_sav(iz(1),mc), lbase(iz(1),mc)
               end if
            end do
            write(*,'(a,i2)') 'IMP_SLV: failed to cvg, see fort.',lat
            stop 'IMP_SLV_DIAGS: species failed to cvg at some lat'
         end if
      end if

      end subroutine IMP_SLV_DIAGS

      end subroutine IMP_SLV
