! subversion Id for THIS file : $Id: chemdr.f 8506 2016-01-28 10:09:58Z simonc $
! $URL: http://svn-ae/ae/COMMON/stand-alone/atmos_models/socrates/tags/07.18.01/src/chemdr.f $
!-----------------------------------------------------------------------

      subroutine CHEMDR( lat,
     $                   phid,
     $                   date,
     $                   it,
     $                   slt,
     $                   pcst_no,
     $                   phve_no,
     $                   aero_surf,
     $                   sza,
     $                   vmr ) 
!-----------------------------------------------------------------------
!         ... Chemistry solver with vertical diffusion
!    v7s08: - solar time loop and reac rates are in calling routine CHEM
!           - all params for tranport byconvection & fronts were removed
!                                           simonc@oma.be, August 2002
!-----------------------------------------------------------------------
      use GRID_DIMS, only : lmax, niz
      use SPECIES_DIMS, only : nbcon
      use CHEM_MODS, only : permute, phtcnt, hetcnt, nextcnt
      use CHEM_TIMES, only : chemdts
      use SPC_NAMES
      use RXT_NAMES
      use CONC, only : hm2d
      use VEN1, only : t2d
      use RATES_MODS, only : rxt_rates
      use SPECIES, only : qn2night, qn2sr, qn2ss     ! output
      use HEAT_TERMS, only : nsh, chemheat, solheat                  ! output
      use TIME_CONTROLS, only : TIMING
      use ZGRID, only : pmb
      use SIM_CONTROLS, only : missval, mainsw, het_sw, chemdtm
     $                       , model_type, jump_chem
      use ELAPSED_TIMES, only : realsecs2
      use DIAG_CONTROLS, only : diags, ldiag, zdiag
      use TRACNM, only : solsym      ! for diags
      use ALLCO, only : zkm          ! for diags

      implicit none

!-----------------------------------------------------------------------
!    ... Dummy args
!-----------------------------------------------------------------------
      integer, intent(in) :: lat, it            ! latitude & tstep indexes
      real, intent(in)    :: phid               ! latitude (degrees)
     $,                      slt                ! solar local time (hours)
      type( TIMING ), intent(in) :: date
      real, dimension(lmax,niz),intent(in):: pcst_no, phve_no, aero_surf
      real, intent(out) :: sza
      real, dimension(niz,nbcon),intent(inout) :: vmr

!-----------------------------------------------------------------------
!    ... Local variables
!-----------------------------------------------------------------------
      integer :: m, n, iz, im
      real    :: start, sec, ac, sltdiag, jdiag, oldval
      real, dimension(niz):: hnm, hnmi, t, tinv, scsza,
     $                       prod_no, aux1, aux2, fh2o
      real, dimension(niz,nbcon) :: ck0      ! current (t0) working densities
     $,                             ck       ! next (t0+1=it) working densities 
      real, dimension(niz,nbcon) :: loss_rate
      real, dimension(niz,nsh) :: psh        ! precursor of solar heating rates
      real, dimension(niz,hetcnt) :: wash_rates
      real, dimension(niz,phtcnt) :: pht_rates 
      logical :: midnight, noon, diags_lat
      logical, dimension(niz) :: sunlight

!-----------------------------------------------------------------------
!         ... Initializations
!-----------------------------------------------------------------------
!      start = SECNDS( 0.0 )
!      sec = SECNDS( 0.0 )
      hnm(:) = hm2d(lat,:)
      hnmi(:) = 1. / hnm(:)
      noon = ( 3600. * ABS( slt - 36. ) <= chemdts )
      midnight = ( 3600. * ABS( slt - 24. ) <= chemdts )
      t(:) = t2d(lat,:)
      wash_rates(:,:) = 0.
      diags_lat = diags .and. ( lat == ldiag )

!-----------------------------------------------------------------------
!         ... Set the diurnal variables
!-----------------------------------------------------------------------
      do n = 1, nbcon
         ck0(:,n) = vmr(:,n) * hnm(:)   ! initcond of first tstep
      end do
      if( diags_lat ) then
         write(*,'(5(a,i4),a,f5.2,a,es12.3)')' CHEMDR BEGINS,date= ',
     $   date%month,'/',date%day,'/',date%year,' ; @(',lat,',',zdiag,
     $    ') & slt= ',slt,':  vmr(O3)= ',vmr(zdiag,vid_o3)
      end if
      if( jump_chem ) goto 100

!-----------------------------------------------------------------------
!    ... Rates for reaction a27 & washout depend on water vapor density
!-----------------------------------------------------------------------
!      sec = SECNDS( 0.0 )
      tinv(:) = 1. / t(:)
      aux1(:) = 1.7e-33*EXP( 1000.*tinv(:) )*hnm(:)
      aux2(:) = 2.3e-13*EXP( 600.*tinv(:) )
      fh2o(:) = 1. + 1.4e-21 * EXP( 2200.*tinv(:) ) * ck0(:,vid_h2o)
      rxt_rates(:,rid_a27,lat) = (aux1(:) + aux2(:)) * fh2o(:)
      if( het_sw(8) == 1 ) then
         call WASHOUT( lat, t, hnm, ck0(:,vid_h2o), wash_rates )
      end if
!      realsecs2(1,lat) = realsecs2(1,lat) + SECNDS( sec )

!-----------------------------------------------------------------------
!    ... Calc scsza (secant/Chapman of sza) & set the photolysis rates
!-----------------------------------------------------------------------
!      sec = SECNDS( 0.0 )
      call SUN_CHEM( lat,slt, date%cal_day, phid, sza, sunlight, scsza )
      if( mainsw(11) == 0 ) then
         call INTERP_PHO_CHEM( lat, sunlight, scsza, pht_rates, psh )
       else
         call PHO_CHEM( lat, sunlight, scsza, ck0, pht_rates, psh )
      end if
      rxt_rates(:,:phtcnt,lat) = pht_rates
!      realsecs2(2,lat) = realsecs2(2,lat) + SECNDS( sec )
      
!-----------------------------------------------------------------------
!     ... Special J process: O2+762nm->O2(1S) : see mlynczak93.f
!-----------------------------------------------------------------------
!      sec = SECNDS( 0.0 )
      call MLYNCZAK93_PHO( lat,sza, sunlight, scsza, 
     $                     rxt_rates(:,rid_j_o2s,lat) )

!-----------------------------------------------------------------------
!    ... Special productions of NO: see special_prod_no.f (v6s16c)
!-----------------------------------------------------------------------
      prod_no(:) = pcst_no(lat,:)
      where( sunlight(:) )
         prod_no(:) = prod_no(:) + phve_no(lat,:) ! *cosza at v6s16a
      end where
!      realsecs2(3,lat) = realsecs2(3,lat) + SECNDS( sec )

!-----------------------------------------------------------------------
!    ... Solve for new densities
!-----------------------------------------------------------------------
!      sec = SECNDS( 0.0 )
      call IMP_SLV(  ck                  ! output densities at timestep it
     $,              ck0                 ! input densities at timestep it-1
     $,              rxt_rates(:,:,lat)
     $,              wash_rates 
     $,              prod_no
     $,              loss_rate
     $,              chemdts
     $,              lat
     $,              it, slt, date )
!      realsecs2(4,lat) = realsecs2(4,lat) + SECNDS( sec )

!      sec = SECNDS( 0.0 )
      if( diags_lat ) then
         write(*,'(3(a,i3),a,f6.3,a,es12.3)') ' CHEMDR solved @ ('
     $     ,lat,',',zdiag,'): it= ',it,' ; slt= ',slt,
     $     ' ; new vmr(HF)= ',ck(zdiag,vid_hf)*hnmi(zdiag)
         write(43,'(i2,''/'',i2,''/'',i4,3(a,f10.5))')date%month,
     $     date%day,date%year,',IMP_SLV, O(3P) increase: ',100.*
     $    (ck(zdiag,vid_o3p)-ck0(zdiag,vid_o3p))/ck0(zdiag,vid_o3p),
     $    ' at slt= ',slt,' and sza= ',sza
      end if
        
!---------------------------------------------------------------------
!         ... Special diagnostics for chemical budgets
!---------------------------------------------------------------------
      if( midnight .or. noon )  then
         call DIAGS_CHEM( lat,date,noon,midnight,hnmi, ck,loss_rate )
      end if
!      realsecs2(5,lat) = realsecs2(5,lat) + SECNDS( sec )

!---------------------------------------------------------------------
!         ... Vertical diffusion - new algorithm by *SC
!---------------------------------------------------------------------
 100  continue
!      sec = SECNDS( 0.0 )
      if( model_type /= 'zerod' ) then
         if( diags_lat) oldval = ck(zdiag,vid_o3p)
         
         call VDIFF( lat, chemdts, t, hnm, ck(:,:) )
         
         if( diags_lat )write(44,'(i2,''/'',i2,''/'',i4,3(a,f10.5))')
     $     date%month,date%day,date%year,',VDIFF, O(3P) increase: ',
     $    100.*(ck(zdiag,vid_o3p)-oldval)/oldval,
     $    ' %, at slt= ',slt,' and sza= ',sza
      end if
!      realsecs2(7,lat) = realsecs2(7,lat) + SECNDS( sec )

!-----------------------------------------------------------------------
!     ... Accumulate the diurnally averaged solar & chemical heating rates
!-----------------------------------------------------------------------
!      sec = SECNDS( 0.0 )
      call HEAT_RATES( lat, psh, chemdts, hnm, ck, 
     $                 solheat(lat,:,:), chemheat(lat,:,:) )
!      realsecs2(8,lat) = realsecs2(8,lat) + SECNDS( sec )
      
!-----------------------------------------------------------------------
!    ... Perturbation study, v6s18a: on 1 Jan 1996, midnight, half H2O
!-----------------------------------------------------------------------
!          if( midnight .and. date%year==1996 .and. lat < 7  ) then
!             write(*,*) 'CHEMDR @midnight, cal_day= ',date%cal_day,
!     $        ' ; lat= ',lat,' ; ck(H2O)/2'
!             if(date%cal_day==1.)ck(71:91,vid_h2o)=0.5*ck(71:91,vid_h2o)
!          end if

!=======================================================================
!      ... Store Solution as vmr
!=======================================================================
!      sec = SECNDS( 0.0 )
      do n = 1,nbcon
         vmr(:,n) = ck(:,n) * hnmi(:)
      end do
      if( midnight ) qn2night(lat,:,:) = vmr(:,:)
      
      if( diags_lat ) then
         write(*,'(2(a,i3),3(a,es12.3))')' CHEMDR ends @ (',lat,',',
     $      zdiag,'): new vmr(O3)= ',vmr(zdiag,vid_o3)
      end if
!      realsecs2(9,lat) = realsecs2(9,lat) + SECNDS( sec )
!      realsecs2(10,lat) = realsecs2(10,lat) + SECNDS( start )

      end subroutine CHEMDR
