! subversion Id for THIS file : $Id: chem.f 8506 2016-01-28 10:09:58Z simonc $
! $URL: http://svn-ae/ae/COMMON/stand-alone/atmos_models/socrates/tags/07.18.01/src/chem.f $
!-----------------------------------------------------------------------

      subroutine CHEM( date )
!-----------------------------------------------------------------------
!         ... Main routine for Chemistry - SOCRATES v7s00
!                                           simonc@oma.be, february 2002
!-----------------------------------------------------------------------
      use GRID_DIMS, only : lmax, niz
      use SPECIES_DIMS, only : nbcon, nfs
      use SIM_CONTROLS, only : mainsw, het_sw, chemdtm, daypas
     $                       , jump_chem, model_type
      use DIAG_CONTROLS, only : diags,  ldiag, zdiag
      use ALLCO, only : phi, omp_lats
      use SPC_NAMES, only : vid_h2o, vid_o3
      use CONC, only : hm2d
      use VEN1, only : t2d
      use RATES_MODS, only : rxt_rates                      ! output
      use SPECIES, only : qn2da, qn2noon                    ! output
      use CHEM_TIMES, only : ntchem, chemdth, chemdts       ! output
      use HEAT_TERMS, only : chemheat, solheat              ! output
      use BACKATM, only : Xmsis, N2, maxh2ovap
      use TROPOPAUSE, only : izm
      use ARCH_WHAT, only : dvout
      use ELAPSED_TIMES, only : cpusecs1, realsecs1
      use TIME_CONTROLS, only : TIMING

      implicit none
!-----------------------------------------------------------------------
!    ... Dummy args
!-----------------------------------------------------------------------
      type( TIMING ), intent(in) :: date

!-----------------------------------------------------------------------
!    ... Local variables
!-----------------------------------------------------------------------
      integer :: lat, it, im, iz, ier, i, j, l
      real :: sec, start, timer, phid, slt, calday, sza(lmax)
      real, dimension(lmax,niz,nbcon) :: vmr   ! next mixing ratios
      real, dimension(lmax,niz) :: pcst_no, phve_no
      real, dimension(niz,nfs) :: invariants
      real, dimension(lmax,niz), save :: aero_surf = 0.
      integer, save :: daycount = 0
      real, external :: SECOND    ! Function declaration

      start = SECOND()
      if( daycount == 0 ) then
         if( MOD( 60.*12., chemdtm ) /= 0. ) then
            write(*,*) 'CHEM: error setting chemdtm= ',chemdtm
            write(*,*) '  chemdtm must fit in 12hr'
            stop 'CHEM: chemdtm must fit in 12hr'
         end if
         chemdts = chemdtm * 60.
         chemdth = chemdts / 3600.
         ntchem = 24. / chemdth
         if( ANY( het_sw(6:7) == 1 ) ) call AERO_INIT( aero_surf )
      end if

!-----------------------------------------------------------------------
!    ... NO production by lightning, cosmic-ray, soft X-ray & aurorae
!-----------------------------------------------------------------------
      if( jump_chem ) goto 100
      timer = SECOND()
      call SPECIAL_NO_PROD( date, pcst_no, phve_no )

!-----------------------------------------------------------------------
!    ... Tables for photodissociation rates and heating rates
!         are re-calculated every 'daypas' days
!-----------------------------------------------------------------------
      if( daycount == 0 .or. MOD(daycount,daypas) == 0 ) then
         calday = date%cal_day
         if( daypas > 1 ) calday = calday + REAL( daypas )  ! "forward" looking
         if( mainsw(11) == 0 ) call TABLES_PHO( calday )
      end if
      cpusecs1(2) = cpusecs1(2) + SECOND() - timer

!-----------------------------------------------------------------------
!    ... Set the chemical reaction rates
!-----------------------------------------------------------------------
      timer = SECOND()
      do lat = 1, lmax
         call SETRXT( lat, t2d(lat,:) )
         invariants(:,1) = hm2d(lat,:)                      ! total density
         invariants(:,2) = Xmsis(lat,:,N2) * hm2d(lat,:)    ! N2 density
         call USRRXT( rxt_rates(:,:,lat), t2d(lat,:), invariants )
         call ADJRXT( rxt_rates(:,:,lat), invariants )
      end do

!-----------------------------------------------------------------------
!         ... Initializations - begins at NOON (slt0=12.),
!             will end at noon of next day (slt=36.)
!-----------------------------------------------------------------------
  100 continue
      slt = 12.
      vmr(:,:,:) = qn2noon(:,:,:)        ! initcond of first tstep
      qn2da(:,:,:) = 0.
      chemheat(:,:,:) = 0.
      solheat(:,:,:) = 0.      
      cpusecs1(3) = cpusecs1(3) + SECOND() - timer
      
!-----------------------------------------------------------------------
!    ... The loop for diurnal time begins here
!-----------------------------------------------------------------------
      do it = 1, ntchem
         slt = slt + chemdth
         if( diags .or. it==1 ) then
            write(*,'(5(a,i4),a,f8.5,a,es12.3)')' CHEM BEGINS,date= ',
     $        date%month,'/',date%day,'/',date%year,' ; @(',ldiag,',',
     $      zdiag,') & slt= ',slt,':  vmr(O3)= ',vmr(ldiag,zdiag,vid_o3)
         end if

!-----------------------------------------------------------------------
!    ... Semi-Langrangian transport - OpenMP parallelized across species
!-----------------------------------------------------------------------
         timer = SECOND()
!         sec = SECNDS( 0.0 )
         if( model_type == 'two_d ' ) call ADVECT_CHEM( chemdts, vmr )
         cpusecs1(4) = cpusecs1(4) + SECOND() - timer
!         realsecs1(4) = realsecs1(4) + SECNDS( sec )
         timer = SECOND()
!         sec = SECNDS( 0.0 )
         
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$OMP PARALLEL DO
!$OMP&PRIVATE( l, lat, phid, sza, im )
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         do l = 1, lmax
            lat = omp_lats(l)
            if( model_type /= 'two_d' .and. lat /= ldiag ) cycle
            phid  = phi(lat)
         
!-----------------------------------------------------------------------
!         ... Calculate and solve the evolution of chemical species, taking 
!           vertical diffusion and boundary conditions into account.
!-----------------------------------------------------------------------
            call CHEMDR( lat,  
     $                   phid,
     $                   date,
     $                   it,
     $                   slt,
     $                   pcst_no,
     $                   phve_no,
     $                   aero_surf,
     $                   sza(lat),
     $                   vmr(lat,:,:) )

            im = izm(lat) + 10
            vmr(lat,1:im,vid_h2o) = MIN( vmr(lat,1:im,vid_h2o), 
     $                                   maxh2ovap(lat,1:im) )

         end do
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$OMP END PARALLEL DO
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         cpusecs1(5) = cpusecs1(5) + SECOND() - timer
!         realsecs1(5) = realsecs1(5) + SECNDS( sec )

         if( diags .and. ANY( dvout(:)%active )
     $                    .and. ANY( dvout(:)%days0do == date%days0 ) )
     $      call DVOUT_ASCII( dvout, date, slt, sza, vmr(ldiag,:,:) )


!-----------------------------------------------------------------------
!    ... Horizontal diffusion
!-----------------------------------------------------------------------
         timer = SECOND()
         if( model_type == 'two_d ' ) call HDIFF( chemdts, vmr )
         cpusecs1(6) = cpusecs1(6) + SECOND() - timer
      
!-----------------------------------------------------------------------
!    ... Archive diurnal cycles, depends on "dvout", see SET_ARCH
!-----------------------------------------------------------------------
         if( ANY( dvout(:)%active ) .and. 
     $       ANY( dvout(:)%days0do == date%days0 ) ) then
            call DVOUT_ARCH( dvout, date, slt, vmr )
         end if     

!-----------------------------------------------------------------------
!    ... tstep completed, accumulate the solution in diurnal average
!-----------------------------------------------------------------------
         qn2da = qn2da + vmr * chemdts / 86400.

      end do
      
      if( 3600*ABS(slt-36.) > 1. ) then
         write(*,*)'CHEM error: chemdtm= ',chemdtm,' ; ntchem= ',ntchem,
     $       ' ; after loop, slt should be 36 but slt= ',slt
         stop 'testing CHEM: error, slt/=36 after loop'
      end if
      qn2noon(:,:,:) = vmr(:,:,:)
      daycount = daycount + 1
      cpusecs1(10) = cpusecs1(10) + SECOND() - start
      
      end subroutine CHEM
