! subversion Id for THIS file : $Id: boundy.f 8506 2016-01-28 10:09:58Z simonc $
! $URL: http://svn-ae/ae/COMMON/stand-alone/atmos_models/socrates/tags/07.18.01/src/boundy.f $
!-----------------------------------------------------------------------

      subroutine BOUNDY( date, tsurf, vmr2d )
!----------------------------------------------------------------------
!    ... The lower, upper and (optionally) intermediate boundary conditions 
!        are specified here.  The chem BC can be further 
!        modified in the following routine(s):
!        - SPECIAL_NO_PROD: UBC for NO is a solar-cycle dependent flux
!                                      v7s05 - July 2001 - simonc@oma.be
!----------------------------------------------------------------------
      use GRID_DIMS, only  : lmax, niz
      use SPECIES_DIMS, only : nbcon
      use PHYS_CST, only : Nav
      use SPC_NAMES
      use BOUNDARIES                        ! output variables are here
      use BACKATM, only : Xmsis, O1, O2, H1, maxh2ovap
      use DIAG_CONTROLS, only : ldiag
      use TIME_CONTROLS, only : TIMING

      implicit none

!----------------------------------------------------------------------
!    ... Parameters
!----------------------------------------------------------------------
      real, parameter    :: conv = 1.e12 * Nav / ( 1.e4*3.1536e7 )

!----------------------------------------------------------------------
!    ... Dummy args
!----------------------------------------------------------------------
      type( TIMING ), intent(in) :: date
      real, dimension(lmax), intent(in)    :: tsurf
      real, dimension(lmax,niz,nbcon), intent(in) :: vmr2d

!----------------------------------------------------------------------
!    ... Local variables
!----------------------------------------------------------------------
      real, dimension(lmax) :: cly, noy, wdno2, wdno, wdno3, wdhno3
      real, dimension(lmax,3) :: fry

!======================================================================
!    ... Set time-independent boundary conditions
!======================================================================
      call CST_BOUNDY( )

!======================================================================
!    ... Species which have specified vmr as lower boundary conditions
!======================================================================
      lbc(vid_o2)%is_vmr = .true.
      lbc(vid_o2)%val(:) = Xmsis(:,1,O2)    ! see MSIS_HWM_PLUG
      lbc(vid_h2o)%is_vmr = .true.
      lbc(vid_h2o)%val(:) = maxh2ovap(:,1)  ! see H2O_MAX (h2o_special.f)
      cly = vmr2d(:,1,vid_cl) + vmr2d(:,1,vid_clo)
     $    + vmr2d(:,1,vid_oclo) + vmr2d(:,1,vid_hcl)
     $    + vmr2d(:,1,vid_clono2) + vmr2d(:,1,vid_hocl)
     $    + vmr2d(:,1,vid_clno2) + vmr2d(:,1,vid_brcl)
     $    + 2.*(vmr2d(:,1,vid_cl2o2) + vmr2d(:,1,vid_cl2))
      lbc(vid_hf)%is_vmr = .true.
      lbc(vid_hf)%val(:) = .25 * cly(:)

!======================================================================
!    ... Species which have specified flux and/or dry deposition velocity 
!         as lower boundary conditions
!======================================================================
!         ... Estimate ocean, sea-ice, and snow fractions
!----------------------------------------------------------------------
      call SURF_COVER( tsurf, fry )

!----------------------------------------------------------------------
!    ... h2 ( convert Tg/yr/m**2 into molecule/s/cm**2 )
!----------------------------------------------------------------------
      lbc(vid_h2)%is_vmr = .false.
      lbc(vid_h2)%val(:) = 0.5 * conv * 
     $                      ( 20.*bmmi + 3.*bsaf + 10.*bbbg + 3.*bocn )
      lbc(vid_h2)%vel(:) = .045 * (1. - fry(:,1)) * (1. - fry(:,3))

!----------------------------------------------------------------------
!    ... co ( convert Tg/yr/m**2 into molecule/s/cm**2)
!           vegetation source (bvgn) includes contribution from
!              isoprene, which is not included in current model.
!----------------------------------------------------------------------
      lbc(vid_co)%is_vmr = .false.
      lbc(vid_co)%val(:) = conv *
     $   ( 381.6*bmmi + 440.*bvgn + 710.5*bbbg + 13.*bocn ) / 28.
      lbc(vid_co)%vel(:) = .03 * (1. - fry(:,1)) * (1. - fry(:,3))

!----------------------------------------------------------------------
!    ... Dry deposition velocities for O3, H2O2 and CH2O
!----------------------------------------------------------------------
      lbc(vid_o3)%vel(:) = (.05 * (1. - fry(:,1)) * (1. - fry(:,3))
     $                   + .07 * (1. - fry(:,1)) * fry(:,3)
     $                   + .07 * fry(:,1) * (1. - fry(:,2))
     $                   + .07 * fry(:,1) * fry(:,2)) 
      lbc(vid_h2o2)%vel(:) = .50 * (1. - fry(:,1)) * (1. - fry(:,3))
     $                     + .32 * (1. - fry(:,1)) * fry(:,3)
     $                     +  fry(:,1) * (1. - fry(:,2))
     $                     + .32 * fry(:,1) * fry(:,2)
      lbc(vid_ch2o)%vel(:) =  lbc(vid_o3)%vel(:)         ! see Muller

!----------------------------------------------------------------------
!    ... NOy members deposition velocities
!----------------------------------------------------------------------
      noy(:) = vmr2d(:,1,vid_no) + vmr2d(:,1,vid_no2)
     $       + vmr2d(:,1,vid_no3) + vmr2d(:,1,vid_hno3)
     $       + vmr2d(:,1,vid_ho2no2) + 2.*vmr2d(:,1,vid_n2o5)
     $       + vmr2d(:,1,vid_clono2) + vmr2d(:,1,vid_clno2)
     $       + vmr2d(:,1,vid_brono2)
     
      where( noy(:) >= 1.e-15 ) 
         wdno2(:) = .10 * (1. - fry(:,1)) * (1. - fry(:,3))
     $            + .01 * (1.-fry(:,1)) * fry(:,3)
     $            + .02 * fry(:,1) * (1. - fry(:,2))
     $            + .01 * fry(:,1) * fry(:,2)
         wdno(:)  = wdno2 / 6.
         wdno3(:) = wdno2
         wdhno3(:) = 4. * (1. - fry(:,1)) * (1. - fry(:,3))
     $             + .5 * (1. - fry(:,1)) * fry(:,3)
     $             + 1. * fry(:,1) * (1. - fry(:,2))
     $             + .5 * fry(:,1) * fry(:,2)
         lbc(vid_hno3)%vel(:) = wdhno3
         lbc(vid_ho2no2)%vel(:) = wdhno3
         lbc(vid_n2o5)%vel(:) = wdhno3
         lbc(vid_no2)%vel(:) = wdno2
         lbc(vid_no)%vel(:)  = wdno
         lbc(vid_no3)%vel(:) = wdno3
      end where

!======================================================================
!    ... Species which have specified vmr as upper boundary conditions
!        The values from MSIS can be solar-cycle dependent (see SOLCYCLES)
!======================================================================
      ubc(vid_h)%is_vmr = .true.
      ubc(vid_h)%val(:) = Xmsis(:,niz,H1)
      ubc(vid_o3p)%is_vmr = .true.
      ubc(vid_o3p)%val(:) = Xmsis(:,niz,O1)
      ubc(vid_o2)%is_vmr = .true.
      ubc(vid_o2)%val(:) = Xmsis(:,niz,O2)

!======================================================================
!    ... Specify the intermediate vmr boundary condition for H2O at 17km
!======================================================================
      ibc(vid_h2o)%iz = 18
      ibc(vid_h2o)%is_vmr = .true.
      ibc(vid_h2o)%val(:) = ibc_vmr_h2o(date%month,:)

!======================================================================
!    ... Optionally, apply long-term trends on some boundary conditions
!        Notice: initial condisitions are changed there if gcc_sw(1)==1 
!======================================================================
      call TRENDS_BOUNDY( date )

      end subroutine BOUNDY
