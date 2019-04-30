! subversion Id for THIS file : $Id: pho.mod.f 8506 2016-01-28 10:09:58Z simonc $
! $URL: http://svn-ae/ae/COMMON/stand-alone/atmos_models/socrates/tags/07.18.01/src/pho.mod.f $
!-----------------------------------------------------------------------

      module PHO_PARMS

         use GRID_DIMS, only : niz
         use CHEM_MODS, only : phtcnt

!-----------------------------------------------------------------------
!    ... Basic photorates parameters for TABLES_SUN and TABLES_PHO
!-----------------------------------------------------------------------
         integer, parameter :: nd2 = 2,      ! nb of intermediate J's
     $                         njpd = nd2+2, ! nb of calc J's (+highest & lowest sun)
     $                         nk = njpd + 2 ! nb of known J
         real, parameter :: minlog = -200.   ! Very small val for logs at night
     
!-----------------------------------------------------------------------
!    ... photorates parameters for PHO and associated routines
!-----------------------------------------------------------------------
         integer, parameter :: mxcly = niz - 1 ! nb of layers for pho.f
     $,                        maxden = 5      ! nb of species densities (incl. air) used by pho.f
     $,                        mxwvn = 171     ! nb of wavelength intervals for pho.f
     $,                        mxwvwc = 24
     $,                        mxsiz = 3
     $,                        mxcof = 3
     $,                        phtmax = 52     ! nb of J processes calc by pho.f

!-----------------------------------------------------------------------
!    ... Set var 'map' to map from the 52 processes 
!        calculated by PHODIS, in the order of data/crsxxx.dat, to the 
!        phtcnt processes needed by SOCRATES and set at the 
!        beginning of RXT_NAMES (preprocessed.mod.f)
!        The processes needed by SOCRATES but not calc by PHODIS have
!        'map' set to -1. Their j is calculated elsewhere, often in setpht.f
!-----------------------------------------------------------------------
      integer, parameter, dimension(phtcnt) :: pho_map =        
!     $      (/  1, 50,  2, 18,  5,  6, 24, 37, 38,  4,         ! map for simple
!     $          7,  8, 14, 46, 17, 42, 48, 43,  3, 23, 52 /)   ! chem (v6s04 & v6s05)
     $      (/ 1, 50, 2, 3, 4, 5, 6, 7, 8, 9
     $,        10, 11, 12, 13, 14, 46, 15, 16, 17, 18
     $,        19, 20, 21, 22, 23, 24, 37, 25, 26, 27
     $,        28, 29, 30, 34, 35, 36, 41, 42, 43, 44
     $,        45, 47, 48, 51, 38, 52, -1 /)

      end module PHO_PARMS

!=======================================================================

      module SUN_VARS
      
      use PHO_PARMS, only : mxwvn
      
      integer, parameter :: maxnd = 100*365

      real :: solfactor   ! based on e107_s2k ; 0 at SOLMIN, 1 at SOLMAX
     $      , f107        ! F10.7 radio index for MSIS and HWM

      real, dimension(maxnd) :: days0_s2k
     $                        , e107_s2k  ! SOLAR2000 new proxy read by PHO_INIT
     $                        , flya_s2k  ! SOLAR2000 Ly-a flux read by PHO_INIT

      real, dimension(mxwvn,0:2) :: solflux ! solar spectrum read by PHO_INIT

      end module SUN_VARS

!=======================================================================

      module SOLDAY

         use GRID_DIMS, only : lmax, niz
         use PHO_PARMS, only : nk
         use CHEM_MODS, only : phtcnt        ! nb of J processes = 47 as of v6j01
         use HEAT_TERMS, only : nsh          ! nb of solheat processes = 3
         use SIM_CONTROLS, only : missval

         real, dimension(niz,nk,lmax) :: scszak = missval
         real, dimension(niz,phtcnt,nk,lmax) :: logjk = missval    
         real, dimension(niz,nsh,nk,lmax) :: logpshk = missval

      end module SOLDAY

!=======================================================================

      module PHO_VARS
!------------------------------------------------------------
!    ... Basic photorates variables
!------------------------------------------------------------

         use PHO_PARMS

         integer, dimension(phtmax) ::  ivbegin, ivend
         real, dimension(phtmax,mxwvn) ::  crs, qy
         real, dimension(mxwvn) ::  tb_o3, tc_o3, crs200co2, crs370co2, 
     $                              ta_no2, tb_hno3,
     $                              ta1_clono2, ta2_clono2,
     $                              ta_ch2o, tb_ch2o, tb_pan
         real, dimension(mxwvn) ::  dwvn, fbeamr, wvn,
     $                              alamb, blamb, hv
     $                            , qyo3cst, qyo3coeff2, qyo3coeff3
     $                            , zlambd
         real, dimension(0:mxcly,mxwvn) :: sigra
         real    ::  wcwvn(mxwvwc)
         real, dimension(mxsiz,mxwvwc,mxcof) ::  wc_asy, wc_ssa, wc_ext
         real, dimension(mxsiz+1)    ::  wvnwcz
         real, dimension(0:mxcly,16) ::  rm, ro2
         real, dimension(0:mxcly)    ::  rmlya, ro2lya
         real, dimension(12,16)      ::  ako, bko
         real, dimension(9,5)        ::  acfc, bcfc

      end module PHO_VARS

!=======================================================================

      module PHO_AERO

         use GRID_DIMS, only : lmax, niz
         use PHO_PARMS, only : mxcly, mxwvn

         real :: haer, aaer
         real :: vaer(0:mxcly)
         real, dimension(mxwvn) :: gaer1, omaer, coeff
         real :: exabs(0:mxcly,mxwvn)
         real :: aerex2(niz)
         real, dimension(lmax,niz) :: aeabs = 0., aedat1 = 0., 
     $                                aerom = 0.
         real :: aerabs(lmax,niz,30) = 0.

      end module PHO_AERO

!=======================================================================

      module AIRGLOW

         use GRID_DIMS, only : niz
         use PHO_PARMS, only : mxwvn

         real, dimension(niz,7) :: efch = 1.
         real, dimension(niz,mxwvn) :: efsho2 = 1., efsho3 = 1.
         real, dimension(mxwvn) :: denerg1, denerg2

      end module AIRGLOW

