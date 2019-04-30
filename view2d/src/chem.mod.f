! subversion Id for THIS file : $Id: chem.mod.f 8506 2016-01-28 10:09:58Z simonc $
! $URL: http://svn-ae/ae/COMMON/stand-alone/atmos_models/socrates/tags/07.18.01/src/chem.mod.f $
!-----------------------------------------------------------------------
      module CHEM_TIMES

      use GRID_DIMS, only : lmax
      use SIM_CONTROLS, only : missval
      
      integer :: ntchem        ! nb of chem tsteps per day

      real :: chemdts, chemdth
      real, dimension(lmax) :: sltss = missval, sltsr = missval
      
      end module CHEM_TIMES

!=======================================================================

      module SPECIES

      use GRID_DIMS, only : lmax, niz
      use SPECIES_DIMS, only : nbcon
      use SIM_CONTROLS, only : missval
       
      real, dimension(lmax,niz,nbcon) :: 
     $      qn2noon  = 0.,          ! noon values of volume mixing ratios
     $      qn2da    = missval,     ! diurnally averaged volume mixing ratios
     $      qn2night = missval,     ! night (closest to midnight) values
     $      qn2ss    = missval,     ! sunset (sza=90, afternoon) values
     $      qn2sr    = missval      ! sunrise (sza=90, before noon) values

      end module SPECIES

!=======================================================================

      module MONTHLY_VAR

      use GRID_DIMS, only : lmax, niz
      use SPECIES_DIMS, only : nbcon
      
      real, dimension(lmax,niz) ::
     $        zgeo_ma = 0.      ! monthly averaged geometric altitudes
     $      , t_ma  = 0.        ! Monthly avg of temperature
     $      , u_ma  = 0.        ! Monthly avg of zonal wind
     $      , v_ma  = 0.        ! Monthly avg of meridional residual wind
     $      , w_ma  = 0.        ! Monthly avg of vertical residual wind
     $      , ntot_ma  = 0.     ! Monthly avg of total nb densities
  
      real, dimension(lmax,niz,nbcon) :: 
     $        ma2noon  = 0.     ! Monthly avg of noon values of vmr
     $      , ma2      = 0.     ! monthly averaged volume mixing ratios
     $      , ma2night = 0.     ! Monthly avg of midnight values of vmr

      end module MONTHLY_VAR

!=======================================================================

      module RATES_MODS
!--------------------------------------------------------------------
!	... Reaction rates
!------------------------------------------------------------------
      use GRID_DIMS, only : lmax, niz
      use CHEM_MODS, only : rxncnt
      
      implicit none

      real     ::  rxt_rates(niz,rxncnt,lmax) = 0.

      end module RATES_MODS

!=======================================================================

      module DIAGS_CHEM_VARS                        ! for archiving only

      use GRID_DIMS, only : lmax, niz
      use SPECIES_DIMS, only : nbcon
      use SIM_CONTROLS, only : missval
      
      integer, parameter :: nsfam = 4,          ! nb of "small" families
     $                      nfam = nsfam + 4    ! total nb of families
      integer, parameter :: ox=1, hox=2, nox=3, clox = 4,
     $                      noy=5, cly=6, bry=7, toth=8
      
      character(len=4), dimension(nfam) :: famname = (/ 'Ox  ', 
     $  'HOx ', 'NOx ', 'ClOx', 'NOy ', 'Cly ', 'Bry ', 'totH' /)
     
      real, dimension(lmax,niz,nbcon) :: 
     $      tau_noon  = missval, tau_night = missval 

      real, dimension(lmax,niz,nsfam) :: 
     $      FamLoss_noon = missval, FamLoss_night = missval
     
      real, dimension(lmax,niz) :: lossOxAnoon, lossOxAnight, 
     $      lossOxBnoon, lossOxBnight, lossOxCnoon, lossOxCnight,
     $       prodO3noon, prodO3night

      real, dimension(lmax,niz,nfam) :: famvmrda = missval

      end module DIAGS_CHEM_VARS

!=======================================================================

      module BOUNDARIES

      use SPECIES_DIMS, only : nbcon
      use GRID_DIMS, only : lmax

      type BOUNDY_COND
         logical :: is_vmr ! .false. -> bc is flux
         integer :: iz
         real, dimension(lmax) :: val   ! vmr or flux
     $                          , vel   ! velocity (cm/s)
      end type BOUNDY_COND
      
!  lbc is lower boundary condition: lbc%iz=1, lbc%vel is dry deposition velocity
!  ibc is intermediate boundary condition at ibc%iz, active only if ibc%iz > 0
!  ubc is upper boundary condition: lbc%iz=niz
      type( BOUNDY_COND ), dimension(nbcon) :: lbc, ibc, ubc
      
      real, dimension(lmax) :: bmmi, bvgn, bsaf, bbbg, bocn
      
      real, dimension(12,lmax) :: ibc_vmr_h2o

      end module BOUNDARIES

!=======================================================================

      module CHEMLIFE

         use SPECIES_DIMS, only : nbcon
         use GRID_DIMS, only : lmax

         real, dimension(lmax,nbcon) :: dens_acc = 0., loss_acc = 0.

      end module CHEMLIFE

!=======================================================================

      module BACKATM
      
      use GRID_DIMS, only : lmax, niz
      use SPECIES_DIMS, only : nbcon
      
!-----------------------------------------------------------------------
! Output variables from MSIS_HWM_PLUG, available everywhere in the model
!-----------------------------------------------------------------------
         integer, parameter :: nbconmsis = 8
         integer, parameter :: H1=1, N1=2, O1=3, 
     $                         He=4, O2=5, Ar=6, N2=7, tot=8
         real, dimension(lmax,niz,nbconmsis) :: Dmsis, Xmsis
         real, dimension(lmax,niz) :: Tmsis
         real, dimension(lmax,niz) :: u_hwm, v_hwm
         real, dimension(lmax,niz) :: maxh2ovap
         
!-----------------------------------------------------------------------
! mass of air (a.m.u./molec.) wmole, Specific heat at constant 
! pressure (J/g/K) Cp and mean density (g/m3) calculated in ATMCOND
!-----------------------------------------------------------------------
         real, dimension(lmax,niz) :: wmole, Cp, ro   
         
      end module BACKATM

