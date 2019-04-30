! subversion Id for THIS file : $Id: heat_rates.f 8506 2016-01-28 10:09:58Z simonc $
! $URL: http://svn-ae/ae/COMMON/stand-alone/atmos_models/socrates/tags/07.18.01/src/heat_rates.f $
!-----------------------------------------------------------------------

      subroutine HEAT_RATES( lat, psh, dts, n, ni, solheatl, chemheatl )
!-----------------------------------------------------------------------
!  	... Calculate diurnal averages of solar and chemical heating 
!           rates by accumulation in the chemistry diurnal loop.
!           The input scsza & psh were calculated in *PHO_CHEM.
!           The outputs solheat and chemheat were initialized to zero
!           in CHEM, before the diurnal loop.
!-----------------------------------------------------------------------
      use GRID_DIMS, only:     niz
      use SPECIES_DIMS, only : nbcon
      use HEAT_TERMS, only :   nch, nsh, heatsol
      use PHYS_CST, only :     Nav, eV, d2r
      use ALLCO, only :        phi
      use SPC_NAMES, only :    vid_o2, vid_o3, vid_o3p, 
     $                         vid_oh, vid_h, vid_ho2
      use RXT_NAMES, only :    rid_a1, rid_a2, rid_a5, rid_a7,
     $                         rid_hk1, rid_hk2, rid_hk3
      use RATES_MODS, only :   rxt => rxt_rates
      use AIRGLOW, only :      efch              
      use ZGRID, only :        zgeo, Hair
      use BACKATM, only :      Dmsis, O2, O1, H1, wmole, Cp
      use SIM_CONTROLS, only : mainsw
      use DIAG_CONTROLS, only: diags, zdiag, ldiag

      implicit none

!------------------------------------------------------------------
!  	... Parameters - energies by exothermic chemical reaction, in J
!------------------------------------------------------------------
      real, parameter    :: ep1 = 5.11 * eV ! J
      real, parameter    :: ep2 = 1.05 * eV
      real, parameter    :: ep3 = 4.06 * eV
      real, parameter    :: ep4 = 3.34 * eV 
      real, parameter    :: ep5 = 0.72 * eV
      real, parameter    :: ep6 = 2.39 * eV
      real, parameter    :: ep7 = 2.00 * eV

!------------------------------------------------------------------
!	... Dummy args
!------------------------------------------------------------------
      integer, intent(in) :: lat                     ! latitude index
      real, intent(in), dimension(niz,nsh):: psh     ! solheat rates precursors
      real, intent(in)    :: dts                     ! timestep (s)
      real, intent(in)    :: n(niz), ni(niz,nbcon)   ! total & chem nb densities
      real, intent(out)   :: solheatl(niz,nsh)       ! Solar heat rates (K/day)
      real, intent(out)   :: chemheatl(niz,nch)      ! Chem heat rates (K/day)

!-----------------------------------------------------------------------
!	... Local variables
!-----------------------------------------------------------------------
      integer :: i, iz, k
      real :: s(niz,nsh), e(niz,nch)
      real, dimension(niz)  :: convfact
      real, dimension(niz)  :: n_o2, n_o3, n_o, n_oh, n_h, n_ho2

      convfact(:) = Nav / ( n(:) * wmole(lat,:) * Cp(lat,:) )   ! cm3 K / J
                                                          
      n_o2(:)  = ni(:,vid_o2)      !      n_o2(81:niz) = Dmsis(lat,81:niz,O2)
      n_o3(:)  = ni(:,vid_o3)

!-----------------------------------------------------------------------
!	... Energies ( J cm-3 s-1 molec2 ) by absorption of solar radiation
!-----------------------------------------------------------------------
      s(:,1) = psh(:,1) * n_o2(:)
      s(:,2) = psh(:,2) * n_o3(:)
      s(:,3) = psh(:,3)              ! heating by aerosol abs

!------------------------------------------------------------------
!	... Convert to solar heating rates ( K/s ) ,integrate over 
!           one full day - will be used in TOTAL_HEAT_RATE
!------------------------------------------------------------------
      do i = 1, nsh
         s(:,i) = convfact(:) * s(:,i)                            ! K/s
         solheatl(:,i) = solheatl(:,i) + dts *s(:,i)              ! K for 1 day
      end do

!------------------------------------------------------------------
!      Chemical heating, see Brasseur and Hoffermann, 1986, 
!      JGR vol.91 p. 10818 .    The efficiencies efch of the 7 
!      exothermic reactions are set in HEAT_COEFF
!------------------------------------------------------------------
      n_o3(:)  = ni(:,vid_o3)
      n_oh(:)  = ni(:,vid_oh)
      n_ho2(:) = ni(:,vid_ho2)
      n_o2(:)  = ni(:,vid_o2)    
      n_o(:)   = ni(:,vid_o3p)   
      n_h(:)   = ni(:,vid_h)     
      if( mainsw(6) == 1 ) then
         n_o2(81:niz) = Dmsis(lat,81:niz,O2)
         n_o(81:niz)  = Dmsis(lat,81:niz,O1)
         n_h(81:niz)  = Dmsis(lat,81:niz,H1)
      end if

!-----------------------------------------------------------------------
!	... Energies ( J cm-3 s-1 molec2 ) by exothermic chemical reactions
!-----------------------------------------------------------------------
      e(:,1) = efch(:,1) * ep1 * rxt(:,rid_hk1,lat) * n_o(:)   * n_o(:) 
      e(:,2) = efch(:,2) * ep2 * rxt(:,rid_hk2,lat) * n_o2(:)  * n_o(:)
      e(:,3) = efch(:,3) * ep3 * rxt(:,rid_hk3,lat) * n_o3(:)  * n_o(:)
      e(:,4) = efch(:,4) * ep4 * rxt(:,rid_a2,lat)  * n_h(:)   * n_o3(:)
      e(:,5) = efch(:,5) * ep5 * rxt(:,rid_a5,lat)  * n_oh(:)  * n_o(:)
      e(:,6) = efch(:,6) * ep6 * rxt(:,rid_a7,lat)  * n_ho2(:) * n_o(:)
      e(:,7) = efch(:,7) * ep7 * rxt(:,rid_a1,lat)  * n_h(:)   * n_o2(:)

!-----------------------------------------------------------------------
!	... Convert to chemical heating rates ( K/s ) and
!	    integrate chemical heating over one full day
!           chemheat will be summed to srheat3 in TOTAL_HEAT_RATE
!-----------------------------------------------------------------------
      do i = 1, nch
         e(:,i) = convfact(:) * e(:,i)                            ! K/s
         chemheatl(:,i) = chemheatl(:,i) + dts *e(:,i)            ! K for 1 day
      end do
      
!-----------------------------------------------------------------------
!	... Diagnostics
!-----------------------------------------------------------------------
      if( diags .and. lat == ldiag ) then
         iz = zdiag
         i = 2                ! diags on  solar heating by O3 abs
         write(*,'(2(a,i3),a)') 'HEAT_RATES@(',lat,',',iz,')'
         write(*,'(5(a,es12.5))') '   '//heatsol(i)//': convfact= ',
     $      convfact(iz),' ; psh= ',psh(iz,i),' ; s= ',s(iz,i)
      end if

      end subroutine HEAT_RATES
