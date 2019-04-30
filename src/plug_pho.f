! subversion Id for THIS file : $Id: plug_pho.f 8506 2016-01-28 10:09:58Z simonc $
! $URL: http://svn-ae/ae/COMMON/stand-alone/atmos_models/socrates/tags/07.18.01/src/plug_pho.f $
!-----------------------------------------------------------------------

      subroutine PLUG_PHO( lat, sunlight, scsza, ck, tjd, presolheat )
!-----------------------------------------------------------------------
!    ... Interface to plug PHO (Photodissociation rate and heating rate 
!        calculation of Kylling, Stamnes code) to SOCRATES
!                                     v7s04 - simonc@oma.be - March 2002
!-----------------------------------------------------------------------
      use GRID_DIMS, only : niz
      use SPECIES_DIMS, only : nbcon
      use CHEM_MODS, only : phtcnt   ! = 47 as of v6j01
      use HEAT_TERMS, only : nsh
      use PHO_PARMS, only : mxcly, maxden, phtmax, pho_map
      use PHYS_CST, only : albedo
      use SPC_NAMES, only :  vid_o3, vid_no, vid_no2, vid_co2, vid_o2
      use ZGRID, only : zgeo
      use VEN1, only : t2d
      use CONC, only : hm2d
      use PHO_AERO, only : aerex2, aedat1
      use SIM_CONTROLS, only : het_sw
      use DIAG_CONTROLS, only : diags, ldiag, zdiag, jdiag

      implicit none

!-----------------------------------------------------------------------
!    ... Parameters: If drop true, water cloud effect will be taken in 
!        account. In this case you must set wccon, the cloud liquid water 
!        content, & wceffr, the drop equivalent radius
!-----------------------------------------------------------------------
      logical, parameter :: drop = .false.
      logical, parameter :: rayli = .true.

!-----------------------------------------------------------------------
!    ... Dummy args
!-----------------------------------------------------------------------
      integer, intent(in) :: lat           ! latitude index
      logical, intent(in) :: sunlight(niz)
      real, intent(in)    :: scsza(niz)    ! Secant/Chapman of SZA
      real, intent(in)    :: ck(niz,nbcon) ! nb densities of species (molec/cm3)
      real, intent(out)   :: tjd(niz,phtcnt), presolheat(niz,nsh)

!-----------------------------------------------------------------------
!    ... Local variables
!-----------------------------------------------------------------------
      integer :: m, n, i, lev, levlo
      real  :: drat(phtmax,0:mxcly)
      real, dimension (0:mxcly) :: temper, zs, secchap
      real  :: dens(0:mxcly,maxden)
      real  :: wrat(3,0:mxcly)
      logical :: aero, diags_lat

      tjd(:,:) = 0.
      presolheat(:,:) = 0.
      levlo = -1
      diags_lat = diags .and. ( lat == ldiag )
      
!-----------------------------------------------------------------------
!    ... Calc level levlo: sunlight penetrates from lev=0 down to lev=levlo
!-----------------------------------------------------------------------
      do lev = 0, mxcly
         if( sunlight(mxcly-lev+1) ) levlo = lev
      end do
      
      if( diags_lat ) write(*,'(4(a,i3))') 'PLUG_PHO @ (',
     $                            lat,',',zdiag,'): levlo= ',levlo
      
      if( levlo < 2 ) then
         if(diags_lat)write(*,'(6x,a)')'SUNLIGHT NOT DEEP ENOUGH: J=0'
         return
      end if

      aero = ( het_sw(7) == 1 )
         
      
!-----------------------------------------------------------------------
!    ... Set altitude grid, temp and density variables
!        Note: we use noon values for *both* afternoon times - approx
!-----------------------------------------------------------------------
      zs(mxcly:0:-1) = zgeo(lat,:niz)
      temper(mxcly:0:-1) = t2d(lat,:niz)
      secchap(mxcly:0:-1) = scsza(:niz)
      dens(mxcly:0:-1,1) = hm2d(lat,:niz)
      dens(mxcly:0:-1,2) = ck(:niz,vid_o3)
      dens(mxcly:0:-1,3) = ck(:niz,vid_o2)
      dens(mxcly:0:-1,4) = ck(:niz,vid_no)
      dens(mxcly:0:-1,5) = ck(:niz,vid_no2)
      if( aero ) then
         aerex2 = aedat1(lat,:)
      else
         aerex2 = 0. 
      end if
     
!-----------------------------------------------------------------------
!    ... Call the main routine for calc of  photodissociation coefficients
!-----------------------------------------------------------------------
      call PHO( diags_lat, rayli, aero, drop, albedo, dens, temper, 
     $          levlo, secchap, zs, drat, wrat )
     
!-----------------------------------------------------------------------
!    ... Map to photodissociation coefficients indexes used by chem
!-----------------------------------------------------------------------
      do m = 1, phtcnt
         n = pho_map(m)
         if( n <= 0 ) cycle
         tjd(:niz,m) = drat(n,mxcly:0:-1)
      end do

!-----------------------------------------------------------------------
!    ... Map the precursors of the solar heating rates
!-----------------------------------------------------------------------
      do i = 1, nsh
         presolheat(:niz,i) = wrat(i,mxcly:0:-1)
      end do
      
      if( diags_lat ) write(*,'(a,i3,2(a,es9.3))') 
     $        'PLUG_PHO ends, tjd(jdiag=',jdiag,')= ',tjd(zdiag,jdiag),
     $                       ' ; presolheat(2)= ',presolheat(zdiag,2)

      end subroutine PLUG_PHO
