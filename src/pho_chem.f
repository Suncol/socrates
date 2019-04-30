! subversion Id for THIS file : $Id: pho_chem.f 8506 2016-01-28 10:09:58Z simonc $
! $URL: http://svn-ae/ae/COMMON/stand-alone/atmos_models/socrates/tags/07.18.01/src/pho_chem.f $
!-----------------------------------------------------------------------

      subroutine PHO_CHEM( lat, sunlight, scsza, ck, pht_rates, psh )
!-----------------------------------------------------------------------
!    ... Compute directly the photorates & precursors of solheat rates 
!        ('psh') at each chem tstep - *very* computationally intensive
!                                      v7s04 - simonc@oma.be, March 2002
!-----------------------------------------------------------------------
      use GRID_DIMS, only : lmax, niz
      use SPECIES_DIMS, only : nbcon
      use CHEM_MODS, only : phtcnt   ! = 47 as of v6j01
      use HEAT_TERMS, only : nsh
      use CONC, only : hm2d
      use SPECIES, only : qn2noon                               ! input
      use SIM_CONTROLS, only : missval, mainsw
      use DIAG_CONTROLS, only : diags, ldiag, zdiag, jdiag
      use RXT_NAMES                                             ! for diags
      use NUMERICAL, only : INTERP

      implicit none      

!-----------------------------------------------------------------------
!    ... Dummy args
!-----------------------------------------------------------------------
      integer, intent(in) :: lat 
      logical, intent(in), dimension(niz) :: sunlight
      real, intent(in), dimension(niz)    :: scsza
      real, intent(in), dimension(niz,nbcon)   :: ck ! nb densities of species
      real, intent(out), dimension(niz,phtcnt) :: pht_rates 
      real, intent(out), dimension(niz,nsh)    :: psh

!-----------------------------------------------------------------------
!    ... Local variables
!-----------------------------------------------------------------------
      integer :: i, ish, jproc
      real, dimension(niz,nbcon) :: ck_noon
      
      pht_rates(:,:phtcnt) = 0.
      psh(:,:nsh) = 0.
      if( .not. sunlight(niz) ) goto 100 ! night at all altitudes: only do diags

      if( mainsw(11) == 2 ) then     ! fully interactive J calculation
         call PLUG_PHO( lat, sunlight, scsza, ck, pht_rates, psh )
       else                          ! use old noon values as in TABLES_PHO
         do i = 1, nbcon
            ck_noon(:,i) = qn2noon(lat,:,i) * hm2d(lat,:)
         end do
         call PLUG_PHO( lat, sunlight, scsza, ck_noon, pht_rates, psh )
      end if
      do jproc = 1, phtcnt
         where( .not. sunlight(:) )
            pht_rates(:,jproc) = 0.
         end where
      end do
      do ish = 1, nsh
         where( .not. sunlight(:) )
            psh(:,ish) = 0.
         end where
      end do

!-----------------------------------------------------------------------
!    ... Everything below is diagnostics
!-----------------------------------------------------------------------
  100 if( diags .and. lat == ldiag ) then
         ish = 2              ! diags on  solar heating by O3 abs
         write(*,'(4(a,i3))')'PHO_CHEM @ (',lat,
     $       ',',zdiag,') ; jproc= ',jdiag,' ; ish= ',ish
         write(*,*)'   sunlight= ',sunlight(zdiag)
         write(*,'(3x,3(a,es12.5))') 'scsza= ',scsza(zdiag),' ; J= ',
     $       pht_rates(zdiag,jdiag),' ; psh= ',psh(zdiag,ish)
      end if
     
      end subroutine PHO_CHEM
