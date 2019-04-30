! subversion Id for THIS file : $Id: interp_pho_chem.f 8506 2016-01-28 10:09:58Z simonc $
! $URL: http://svn-ae/ae/COMMON/stand-alone/atmos_models/socrates/tags/07.18.01/src/interp_pho_chem.f $
!-----------------------------------------------------------------------

      subroutine INTERP_PHO_CHEM( lat, sunlight, scsza, pht_rates, psh )
!-----------------------------------------------------------------------
!    ... Set the photorates & precursors of solheat rates ('psh') at each
!        chem tstep by interpolation of the tables calc in TABLES_PHO, 
!        using solar parameters calc by SUN_CHEM.
!        Extracted from SETPHT & HEAT_RATES 
!                                      v7s04 - simonc@oma.be, March 2002
!-----------------------------------------------------------------------
      use GRID_DIMS, only : lmax, niz
      use CHEM_MODS, only : phtcnt   ! = 47 as of v6j01
      use PHO_PARMS, only : nk, minlog, pho_map
      use HEAT_TERMS, only : nsh
      use SOLDAY, only : scszak, logjk, logpshk                   ! input
      use SIM_CONTROLS, only : missval, daypas
      use ALLCO, only : phi
      use DIAG_CONTROLS, only : diags, debug, ldiag, zdiag, jdiag ! for diags
      use RXT_NAMES                                               ! for diags
      use NUMERICAL, only : INTERP

      implicit none      

!-----------------------------------------------------------------------
!    ... Dummy args
!-----------------------------------------------------------------------
      integer, intent(in) :: lat 
      logical, intent(in), dimension(niz) :: sunlight
      real, intent(in), dimension(niz)    :: scsza
      real, intent(out), dimension(niz,phtcnt) :: pht_rates 
      real, intent(out), dimension(niz,nsh)    :: psh

!-----------------------------------------------------------------------
!    ... Local variables
!-----------------------------------------------------------------------
      logical :: ok
      integer :: ki, iz, jproc, ish, i, ier
      real :: minscsza, maxscsza, logj(phtcnt), logpsh(nsh)
      
      pht_rates(:,:phtcnt) = 0.
      psh(:,:nsh) = 0.
      ier = 0

      zloop: do iz = 1, niz
      
         jproc = -1
         ish = -1

!-----------------------------------------------------------------------
!    ... Day/night tests
!-----------------------------------------------------------------------
         if( .not. sunlight(iz) ) cycle zloop
         if( scsza(iz) == missval ) ier = 1
         if( ier /= 0 ) exit zloop

!-----------------------------------------------------------------------
!    ... Test for consistency with angles computed in TABLES_SUN
!-----------------------------------------------------------------------
         minscsza = scszak(iz,1,lat)
         maxscsza = scszak(iz,nk,lat)
         if( scsza(iz) < minscsza ) then
!            getting out of polar night during 'daypas' period: accepted(J=0)
            if( daypas > 1 .and. scsza(iz) > 10. .and.
     $                                 ABS(phi(lat)) > 65. ) cycle zloop  ! else
            ier = 2
         end if
         if( scsza(iz) > maxscsza ) then
!            sun lower than lowest sun in table, during 'daypas' period: accepted (J=0)
            if( daypas > 1 .and. scsza(iz) > 1.e3 ) cycle zloop  ! else...
            ier = 3
         end if
         if( ier /= 0 ) exit zloop

         logj(:) = minlog - 1.
         logpsh(:) = minlog - 1.
         
!-----------------------------------------------------------------------
!    ... Interpolate the J on the grid prepared in TABLES_PHO
!-----------------------------------------------------------------------
         do jproc = 1, phtcnt
            if( pho_map(jproc) <= 0 ) cycle
            call INTERP( 1, scsza(iz), logj(jproc), 
     $                   nk, scszak(iz,:,lat), logjk(iz,jproc,:,lat),
     $                   spline=.false., on_log=.false., ok=ok )
            if( .not. ok ) ier = 4
            if( ier == 0 .and. logj(jproc) > 1. ) ier = 5
            if( ier /= 0 ) exit zloop
         end do
         jproc = -1
         where( logj(:) > minlog )
            pht_rates(iz,:) = EXP( logj(:) )
         end where

!-----------------------------------------------------------------------
!    ... Interpolate the precursors of solheat rates (grid from TABLES_PHO)
!-----------------------------------------------------------------------
         do i = 1, nsh
            call INTERP( 1, scsza(iz), logpsh(i), 
     $                   nk, scszak(iz,:,lat), logpshk(iz,i,:,lat),
     $                   spline=.false., on_log=.false., ok=ok )
            if( .not. ok ) ier = 6
            if( ier == 0 .and. logpsh(i) > 1. ) ier = 7
            if( ier /= 0 ) exit zloop
         end do
         where( logpsh(:) > minlog )
            psh(iz,:) = EXP( logpsh(:) )
         end where

      end do zloop

!-----------------------------------------------------------------------
!    ... Everything below is diagnostics and error reporting
!-----------------------------------------------------------------------
      if( ( diags .and. lat==ldiag ) .or. ( ier /= 0 ) ) then
         if( ier == 0 ) then
            iz = zdiag
          else
            write(*,*)' !!!! INTERP_PHO_CHEM ERROR: ier= ',ier
         end if
         write(*,'(4(a,i3))')'INTERP_PHO_CHEM @ (',lat,
     $       ',',iz,') ; jproc= ',jproc,' ; ish= ',ish
         write(*,*)'   sunlight= ',sunlight(iz)
         if( .not. sunlight(iz) ) goto 100
         if( ier == 0 .or. jproc < 0 ) jproc = jdiag
         if( ier == 0 .or. ish < 0 ) ish = 2 ! diags on  solar heating by O3 abs
         write(*,'(3x,a,es12.5)') 'scsza= ',scsza(iz)
         if( jproc > 0 ) write(*,'(6x,2(a,es12.5))') 'logJ= ',
     $        logj(jproc),' ; J= ',pht_rates(iz,jproc)
         if( ish > 0 ) write(*,'(6x,2(a,es12.5))') 'logpsh= ',
     $        logpsh(ish),' ; psh= ',psh(iz,ish)
      end if
     
  100 if( ier /= 0 ) then
         write(*,*) ' !!!! Input scszak grid:'
         do ki = 1, nk
            write(*,'(a,i2,3(a,es12.5))')'   ki= ',ki,' ; scszak= ',
     $          scszak(iz,ki,lat),' ; logjk= ',logjk(iz,jproc,ki,lat),
     $          ' ; logpshk= ',logpshk(iz,ish,ki,lat)
         end do
         if( debug )stop'INTERP_PHO_CHEM: Fatal Error - see stdout' 
      end if

      end subroutine INTERP_PHO_CHEM
