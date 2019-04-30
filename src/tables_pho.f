! subversion Id for THIS file : $Id: tables_pho.f 8506 2016-01-28 10:09:58Z simonc $
! $URL: http://svn-ae/ae/COMMON/stand-alone/atmos_models/socrates/tags/07.18.01/src/tables_pho.f $
!-----------------------------------------------------------------------

      subroutine TABLES_PHO( cal_day )
!-----------------------------------------------------------------------
!    ... This routine calculates the solar parameters 
!        scszak (secant/Chapman of solar zenith angle) and 
!        logjk (log of the corresponding photolysis rates J) in tables
!        to be interpolated in INTERP_PHO_CHEM and HEAT_RATES
! v7s08: prepared OpenMP , does not work yet (see v7s_diary.txt)
!-----------------------------------------------------------------------
      use GRID_DIMS, only : lmax, niz
      use SPECIES_DIMS, only : nbcon
      use CHEM_MODS, only : phtcnt
      use PHO_PARMS, only : njpd, nk, minlog                   ! nk=njpd+2
      use CONC, only : hm2d
      use ALLCO, only : omp_lats
      use SPECIES, only : qn2noon                              ! input
      use SOLDAY, only : scszak, logjk, logpshk                ! output
      use HEAT_TERMS, only : nsh
      use SIM_CONTROLS, only : ncpus
      use DIAG_CONTROLS, only : debug, diags, ldiag, zdiag, jdiag
      use RXT_NAMES                                            ! for diags

      implicit none

!-----------------------------------------------------------------------
!    ... Dummy args
!-----------------------------------------------------------------------
      real, intent(in)    :: cal_day            ! day of year, 1.->365. or more

!-----------------------------------------------------------------------
!    ... Local variables
!-----------------------------------------------------------------------
      logical :: diags_lat
      logical, dimension(niz) :: polar_night, sunlight
      integer :: i, ki, jproc, l, lat, iz, ier(lmax)
      real ::  ck_noon(niz,nbcon),tjlat(niz,phtcnt), presolheat(niz,nsh)

      ier(:) = 0

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!$OMP PARALLEL DO
!!!!$OMP&PRIVATE(l,lat,i,ki,polar_night,sunlight,ck_noon,tjlat,presolheat)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      do l = 1, lmax

         lat = omp_lats(l)

         logjk(:,:,1:nk,lat) = minlog      ! to set below for ki=1,1+njpd
         logpshk(:,:,1:nk,lat) = minlog    ! to set below for ki=1,1+njpd

!-----------------------------------------------------------------------
!	... Calculate the SCSZA for J tables
!-----------------------------------------------------------------------
         call TABLES_SUN( cal_day, lat, polar_night, scszak(:,:,lat) )
         if( ncpus==1 .and. ALL(polar_night(:)) ) cycle   ! polar night at all altitudes
     
!-----------------------------------------------------------------------
!         ... Calculate the corresponding photodissociation rates tjlat 
!           and heating rates precursors presolheat
!-----------------------------------------------------------------------
         do i = 1, nbcon
            ck_noon(:,i) = qn2noon(lat,:,i) * hm2d(lat,:)
         end do
         sunlight(:) = .not. polar_night
         do ki = 1+1, 1+njpd
            call PLUG_PHO( lat, sunlight, scszak(:,ki,lat), ck_noon,
     $                     tjlat, presolheat )
            if( ANY( presolheat(:,:) < 0. ) ) ier(lat) = -1

!-----------------------------------------------------------------------
!    ... Create the functions logjk=f(scsza) and logpshk=f(scsza)
!        on which the J and presolheat will be interpolated, respectively
!        in INTERP_PHO_CHEM and HEAT_RATES
!-----------------------------------------------------------------------
            where( tjlat(:,:) > 1.e-60 )         
               logjk(:,:,ki,lat) = LOG( tjlat(:,:) )
             else where
               logjk(:,:,ki,lat) = minlog
            end where
            where( presolheat(:,:) > 1.e-60 )
               logpshk(1:niz,1:nsh,ki,lat) = LOG( presolheat(:,:) )
             else where 
               logpshk(1:niz,1:nsh,ki,lat) = minlog
            end where
         end do

!-----------------------------------------------------------------------
!         ... Add extra values for sun too high (ki=1) and sun too low (ki=nk)
!-----------------------------------------------------------------------
         logjk(:,:,1,lat) = logjk(:,:,2,lat)
         logpshk(1:niz,1:nsh,1,lat) = logpshk(1:niz,1:nsh,2,lat)
         logjk(:,:,nk,lat) = minlog
         logpshk(1:niz,1:nsh,nk,lat) = minlog

      end do
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!$OMP END PARALLEL DO
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!-----------------------------------------------------------------------
!	... Diagnostics & error reporting
!-----------------------------------------------------------------------  
      do lat = 1, lmax
         diags_lat = diags .and. (lat == ldiag)
         if( diags_lat .or. ier(lat) /= 0 ) then
            if( ier(lat) == 0 ) iz = zdiag
            write(*,'(2(a,i3),a,f6.1)') 'TABLES_PHO @ (',lat,',',iz,
     $            ') ; cal_day= ',cal_day
            do ki = 1, nk
               write(*,'(3(a,i3,a,es12.5))') '   ki= ',ki,
     $           ' ; scszak= ',scszak(iz,ki,lat),
     $           ' ; logjk(jdiag=',jdiag,')= ',logjk(iz,jdiag,ki,lat)
            end do
            if( ier(lat) /= 0 ) then
               write(*,*) 'TABLES_PHO: error code ier= ',ier(lat)
               if( debug ) stop 'TABLES_PHO: fatal error.'
            end if
         end if
      
      end do

      end subroutine TABLES_PHO
