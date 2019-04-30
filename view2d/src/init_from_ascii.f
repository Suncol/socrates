! subversion Id for THIS file : $Id: init_from_ascii.f 8506 2016-01-28 10:09:58Z simonc $
! $URL: http://svn-ae/ae/COMMON/stand-alone/atmos_models/socrates/tags/07.18.01/src/init_from_ascii.f $
!-----------------------------------------------------------------------

      subroutine INIT_FROM_ASCII(  )
!----------------------------------------------------------------------
!    A special initialization routine to use when netcdf is missing
!    (no *.nc restart file is available or the netcdf library couldn't
!    be linked)    -  v6s03    - simonc@oma.be   -   sept 2000
!----------------------------------------------------------------------
      use GRID_DIMS, only : lmax, niz
      use SPECIES_DIMS, only : nbcon
      use VEN1, only : t2d
      use CONC, only : hm2d
      use BACKATM, only : Tmsis, Xmsis, Dmsis, O2, O1, H1, tot
      use SPECIES, only : qn2da, qn2noon
      use SPC_NAMES
      use VEN9, only : xkz
      use SIM_CONTROLS, only : data_dir
      use ASCII_UTILS, only : NAVU, LSKIP

      implicit none

!----------------------------------------------------------------------
!    ... Local variables
!----------------------------------------------------------------------
      integer :: iunit, iz
      character(len=80) :: filenm, desc_text(50)

!-----------------------------------------------------------------------
!      ... Read H2O, O3 and Kzz in data/init_no_nc/*.dat ASCII files
!-----------------------------------------------------------------------
      filenm = data_dir(:LEN_TRIM(data_dir)) // 'init_no_nc/kzz.dat'
      call READ_VIEW2D45_COLDAT( filenm, xkz )
      filenm = data_dir(:LEN_TRIM(data_dir)) // 'init_no_nc/o3.dat'
      call READ_VIEW2D45_COLDAT( filenm, qn2noon(:,:,vid_o3) )
      filenm = data_dir(:LEN_TRIM(data_dir)) // 'init_no_nc/h2o.dat'
      call READ_VIEW2D45_COLDAT( filenm, qn2noon(:,:,vid_h2o) )

!-----------------------------------------------------------------------
!      ... Initialize CO2 at 360 ppm and everything possible from MSIS
!-----------------------------------------------------------------------
      qn2noon(:,:,vid_co2) = 360.e-6
      qn2noon(27,1,vid_x) = 300.e-6            ! v5s10a test
      qn2noon(:,:,vid_o2) = Xmsis(:,:,O2)
      qn2noon(:,81:niz,vid_o3p) = Xmsis(:,81:niz,O1)
      qn2noon(:,81:niz,vid_h) = Xmsis(:,81:niz,H1)
      t2d(:,:) = Tmsis(:,:)
      hm2d(:,:) = Dmsis(:,:,tot)
      
      qn2da(:,:,:) = qn2noon(:,:,:)

      end subroutine INIT_FROM_ASCII

!=======================================================================

      subroutine READ_VIEW2D45_COLDAT( flnm, field )
!-----------------------------------------------------------------------
!    ... Subroutine name is clear enough
!-----------------------------------------------------------------------
      use GRID_DIMS, only : lmax, niz
      use ALLCO, only : zkm
      use ASCII_UTILS, only : NAVU, LSKIP

!-----------------------------------------------------------------------
!    ... Dummy args
!-----------------------------------------------------------------------
      character(len=80), intent(in) :: flnm
      real, intent(out), dimension(lmax,niz) :: field

!-----------------------------------------------------------------------
!    ... Local variables
!-----------------------------------------------------------------------
      integer :: iunit, iz, l, ios
      real :: zstar, coldat(niz)
      
      iunit = NAVU()
      OPEN( unit=iunit, file = flnm(:LEN_TRIM(flnm)), status='OLD',
     $      form = 'formatted', iostat = ios )
      if( ios /= 0 ) then
         write(*,*) ' READ_VIEW2D45_COLDAT : Failed to open...'
         write(*,*) '    >' // flnm(:LEN_TRIM(flnm)) //'<'
         write(*,*) '    Error code = ',ios
         stop 'READ_VIEW2D45_COLDAT : Failed to open ASCII coldat file'
      end if
      call LSKIP( 7, iunit )
 
      do iz = 1, niz
         read(iunit,*) zstar, coldat(iz)
         if( zstar /= zkm(iz) ) then
            write(*,*) ' READ_VIEW2D45_COLDAT : error reading file...'
            write(*,*) '   >' // flnm(:LEN_TRIM(flnm)) //'<'
            write(*,*) '   altitude= ',zstar,' /= zkm(',iz,')= ',zkm(iz)
            stop 'READ_VIEW2D45_COLDAT : error reading ASCII file'
         end if
      end do
      
      close( iunit )
      
      do l = 1, lmax
         field(l,:) = coldat(:)
      end do
         

      end subroutine READ_VIEW2D45_COLDAT
