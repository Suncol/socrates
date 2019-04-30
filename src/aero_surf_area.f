! subversion Id for THIS file : $Id: aero_surf_area.f 8506 2016-01-28 10:09:58Z simonc $
! $URL: http://svn-ae/ae/COMMON/stand-alone/atmos_models/socrates/tags/07.18.01/src/aero_surf_area.f $
!-----------------------------------------------------------------------

      subroutine AERO_SURF_AREA( aero_surf )
!-----------------------------------------------------------------------
!   	... Read aerosol surface areas
!-----------------------------------------------------------------------
      use SIM_CONTROLS, only : data_dir
      use PHO_AERO, only :  aedat1
      use ASCII_UTILS, only : NAVU, LSKIP

      implicit none

!-----------------------------------------------------------------------
!   	... Dummy args
!-----------------------------------------------------------------------
      real, intent(out) :: aero_surf(35,121)

!-----------------------------------------------------------------------
!   	... Local variables
!-----------------------------------------------------------------------
      integer :: iunit = 12
      integer :: lat, il, ill, iz, ik, ios
      integer :: izt
      integer :: izz, imonth
      real :: aill
      real :: wmo_uadp(37,31,2)  !(l,iz,month)
      character(len=64) :: filenm

!-----------------------------------------------------------------------
!	... READ aerosol surface area density from WMO for april 15
!           and oct 15 from SURFACE_AREA.dat . 
!-----------------------------------------------------------------------
      filenm = data_dir(:LEN_TRIM(data_dir)) //
     $          'SURFACE_AREA.dat'
      iunit = NAVU()
      OPEN( unit = iunit,
     $      file = filenm,
     $      form = 'formatted',
     $      status = 'OLD',
     $      err = 99,
     $      iostat = ios )
!-----------------------------------------------------------------------
!  	... from SURFACE_AREA.dat  for background aerosol case. 
!-----------------------------------------------------------------------
      call LSKIP( 19, iunit)
      do imonth = 1,2
         call LSKIP( 2, iunit)
         READ(iunit,*) ((wmo_uadp(lat,iz,imonth),iz=1,31), 
     $   lat=1,37)
      end do
      CLOSE( iunit )

      aero_surf = 0.

      do il = 1,35
         do iz = 1,31
            izz = 2*iz-1
            aero_surf(il,izz) = wmo_uadp(il+1,iz,2)
         end do
      end do

      do il = 1,35
         do iz = 2,60,2
            aero_surf(il,iz) = (aero_surf(il,iz-1)
     $                          + aero_surf(il,iz+1))*.5
         end do
      end do
      return

!-----------------------------------------------------------------------
!     	... Open error
!-----------------------------------------------------------------------
99    write(*,'(''Error return '',i5,'' opening file: '',a)')
     $        ios, filenm
      stop

1224  format(12(1x,f5.2))

      end subroutine AERO_SURF_AREA
