! subversion Id for THIS file : $Id: mlynczak93.pho.f 8506 2016-01-28 10:09:58Z simonc $
! $URL: http://svn-ae/ae/COMMON/stand-alone/atmos_models/socrates/tags/07.18.01/src/mlynczak93.pho.f $
!-----------------------------------------------------------------------
      module MLYNCZAK93_PHO_MODULE
      
      use GRID_DIMS, only : niz

      integer, parameter :: nsza = 17
      real, dimension(nsza), parameter :: sza_file = 
     $   (/ 10., 15., 20., 25., 30., 35., 40., 45., 50., 55., 60., 
     $                                   65., 70., 75., 80., 85., 90. /)

      real, dimension(niz,nsza) :: g, gd2, scsza_file
      
      end module MLYNCZAK93_PHO_MODULE
      
!=======================================================================

      subroutine MLYNCZAK93_PHO_INIT( )
!-----------------------------------------------------------------------
!	... Open and read the data file to
!           Compute prod rate of O2(1Sigma) by O2+hv(762nm)->O2(1Sigma)
!           as needed in Mlynczak & al, 1993, JGR, p. 18639
!           param by Mlynczak, 1993, GRL, p. 1439
!	... Get 2d derivatives gd2 to prepare SPLINT interp 
!-----------------------------------------------------------------------
      use PHYS_CST, only : d2r
      use SIM_CONTROLS, only : data_dir, model_type
      use DIAG_CONTROLS, only : ldiag
      use ALLCO, only : zkm
      use ZGRID, only : Hair, zgeo
      use MLYNCZAK93_PHO_MODULE               ! output
      use SUN_UTILS, only : SUN_CHAPMAN
      use NUMERICAL, only : SPLINE
      use ASCII_UTILS, only : NAVU, LSKIP

      implicit none
!-----------------------------------------------------------------------
!	... Local variables
!-----------------------------------------------------------------------
      logical :: ok
      integer :: iunit, ios, iz, iok, iza, lat
      real :: dummy
      character (len=64) :: filenm

      iunit = NAVU()
      filenm = data_dir(:LEN_TRIM(data_dir)) // 'mlynczak93.dat'
      OPEN( unit = iunit, file = filenm, status = 'OLD',
     $      form = 'formatted', iostat = ios )
      if( ios /= 0 ) then
         write(*,*) 'MLYNCZAK93_PHO_INIT : Failed to open ' // filenm
         write(*,*) '   Error code = ',ios
         stop 'MLYNCZAK93_PHO_INIT : Failed to open mlynczak93.dat'
      end if
      call LSKIP( 9, iunit )
      do iz = niz, 1, -1
         read(iunit,*) dummy, g(iz,:)
         if( dummy /= zkm(iz) ) then
            write(*,*) 'MLYNCZAK93_PHO_INIT: error reading data file'
     $          // ', alt grid does not match at iz= ',iz
            stop 'MLYNCZAK93_PHO_INIT: fatal error reading data file'
         end if

!-----------------------------------------------------------------------
!	... Calc secant/Chapman of SZA on file, using initial
!               zgeo and Hair at lat index 9 (approx)
!-----------------------------------------------------------------------
         lat = 9
         if( model_type /= 'two_d' ) lat = ldiag
         do iza = 1, nsza
            scsza_file(iz,iza) = SUN_CHAPMAN( zgeo(lat,iz), 
     $                 Hair(lat,iz), COS( sza_file(iza)*d2r ), iok )
            if(iok/=0) then
               write(*,*)'MLYNCZAK93_PHO_INIT: error at SUN_CHAPMAN'
               write(*,'(3(a,i3),3(a,es16.2))')'  lat= ',lat,' ; iz= '
     $          ,iz,' ; iza= ',iza,' ; zg= ',zgeo(lat,iz),' ; Hair= '
     $          ,Hair(lat,iz),' ; cos= ',COS( sza_file(iza)*d2r )
               stop 'MLYNCZAK93_PHO_INIT: error at SUN_CHAPMAN'
            end if
         end do

!-----------------------------------------------------------------------
!	... Get 2d derivatives gd2 to prepare SPLINT interp 
!-----------------------------------------------------------------------
         call SPLINE( scsza_file(iz,:), g(iz,:), nsza, 
     $                0., 0., gd2(iz,:), iok )
         if(iok /= 0)stop 'MLYNCZAK93_PHO_INIT: error at SPLINE for gd2'
      end do
      CLOSE( iunit )
      
      end subroutine MLYNCZAK93_PHO_INIT

!=======================================================================

      subroutine MLYNCZAK93_PHO( lat, sza, sunlight, scsza, o2s_prod )
!-----------------------------------------------------------------------
!	... Compute prod rate of O2(1Sigma) by O2+hv(762nm)->O2(1Sigma)
!           as needed in Mlynczak & al, 1993, JGR, p. 18639
!           param by Mlynczak, 1993, GRL, p. 1439
!-----------------------------------------------------------------------
      use GRID_DIMS, only : niz
      use MLYNCZAK93_PHO_MODULE               ! input
      use DIAG_CONTROLS, only : debug
      use NUMERICAL, only : SPLINT

      implicit none
!-----------------------------------------------------------------------
!	... Parameters
!-----------------------------------------------------------------------
      real, parameter :: rate_top = 5.34e-9              ! 1/s

!-----------------------------------------------------------------------
!	... Dummy args
!-----------------------------------------------------------------------
      integer, intent(in) :: lat                       ! latitude index
      real, intent(in)    :: sza                       ! Solar Zenith Angle (degrees)
      logical, dimension(niz), intent(in)  :: sunlight
      real,    dimension(niz), intent(in)  :: scsza    ! Secant/Chapman of SZA
      real,    dimension(niz), intent(out) :: o2s_prod ! 1/s

!-----------------------------------------------------------------------
!	... Local variables
!-----------------------------------------------------------------------
      logical :: ok
      integer :: iz, iok, iza
      real :: g_local
      
!-----------------------------------------------------------------------
!	... Get the prod rate by interpolation from the table
!-----------------------------------------------------------------------
      o2s_prod(:) = 0.
      if( sza < sza_file(1) ) then
         o2s_prod(:) = g(:,1) * rate_top
       else if( sza > sza_file(nsza) ) then
         o2s_prod(:) = 0.
       else
         do iz = 1, niz
            if( .not. sunlight(iz) ) cycle
            if( g(iz,nsza) == 1. ) then
               o2s_prod(iz) = rate_top
             else
               call SPLINT( scsza_file(iz,:), g(iz,:), gd2(iz,:), nsza,
     $                      scsza(iz), g_local, iok )
               if( iok/=0 ) stop 'MLYNCZAK93_PHO: error calling SPLINT'
               if( g_local<0. .and. g_local > -1.e-4 .and. sza > 85. )
     $             g_local = 0.
               if( g_local < 0. .or. g_local > 1. ) then
                  write(*,*)'MLYNCZAK93_PHO warning: interp error'
                  write(*,*)'  lat= ',lat,' ; iz= ',iz,' ; sza= ',sza
                  write(*,*)'  scsza= ',scsza(iz),' ; g_local= ',g_local
                  if( debug ) stop 'MLYNCZAK93_PHO: interpolation error'
                  g_local = 0.
               end if
               o2s_prod(iz) = g_local * rate_top
            end if
         end do
      end if
      
      where( .not. sunlight(:) )
         o2s_prod(:) = 0.
      end where

      end subroutine MLYNCZAK93_PHO
