! subversion Id for THIS file : $Id: res_hdr.f 8506 2016-01-28 10:09:58Z simonc $
! $URL: http://svn-ae/ae/COMMON/stand-alone/atmos_models/socrates/tags/07.18.01/src/preproc/src/res_hdr.f $
!-----------------------------------------------------------------------

      subroutine RES_HDR( plon, &
                          plat, &
                          plev, &
                          jintmx, &
                          nxpt, &
                          cpucnt )

      implicit none

!-----------------------------------------------------------------------
!        ... The arguments
!-----------------------------------------------------------------------
      integer, intent(in) ::    plon
      integer, intent(in) ::    plat
      integer, intent(in) ::    plev
      integer, intent(in) ::    jintmx
      integer, intent(in) ::    nxpt
      integer, intent(in) ::    cpucnt

!-----------------------------------------------------------------------
!        ... The local variables
!-----------------------------------------------------------------------
      logical      :: lexist
      
      INQUIRE( file = 'res.h', exist = lexist )
      if( lexist ) then
	 call SYSTEM( 'rm res.h' )
      end if
      OPEN( unit = 30, file = 'res.h' )

      write(30,'(''# define PLEV '',i3)') plev
      write(30,'(''# define PLEVP '',i3)') plev + 1
      write(30,'(''# define PLEVM '',i3)') plev - 1
      write(30,'(''# define NLON '',i3)') plon
      write(30,'(''# define PLAT '',i3)') plat
      write(30,'(''# define PLATM '',i3)') plat - 1
      if( MOD(plat,2) /= 0 ) then
         write(30,'(''# define EQUATOR '',i3)') plat/2 + 1
      end if
      
      CLOSE(30)
      
      end subroutine RES_HDR
