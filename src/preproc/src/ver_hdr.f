! subversion Id for THIS file : $Id: ver_hdr.f 8506 2016-01-28 10:09:58Z simonc $
! $URL: http://svn-ae/ae/COMMON/stand-alone/atmos_models/socrates/tags/07.18.01/src/preproc/src/ver_hdr.f $
!-----------------------------------------------------------------------

      subroutine VER_HDR( options, &
                          ohstflag, &
                          diagprnt, &
                          tavgprnt, &
                          srf_flx_cnt, &
                          dvel_cnt, &
			  machine )

      implicit none

!-----------------------------------------------------------------------
!        ... The arguments
!-----------------------------------------------------------------------
      integer, intent(in)  ::      srf_flx_cnt      ! species with srf flux
      integer, intent(in)  ::      dvel_cnt         ! species with dep vel
      character(len=8), intent(in) :: machine       ! target hardware
      logical, intent(in)  ::      options(*)       ! options array
      logical, intent(in)  ::      ohstflag         ! hist tape write flag
      logical, intent(in)  ::      diagprnt         ! chktrc, negtrc diag printout flag
      logical, intent(in)  ::      tavgprnt         ! time averaged printout flag

!-----------------------------------------------------------------------
!        ... The local variables
!-----------------------------------------------------------------------
      integer           :: i
      character(len=6)  :: opts(6)
      logical           :: lexist
      
      opts(1) = 'CHEM'
      opts(3) = 'VDIFF'
      opts(4) = 'CMFA'
      opts(5) = 'NORMS'
      opts(6) = 'CONCHK'
      
      INQUIRE( file = 'version.h', exist = lexist )
      if( lexist ) then
	 call SYSTEM( 'rm version.h' )
      end if
      OPEN( unit = 30, file = 'version.h' )

      if( options(1) ) then
         write(30,'(''# define CHEM'')')
      end if

      if( options(2) ) then
         write(30,'(''# define CRAY'')')
      else if( machine == 'RS6000' .or. machine == 'IBM' ) then
         write(30,'(''# define IBM'')')
      else if( machine == 'SGI' ) then
         write(30,'(''# define SGI'')')
      else if( machine == 'DEC' ) then
         write(30,'(''# define DEC'')')
      else if( machine == 'HP' ) then
         write(30,'(''# define HP'')')
      end if

      if( ohstflag ) then
         write(30,'(''# define HISTTAPE'')')
      end if

      if( diagprnt ) then
         write(30,'(''# define DIAGPRNT'')')
      end if

      if( tavgprnt ) then
         write(30,'(''# define TAVGPRNT'')')
      end if

      if( options(12) ) then
         write(30,'(''# define RXTNLOOKUP'')')
      end if

      if( options(14) ) then
         write(30,'(''# define F90'')')
      end if

      if( options(16) ) then
         write(30,'(''# define USRHOOK'')')
      end if

      if( options(17) ) then
         write(30,'(''# define MODULES'')')
      end if

      CLOSE(30)
      
      end subroutine VER_HDR
