! subversion Id for THIS file : $Id: arch_plug.f 8506 2016-01-28 10:09:58Z simonc $
! $URL: http://svn-ae/ae/COMMON/stand-alone/atmos_models/socrates/tags/07.18.01/src/arch_plug.f $
!-----------------------------------------------------------------------
      subroutine ARCH_PLUG( time )
!----------------------------------------------------------------------
!	... Call the archiving routines depending on 
!           arch(:)%mode & arch(:)%days0do 
!                                        v6s20, Mar 2001, simonc@oma.be
!----------------------------------------------------------------------
      use SIM_CONTROLS, only : model_type
      use ARCH_WHAT, only : n_arch_modes, arch
      use TIME_CONTROLS, only : TIMING

      implicit none

!--------------------------------------------------------------------- 
!	... Dummy arguments
!--------------------------------------------------------------------- 
      type( TIMING ), intent(in) :: time

!-----------------------------------------------------------------------
!	... Local variables
!----------------------------------------------------------------------- 
      integer :: j, k
      
      if( model_type == 'tests' ) return
      
      do j = 1, n_arch_modes
         if( arch(j)%active .and. arch(j)%mode /= 'ma   ' .and.
     $                    ANY( arch(j)%days0do(:) == time%days0 ) ) then
            write(*,'(a,i2,a,i2,a,i4)') arch(j)%mode // ' archive to ' 
     $        // TRIM( ADJUSTL( arch(j)%flnm ) ) // ' , date= ',
     $        time%month,'/',time%day,'/',time%year
            if( arch(j)%mode == 'save ' ) then
               call SAVE_ARCH( arch(j), time )
              else
               call FAMILIES_DIAGS_CHEM( )
               call ARCHIVE( arch(j), time )
            end if
         end if
      end do

      end subroutine ARCH_PLUG
