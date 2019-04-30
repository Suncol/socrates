! subversion Id for THIS file : $Id: monthly_avg.f 8506 2016-01-28 10:09:58Z simonc $
! $URL: http://svn-ae/ae/COMMON/stand-alone/atmos_models/socrates/tags/07.18.01/src/monthly_avg.f $
!-----------------------------------------------------------------------
      subroutine MONTHLY_AVG( time )
!----------------------------------------------------------------------
!	... Calculate the monthly avergaes of T, totdens, zgeo & chem vars
!                                        v7s15, Feb 2003, simonc@oma.be
!----------------------------------------------------------------------
      use GRID_DIMS, only : lmax, niz
      use SPECIES_DIMS, only : nbcon
      use VEN1, only  : t2d
      use VEN2, only  : u
      use VEN6, only  : x,v,w
      use CONC, only  : hm2d
      use ZGRID, only : zgeo
      use SPECIES, only : qn2da, qn2noon, qn2night
      use MONTHLY_VAR                                         ! output
      use SIM_CONTROLS, only : arch_month_avg
      use TIME_CONTROLS, only : TIMING

      implicit none

!--------------------------------------------------------------------- 
!	... Dummy arguments
!--------------------------------------------------------------------- 
      type( TIMING ), intent(in) :: time

!-----------------------------------------------------------------------
!	... Local variables
!----------------------------------------------------------------------- 
      integer, save ::  mon(12) = (/ 31, 28, 31, 30, 31, 30,
     $                               31, 31, 30, 31, 30, 31 /)
      logical, save :: month1begun = .false.
          
      if( time%day == 1 ) month1begun = .true.
      if( .not. month1begun ) return
      
      t_ma = t_ma + t2d
      u_ma = u_ma + u
      v_ma = v_ma + v
      w_ma = w_ma + w
      zgeo_ma = zgeo_ma + zgeo
      ntot_ma = ntot_ma + hm2d
      ma2 = ma2 + qn2da
      ma2noon = ma2noon + qn2noon
      ma2night = ma2night + qn2night
      
      if( time%day == mon(time%month) ) then
         t_ma = t_ma / mon(time%month)
         u_ma = u_ma / mon(time%month)
         v_ma = v_ma / mon(time%month)
         w_ma = w_ma / mon(time%month)
         zgeo_ma = zgeo_ma / mon(time%month)
         ntot_ma = ntot_ma / mon(time%month)
         ma2 = ma2 / mon(time%month)
         ma2noon = ma2noon / mon(time%month)
         ma2night = ma2night / mon(time%month)

         if( arch_month_avg ) call MONTHLY_ARCH( time )
         
         t_ma = 0.
         u_ma = 0.
         v_ma = 0.
         w_ma = 0.
         zgeo_ma = 0.
         ntot_ma = 0.
         ma2 = 0.
         ma2noon = 0.
         ma2night = 0.
      end if
      
      contains
      
!=======================================================================

      subroutine MONTHLY_ARCH( time )
!-----------------------------------------------------------------------
!	... Archive the monthly averages, using settings in arch(8)
!-----------------------------------------------------------------------

      use SIM_CONTROLS, only : arch_dir, label_short
      use ARCH_WHAT, only : arch
      use NETCDF, only : nf_write
      use NETCDF_UTILS, only : OPEN_FILE,CLOSE_FILE,SET_TIME,NETCDF_ARCH

      implicit none

!-----------------------------------------------------------------------
!	... Dummy args
!-----------------------------------------------------------------------
      type( TIMING), intent(in)  ::  time

!----------------------------------------------------------------------- 
!	... Local variables
!-----------------------------------------------------------------------
      integer :: i
      
       write(*,'(a,i2,a,i2,a,i4)') arch(8)%mode // ' archive to ' 
     $     // TRIM( ADJUSTL( arch(8)%flnm ) ) // ' , date= ',
     $       time%month,'/',time%day,'/',time%year
     
      if( .not. arch(8)%defined ) then
         call ARCHIVE_INIT( arch(8) ) 
       else                                    ! Open the file for writing
         call OPEN_FILE( arch(8)%flnm, nf_write )
      end if

!----------------------------------------------------------------------- 
!	... Set the time & Output the next time
!----------------------------------------------------------------------- 
      call SET_TIME( REAL( time%days0 - 15 ) )
      call NETCDF_ARCH( 'time' )

!-----------------------------------------------------------------------
!	... Output temperature, zonal wind, meridional wind & total density
!----------------------------------------------------------------------- 
      call ARCH_VAR2D( arch(8), 'temperature', 'K', t_ma )
      call ARCH_VAR2D( arch(8), 'u', 'm/s', u_ma )
      call ARCH_VAR2D( arch(8), 'v', 'm/s', v_ma)
      call ARCH_VAR2D( arch(8), 'w', 'm/s', w_ma )
      call ARCH_VAR2D( arch(8), 'totdens', 'molec/cm3', ntot_ma )

!-----------------------------------------------------------------------
!	... Output the same chemical variables as the "heat" archive (arch(2))
!----------------------------------------------------------------------- 
      do i = 1, nbcon
         if( arch(8)%vid_qn2da(i) ) 
     $      call ARCH_CHEM( arch(8), i, '', 'vmr', ma2 )
         if( arch(8)%vid_qn2dv(i) ) then
            call ARCH_CHEM( arch(8), i, 'noon', 'vmr', ma2noon )
            call ARCH_CHEM( arch(8), i, 'night', 'vmr', ma2night )
         end if
      end do   
      
      call CLOSE_FILE()
      arch(8)%defined = .true.
     
      end subroutine MONTHLY_ARCH

      end subroutine MONTHLY_AVG
