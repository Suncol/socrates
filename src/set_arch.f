! subversion Id for THIS file : $Id: set_arch.f 8506 2016-01-28 10:09:58Z simonc $
! $URL: http://svn-ae/ae/COMMON/stand-alone/atmos_models/socrates/tags/07.18.01/src/set_arch.f $
!-----------------------------------------------------------------------
      subroutine SET_ARCH( arch_set )
!-----------------------------------------------------------------------
!	... Set the archive times and groups
!           At v6s20: each group goes to a different netcdf archive file
!                     group 'dvout' goes to an ASCII (*.dat) file
!                     group 'save' will overwrite values for current time
!
!                                      v6s20, March 2001 - simonc@oma.be
!-----------------------------------------------------------------------
      use SPC_NAMES
      use SIM_CONTROLS, only : jump_chem, model_type, arch_dir
     $                       , arch_month_avg, label_short
     $                       , sim_start_time, sim_stop_time
      use ARCH_WHAT, only : arch, dvout,                                ! output
     $                     max_arch_dates, max_dvout_dates, n_arch_modes
      use TIME_CONTROLS, only : TIMING, ARCH_SETTING
      use DATE_UTILS, only : DATE2TIME, TIME2DATE

      implicit none

!-----------------------------------------------------------------------
!	... Dummy args
!-----------------------------------------------------------------------
      type( ARCH_SETTING ), intent(in) :: arch_set(99)

!-----------------------------------------------------------------------
!	... Local variables
!-----------------------------------------------------------------------
      logical :: found
      integer :: i, j, inc, days0, ndt(n_arch_modes)
      character(len=128) :: flnm
      type( TIMING ) :: t0, t1
      
!-----------------------------------------------------------------------
!	... No archiving when no arch_set in input namelist io_parms
!-----------------------------------------------------------------------
      if( .not. ANY( .not. arch_set(:)%increment >= 0 ) ) return
      if( label_short == ' ' ) stop'SET_ARCH error: label_short not set'

!-----------------------------------------------------------------------
!	... Initialize the archive groups, set them to inactive
!           In group j, the diurnal averages of species ic will be archived
!                if arch(j)%vid_qn2da(ic) is .true.
!           In group j, the noon & midnight values of species ic will be 
!                archived if arch(j)%vid_qn2dv(ic) is .true.
!                This can also include sunrise & sunset values, and 
!                lifetimes at noon & midnight, depending on the group
!           The mode 'dvout' is treated differently (one netCDF file per date)
!-----------------------------------------------------------------------
      do j = 1, n_arch_modes
         arch(j)%active = .false.
         arch(j)%defined = .false.
         arch(j)%vid_qn2da(:) = .false.
         arch(j)%vid_qn2dv(:) = .false.
         arch(j)%days0do(:) = -999
      end do

      arch(1)%mode = 'basic'
      arch(1)%vid_qn2da( (/ vid_no, vid_o3,  vid_o3p, vid_h,  
     $                      vid_oh, vid_o2, vid_ch4, vid_co2, 
     $                      vid_h2o, vid_x /) ) = .true.     ! also families
      
      arch(2)%mode = 'heat '
      if( jump_chem ) then
         arch(2)%vid_qn2da( (/ vid_no, vid_ch4, vid_co2, vid_h2o
     $                          , vid_x  /) ) = .true.
         goto 100
      end if
      arch(2)%vid_qn2da( (/ vid_ch4, vid_h2o, vid_co2, vid_co, vid_h2, 
     $                      vid_o3, vid_o2, vid_no, vid_x /) ) = .true. 
      arch(2)%vid_qn2dv( (/ vid_o3, vid_oh, vid_o3p, vid_h /) ) = .true.

      arch(3)%mode = 'chem '
      arch(3)%vid_qn2da(:) = .true.                          ! also families
      arch(3)%vid_qn2dv = arch(2)%vid_qn2dv   ! Diuvar ouptuts same as ' heat'
      
      arch(4)%mode = 'chem2'
      arch(4)%vid_qn2dv( (/ vid_o3, vid_oh, vid_o3p, vid_h, vid_ho2,
     $                      vid_no, vid_no2, vid_h2o /) ) = .true.
      
      arch(5)%mode = 'ubaim'
      arch(5)%vid_qn2dv( (/ vid_ch2o, vid_ch4, vid_cl2, vid_clono2, 
     $ vid_co, vid_co2, vid_h, vid_h2, vid_h2o, vid_hbr, vid_hcl,
     $ vid_hno3, vid_n, vid_n2o, vid_n2o5, vid_no, vid_no2, vid_o3p,
     $ vid_o2, vid_o2dg, vid_o2s, vid_o3, vid_oh, vid_h2o2 /) ) = .true.
      
      arch(6)%mode = 'full '
      arch(6)%vid_qn2da(:) = .true.                          ! also families
      arch(6)%vid_qn2dv = arch(3)%vid_qn2dv   ! Diuvar ouptuts same as ' chem'
       
      arch(7)%mode = 'save '
      arch(7)%vid_qn2da( (/ vid_h2o, vid_co2, vid_o3, vid_no, vid_o2, 
     $                      vid_o3p /) ) = .true.  ! needed by HEAT_IRCOOL
      arch(7)%vid_qn2dv(:) = .true.                ! only noon values
      
      arch(8) = arch(2)    ! same as heat output
      arch(8)%mode = 'ma   '

  100 continue
      if( ALL( arch_set(:)%increment < 0 ) ) goto 200
      
!-----------------------------------------------------------------------
!	... Using the user-specified arch_set(:), find which groups will 
!           be archived and at which dates
!-----------------------------------------------------------------------
      ndt(:) = 0
      do i = 1, 99
         if( arch_set(i)%increment >= 0 ) then
            if( arch_set(i)%mode == 'dvout' ) cycle    ! This case is trated below
            found = .false.
            do j = 1, n_arch_modes - 1
               t0 = arch_set(i)%start_time
               call DATE2TIME( t0 )
               if( arch_set(i)%increment == 0 ) then  ! days0 loop executed once
                  inc = 1
                  t1 = t0
                else
                  inc = arch_set(i)%increment
                  t1 = arch_set(i)%stop_time
                  call DATE2TIME( t1 )
                  if( t1%days0 <= t0%days0 ) then
                     write(*,*)'SET_ARCH processing arch_set(',i,'):'
                     write(*,*)' increment > 0 & end date <= begin date'
                     stop 'SET_ARCH error: end date <= begin date'
                  end if
               end if
               if( arch_set(i)%mode == arch(j)%mode ) then
                  found = .true.
                  arch(j)%active = .true.
                  do days0 = t0%days0, t1%days0, inc
                     ndt(j) = ndt(j) + 1
                     if( ndt(j) > max_arch_dates ) then
                        write(*,*)'SET_ARCH processing arch_set(',i,'):'
                        write(*,*) ' reached max nb of ' //
     $                          'archiving dates, mode ' // arch(j)%mode
                        stop 'SET_ARCH error: max nb of arch dates'
                     end if
                     arch(j)%days0do(ndt(j)) = days0
                  end do
               end if
            end do
            if( .not. found ) then
               write(*,*) 'SET_ARCH processing arch_set(',i,'):'
               write(*,*) ' wrong mode "' // arch_set(i)%mode // '"'
               write(*,*) ' The only authorized modes are "dvout" and..'
               write(*,*) ( '"'//arch(j)%mode//'" ,' ,j=1,n_arch_modes )
               stop 'SET_ARCH error: wrong mode in arch_set(:)'
            end if
         end if
      end do
      
      if( model_type /= 'two_d' .and. arch(7)%active ) then
         write(*,*) 'SET_ARCH error: model running in mode ',model_type
         write(*,*) '  conflicts with some arch_set(:)%mode="save " '
         write(*,*) '  Wrong set_arch in input namelist io_parms'
         stop 'SET_ARCH error: model not two_d but arch_set to "save "'
      end if

  200 continue
      arch(8)%active = arch_month_avg
      
!-----------------------------------------------------------------------
!	... The file names are arch_dir // label_short // mode // '.nc'
!-----------------------------------------------------------------------
      do j = 1, n_arch_modes
         if( arch(j)%active ) then
            if( arch(j)%mode /= 'ma   ' ) then
               if(ALL( arch(j)%days0do(1:ndt(j))<sim_start_time%days0 )
     $          .or. ALL(arch(j)%days0do(1:ndt(j))>sim_stop_time%days0))
     $          stop 'SET_ARCH error: dates in active arch out of bound'
            end if
            flnm =  arch_dir(:LEN_TRIM(arch_dir))
     $           // label_short(:LEN_TRIM(label_short)) // '.'
     $           // arch(j)%mode(:LEN_TRIM(arch(j)%mode)) // '.nc'
            i = LEN_TRIM( flnm )
            if( i>60 )stop'SET_ARCH error: arch filename is too long'
            arch(j)%flnm = flnm
         end if
      end do
      if( .not. ANY( arch_set(:)%mode == 'dvout' ) ) return

!-----------------------------------------------------------------------
!       ... Now treat the special 'dvout' mode: creates ONE nc file per arch date
!-----------------------------------------------------------------------
      do i = 1, max_dvout_dates
         dvout(i)%vid_qn2dv = arch(5)%vid_qn2dv      ! Diuvar ouptuts same as 'ubaim'
      end do

      inc = 0
      do i = 1, 99
         if( arch_set(i)%mode /= 'dvout' ) cycle
         if( arch_set(i)%increment /= 0 ) then
            write(*,*) 'SET_ARCH for arch_set(',i,')= ',arch_set(i)
            write(*,*) ' wrong increment for mode "dvout", should be 0'
            write(*,*) '   i.e. ONE arch date per arch in "dvout" mode'
!! gfortran seems unable to read the second sub-structure (stop_time) -> increment not read properly
!!    this is probably harmless (?)
!            stop 'SET_ARCH error: wrong increment for mode "dvout"'   
         end if
         t0 = arch_set(i)%start_time
         call DATE2TIME( t0 )
         if( t0%days0 < sim_start_time%days0  .or.
     $       t0%days0 > sim_stop_time%days0 ) cycle
         inc = inc + 1
         if( inc > max_dvout_dates ) then
            write(*,*)'SET_ARCH processing arch_set(',i,'): reached...'
            write(*,*) ' max nb of archiving dates for "dvout" mode'
            stop 'SET_ARCH error: max nb of arch dates in "dvout" mode'
         end if

!-----------------------------------------------------------------------
!       ... Creates ONE filename per arch date
!-----------------------------------------------------------------------
         flnm =  arch_dir(:LEN_TRIM(arch_dir))
     $        // label_short(:LEN_TRIM(label_short)) // '.dvout_'
         write(flnm,'(a,i4.4)') flnm(:LEN_TRIM(flnm)), t0%year
         write(flnm,'(a,i2.2)') flnm(:LEN_TRIM(flnm)), t0%month
         write(flnm,'(a,i2.2)') flnm(:LEN_TRIM(flnm)), t0%day
         flnm = flnm(:LEN_TRIM(flnm)) // '.nc'
         j = LEN_TRIM( flnm )
         if(j>60) stop 'SET_ARCH error: filename too long, "dvout" mode'
         dvout(inc)%active = .true.
         dvout(inc)%ascii_def = .false.
         dvout(inc)%flnm = flnm
         dvout(inc)%days0do = t0%days0
      end do
             
      end subroutine SET_ARCH
