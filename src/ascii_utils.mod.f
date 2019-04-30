! subversion Id for THIS file : $Id: ascii_utils.mod.f 8506 2016-01-28 10:09:58Z simonc $
! $URL: http://svn-ae/ae/COMMON/stand-alone/atmos_models/socrates/tags/07.18.01/src/ascii_utils.mod.f $
!-----------------------------------------------------------------------
      module ASCII_UTILS
      
      implicit none
      PRIVATE
      PUBLIC :: NAVU, LSKIP, READ_COLDAT
      
      CONTAINS

!=======================================================================

      integer function NAVU( )
!-----------------------------------------------------------------------
!	... Utility function to return index of next
!	    available Fortran unit number.
!-----------------------------------------------------------------------

      implicit none

!-----------------------------------------------------------------------
!	... Local variables
!-----------------------------------------------------------------------
      integer :: unit      ! Index
      logical :: opened

      do unit = 20,99
         inquire( unit=unit, opened=opened )
         if( .not. opened ) then
            NAVU = unit
            return
         end if
      end do

      write(*,*) 'NAVU: Ran out of Fortran unit numbers'
      stop 'NAVU: Ran out of Fortran unit numbers'

      end function NAVU 

!=======================================================================

      subroutine LSKIP( n , iunit )
!--------------------------------------------------------------------
!  	... Skip n lines from file on unit iunit 
!--------------------------------------------------------------------

      implicit none
!--------------------------------------------------------------------
!  	... Dummy args
!--------------------------------------------------------------------
      integer, intent(in) :: n, iunit

!--------------------------------------------------------------------
!  	... Local variables
!--------------------------------------------------------------------
      integer :: l

      do l = 1, n 
         read(iunit,'(1x)')
      end do

      end subroutine LSKIP
       
!=======================================================================

      subroutine  READ_COLDAT( dir, name, n_out, z, val,
     $                         interp_method, on_log, ext_vals )
!----------------------------------------------------------------------
!     General function to read 2-cols (zf, valf) ASCII input file
!     and interpolate it  to (z, val) of dimension n_out. 
!     "dir" is directory of data file and must end with '/'. 
!     "name" is the input file name WITH NO FILE EXTENSION (*.dat assumed)
!     Expected format for input file *.dat :
!         - 3 lines to skip
!         - an integer = number of data points -> nf   (free format)
!         - 3 lines to skip
!         - nf lines of 2 reals: x (or z), f(x) (or f(z)) (free format)
!     
!   Optional keyword interp_method:
!         - set to 'samegrid' if zf and z are identical
!         - set to 'straight' for linear interpolation (DEFAULT)
!         - set to 'splines3' for cubic splines interpolation
!   Optional keywords 'on_log' and 'ext_vals': see INTERP, module NUMERICAL:
!      on_log : set to .false. if you want to interpolate on the values (DEFAULT)
!               set to .true. if you want to interpolate on the logs of values
!      ext_vals : to deal with the values of the target grid which are out of 
!                 the input grid. 
!            Set to 'notouch' to not touch these values
!                   'allzero' to set these output values to zero (DEFAULT)
!                   'asbndry' to set these values to the boundary input values
!                   'extrapo' to extrapolate linearly outside of the target grid.
!                         DANGEROUS! At least the extrapolated values won't
!                         change sign (floor/ceiling set to zero)
!----------------------------------------------------------------------

      use NUMERICAL, only : INTERP
      
      implicit none
!-----------------------------------------------------------------------
!	... parameters
!-----------------------------------------------------------------------
      integer, parameter :: nmax = 10000

!----------------------------------------------------------------------
!	... Dummy args
!----------------------------------------------------------------------
      character(len=*), intent(in) :: dir
      character(len=*), intent(in) :: name
      integer, intent(in) :: n_out
      real, dimension(n_out), intent(in) :: z
      real, dimension(n_out), intent(inout) :: val
      logical, optional, intent(in) :: on_log
      character(len=*), optional, intent(in) :: interp_method, ext_vals

!-----------------------------------------------------------------------
!  	... Local variables
!-----------------------------------------------------------------------
      integer :: ios, l, iz, nf, flen, junk, iunit
      real, dimension(nmax) :: zf, valf
      character(len=8) :: method
      character(len=64) :: filenm
      logical :: ok

      method = 'straight'
      if( PRESENT( interp_method ) ) method = interp_method
      if( method/='samegrid' .and. method/='straight' .and.
     $          method/='splines3' ) then
         write(*,*) 'READ_COLDAT: Warning: "interp_method" keyword'
         write(*,*) '  not understood. Using linear interpolation.'
         method = 'straight'
      end if

      filenm = TRIM(ADJUSTL( dir )) // TRIM(ADJUSTL( name )) // '.dat'
      flen = LEN_TRIM( filenm )
      write(*,*) ' READ_COLDAT: reading ',filenm(:flen)
      iunit = NAVU()
      OPEN( unit = iunit,
     $      file = filenm(:flen),
     $      form = 'FORMATTED',
     $      status = 'OLD',
     $      iostat = ios )
      if( ios /= 0 ) goto 9000
      call LSKIP( 3, iunit)
      read( iunit, * ) nf
      if( nf > nmax ) goto 9100
      call LSKIP( 3, iunit)
      do iz = 1, nf
            READ(iunit,*) zf(iz), valf(iz)
      end do
      close( iunit ) 
      
      if( interp_method == 'samegrid' ) then
         if( nf /= n_out ) goto 9200
         if( ANY( zf(1:n_out) /= z(1:n_out) ) ) goto 9200
         val = valf(1:n_out)
       else if ( interp_method == 'straight' ) then
         call INTERP( n_out, z, val, nf, zf(1:nf), valf(1:nf), 
     $               spline=.false., on_log=on_log, ext_vals=ext_vals, 
     $               ok=ok )
         if( .not. ok ) goto 9300 
       else if ( interp_method == 'splines3' ) then
         call INTERP( n_out, z, val, nf, zf(1:nf), valf(1:nf), 
     $               spline=.true., on_log=on_log, ext_vals=ext_vals, 
     $               ok=ok )
         if( .not. ok ) goto 9400
      end if 
      
      return

 9000 write(*,*) ' READ_COLDAT: Failed to open >',filenm(:flen),'<'
      write(*,*) '   Error code = ',ios
      stop 'Fatal error opening input file'
 9100 write(*,*) ' READ_COLDAT: error with input file ',filenm
      write(*,*) '   The number of data lines ',nf,
     $                     ' is > max of ',nmax
      stop 'Fatal error input file is too long'
 9200 write(*,*) ' READ_COLDAT: error with input file ',filenm
      write(*,*) '   keyword interp_method set to "samegrid" but..'
      write(*,*) '   grid on file different than target grid'
      stop 'Fatal error reading data from input file'
 9300 write(*,*) ' READ_COLDAT: error with input file ',filenm
      write(*,*)'   linear interpolation of data to target grid failed.'
      stop 'Fatal error in linear interpolation of data from input file'
 9400 write(*,*) ' READ_COLDAT: error with input file ',filenm
      write(*,*)'   spline interpolation of data to target grid failed.'
      stop 'Fatal error in spline interpolation of data from input file'
      
      end subroutine READ_COLDAT

!=======================================================================
      
      end module ASCII_UTILS 
