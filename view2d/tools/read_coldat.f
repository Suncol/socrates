      program TEST
      
      integer, parameter :: niz = 121
      real, dimension(niz) :: zkm, vals, p
      character(len=64) :: data_dir, filenm

      zkm(1:niz) = (/ (FLOAT(iz-1),iz=1,niz) /)
      p(:) =  1.013e5 * EXP( -zkm(:) / 7. )
      data_dir = '/home/simonc/socrates/data/'
      filenm = data_dir(:LEN_TRIM(data_dir)) // 'fix_N2_vmr.dat'
      call READ_COLDAT( filenm, niz, 2, 1, p, vals)
      do iz = 1, niz
         write(*,*) zkm(iz), vals(iz), p(iz)
      end do
      
      end program TEST

      subroutine READ_COLDAT( filenm, nout, glinepos, dlinepos,
     $                        grid, coldat )

!-----------------------------------------------------------------------
!     General routine to read column data ASCII file
!     Output: coldat: column data from file, eventually interpolated
!                       to the input grid
!     Input:  - filenm: full path and name of input file
!             - nout: dimension of grid and coldat
!             - glinepos: index of the column of input file containing
!                          the grid on the file
!             - dlinepos: index of the column of input file containing
!                          the data to be read
!             - grid: coordinate grid for output variable coldat
!-----------------------------------------------------------------------

      implicit none

!-----------------------------------------------------------------------
!	... Dummy args
!-----------------------------------------------------------------------
      character(len=*), intent(in) :: filenm
      integer, intent(in) :: nout, glinepos, dlinepos
      real, dimension(nout), intent(in) :: grid
      real, dimension(nout), intent(out) :: coldat
      
!-----------------------------------------------------------------------
!  	... Local variables
!-----------------------------------------------------------------------
      real, dimension(:), allocatable :: file_grid, file_coldat
      real, dimension(:), allocatable :: dataline
      integer :: iunit, ios, i, j, ndat, ncol

      if( glinepos == dlinepos ) goto 8900
      if( (glinepos < 0) .or. (dlinepos < 0) ) goto 8910
      iunit = 12
      OPEN( unit = iunit,
     $      file = filenm,
     $      form = 'FORMATTED',
     $      status = 'OLD',
     $      iostat = ios )
      if( ios /= 0 ) goto 9000
      READ(iunit,'(30x,i5)') ndat
      ALLOCATE( file_grid(ndat) )
      ALLOCATE( file_coldat(ndat) )
      READ(iunit,'(30x,i5)') ncol
      if( glinepos > ncol) goto 9010
      if( dlinepos > ncol) goto 9020
      ALLOCATE( dataline(ncol) )
      call LSKIP( 4, iunit)
      do i = 1, ndat
         READ(iunit,*) ( dataline(j), j = 1, ncol )
         file_grid(i) = dataline(glinepos)
         file_coldat(i) = dataline(dlinepos)
      end do
      
      if( file_grid(1) > file_grid(2) ) then
         file_grid(1:ndat) = file_grid(ndat:1:-1)
         file_coldat(1:ndat) = file_coldat(ndat:1:-1)
      end if
      if( nout == ndat ) then
         if( ALL(grid(:) == file_grid(:)) ) then
            coldat(:) = file_coldat(:)
            return
         end if
      end if
      
      call INTER2( nout, grid, coldat, ndat, file_grid, file_coldat )
               
      return

 8900 write(*,*) ' READ_COLDAT: grid column index and data column index'
      write(*,*) '    ... cannot be the same. Aborting.'
      stop 'Fatal error before reading column data ASCII file'
      
 8910 write(*,*) ' READ_COLDAT: grid column index and data column index'
      write(*,*) '    ... must be bigger than zero. Aborting.'
      stop 'Fatal error before reading column data ASCII file'
      
 9000 write(*,*) ' READ_COLDAT: Failed to open ',filenm
      write(*,*) ' Error code = ',ios
      stop 'Fatal error opening column data ASCII file'
      
 9010 write(*,*) ' READ_COLDAT: column index specified for',
     $                                 ' grid data is ',glinepos
      write(*,*) ' This is bigger than the number of columns written'
      write(*,*) '   ... in ASCII file ',filenm
      write(*,*) ' (read ',ncol,' columns per data line). Aborting.'
      stop 'Fatal error reading column data ASCII file'
      
 9020 write(*,*) ' READ_COLDAT: column index specified for',
     $                                 ' data is ',dlinepos
      write(*,*) ' This is bigger than the number of columns written'
      write(*,*) '   ... in ASCII file ',filenm
      write(*,*) ' (read ',ncol,' columns per data line). Aborting.'
      stop 'Fatal error reading column data ASCII file'
      
      end subroutine READ_COLDAT

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

      SUBROUTINE inter2(ng,xg,yg,n,x,y)
*_______________________________________________________________________
* Maps points n,x,y onto ng-1 intervals xg. Result is yg.
*  trapezoidal average y value for each 
*      grid interval
*  x  is an array of input x values (e.g., wavelength)
*  y  is an array of input y values (e.g., quantum yields, or fluxes)
*       corresponding to the values of x
*  n  is the number of input (x,y) data points
*  xg is the array of x values (e.g.,wavelengths) associated with 
*       the endpoints of the grid intervals
*  yg is the array of y values calculated for each grid interval
*  ng is the number of grid delimiters (one more than the number
*       of grid intervals)
*_______________________________________________________________________
*
*    Edit history:
*
*         01/04/95 - Subroutine has been completely rewritten      -SF-
*                    to order loops and eliminate confusing
*                    GOTOs
*_______________________________________________________________________

      IMPLICIT NONE

* input:
      INTEGER ng, n
      REAL x(n), y(n), xg(ng)

* output:
      REAL yg(ng)

* local:
      REAL area, xgl, xgu
      REAL darea, slope
      REAL a1, a2, b1, b2
      INTEGER ngintv
      INTEGER i, k, jstart
*_______________________________________________________________________

*  test for correct ordering of data, by increasing value of x

      DO 10, i = 2, n
         IF (x(i) .LE. x(i-1)) THEN
            WRITE(*,*)'data not sorted'
            STOP
         ENDIF
   10 CONTINUE     

*  find the integral of each grid interval and use this to 
*  calculate the average y value for the interval      
*  xgl and xgu are the lower and upper limits of the grid interval

      jstart = 1
      ngintv = ng - 1
      DO 50, i = 1,ngintv

*  if grid interval is outside data range, set yg to zero      

         IF ((xg(i).LT.x(1)) .OR. (xg(i+1).GT.x(n))) THEN
            yg(i) = 0.

         ELSE

* initalize:

            area = 0.0
            xgl = xg(i)
            xgu = xg(i+1)

*  discard data before the first grid interval and after the 
*  last grid interval
*  for internal grid intervals, start calculating area by interpolating
*  between the last point which lies in the previous interval and the
*  first point inside the current interval

            k = jstart
            IF (k .LE. n-1) THEN

*  if both points are before the first grid, go to the next point
   30         CONTINUE
                IF (x(k+1) .LE. xgl) THEN
                   jstart = k - 1
                   k = k+1
                   IF (k .LE. n-1) GO TO 30
                ENDIF


*  if the last point is beyond the end of the grid, complete and go to the next
*  grid
   40         CONTINUE
                 IF ((k .LE. n-1) .AND. (x(k) .LT. xgu)) THEN          

                    jstart = k-1

* compute x-coordinates of increment

                    a1 = MAX(x(k),xgl)
                    a2 = MIN(x(k+1),xgu)

*  if points coincide, contribution is zero

                    IF (x(k+1).EQ.x(k)) THEN
                       darea = 0.e0
                    ELSE
                       slope = (y(k+1) - y(k))/(x(k+1) - x(k))
                       b1 = y(k) + slope*(a1 - x(k))
                       b2 = y(k) + slope*(a2 - x(k))
                       darea = (a2 - a1)*(b2 + b1)/2.
                    ENDIF


*  find the area under the trapezoid from a1 to a2

                    area = area + darea

* go to next point
              
                    k = k+1
                    GO TO 40

                ENDIF

            ENDIF

*  calculate the average y after summing the areas in the interval
            yg(i) = area/(xgu - xgl)

         ENDIF
   50 CONTINUE
*_______________________________________________________________________

      RETURN
      END
