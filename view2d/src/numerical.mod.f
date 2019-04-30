! subversion Id for THIS file : $Id: numerical.mod.f 8506 2016-01-28 10:09:58Z simonc $
! $URL: http://svn-ae/ae/COMMON/stand-alone/atmos_models/socrates/tags/07.18.01/src/numerical.mod.f $
!-----------------------------------------------------------------------
      module NUMERICAL
      
      implicit none
      PRIVATE
      PUBLIC :: CGEFA_F90, CGEDI_F90, LINPOL, TRIDLA, TRIDEC, TRISLV,
     $          BAND_FAC, BAND_SLV, SPLINE, SPLINT, INTERP, HPSORT

      CONTAINS  

!=======================================================================

      real function CABS1( zdum )
      complex, intent(in) :: zdum
      CABS1 = ABS(REAL(zdum)) + ABS(AIMAG(zdum))
      end function CABS1

!=======================================================================
       
      subroutine CGEFA_F90( a, n, ipvt, info )
!-------------------------------------------------------------
!     	... Factors a complex matrix by gaussian elimination
!           cgefa is usually called by cgeco, but it can be called
!           directly with a saving in time if  rcond  is not needed.
!           (time for cgeco) = (1 + 9/n)*(time for cgefa) .
!             Aug 10, 1998: - Subroutine renamed from CGEFA to 
!              CGEFA_F90 to avoid confusion with locally installed libraries.
!                           - Statement function CABS1 made internal fct 
!                              (didn't compile under HP f90 v2.0)
!-------------------------------------------------------------

!-------------------------------------------------------------
!     on entry
!
!        a       complex(lda, n)
!                the matrix to be factored.
!
!        n       integer
!                the order of the matrix  a .
!
!     on return
!
!        a       an upper triangular matrix and the multipliers
!                which were used to obtain it.
!                the factorization can be written  a = l*u  where
!                l  is a product of permutation and unit lower
!                triangular matrices and  u  is upper triangular.
!
!        ipvt    integer(n)
!                an integer vector of pivot indices.
!
!        info    integer
!                = 0  normal value.
!                = k  if  u(k,k) .eq. 0.0 .  this is not an error
!                     condition for this subroutine, but it does
!                     indicate that cgesl or cgedi will divide by zero
!                     if called.  use  rcond  in cgeco for a reliable
!                     indication of singularity.
!
!     linpack. this version dated 08/14/78 .
!     cleve moler, university of new mexico, argonne national lab.
!
!     subroutines and functions
!
!     blas caxpy,cscal,icamax
!     fortran abs,aimag,real
!-------------------------------------------------------------

      implicit none

!-------------------------------------------------------------
!	... Dummy args
!-------------------------------------------------------------
      integer, intent(in)    :: n
      integer, intent(out)   :: info
      integer, intent(out)   :: ipvt(:)
      complex, intent(inout) :: a(:,:)

!-------------------------------------------------------------
!	... Local variables
!-------------------------------------------------------------
      complex :: t
      integer :: j, k, kp1, l

!-------------------------------------------------------------
!     	... Check matrix and pivot dimensions
!-------------------------------------------------------------
      if( SIZE( a, dim = 1 ) < n .or. SIZE( a, dim = 2 ) < n
     $                           .or. SIZE( ipvt ) < n ) then
	 info = -1
	 return
      end if

!-------------------------------------------------------------
!     	... Gaussian elimination with partial pivoting
!-------------------------------------------------------------
      info = 0
      if( n >= 2 ) then
         do k = 1,n-1
            kp1 = k + 1
!-------------------------------------------------------------
! 	... Find l = pivot index
!-------------------------------------------------------------
            l = ICAMAX( a(k:n,k) ) + k - 1
            ipvt(k) = l
!-------------------------------------------------------------
!	... Zero pivot implies this column already triangularized
!-------------------------------------------------------------
            if( CABS1( a(l,k) ) /= 0. ) then
!-------------------------------------------------------------
!     	... Interchange if necessary
!-------------------------------------------------------------
               if( l /= k ) then
                  t = a(l,k)
                  a(l,k) = a(k,k)
                  a(k,k) = t
               end if
!-------------------------------------------------------------
!     	... Compute multipliers
!-------------------------------------------------------------
               t = -(1.,0.) / a(k,k)
	       a(k+1:n,k) = t * a(k+1:n,k)
!-------------------------------------------------------------
!      	... Row elimination with column indexing
!-------------------------------------------------------------
               do j = kp1,n
                  t = a(l,j)
                  if( l /= k ) then
                     a(l,j) = a(k,j)
                     a(k,j) = t
                  end if
		  a(k+1:n,j) = a(k+1:n,j) + t * a(k+1:n,k)
               end do
            else
               info = k
	       return
            end if
         end do
      end if
      ipvt(n) = n
      if( CABS1( a(n,n) ) == 0. ) then
         info = n
      end if

      CONTAINS

      integer function ICAMAX( cx )
!--------------------------------------------------------------
!  	... Find the index of element having max. absolute value.
!           jack dongarra, linpack, 3/11/78.
!--------------------------------------------------------------

      implicit none

!--------------------------------------------------------------
!	... Dummy args
!--------------------------------------------------------------
      complex, intent(in) :: cx(:)

!--------------------------------------------------------------
!	... Local variables
!--------------------------------------------------------------
      real :: smax
      integer :: i, ix, n

      ICAMAX = 0
      n = SIZE( cx )
      if( n < 1 ) then
         return
      end if
      ICAMAX = 1
      if( n == 1 ) then
         return
      end if
      smax = CABS1( cx(1) )
      do i = 2,n
         if( CABS1( cx(i) ) > smax ) then
            ICAMAX = i
            smax = CABS1( cx(i) )
         end if
      end do

      end function ICAMAX
      
      end subroutine CGEFA_F90

!=======================================================================

      subroutine CGEDI_F90( a, n, ipvt, det, job )
!--------------------------------------------------------------
!     	... Computes the determinant and inverse of a matrix
!           using the factors computed by cgeco or cgefa
!--------------------------------------------------------------

!--------------------------------------------------------------
!     on entry
!
!        a       complex(lda, n)
!                the output from cgeco or cgefa.
!
!        n       integer
!                the order of the matrix  a .
!
!        ipvt    integer(n)
!                the pivot vector from cgeco or cgefa.
!
!        work    complex(n)
!                work vector.  contents destroyed.
!
!        job     integer
!                = 11   both determinant and inverse.
!                = 01   inverse only.
!                = 10   determinant only.
!
!     on return
!
!        a       inverse of original matrix if requested.
!                otherwise unchanged.
!
!        det     complex(2)
!                determinant of original matrix if requested.
!                otherwise not referenced.
!                determinant = det(1) * 10.0**det(2)
!                with  1.0 .le. cabs1(det(1)) .lt. 10.0
!                or  det(1) .eq. 0.0 .
!
!     error condition
!
!        a division by zero will occur if the input factor contains
!        a zero on the diagonal and the inverse is requested.
!        it will not occur if the subroutines are called correctly
!        and if cgeco has set rcond .gt. 0.0 or cgefa has set
!        info .eq. 0 .
!
!     linpack. this version dated 08/14/78 .
!     cleve moler, university of new mexico, argonne national lab.
!
!     subroutines and functions
!
!     blas caxpy,cscal,cswap
!     fortran abs,aimag,cmplx,mod,real
!--------------------------------------------------------------

      implicit none

!--------------------------------------------------------------
!	... Dummy args
!--------------------------------------------------------------
      integer, intent(in)    :: n, job
      integer, intent(in)    :: ipvt(:)
      complex, intent(out)   :: det(2)
      complex, intent(inout) :: a(:,:)

!--------------------------------------------------------------
!	... Local variables
!--------------------------------------------------------------
      complex :: t
      complex :: work(n)
      real    :: ten
      integer :: i, j, k, kb, kp1, l

!--------------------------------------------------------------
!     	... Check matrix dimensions
!--------------------------------------------------------------
      if( SIZE( a, dim = 1 ) < n .or. SIZE( a, dim = 2 ) < n
     $                           .or. SIZE( work ) < n ) then
	 return
      end if

!--------------------------------------------------------------
!     	... Compute determinant
!--------------------------------------------------------------
      if( job/10 /= 0 ) then
         det(1) = (1.,0.)
         det(2) = (0.,0.)
         ten = 10.
         do i = 1,n
            if( ipvt(i) /= i ) then
	       det(1) = -det(1)
	    end if
            det(1) = a(i,i)*det(1)
            if( CABS1( det(1) ) == 0. ) then
	       exit
	    end if
	    do
               if( CABS1( det(1) ) < 1. ) then
                  det(1) = CMPLX(ten,0.0e0)*det(1)
                  det(2) = det(2) - (1.0e0,0.0e0)
	       else
		  exit
               end if
	    end do
	    do
               if( CABS1( det(1) ) >= ten ) then
                  det(1) = det(1)/CMPLX(ten,0.0e0)
                  det(2) = det(2) + (1.0e0,0.0e0)
	       else
		  exit
               end if
	    end do
         end do
      end if

!--------------------------------------------------------------
!     	... Compute inverse(u)
!--------------------------------------------------------------
      if( MOD( job,10 ) /= 0 ) then
         do k = 1,n
            a(k,k) = (1.,0.) / a(k,k)
            t = -a(k,k)
	    a(1:k-1,k) = t * a(1:k-1,k)
            kp1 = k + 1
            if( n >= kp1 ) then
               do j = kp1,n
                  t = a(k,j)
                  a(k,j) = (0.,0.)
		  a(1:k,j) = a(1:k,j) + t * a(1:k,k)
               end do
            end if
         end do
!--------------------------------------------------------------
!  	... Form inverse(u)*inverse(l)
!--------------------------------------------------------------
         if( n >= 2 ) then
            do kb = 1,n-1
               k = n - kb
               kp1 = k + 1
               do i = kp1,n
                  work(i) = a(i,k)
                  a(i,k) = (0.,0.)
               end do
               do j = kp1, n
                  t = work(j)
		  a(1:n,k) = a(1:n,k) + t * a(1:n,j)
               end do
               l = ipvt(k)
               if( l /= k ) then
		  work(1:n) = a(1:n,k)
		  a(1:n,k) = a(1:n,l)
		  a(1:n,l) = work(1:n)
	       end if
            end do
         end if
      end if

      end subroutine CGEDI_F90

!=======================================================================

      subroutine LINPOL( x1, y1, x2, y2, x, y )
!--------------------------------------------------------------------
! 	... Linearly interpolate between (x1,y1) and (x2,y2),
!	    the new value at x is y.
!--------------------------------------------------------------------

      implicit none

!--------------------------------------------------------------------
!	... Dummy args
!--------------------------------------------------------------------
      real, intent(in)  ::  x1, y1, x2, y2, x
      real, intent(out) :: y

!--------------------------------------------------------------------
!	... Local variables
!--------------------------------------------------------------------
      real :: slope, intercept

      slope = (y2 - y1) / (x2 - x1)
      intercept = y1 - slope*x1
      y = slope*x + intercept

      end subroutine LINPOL

!=======================================================================

      subroutine TRIDLA( n, sub, diag, sup, x )
!-------------------------------------------------------------------
!
! dimension of           a(n), b(n), c(n), x(n)
! arguments
!
! latest revision        june 1989
!
! purpose                trdi computes the solution of the tridiagonal
!                        linear system,
!                            b(1)*x(1)+c(1)*x(2)               = y(1)
!                            a(i)*x(i-1)+b(i)*x(i)+c(i)*x(i+1) = y(i)
!                                i=2,3,...,n-1
!                            a(n)*x(n-1)+b(n)*x(n)             = y(n)
!
! usage                  call trdi (n, a, b, c, x )
!
! arguments
!
! on input               n
!                          the number of unknowns.  this subroutine
!                          requires that  n  be greater than  2.
!
!                        sub
!                          the subdiagonal of the matrix is stored in
!                          locations a(2) through a(n).
!
!                        diag
!                          the diagonal of the matrix is stored in
!                          locations b(1) through b(n).
!
!                        sup
!                          the super-diagonal of the matrix is stored in
!                          locations c(1) through c(n-1).
!
!                        x
!                          the right-hand side of the equations is
!                          stored in y(1) through y(n).
!
! on output              x
!                          an array which contains the solution to the
!                          system of equations.
!
! special conditions     this subroutine executes satisfactorily
!                        if the input matrix is diagonally dominant
!                        and non-singular.  the diagonal elements
!                        are used to pivot, and no tests are made to
!                        determine singularity.  if a singular, or
!                        nearly singular, matrix is used as input,
!                        a divide by zero or floating point overflow
!                        may result.
!
! precision              single
!
! language               fortran
!
! history                originally written by nancy werner at ncar
!                        in october, 1973. modified by s. walters at
!                        ncar in june 1989 to functionally replace
!                        the cal routine tridla.
!
! portability            fortran 90
!
! algorithm              an lu-decomposition is obtained using the
!                        algorithm described in the reference below.
!
!                        to avoid unnecessary divisions, the alpha
!                        values used in the routine are the
!                        reciprocals of the alpha values described
!                        in the reference below.
!
! accuracy               every component of the residual of the linear
!                        system (i.e. the difference between  y  and
!                        the matrix applied to x) should be less in
!                        magnitude than ten times the machine precision
!                        times the matrix order times the maximum
!                        absolute component of the solution vector
!                        times the largest absolute row sum of the
!                        input matrix.
!
! timing                 the timing is roughly proportional to the
!                        order n of the linear system.
!
! references             analysis of numerical methods by
!                        e. isaacson and h. keller
!                        (john wiley and sons, 1966) pp. 55-58.
!-------------------------------------------------------------------

      implicit none

!-------------------------------------------------------------------
!	... Dummy args
!-------------------------------------------------------------------
      integer, intent(in) ::  n
      real, intent(in)    ::  sub(n)
      real, dimension(n), intent(inout) ::  diag, sup, x
      
!-------------------------------------------------------------------
!	... Local variables
!-------------------------------------------------------------------
      integer :: i, nm1
      
!----------------------------------------------------------------------
!     	... Check if n and ks are within their specified ranges
!----------------------------------------------------------------------
      if( n <= 2 ) then
         write(6,*) 'TRIDLA error: order n= ',n,' must be > 2'
         stop 'TRIDLA in numerical.mod.f: fatal error, n <= 2'
      end if

      nm1 = n - 1
!----------------------------------------------------------------------
!     	... Perform the lu-decomposition
!----------------------------------------------------------------------
      diag(1) = 1. / diag(1)
      sup(1) = sup(1)*diag(1)
      do i = 2,nm1
         diag(i) = 1. / (diag(i) - sub(i)*sup(i-1))
         sup(i) = sup(i)*diag(i)
      end do
!----------------------------------------------------------------------
!     	... Solve the system
!----------------------------------------------------------------------
      x(1) = x(1)*diag(1)
      do i = 2,nm1
         x(i) = (x(i) - sub(i)*x(i-1))*diag(i)
      end do

      x(n) = (x(n) - sub(n)*x(nm1)) / (diag(n) - sub(n)*sup(nm1))
      do i = nm1,1,-1
         x(i) = x(i) - sup(i)*x(i+1)
      end do

      end subroutine TRIDLA
      
!=======================================================================

      subroutine TRIDEC( syscnt, order, lower, main, upper )
!--------------------------------------------------------------------
!
! dimension of           lower(order,syscnt), main(order,syscnt), upper(order,syscnt)
! arguments
!
! latest revision        june 1995
!
! purpose                trdi computes the solution of the tridiagonal
!                        linear system,
!                            b(1)*x(1)+c(1)*x(2)               = y(1)
!                            a(i)*x(i-1)+b(i)*x(i)+c(i)*x(i+1) = y(i)
!                                i=2,3,...,n-1
!                            a(n)*x(n-1)+b(n)*x(n)             = y(n)
!
! usage                  call trdi (n, a, b, c, x )
!
! arguments
!
! on input               n
!                          the number of unknowns.  this subroutine
!                          requires that  n  be greater than  2.
!
!                        a
!                          the subdiagonal of the matrix is stored in
!                          locations a(2) through a(n).
!
!                        b
!                          the diagonal of the matrix is stored in
!                          locations b(1) through b(n).
!
!                        c
!                          the super-diagonal of the matrix is stored in
!                          locations c(1) through c(n-1).
!
!                        x
!                          the right-hand side of the equations is
!                          stored in y(1) through y(n).
!
! on output              x
!                          an array which contains the solution to the
!                          system of equations.
!
! special conditions     this subroutine executes satisfactorily
!                        if the input matrix is diagonally dominant
!                        and non-singular.  the diagonal elements
!                        are used to pivot, and no tests are made to
!                        determine singularity.  if a singular, or
!                        nearly singular, matrix is used as input,
!                        a divide by zero or floating point overflow
!                        may result.
!
! precision              compiler dependent
!
! language               fortran
!
! history                originally written by nancy werner at ncar
!                        in october, 1973. modified by s. walters at
!                        ncar in june 1989 to functionally replace
!                        the cal routine tridla.
!
! portability            fortran 90
!
! algorithm              an lu-decomposition is obtained using the
!                        algorithm described in the reference below.
!
!                        to avoid unnecessary divisions, the alpha
!                        values used in the routine are the
!                        reciprocals of the alpha values described
!                        in the reference below.
!
! accuracy               every component of the residual of the linear
!                        system (i.e. the difference between  y  and
!                        the matrix applied to x) should be less in
!                        magnitude than ten times the machine precision
!                        times the matrix order times the maximum
!                        absolute component of the solution vector
!                        times the largest absolute row sum of the
!                        input matrix.
!
! timing                 the timing is roughly proportional to the
!                        order n of the linear system.
!
! references             analysis of numerical methods by
!                        e. isaacson and h. keller
!                        (john wiley and sons, 1966) pp. 55-58.
!--------------------------------------------------------------------

      implicit none

!--------------------------------------------------------------------
!	... Dummy args
!--------------------------------------------------------------------
      integer, intent(in) ::  syscnt, order
      real, intent(in), dimension(order,syscnt)    ::  lower
      real, intent(inout), dimension(order,syscnt) ::  main, upper
      
!--------------------------------------------------------------------
!	... Local variables
!--------------------------------------------------------------------
      integer :: i, im1, j, nm1
      
!----------------------------------------------------------------------
!     	... Check if n and ks are within their specified ranges
!----------------------------------------------------------------------
      if( order <= 2 ) then
         write(6,*) 'TRIDEC error: order= ',order,' must be > 2'
         stop 'TRIDEC in numerical.mod.f: fatal error, order <= 2'
      end if

      nm1 = order - 1
!----------------------------------------------------------------------
!     	... lu-decomposition
!----------------------------------------------------------------------
      main(1,:syscnt) = 1. / main(1,:syscnt)
      upper(1,:syscnt) = upper(1,:syscnt)*main(1,:syscnt)
      do i = 2,nm1
         im1 = i - 1
         main(i,:syscnt) = 1. / (main(i,:syscnt)
     $                         - lower(im1,:syscnt)*upper(im1,:syscnt))
         upper(i,:syscnt) = upper(i,:syscnt)*main(i,:syscnt)
      end do

      end subroutine TRIDEC

!=======================================================================

      subroutine TRISLV( syscnt, order, lower, main, upper, x )

      implicit none

!----------------------------------------------------------------------
!     	... Dummy args
!----------------------------------------------------------------------
      integer, intent(in) ::  syscnt, order
      real, intent(in), dimension(order,syscnt)    ::  lower,
     $                                                 main,
     $                                                 upper
      real, intent(inout), dimension(order,syscnt) ::  x
      
!----------------------------------------------------------------------
!     	... Local variables
!----------------------------------------------------------------------
      integer :: i, im1, j, n, nm1

      nm1 = order - 1
      n   = order
!----------------------------------------------------------------------
!     	... Solve the system
!----------------------------------------------------------------------
      x(1,:syscnt) = x(1,:syscnt)*main(1,:syscnt)

      do i = 2,nm1
         im1 = i - 1
         x(i,:syscnt) = (x(i,:syscnt)
     $                    - lower(im1,:syscnt)*x(im1,:syscnt))
     $                   *main(i,:syscnt)
      end do

      x(n,:syscnt) = (x(n,:syscnt) - lower(nm1,:syscnt)*x(nm1,:syscnt))
     $                /(main(n,:syscnt)
     $                  - lower(nm1,:syscnt)*upper(nm1,:syscnt))

      do i = nm1,1,-1
         x(i,:syscnt) = x(i,:syscnt) - upper(i,:syscnt)*x(i+1,:syscnt)
      end do

      end subroutine TRISLV

!=======================================================================

      subroutine BAND_FAC( mat,
     $                     n,
     $                     ml,
     $                     mu,
     $                     info )
!------------------------------------------------------------
!     	... Factors a real band matrix by elimination
!           sgbfa is usually called by sgbco, but it can be called
!           directly with a saving in time if  rcond  is not needed.
!------------------------------------------------------------

!------------------------------------------------------------
!     on entry
!
!        mat     real(lda, n)
!                contains the matrix in band storage.  the columns
!                of the matrix are stored in the columns of  mat  and
!                the diagonals of the matrix are stored in rows
!                ml+1 through 2*ml+mu+1 of  mat .
!                see the comments below for details.
!
!        n       integer
!                the order of the original matrix.
!
!        ml      integer
!                number of diagonals below the main diagonal.
!                0 .le. ml .lt. n .
!
!        mu      integer
!                number of diagonals above the main diagonal.
!                0 .le. mu .lt. n .
!                more efficient if  ml .le. mu .
!     on return
!
!        mat     an upper triangular matrix in band storage and
!                the multipliers which were used to obtain it.
!                the factorization can be written  a = l*u  where
!                l  is a product of permutation and unit lower
!                triangular matrices and  u  is upper triangular.
!
!        info    integer
!                = 0  normal value.
!                = k  if  u(k,k) .eq. 0.0 .  this is not an error
!                     condition for this subroutine, but it does
!                     indicate that sgbsl will divide by zero if
!                     called.  use  rcond  in sgbco for a reliable
!                     indication of singularity.
!
!     band storage
!
!           if  a  is a band matrix, the following program segment
!           will set up the input.
!
!                   ml = (band width below the diagonal)
!                   mu = (band width above the diagonal)
!                   m = ml + mu + 1
!                   do 20 j = 1, n
!                      i1 = max0(1, j-mu)
!                      i2 = min0(n, j+ml)
!                      do 10 i = i1, i2
!                         k = i - j + m
!                         mat(k,j) = a(i,j)
!                10    continue
!                20 continue
!
!           this uses rows  ml+1  through  2*ml+mu+1  of  mat .
!           in addition, the first  ml  rows in  mat  are used for
!           elements generated during the triangularization.
!           the total number of rows needed in  mat  is  2*ml+mu+1 .
!           the  ml+mu by ml+mu  upper left triangle and the
!           ml by ml  lower right triangle are not referenced.
!
!     linpack. this version dated 08/14/78 .
!     cleve moler, university of new mexico, argonne national lab.
!
!     subroutines and functions
!
!     blas saxpy,sscal,isamax
!------------------------------------------------------------

      implicit none
      
!------------------------------------------------------------
!	... Dummy args
!------------------------------------------------------------
      integer, intent(in)  ::  n, ml, mu
      integer, intent(out) ::  info
      real, intent(inout)  ::  mat(:,:)

!------------------------------------------------------------
!     	... Local variables
!------------------------------------------------------------
      integer :: i, i0, j, ju, jz, j0, j1, k, kp1, l,
     $           lm, m, mm
      integer :: loc(1)
      real ::  t

!------------------------------------------------------------
!     	... Check dimesionality
!------------------------------------------------------------
      m = ml + mu + 1
      if( SIZE( mat, dim = 1 ) < m .or.
     $    SIZE( mat, dim = 2 ) < n  ) then
	 info = -1
	 return
      end if

      info = 0
      m = mu + 1
!------------------------------------------------------------
!     	... Gaussian elimination with no pivoting
!------------------------------------------------------------
      if( n < 2 ) then
         if( mat(m,n) == 0. ) then
	    info = n
         end if
         return
      end if
      ju = 0
      do k = 1,n-1
         kp1 = k + 1
         if( mat(m,k) /= 0. ) then
	    lm = MIN( ml,n-k )
!------------------------------------------------------------
!   	... Compute multipliers
!------------------------------------------------------------
            t = -1. / mat(m,k)
	    mat(m+1:m+lm,k) = t * mat(m+1:m+lm,k)
!------------------------------------------------------------
!     	... Row elimination
!------------------------------------------------------------
            ju = MIN( mu+k,n )
            mm = m
            if( ju >= kp1 ) then
               do j = kp1,ju
                  mm = mm - 1
                  t  = mat(mm,j)
		  mat(mm+1:mm+lm,j) = mat(mm+1:mm+lm,j) 
     $                                + t * mat(m+1:m+lm,k)
               end do
            end if
         else
!------------------------------------------------------------
!  	... Zero pivot
!------------------------------------------------------------
            info = k
	    exit
         end if
      end do

      if( info == 0 .and. mat(m,n) == 0. ) then
	 info = n
      end if

      end subroutine BAND_FAC

!=======================================================================

      subroutine BAND_SLV( mat,
     $                     n,
     $                     ml,
     $                     mu,
     $                     b )
!--------------------------------------------------------------
!     	... Solves the real band system
!           a * x = b  or  trans(a) * x = b
!           using the factors computed by sgbco or sgbfa.
!--------------------------------------------------------------
!--------------------------------------------------------------
!     on entry
!
!        mat     real(lda, n)
!                the output from sgbco or sgbfa.
!
!        n       integer
!                the order of the original matrix.
!
!        ml      integer
!                number of diagonals below the main diagonal.
!
!        mu      integer
!                number of diagonals above the main diagonal.
!
!        b       real(n)
!                the right hand side vector.
!
!     on return
!
!        b       the solution vector  x .
!
!     error condition
!
!        a division by zero will occur if the input factor contains a
!        zero on the diagonal.  technically this indicates singularity
!        but it is often caused by improper arguments or improper
!        setting of lda .  it will not occur if the subroutines are
!        called correctly and if sgbco has set rcond .gt. 0.0
!        or sgbfa has set info .eq. 0 .
!
!     to compute  inverse(a) * c  where  c  is a matrix
!     with  p  columns
!           call sgbco(mat,lda,n,ml,mu,ipvt,rcond,z)
!           if (rcond is too small) go to ...
!           do 10 j = 1, p
!              call sgbsl(mat,lda,n,ml,mu,ipvt,c(1,j),0)
!        10 continue
!
!     linpack. this version dated 08/14/78 .
!     cleve moler, university of new mexico, argonne national lab.
!
!--------------------------------------------------------------

      implicit none

!--------------------------------------------------------------
!	... Dummy args
!--------------------------------------------------------------
      integer, intent(in) ::  n, ml, mu
      real, intent(in)    ::  mat(:,:)
      real, intent(inout) ::  b(:)

!--------------------------------------------------------------
!     	... Local variables
!--------------------------------------------------------------
      real     ::  t
      integer  ::  k, la, lb, lm, m

!--------------------------------------------------------------
!     	... Check dimensionality
!--------------------------------------------------------------
      m = ml + mu + 1
      if( SIZE( mat, dim = 1 ) < m .or.
     $    SIZE( mat, dim = 2 ) < n .or.
     $    SIZE( b ) < n ) then
	 return
      end if
!--------------------------------------------------------------
!   	... First solve l*y = b
!--------------------------------------------------------------
      m = mu + 1
      if( ml /= 0 .and. n > 1 ) then
         do k = 1,n-1
            lm = MIN( ml,n-k )
            t  = b(k)
	    b(k+1:k+lm) = b(k+1:k+lm) + t * mat(m+1:m+lm,k)
         end do
      end if
!--------------------------------------------------------------
!  	... Now solve  u*x = y
!--------------------------------------------------------------
      do k = n,1,-1
         b(k) = b(k) / mat(m,k)
         lm   = MIN( k,m ) - 1
         la   = m - lm
         lb   = k - lm
         t    = -b(k)
	 b(lb:lb+lm-1) = b(lb:lb+lm-1) + t * mat(la:la+lm-1,k)
      end do

      end subroutine BAND_SLV
 
!=========================================================================== 
 
      subroutine SPLINE( x, y, n, yp1, ypn, y2, ier )
!---------------------------------------------------------------------------
!    purpose:
!      given a set of points cooresponding to an interpolated function, and
!    its first derivative at each end of the function, returns the second
!    derivative of the interpolated function at each point
!
!    usage:
!      call spline(x,y,n,yp1,ypn,y2,ier)
!
!   arguments:
!
!  input
! parameters   type    units   dimensions               description
!   n          integer   -     none                     number of data points
!   x          real      -     n                        data points in
!   y          real      -     n                           ascending order of x
!   yp1        real      -     none                     1st derivatives at x(1)
!   ypn        real      -     none                       and x(n)
!
!  output
! parameters   type    units   dimensions               description
!   y2         real      -     n                        2nd derivatives at each point
!   ier        integer   -     none                   error code
!                                                     = 0, no error
!
!  restrictions:
!
!  subroutine and functions required:
!    none
!
!  include code files required:
!    none
!
!    nmax...maximum expected n
!--------------------------------------------------------------------

      implicit none

!--------------------------------------------------------------------
!    	... Parameters
!--------------------------------------------------------------------
      integer, parameter :: nmax = 200

!--------------------------------------------------------------------
!    	... Dummy args
!--------------------------------------------------------------------
      integer, intent(in)  :: n
      integer, intent(out) :: ier
      real, intent(in)  :: yp1, ypn
      real, intent(in)  :: x(n), y(n)
      real, intent(out) :: y2(n)

!--------------------------------------------------------------------
!    	... Local variables
!--------------------------------------------------------------------
      integer :: i, k
      real :: sig, p, qn, un
      real :: u(nmax)

      ier = 0
      if( n > nmax ) ier = 1
      do i = 2, n
         if( x(i) <= x(i-1) ) ier = 2
      end do
      if( ier /= 0 ) return

!--------------------------------------------------------------------
!    	... Lower boundary condition set to be natural or...
!--------------------------------------------------------------------
      if( yp1 > .99e30 ) then
         y2(1) = 0.
         u(1) = 0.
      else
!--------------------------------------------------------------------
!    	... Have a specified 1st derivative
!--------------------------------------------------------------------
        y2(1) = -.5
        u(1) = (3./(x(2) - x(1)))
     $         *((y(2) - y(1))/(x(2) - x(1)) - yp1)
      end if

!--------------------------------------------------------------------
!    	... decomposition loop of tridiagonal algorithm
!           (y2 and u are used as temp. storage)
!--------------------------------------------------------------------
      do i = 2,n-1
        sig = (x(i) - x(i-1)) / (x(i+1) - x(i-1))
        p = sig * y2(i-1) + 2.
        y2(i) = (sig - 1.) / p
        u(i) = (6.*((y(i+1) - y(i))
     $         / (x(i+1) - x(i)) - (y(i) - y(i-1))
     $         / (x(i) - x(i-1)))
     $         / (x(i+1) - x(i-1)) - sig*u(i-1)) / p
      end do

!--------------------------------------------------------------------
!    	... Set upper boundary condition
!--------------------------------------------------------------------
      if( ypn > .99e30 ) then
         qn = 0.
         un = 0.
      else
!--------------------------------------------------------------------
!    	... Have a specified first derivative
!--------------------------------------------------------------------
        qn = .5
        un = (3./(x(n) - x(n-1)))
     $       *(ypn - (y(n) - y(n-1)) / (x(n) - x(n-1)))
      end if
      y2(n) = (un - qn*u(n-1)) / (qn*y2(n-1) + 1.)

!--------------------------------------------------------------------
!    	... Backsubstitution loop of tridiagonal algorithm
!--------------------------------------------------------------------
      do k = n-1,1,-1
        y2(k) = y2(k)*y2(k+1) + u(k)
      end do

      end subroutine SPLINE
 
!=========================================================================== 
 
      subroutine SPLINT( xa, ya, y2a, n, x, y, ier )
!---------------------------------------------------------------------------
!    purpose:
!      preform a cubic-spline interpolation
!
!    usage:
!      call splint(xa,ya,y2a,n,x,y,ier)
!
!   arguments:
!
!  input
! parameters   type    units   dimensions               description
!   n          integer   -     none                     number of points
!   x          real      -     none                     point to interpolate
!   xa         real      -     n                        data points
!   ya         real      -     n
!   y2a        real      -     n                        array output from spline
!
!  output
! parameters   type    units   dimensions               description
!   y          real      -     none                     interpolation
!   ier        integer   -     none                     error code
!                                                       = 0, no error
!                                                       = 1, bad xa input
!
!
!  subroutine and functions required:
!    none
!
!  include code files required:
!    none
!---------------------------------------------------------------------------

      implicit none

!--------------------------------------------------------------------
!    	... Dummy args
!--------------------------------------------------------------------
      integer, intent(in)  :: n
      integer, intent(out) :: ier
      real, intent(in)  :: x
      real, intent(in)  :: xa(n), ya(n), y2a(n)
      real, intent(out) :: y

!--------------------------------------------------------------------
!    	... Local variables
!--------------------------------------------------------------------
      integer :: k, klo, khi
      real :: a, b, h

      ier = 0

!--------------------------------------------------------------------
!    	... find place by bisection (good random calls of points)
!--------------------------------------------------------------------
      klo = 1
      khi = n
      do
         if( (khi - klo) > 1 ) then
            k = (khi + klo) / 2
            if( xa(k) > x )then
               khi = k
            else
               klo = k
            end if
         else
	    exit
	 end if
      end do

!--------------------------------------------------------------------
!    	... klo and khi bracket the point
!--------------------------------------------------------------------
      h = xa(khi) - xa(klo)
!--------------------------------------------------------------------
!    	... Bad xa
!--------------------------------------------------------------------
      if( h == 0. ) then
         ier = 1
      else
!--------------------------------------------------------------------
!    	... Cubic spline polynomial
!--------------------------------------------------------------------
         a = (xa(khi) - x) / h
         b = (x - xa(klo)) / h
!--------------------------------------------------------------------
!    	... and the value is
!--------------------------------------------------------------------
         y = a*ya(klo) + b*ya(khi)
     $     + ((a**3 - a)*y2a(klo) + (b**3 - b)*y2a(khi))*(h**2)/6.
      end if

      end subroutine SPLINT

!=========================================================================== 

      subroutine INTERP( n_out, x_out, y_out, n, x_in, y_in, 
     $                   spline, on_log, ext_vals, ok )
!-----------------------------------------------------------------------
!  General interpolation routine. 
!  BEWARE! This code is not optimized and should NOT be used inside 
!          big/nested loops
!                               simonc@oma.be,   March 2001
!
!  x_in  is an array of input x values (e.g., altitudes)
!  y_in  is an array of input y values (e.g., temperatures or number densities)
!       corresponding to the values of x
!  n  is the number of input (x_in,y_in) data points
!  x_out is the target array of x values 
!  y_out is the output array of corresponding y values 
!  n_out is the dimension of x_out and y_out
!
!  Optional arguments:
!      spline : to choose the interpolation method
!               Set to .false. for linear interpolation (DEFAULT)
!               Set to .true. to use cubic spline interpolation (as in SPLINE 
!                   and SPLINT from Numerical Recipes, chapter 3. Zero second 
!                   derivative are assumed at the boundaries of the input grid)
!      on_log : set to .false. if you want to interpolate on the values (DEFAULT)
!               set to .true. if you want to interpolate on the logs of values
!      ext_vals : to deal with the values of the target grid which are out of 
!            the input grid. 
!            Set to 'notouch' to not touch these values
!                   'allzero' to set these output values to zero (DEFAULT)
!                   'asbndry' to set these values to the boundary input values
!                   'extrapo' to extrapolate linearly outside of the target grid.
!                         DANGEROUS! At least the extrapolated values won't
!                         change sign (floor/ceiling set to zero)
!-----------------------------------------------------------------------
      implicit none

!-----------------------------------------------------------------------
!	... Dummy args
!-----------------------------------------------------------------------
      integer, intent(in) :: n_out, n
      real, intent(in)    :: x_in(n), y_in(n), x_out(n_out)
      real, intent(inout) :: y_out(n_out)
      logical, optional, intent(in) :: spline, on_log
      character(len=7), optional, intent(in) :: ext_vals
      logical, intent(out) :: ok

!-----------------------------------------------------------------------
!  	... Local variables
!-----------------------------------------------------------------------
      logical :: log_interp, interp_spline
      integer :: i, ilo, ihi, k, klo, khi
      real :: p, qn, sig, a, b, h
      real, dimension(n)     :: x, y, y2, u
      real, dimension(n_out) :: x_out1
      character(len=7) :: ext

      ok = .false.
      log_interp = .false.
      if( PRESENT( on_log ) ) log_interp = on_log
      interp_spline = .false.
      if( PRESENT( spline ) ) interp_spline = spline
      ext = 'allzero'
      if( PRESENT( ext_vals ) ) ext = ext_vals
      if( ext/='notouch' .and. ext/='allzero' .and. ext/='asbndry' .and.
     $    ext/='extrapo' ) then
         write(*,*) 'INTERP: Error: "ext_vals" keyword not understood'
         return
      end if
!-----------------------------------------------------------------------
!  Make sure vectors conform to increasing x.
!-----------------------------------------------------------------------
      if( x_in(2) < x_in(1) ) then
         x(n:1:-1) = x_in(1:n)
         y(n:1:-1) = y_in(1:n)
       else
         x = x_in
         y = y_in
      end if
      x_out1 = x_out
      if( n_out > 1 ) then
         if( x_out(2) < x_out(1) ) x_out1(n_out:1:-1) = x_out(1:n_out)
      end if

!-----------------------------------------------------------------------
!  Take log of values if necessary
!-----------------------------------------------------------------------
      if( log_interp ) then
         if ( ANY( y(1:n) <= 0. ) ) then
            write(*,*) ' INTERP: Error: some input data is neg/null,'
            write(*,*) '   but log interpolation was specified.'
            return
         end if
         y(1:n) = ALOG( y(1:n) )
      end if
     
!-----------------------------------------------------------------------
!  test if data is sorted
!-----------------------------------------------------------------------
      if( ANY( x(2:n) < x(1:n-1) ) ) then
         write(*,*)' INTERP: Error: input grid not sorted'
         return
      end if
      if( ANY( x_out1(2:n_out) <= x_out1(1:n_out-1) ) ) then
         write(*,*)' INTERP: Error: target grid not sorted'
         return
      end if
      if( (x_out1(1) >= x(n)) .or. (x_out1(n_out) <= x(1)) ) then
         write(*,*)' INTERP: Error: target grid outside of input grid'
         return
      end if

      if( interp_spline ) then
!-----------------------------------------------------------------------
!  Calculate y2, the 2d derivative of interpolating function at input x
!-----------------------------------------------------------------------
         y2(1) = 0.
         y2(n) = 0.
         u(1) = 0.
         do i= 2, n-1
            sig = ( x(i) - x(i-1) ) / ( x(i+1) - x(i-1) )
            p = sig * y2(i-1) + 2.
            y2(i) = ( sig - 1. ) / p
            u(i) = ( 6. * ( (y(i+1)-y(i)) / (x(i+1)-x(i)) 
     $                    - (y(i)-y(i-1)) / (x(i)-x(i-1)) )
     $                  / (x(i+1)-x(i-1)) - sig * u(i-1) )
     $           / p
         end do
         do i = n-1, 1, -1
           y2(i) = y2(i)*y2(i+1) + u(i)
         end do
      end if
      
!-----------------------------------------------------------------------
!  Find interval (ilo,ihi) where the target grid is inside the input grid
!-----------------------------------------------------------------------
      do ilo = 1, n_out
         if( x_out1(ilo) >= x(1) ) exit
      end do
      do ihi = n_out, 1, -1
         if( x_out1(ihi) < x(n) ) exit
      end do
      
!-----------------------------------------------------------------------
!  Interpolate linearily or on cubic spline between ilo and ihi
!-----------------------------------------------------------------------
      klo = 1
      khi = 2
      do i = ilo, ihi
         if( (x(klo) > x_out1(i)) .or. (x(khi) <= x_out1(i)) ) then
            do k = klo, n-1
               if( (x(k) <= x_out1(i)) .and. (x(k+1) > x_out1(i)) )
     $            exit
            end do
            klo = k
            khi = k + 1
         end if
         h = x(khi) - x(klo)
         a = ( x(khi) - x_out1(i) ) / h
         b = ( x_out1(i) - x(klo) ) / h
         y_out(i) = a*y(klo) + b*y(khi)
         if( interp_spline ) y_out(i) = y_out(i)
     $            + ( (a*a*a-a)*y2(klo) + (b*b*b-b)*y2(khi) )*h*h / 6.
      end do
      
!-----------------------------------------------------------------------
!  if ext == 'extrapo', extrapolate linearly below and/or above x(n)
!-----------------------------------------------------------------------
      if( ext == 'extrapo' .and. (ihi < n_out) ) then
         a = (y(n) - y(n-1)) / (x(n) - x(n-1))
         y_out(ihi+1:n_out) = y_out(ihi) 
     $               + a * (x_out1(ihi+1:n_out) - x_out1(ihi))
         if( .not. log_interp ) then
            if( y_out(ihi) > 0. ) then
               y_out(ihi+1:n_out) = MAX( y_out(ihi+1:n_out), 0. )
             else 
               y_out(ihi+1:n_out) = MIN( y_out(ihi+1:n_out), 0. )
            end if
         end if
      end if
      if( ext == 'extrapo' .and. (ilo > 1) ) then
         a = (y(2) - y(1)) / (x(2) - x(1))
         y_out(1:ilo-1) = y_out(ilo) 
     $               + a * (x_out1(1:ilo-1) - x_out1(ilo))
         if( .not. log_interp ) then
            if( y_out(ilo) > 0. ) then
               y_out(1:ilo-1) = MAX( y_out(1:ilo-1), 0. )
             else 
               y_out(1:ilo-1) = MIN( y_out(1:ilo-1), 0. )
            end if
         end if
      end if

!-----------------------------------------------------------------------
!  Get back to original order, make a last test
!-----------------------------------------------------------------------
      if( log_interp ) then
         if( ext == 'extrapo' ) then
            y_out(1:n_out) = EXP( y_out(1:n_out) )
          else
            y_out(ilo:ihi) = EXP( y_out(ilo:ihi) )
         end if
      end if
      if( ext == 'allzero' ) then
         if( ilo > 1 ) y_out(1:ilo-1) = 0.
         if( ihi < n_out ) y_out(ihi+1:n_out) = 0.
       else if( ext == 'asbndry' ) then
         if( ilo > 1 ) y_out(1:ilo-1) = y_out(ilo)
         if( ihi < n_out ) y_out(ihi+1:n_out) = y_out(ihi)
      end if
      if( n_out > 1 ) then
         if( x_out(2) < x_out(1) ) y_out(1:n_out) = y_out(n_out:1:-1)
      end if
      if( ALL( y_in > 0. ) .and. ANY( y_out(ilo:ihi) < 0. ) ) then
         write(*,*)' INTERP: Error: all input > 0., some output < 0.'
         return
      end if
      
      ok = .true.
      
      end subroutine INTERP


!=========================================================================== 

      subroutine HPSORT( n, ra )
!-----------------------------------------------------------------------
!  Sorts an array ra(1:n) into ascending numerical order using the 
!  Heapsort algorithm. n is input; ra is replaced on output by its 
!  sorting rearrangement. 
!  From Numerical Recipes in Fortran, Press/et-al-1992, paragraph 8.3
!-----------------------------------------------------------------------
      implicit none

!-----------------------------------------------------------------------
!	... Dummy args
!-----------------------------------------------------------------------
      integer, intent(in) :: n
      real, intent(inout), dimension(n) :: ra

!-----------------------------------------------------------------------
!  	... Local variables
!-----------------------------------------------------------------------
      integer :: i, ir, j, l
      real :: rra
      
      if( n < 2 ) return
     
!-----------------------------------------------------------------------
!  The index l will be decremented from its initial value down to 1 
!  during the "hiring" (heap creation) phase. Once it reaches 1, 
!  the index ir will be decremented from its initial value down to 1 
!  during during the "retirement-and-promotion" (heap selection) phase
!-----------------------------------------------------------------------
      l = n/2 + 1
      ir = n
      
   10 continue
         if( l > 1 ) then         ! still in hiring phase
            l = l - 1             
            rra = ra(l)
          else                    ! In retirement-and-promotion phase
            rra = ra(ir)          ! Clear a space at end of array
            ra(ir) = ra(1)        ! Retire the top of the heap into it
            ir = ir - 1           ! Decrease the size of the corporation
            if( ir == 1 ) then    ! Done with the last promotion
               ra(1) = rra        ! The least competent worker of all!
               return
            end if
         end if
         i = l                    ! Whether in hiring or promotion phase, we here 
         j = l + l                !   set up to sift down rra to its proper level
   20    if( j <= ir ) then       ! ie "Do while j < ir: "
            if( j < ir .and. ra(j) < ra(j+1) ) j = j + 1   ! Compare to better underling
            if( rra < ra(j) ) then  ! Demote rra
               ra(i) = ra(j)
               i = j
               j = j + j
             else                 ! This is rra's level. Set j to terminate sift-down
               j = ir + 1
            end if
         goto 20
         end if
         ra(i) = rra              ! Put rra into its slot
      goto 10
              
      end subroutine HPSORT

!=========================================================================== 

      end module NUMERICAL
