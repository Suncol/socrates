      module FIT_UTILS
!-----------------------------------------------------------------------
!	... The utilities to find parameters for fitting functions 
!          (FIT_GET_*) and to calculate these fitting functions (FIT_FCT_*)
!-----------------------------------------------------------------------
      implicit none
      
      PRIVATE
      PUBLIC :: FIT_GET_LIN, FIT_GET_EXP, FIT_FCT_EXP, FIT_FCT_ASYMSIG

      CONTAINS  

!=======================================================================

      subroutine FIT_GET_LIN( n, x, y, a, b  )
!-----------------------------------------------------------------------
!	... Find the params a,b of linear fit (y=ax+b) to vectors x,y
!           Translated from hercules:/usr/local/rsi/idl_5.3/lib/linfit.pro
!           where more statistics (chisq, prob) are available
!-----------------------------------------------------------------------
      integer, intent(in) :: n
      real, dimension(n), intent(in) :: x, y
      real, intent(out) :: a, b
      
      real, dimension(n) :: t
      real :: ss, sx, sy, st2
      
      ss = REAL( n )
      sx = SUM( x )
      sy = SUM( y )
      t = x - sx/ss
      st2 = SUM( t*t )
      a = SUM( t*y ) / st2
      b = (sy - sx * a) / ss
      
      end subroutine FIT_GET_LIN

!=======================================================================

      subroutine FIT_GET_EXP( n, x, y, c, y0, missval )
!-----------------------------------------------------------------------
!	... Find the params c,y0 for exponential fit to vectors x,y
!  The fit fct has the form   y = y0 * ( 1. + c/100. )^x (as FIT_FCT_EXP)
!  c is the increase rate (%/year if x is in years). 
!  This is identical to finding the LINEAR fit to log(y): log(y) = ax+b    
!  -> c = 100*(exp(a)-1) and y0 = exp(b)
!-----------------------------------------------------------------------
      integer, intent(in) :: n
      real, dimension(n), intent(in) :: x, y
      real, intent(out) :: c, y0
      real, optional, intent(in) :: missval
      
      real, parameter :: missval_default = -1.e29
      real :: a, b
      real, dimension(n) :: ypos
      logical :: chgsign
      
      if( .not. ( ALL( y > 0. ) .or. ALL( y < 0. ) ) ) then
         if( PRESENT( missval ) ) then
            c = missval
            y0 = missval
           else
            write(*,*)'FIT_GET_EXP warning: y must be all-positive or '
     $        ,'all-negative. Sending missval_default= ',missval_default
            c = missval_default
            y0 = missval_default
         end if
         return
      end if
      
      if( y(1) < 0. ) then
         ypos(:) = - y(:)
         chgsign = .true.
       else
         ypos(:) = y(:)
         chgsign = .false.
      end if
      
      call FIT_GET_LIN( n, x, LOG(ypos), a, b )
      c = 100. * ( EXP(a) - 1. )
      y0 = EXP( b )
      if( chgsign ) y0 = -y0
      
      end subroutine FIT_GET_EXP

!=======================================================================

      real function FIT_FCT_EXP( x, y0, c )
!-----------------------------------------------------------------------
! Exponential increase/decrease over time at rate c (%/year if x in year)
! with y = y0 when x = 0. (i.e. if you want y=y0 when x=x0, send x-x0 i.o. x)
!-----------------------------------------------------------------------
      real, intent(in) :: x             ! free var: most often, time (in years)
     $                  , y0, c         ! parameters   
      
      FIT_FCT_EXP = y0 * ( 1. + 1.e-2*c )**x
      
      end function FIT_FCT_EXP

!=======================================================================

      real function FIT_FCT_ASYMSIG( x, a, b, c, d, e )
!-----------------------------------------------------------------------
! Asymetric sigma fitting fct (transition class) found in program 
!     TableCurve2D on pandora.oma.be
!-----------------------------------------------------------------------
      
      real, intent(in) :: x             ! free var: most often, time
     $                  , a, b, c, d, e ! parameters   
      real :: y
      
      y = a + b / ( 1.0 + EXP( -( x - d*LOG(2.**(1./e)-1.)-c) / d) )**e
      FIT_FCT_ASYMSIG = y
      
      end function FIT_FCT_ASYMSIG
       
!=======================================================================

      end module FIT_UTILS
