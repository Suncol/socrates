! subversion Id for THIS file : $Id: fit_utils.mod.f 8506 2016-01-28 10:09:58Z simonc $
! $URL: http://svn-ae/ae/COMMON/stand-alone/atmos_models/socrates/tags/07.18.01/src/fit_utils.mod.f $
!-----------------------------------------------------------------------
      module FIT_UTILS
      
      implicit none
      PRIVATE
      PUBLIC :: FIT_EXP_TREND, FIT_ASYMSIG

      CONTAINS  

!=======================================================================

      real function FIT_EXP_TREND( t, y0, yi, c, t1 )
!-----------------------------------------------------------------------
! Exponential increase/decrease over time. At minus infinity: y = y0
!   (e.g. pre-industrial) ; at year t1: y=y0+yi ; Increase of c/100 % per year
!-----------------------------------------------------------------------
      
      real, intent(in) :: t             ! free var: most often, time (in years)
     $                  , y0, yi, c, t1 ! parameters   
      real :: y
      
      y = y0 + yi * ( 1. + c )**( t - t1 )
      FIT_EXP_TREND = y
      
      end function FIT_EXP_TREND

!=======================================================================

      real function FIT_ASYMSIG( x, a, b, c, d, e )
!-----------------------------------------------------------------------
! Asymetric sigma fitting fct (transition class) found in program 
!     TableCurve2D on pandora.oma.be
!-----------------------------------------------------------------------
      
      real, intent(in) :: x             ! free var: most often, time
     $                  , a, b, c, d, e ! parameters   
      real :: y
      
      y = a + b / ( 1.0 + EXP( -( x - d*LOG(2.**(1./e)-1.)-c) / d) )**e
      FIT_ASYMSIG = y
      
      end function FIT_ASYMSIG
       
!=======================================================================

      end module FIT_UTILS
