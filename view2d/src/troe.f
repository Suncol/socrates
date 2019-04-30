! subversion Id for THIS file : $Id: troe.f 8506 2016-01-28 10:09:58Z simonc $
! $URL: http://svn-ae/ae/COMMON/stand-alone/atmos_models/socrates/tags/07.18.01/src/troe.f $
!-----------------------------------------------------------------------

      subroutine TROE( rate
     $,                m
     $,                factor
     $,                ko
     $,                kinf )
!-----------------------------------------------------------------
!        ... Calculate JPL troe rate
!-----------------------------------------------------------------
      
      implicit none


!-----------------------------------------------------------------
!        ... Dummy args
!-----------------------------------------------------------------
      real, intent(in)  ::   factor
      real, intent(in)  ::   ko(121)
      real, intent(in)  ::   kinf(121)
      real, intent(in)  ::   m(121)
      real, intent(out) ::   rate(121)


!-----------------------------------------------------------------
!        ... Local variables
!-----------------------------------------------------------------

      real  ::     xpo(121)

      

      xpo(:)  = ko(:) * m(:) / kinf(:)
      rate(:) = ko(:) / (1. + xpo(:))
      xpo(:)  = LOG10( xpo(:) )
      xpo(:)  = 1. / (1. + xpo(:)*xpo(:))
      rate(:) = rate(:) * factor**xpo(:)

      end subroutine TROE
