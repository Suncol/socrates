! subversion Id for THIS file : $Id: tau1_diags.pho.f 8506 2016-01-28 10:09:58Z simonc $
! $URL: http://svn-ae/ae/COMMON/stand-alone/atmos_models/socrates/tags/07.18.01/src/tau1_diags.pho.f $
!-----------------------------------------------------------------------

      subroutine TAU1_DIAGS_PHO( colo2top, mu2, zs, dens, xsect )
!------------------------------------------------------------------
!     Calculate optical depth tau(lambda,z)
!     This is a diagnostics routine to find & write altitude where
!      tau=1 (i.e. alt where absorption is max)
!                  simonc@oma.be
!------------------------------------------------------------------
      use PHO_PARMS, only : mxcly, maxden, phtmax, mxwvn
      use PHO_VARS, only : wvn, rm, ro2, rmlya, ro2lya

      implicit none
      
!------------------------------------------------------------------
!	... Parameters
!------------------------------------------------------------------
      integer, parameter :: lya_iv = 8     ! Lyman-alpha wavelength interval
      integer, parameter :: srb_iv0 = 46   ! 1st wavelength interval of SRB
      integer, parameter :: srb_iv1 = 61   ! last wavelength interval of SRB
      integer, parameter :: jo2 = 1, jo3 = 2, jno2 = 7

!------------------------------------------------------------------
!	... Dummy args
!------------------------------------------------------------------
      real, intent(in)    ::  colo2top
      real, intent(in), dimension(0:mxcly) :: mu2, zs
      real, intent(in)    ::  xsect(phtmax,0:mxcly,mxwvn)
      real, intent(in)    ::  dens(0:mxcly,maxden)

!------------------------------------------------------------------
!	... Local variables
!------------------------------------------------------------------
      integer :: iv, ivsr, lev
      real :: alt_tau1
      real, dimension(mxcly) ::  deltaz
      real, dimension(0:mxcly) :: crso2iv, tau_iv

!------------------------------------------------------------------
!	... Note: zs is in km and deltaz in cm
!                 The 5.e4 factor is because the formulas below
!                 have an implicit .5 * deltaz
!------------------------------------------------------------------
      deltaz(:) = (zs(0:mxcly-1) - zs(1:mxcly)) * 5.e4
      
      write(*,'(2a16)') 'lambda (nm)','alt_tau1(km)'
      do iv = 1, mxwvn
         
         do lev = 0, mxcly
            crso2iv(lev) = -9.e9
            if( iv == lya_iv ) then
               if( rmlya(lev) > 1.e-60 ) then
                  crso2iv(lev) = ro2lya(lev) / rmlya(lev)
               end if
             else if( iv >= srb_iv0 .and. iv <= srb_iv1 ) then
                ivsr = iv - srb_iv0 + 1
                if( ivsr < 0 .or. ivsr > 16) stop 'TAU1: ivsr error'
                if( rm(lev,ivsr) > 1.e-60 ) then
                   crso2iv(lev) = ro2(lev,ivsr) / rm(lev,ivsr)
                end if
            end if
            if( crso2iv(lev) < 0. ) crso2iv(lev) = xsect(jo2,lev,iv)
         end do
                
         tau_iv(0) = mu2(0) * crso2iv(0) * colo2top
         
         do lev = 1, mxcly
            tau_iv(lev) = tau_iv(lev-1) + (1./mu2(lev)) * deltaz(lev) *
     $       (  crso2iv(lev-1)*dens(lev-1,3) + crso2iv(lev)*dens(lev,3)
     $        + xsect(jo3,lev-1,iv)*dens(lev-1,2) 
     $                                + xsect(jo3,lev,iv)*dens(lev,2)
     $        + xsect(jno2,lev-1,iv)*dens(lev-1,5) 
     $                                + xsect(jno2,lev,iv)*dens(lev,5) )
         end do

         alt_tau1 = -9.e9
         do lev = 1, mxcly
            if( tau_iv(lev-1) < 1. .and. tau_iv(lev) >= 1. ) then
               alt_tau1 = zs(lev-1) + 
     $            (1.-tau_iv(lev-1)) * (zs(lev)-zs(lev-1))
     $              / (tau_iv(lev)-tau_iv(lev-1))
            end if
         end do
         if( alt_tau1 < 0. .and. iv < srb_iv0 ) alt_tau1 = 120.
         if( alt_tau1 < 0. .and. iv > srb_iv1 ) alt_tau1 = 0.
         
         write(*,'(2f16.2)') wvn(iv), alt_tau1

      end do

      stop 'testing TAU1_DIAGS_PHO'
      
      end subroutine TAU1_DIAGS_PHO
