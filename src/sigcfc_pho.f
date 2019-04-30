! subversion Id for THIS file : $Id: sigcfc_pho.f 8506 2016-01-28 10:09:58Z simonc $
! $URL: http://svn-ae/ae/COMMON/stand-alone/atmos_models/socrates/tags/07.18.01/src/sigcfc_pho.f $
!-----------------------------------------------------------------------

      subroutine SIGCFC_PHO( crst, temper, crs )
!---------------------------------------------------------------------------
!   	... Calculation of sigma (CFCs) as a function of temperature
!---------------------------------------------------------------------------
      use PHO_PARMS, only : mxcly, mxwvn, phtmax
      use PHO_VARS, only : zlambd, acfc, bcfc

      implicit none
!---------------------------------------------------------------------------
!   	... Parameters
!---------------------------------------------------------------------------
      integer, parameter :: jcfc10 = 11, jch3ccl3 = 13, jch3cl = 15
     $,                     jcfc113 = 19, jhcfc22 = 20, jha1211 = 21
     $,                     jha1301 = 22, jch3br = 27, jcfc114 = 34
      integer, parameter :: cfc_index(9) =
     $                 (/ jcfc10, jch3ccl3, jch3cl, jcfc113,
     $                    jhcfc22, jha1211, jha1301, jch3br, jcfc114 /)
      integer, parameter :: lamin(9) = (/ 60,50,45,52,45,60,42,62,44 /)
      integer, parameter :: lamax(9) = (/ 79,76,67,72,61,93,88,88,68 /)

!---------------------------------------------------------------------------
!   	... Dummy args
!---------------------------------------------------------------------------
      real, intent(in)  ::  temper(0:mxcly)
      real, intent(in)  ::  crs(phtmax,mxwvn)
      real, intent(out) ::  crst(phtmax,0:mxcly,mxwvn)

!---------------------------------------------------------------------------
!   	... Local variables
!---------------------------------------------------------------------------
      integer   :: iv, icfc, iproc
      real ::       wfac1, wfac2

      do icfc = 1,9
         iproc = cfc_index(icfc)
         do iv = 1,mxwvn
            crst(iproc,:,iv) = crs(iproc,iv)
         end do
         do iv = lamin(icfc),lamax(icfc)
            wfac1 = acfc(icfc,1)
     $            + zlambd(iv)*(acfc(icfc,2)
     $            + zlambd(iv)*(acfc(icfc,3)
     $            + zlambd(iv)*(acfc(icfc,4)
     $            + zlambd(iv)*acfc(icfc,5))))
            wfac2 = bcfc(icfc,1)
     $            + zlambd(iv)*(bcfc(icfc,2)
     $            + zlambd(iv)*(bcfc(icfc,3)
     $            + zlambd(iv)*(bcfc(icfc,4)
     $            + zlambd(iv)*bcfc(icfc,5))))
            crst(iproc,:,iv)  = 10. **(wfac1 + (temper(:) - 273.)*wfac2)
         end do
      end do
      
      end subroutine SIGCFC_PHO
