! subversion Id for THIS file : $Id: lya_srb_pho.f 8506 2016-01-28 10:09:58Z simonc $
! $URL: http://svn-ae/ae/COMMON/stand-alone/atmos_models/socrates/tags/07.18.01/src/lya_srb_pho.f $
!-----------------------------------------------------------------------

      subroutine LYA_SRB_PHO( sw_lya, levlo, colo2, secchap )

      use PHO_PARMS, only : mxcly
      use PHO_VARS, only : ako, bko, rm, ro2, rmlya, ro2lya
      
      implicit none
!-----------------------------------------------------------------------          
!  	... Parameters
!   Ly-a params finally updated to PUBLISHED vals ! simonc, SOCRATES v7s17
!-----------------------------------------------------------------------          
      real, parameter :: b(3)= (/ 0.68431, 0.229841, 0.0865412 /)
      real, parameter :: c(3)= 
     $                       (/ 8.22114e-21, 1.77556e-20, 8.22112e-21 /)
      real, parameter :: d(3)= 
     $                        (/ 6.0073e-21, 4.28569e-21, 1.28059e-20 /)
      real, parameter :: e(3)= 
     $                       (/ 8.21666e-21, 1.63296e-20, 4.85121e-17 /)

!-----------------------------------------------------------------------          
!  	... Dummy args
!-----------------------------------------------------------------------          
      logical, intent(in) :: sw_lya
      integer, intent(in) :: levlo
      real, dimension(0:mxcly), intent(in) :: colo2, secchap

!-----------------------------------------------------------------------          
!  	... Local variables
!-----------------------------------------------------------------------          
      integer :: iv, ic
      real :: colo2x(0:mxcly)

!-----------------------------------------------------------------------          
!  	... Slant overhead O2 column amount
!-----------------------------------------------------------------------
      colo2x(0:levlo) = colo2(0:levlo) * secchap(0:levlo)

!-----------------------------------------------------------------------          
!  	... Calculation of reduction factors Rm and RO2 for Lyman-a
!           line (Chabrillat and Kockarts, GRL, vol24, p. 2659, 1997)
!-----------------------------------------------------------------------          
      rmlya = 0.
      ro2lya = 0.
      if( sw_lya ) then
         do ic = 1, 3
            rmlya(0:levlo) = rmlya(0:levlo) 
     $                     + b(ic) * EXP( -c(ic) * colo2x(0:levlo) )
            ro2lya(0:levlo) = ro2lya(0:levlo) 
     $                      + d(ic) * EXP( -e(ic) * colo2x(0:levlo) )
         end do
      end if
       
!-----------------------------------------------------------------------          
!  	... Calculation of reduction factors Rm and Ro2 for Schumann-Runge 
!           bands (Kockarts, Ann. Geophys., Vol.12 p.1207, 1994)
!-----------------------------------------------------------------------          
      rm = 0.
      ro2 = 0.
      do iv = 1,16
         do ic = 1,11,2
            rm(0:levlo,iv) = rm(0:levlo,iv) 
     $               + ako(ic,iv)*EXP( -ako(ic+1,iv)*colo2x(0:levlo) )
            ro2(0:levlo,iv) = ro2(0:levlo,iv)
     $                + bko(ic,iv)*EXP( -bko(ic+1,iv)*colo2x(0:levlo) )
         end do
      end do
        
      end subroutine LYA_SRB_PHO
