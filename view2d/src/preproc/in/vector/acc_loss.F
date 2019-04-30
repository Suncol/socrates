! subversion Id for THIS file : $Id: acc_loss.F 8506 2016-01-28 10:09:58Z simonc $
! $URL: http://svn-ae/ae/COMMON/stand-alone/atmos_models/socrates/tags/07.18.01/src/preproc/in/vector/acc_loss.F $
!-----------------------------------------------------------------------


      subroutine ACC_LOSS( lat, delt, dens
     $,                    loss, spcno )
!----------------------------------------------------------------------
!     	... Calculates the global lifetimes of the long-lived slosscies
!	    at each diurnal timestep. Diurnal averaging of the global
!	    lifetime is done later.
!----------------------------------------------------------------------

      use ALLCO
      use DIURNAL_PARMS
      use CONC
      use SPECIES
      use CHEMLIFE

      implicit none

!------------------------------------------------------------------
!       ... Dummy args
!------------------------------------------------------------------
      integer, intent(in) :: lat, spcno
      real, intent(in)    :: delt
      real, dimension(PLEV), intent(in)  :: loss, dens

!------------------------------------------------------------------
!       ... Accumulate diurnal average of mr*hm2d, loss*mr*hm2d 
!------------------------------------------------------------------
      dens_acc(lat,spcno) = dens_acc(lat,spcno) + delt * SUM( dens(:) )
      loss_acc(lat,spcno) = loss_acc(lat,spcno) + delt * SUM( loss(:) )

      end subroutine ACC_LOSS

