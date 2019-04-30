! subversion Id for THIS file : $Id: total_molec.f 8506 2016-01-28 10:09:58Z simonc $
! $URL: http://svn-ae/ae/COMMON/stand-alone/atmos_models/socrates/tags/07.18.01/src/total_molec.f $
!-----------------------------------------------------------------------
      real function TOTAL_MOLEC( vmr )
!--------------------------------------------------------------
!	... Compute and print the total number of molecules 
!           of species vid
!--------------------------------------------------------------
      use GRID_DIMS, only : lmax, niz
      use ALLCO, only : phir, dlatr, zkm
      use ZGRID, only : zgeo
      use SPECIES, only: qn2noon, qn2da
      use CONC, only : hm2d
      use PHYS_CST, only : pi, d2r, R0
      use BACKATM, only : Dmsis, tot

      implicit none

!--------------------------------------------------------------
!	... Dummy args
!--------------------------------------------------------------
      real, dimension(lmax,niz), intent(in) :: vmr
      
!--------------------------------------------------------------
!	... local variables
!--------------------------------------------------------------
      integer :: l, iz
      real :: nitot                             ! molec
      real, dimension(lmax) :: nicol            ! molec/cm2
      real, dimension(lmax,niz) :: ni           ! molec/cm3
      real, save, dimension(lmax,niz) :: ntot   ! molec/cm3
      real, save, dimension(lmax) :: cosp
      logical, save :: entered = .false.
      
      if( .not. entered ) then
         cosp(:) = COS( phir(:) )
         ntot(:,:) = Dmsis(:,:,tot)
         entered = .false.
      end if
      
      ni(:,:) = vmr(:,:)*ntot(:,:) ! *hm2d(:,:)
      
      nicol(:) = 0.
      nitot = 0.
      do l = 1, lmax
         do iz = niz, 2, -1
!            nicol(l) = nicol(l) + 5.e4 * ( zgeo(l,iz) - zgeo(l,iz-1) )
            nicol(l) = nicol(l) + 5.e4 * ( zkm(iz) - zkm(iz-1) )
     $                                 * ( ni(l,iz) + ni(l,iz-1) )
         end do
         nitot = nitot + nicol(l) * 1.e5*R0*dlatr * 2.e5*pi*R0 * cosp(l)
      end do
 
      TOTAL_MOLEC = nitot     
      
      end function TOTAL_MOLEC
      
