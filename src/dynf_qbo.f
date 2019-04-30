!tropical qbo forcing by kelvin waves and rossby gravity waves
      subroutine DYNF_QBO()
      USE, INTRINSIC :: IEEE_ARITHMETIC, ONLY: IEEE_IS_FINITE
      use TIME_CONTROLS
      use PHYS_CST, only : R0, g0                  ! effective planet radius(km)
      use GRID_DIMS, only : lmax, niz              ! dimension of horizontal and vertical
      use ALLCO, only : phi, zkm
      use VEN2, only : u                           ! zonal mean zonal wind
      use VEN5, only : f_tropic
      use TROPOPAUSE, only : izm                   ! tropopause height index
      use ZGRID, only : Hair                       ! lmax x niz matrix scale height

      implicit none
      
      ! slow or fast damping 
      character(len=4), parameter :: damping = 'slow'
      
      ! Dominant Kelvin wave(wavenumber 1,eastward propagation) and Rossby ravity wave(wavenumber 4,westward propagation),
      ! phase speed c,wavenumber k,and momentum flux at z0,A(Z0). a:Earth radius(6837km)

      ! c1, k1, A1 for Kelvin wave
      real, parameter :: c1 = 25.
      real, parameter :: k1 = 1/(R0*1000.)
      real, parameter :: A1 = 12.5e-3

      !c2, k2, A2 for Rossby gravity wave
      real, parameter :: c2 = -25.
      real, parameter :: k2 = 4/(R0*1000.)
      real, parameter :: A2 = 15.0e-3
    
      ! const scale Height or real scale Height
      character(len=5), parameter :: scale_height = 'const'

      real,parameter :: N = 2.16e-2                       ! boyancy frequecy in s-1
      real,parameter :: H = 7.0                           ! scale height for const scale height
      real,parameter :: beta = 2.28e-11                   ! unit in m-1s-1

      real :: omega1, omega2, alpha
      real :: z0, dy, y, zmax                                   ! dy = dlatr * R0 unit in km
      real :: Yl2_kelvin, YL2_Rossby

      real :: a1_damping
      real :: a2_damping

      integer :: lmid, ind, i, l

      real, dimension(niz) ::  D1=0.0, D2=0.0, P1=0.0, P2=0.0, F1=0.0
      real, dimension(niz) ::  F_kelvin=0.0, F_rossby=0.0,F2=0.0

      lmid = (lmax+1)/2
      ind = izm(lmid) ! tropopause height index 
      !z0 = zkm(ind)
      z0 = 17.
      zmax = 50.

      if (damping == 'slow') then
        ! for slow damping
        a1_damping = 7.5257e-4
        a2_damping = 0.69897
      else if (damping == 'fast') then 
        ! For fast damping
        a1_damping = 1.7474e-3
        a2_damping = 0.30103
      end if

      !-------------------------------------------------------------------------        
      ! The forcing of Kelvin wave and Rossby wave 
      ! D1 is dissipating rate for Kelvin wave, D2 for Rossby wave. For simulating QBO, we choose slow damping.

      do i = ind,zmax
        if (zkm(i)>=30.) then 
          alpha = 1/86400. * exp(-2.3*(a1_damping*(zkm(i)-50)
     &       *(zkm(i)-50)+a2_damping))
        else
          alpha = (4.0e-7)+(8.0e-7)*(zkm(i)-z0)/13
        end if
        D1(i) = (alpha*N)/(k1*(c1-u(lmid,i))*(c1-u(lmid,i)))
        D2(i) = (alpha*N)/(k2*(c2-u(lmid,i))*(c2-u(lmid,i)))
     &      *(beta/(k2*k2*(u(lmid,i)-c2))-1)
        if ((i > ind).and.(scale_height == 'const')) then 
            P1(i) = P1(i-1) + D1(i) * (zkm(i)-zkm(i-1))
     &      *1000.0
            F1(i) = A1 * exp( (zkm(i)-z0) / H)*D1(i)*exp(-P1(i))
            P2(i) = P2(i-1) + D2(i) * (zkm(i)-zkm(i-1))
     &      *1000.0
            F2(i) = A2 * exp( (zkm(i)-z0) / H)*D2(i)*exp(-P2(i))
        else if ((i>ind).and.(scale_height == 'dyn')) then
            P1(i) = P1(i-1) + D1(i) * (zkm(i)-zkm(i-1))
     &      *1000.0 
            F1(i) = A1 * exp( (zkm(i)-z0) / Hair(lmid,i))*D1(i)
     &       *exp(-1 * P1(i))
            P2(i) = P2(i-1) + D2(i) * (zkm(i)-zkm(i-1))
     &      *1000.0
            F2(i) = A2 * exp( (zkm(i)-z0) / Hair(lmid,i))*D2(i)
     &       *exp(-1 * P2(i))
        end if
      end do
      
      ! To simulate the meridional extension of QBO, a Gaussian distribution about equator with function exp(-y*y/(Yl*Yl) is
      ! applied to forcing. y is distance in the meridional direction

      ! For kelvin wave: 
      omega1 = k1*c1
      YL2_kelvin = 2*omega1*1.0e-6/(k1*beta)          !unit in km2

      ! For Rossby gravity wave
      omega2 = k2*c2
      YL2_Rossby = 2*omega2*1.0e-6/(beta*(beta/omega2 - k2))     !unit in km2

      !dy = dlatr * R0               ! latitude step, unit in km
      dy = 110.948
      do l=1, lmax
        if (abs(phi(l)) <=30.0) then
            y = dy *  abs(phi(l))
            F_kelvin(:) = F1(:) * exp((-1*y*y)/YL2_kelvin) 
            F_rossby(:) = F2(:) * exp((-1*y*y)/YL2_Rossby) 
            f_tropic(l,:) = F_kelvin(:) - F_rossby(:)

            do  i=ind,zmax
            if((.not.IEEE_IS_FINITE(f_tropic(l,i))).or.(f_tropic(l,i)
     $              >1.e-4)) then
                    f_tropic(l,i) = 0.0
                   !write(*,*) 'problem'
                end if
            end do
            !write(*,*) f_tropic(l,:)
        end if 
      end do
      
      end subroutine DYNF_QBO

