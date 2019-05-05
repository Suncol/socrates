!tropical qbo forcing by kelvin wave and mixed rossby wave with an update method
      subroutine DYNF_QBO2(daynum)
      use TIME_CONTROLS
      use PHYS_CST, only : R0, g0, pi
      use GRID_DIMS, only : lmax, niz
      use ALLCO, only : phi, zkm
      use VEN2, only : u
      use VEN5, only : f_tropic
      use TROPOPAUSE, only : izm
      use ZGRID, only : Hair
      use BACKATM, only : ro ! mean density in chem.mod.f module BACKATM , unit in g/m-3 
      use TRANSFORM, only : HDERIV, VDERIV, SMOOTHV, SMOOTHL 

      implicit none

      real, parameter :: k_e = 0.3 ! eddy diffusion coefficient unit in m2 sec-1
      
      ! for kelvin wave
      real, parameter :: c1 = 30. ! unit in m/sec
      real :: k1  ! unit in m-1
      real, dimension(niz) :: g_0
      real ::A0

      ! for mixed rossby wave
      real, parameter :: c2 = -30. ! unit in m/sec
      real :: k2  ! unit in m-1
      real, dimension(niz) :: g_1
      real :: A1

      real, dimension(niz) :: Fmw1=0., Fmw2=0. ! average vertical flux of mean momentum 
      real  :: G ! semiannual forcing
      
      real :: alpha
      real, parameter :: beta = 2.28e-11 ! unit in m-1s-1

      real, parameter :: S = 4.75e-5 ! unit in m-1

      real :: z0, zmax
      real :: omega1, omega2
      real :: Yl2_kelvin, Yl2_Rossby
      real :: dy, y

      real :: t

      integer :: lmid, ind, i, l
      integer :: z0_index, zmax_index

      real, dimension(niz) :: F1=0.0, F2=0.0
      real, dimension(lmax,niz) :: F_bd1=0., F_d1=0.
      real, dimension(lmax,niz) :: F_bd2=0., F_d2=0.
      real, dimension(lmax,niz) :: us=0.,uzz=0.

      real, intent(in) :: daynum
      
      k1 = (2*pi)/40000000.
      k2 = (2*pi)/10000000.

      lmid = (lmax+1)/2
      ind  = izm(lmid) ! tropipause height index
      !z0 = zkm(ind)
      z0 = 17.
      zmax = 50.
      z0_index = 17
      zmax_index = 50
      ! forcing of kelvin wave and mixed rossby wave
      do i = z0_index,zmax_index
        if (zkm(i)>=30.) then
            alpha = 1/21. + (1/7.-1/21.)*((zkm(i)-17.)/13.) ! unit in day-1
            alpha = alpha * 86400. ! unit in sec-1
        else 
            alpha = 1/7. ! unit in day-1
            alpha = alpha * 86400. ! unit in sec-1
        end if
        
        ! for kelvin wave
        A0 = 4e-3 * ro(lmid,i)*1e-3 ! ro is unit in g/m3
        g_0(i)=g_0(i-1)+0.5*(sqrt(g0*S)/((c1-u(lmid,i))*(c1-u(lmid,i))))
     $                     *(alpha/k1)*(zkm(i)-zkm(i-1))
        ! for mixed rossby wave
        A1 = -4e-3*ro(lmid,i)*1e-3  ! ro is unit in g/m/3
        g_1(i)=g_1(i-1)+0.5*(sqrt(g0*S)/((c2-u(lmid,i))*(c2-u(lmid,i))))
     $      *(1-(k2*k2/beta)*(u(lmid,i)-c2))*(alpha/k2)*(beta/(k2*k2))
     $      *(zkm(i)-zkm(i-1))

        Fmw1(i) = A0*exp(-2*g_0(i))
        Fmw2(i) = A1*exp(-2*g_1(i))
      end do
      
      ! calculate F_d
      F_bd1(lmid,:) = Fmw1(:)
      F_bd2(lmid,:) = Fmw2(:)
      call SMOOTHV(F_bd1,1,121,3)
      call SMOOTHV(F_bd2,1,121,3)
      call VDERIV(F_bd1,basedz=F_d1)
      call VDERIV(F_BD2,basedz=F_d2)
      call SMOOTHV(F_d1,1,121,1)
      call SMOOTHV(F_d2,1,121,1)
        
      ! calculate uzz
      us = u
      call SMOOTHV(us,1,121,3)
      call VDERIV(u,basedzz=uzz)
      call SMOOTHV(uzz,1,121,1)
    
      do i = z0_index,zmax_index
         if (zkm(i)<=28) then
             G = 0.
         else
             t = daynum*86400.
             G = 2*(zkm(i)-28)*sin(4e-7*t)*4.e-7 ! unit problem possibility
         end if
         
         F1(i) = -1./(ro(lmid,i)*1e-3)*F_d1(lmid,i)+k_e*uzz(lmid,i)+G
         F2(i) = -1./(ro(lmid,i)*1e-3)*F_d2(lmid,i)+k_e*uzz(lmid,i)+G
         !write(*,*)'F1=',F1
      end do

    
      ! To simulate the meridional extension of QBO, a gaussian
      ! distribution about equator with function exp(-y*y/Yl*Yl) is
      ! appled to forcing.

      ! for kelvin wave 
      omega1 = k1*c1
      Yl2_kelvin = 2*omega1*1.e-6/(k1*beta)

      ! for rossby wave 
      omega2 = k2*c2
      Yl2_rossby = 2*omega2*1.e-6/(beta*(beta/omega2-k2)) 

      dy = 110.948 ! latitude step, unit in km
      
      do l=1,lmax
        if (abs(phi(l)) <= 30.0) then
            y = dy *abs(phi(l))
            f_tropic(l,:) = F1(:)*exp((-1.*y*y)/Yl2_kelvin)
     $                       + F2(:)*exp((-1.*y*y)/Yl2_rossby)
        end if
      end do
      !write(*,*) 'f_tropic=', f_tropic 
      
      end subroutine DYNF_QBO2
