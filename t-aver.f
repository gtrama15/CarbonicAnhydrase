C  This program computes the relative total energy of the globules/or part of the protein
C  without the energy of the water  molecules.
       implicit real*8 (a-h,o-z)
       parameter(n=893*893)
       dimension c1(n)
       integer a1(n),b1(n)
       dimension cent(20)
c
        open(10,file='full-locdiff.50.txt')
        open(20,file='full-locdiff.100.txt')
        open(30,file='full-locdiff.150.txt')
        open(40,file='full-locdiff.200.txt')
        open(50,file='full-locdiff.300.txt')
        open(60,file='full-locdiff.400.txt')
        open(70,file='full-locdiff.500.txt')
        open(15,file='t-aver-locdiff-50To500.txt')
        open(45,file='nu.dat')
C
        T=300           ! Temperature in Kelvin
        p=1.3806503E-23 ! Boltzmann constant
        h  = 6.626E-34  ! planck's constant
        c  = 3.0E10     ! cm s-1    speed of light
        beta =  1/(p*T)
        const= -beta*h*c
c
        do i=1,7                ! for  Z
           read(45,*)cent(i)
           Z =Z + exp(const*cent(i))
           write(*,*)cent(i),const,const*cent(i)
        enddo
        rewind (45)
c
           do k = 1,n       ! for reading the energy
              Z2=0.0
              m=1
              do l = 10,70,10
                 read(l,*)a1(k),b1(k),c1(k)
                 Z2= Z2 + (c1(k)*exp(const*cent(m)))/Z
               m=m+1
              enddo           ! from different ranges 0-550
           if(Z2.gt.0.) then
          write(15,103)a1(k),b1(k),Z2
           endif
           enddo                ! for Z2
c
103    Format(i5,1x,i5,1x,f16.2)
       END

