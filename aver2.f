      implicit real*4(a-h,o-z)
      parameter (n=798*798)
      dimension a1(n),b1(n),c1(n)
      dimension a2(n),b2(n),c2(n)
      dimension a3(n),b3(n),c3(n)
      dimension a4(n),b4(n),c4(n)
      dimension af3(n)
      integer p1,p2,af1(n),af2(n)
c
       open(20,file='file0', status='old')
       open(30,file='file1', status='old')
       open(40,file='file2', status='old')
       open(50,file='file3', status='old')
c
       open(200,file='aver-locdiff.300.txt',status='unknown')
       open(210,file='full-locdiff.300.txt',status='unknown')
c
       do  i = 1, n
           p1=0
           p2=0
           p3=0.0
c
           read(20,*)a1(i),b1(i),c1(i)
           read(30,*)a2(i),b2(i),c2(i) 
           read(40,*)a3(i),b3(i),c3(i)  
           read(50,*)a4(i),b4(i),c4(i)  
c
          p1 = (a1(i)+a2(i)+a3(i)+a4(i))/4
          p2 = (b1(i)+b2(i)+b3(i)+b4(i))/4
          p3 = (c1(i)+c2(i)+c3(i)+c4(i))/4
c
           af1(i)=p1
           af2(i)=p2
           af3(i)=p3
       enddo
c
         do i = 1,n
               write(210,*)af1(i),af2(i),af3(i)
            if (af3(i).ne.0) then
               write(200,*)af1(i),af2(i),af3(i)
             endif
         enddo
       end
