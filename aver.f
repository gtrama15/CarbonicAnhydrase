      implicit real*4(a-h,o-z)
      parameter (n=893*893)
      dimension a1(n),b1(n),c1(n)
      dimension a2(n),b2(n),c2(n)
      dimension a3(n),b3(n),c3(n)
      dimension a4(n),b4(n),c4(n)
      dimension a5(n),b5(n),c5(n)
      dimension a6(n),b6(n),c6(n)
      dimension a7(n),b7(n),c7(n)
      dimension a8(n),b8(n),c8(n)
      dimension a9(n),b9(n),c9(n)
      dimension a10(n),b10(n),c10(n)
      integer af1(n),af2(n)
      dimension af3(n)
      integer p1,p2
c
       open(20,file='file0',  status='old')
       open(30,file='file1',  status='old')
       open(40,file='file2',  status='old')
       open(50,file='file3',  status='old')
       open(60,file='file4',  status='old')
       open(70,file='file5',  status='old')
       open(80,file='file6',  status='old')
       open(90,file='file7',  status='old')
       open(100,file='file8', status='old')
       open(110,file='file9', status='old')
c
       open(200,file='aver-locdiff.NMA1.100.avg',status='unknown')
       open(210,file='full-locdiff.NMA1-100',status='unknown')
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
           read(60,*)a5(i),b5(i),c5(i)  
           read(70,*)a6(i),b6(i),c6(i)  
           read(80,*)a7(i),b7(i),c7(i)  
           read(90,*)a8(i),b8(i),c8(i)  
           read(100,*)a9(i),b9(i),c9(i)  
           read(110,*)a10(i),b10(i),c10(i)  
c
          p1 = (a1(i)+a2(i)+a3(i)+a4(i)+a5(i)+a6(i)+a7(i)+a8(i)
     & +a9(i)+a10(i))/10
          p2 = (b1(i)+b2(i)+b3(i)+b4(i)+b5(i)+b6(i)+b7(i)+b8(i)
     & +b9(i)+b10(i))/10
          p3 = (c1(i)+c2(i)+c3(i)+c4(i)+c5(i)+c6(i)+c7(i)+c8(i)
     &  +c9(i)+c10(i))/10
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
