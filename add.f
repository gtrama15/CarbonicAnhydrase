      implicit real*4(a-h,o-z)
      parameter (n=893*893)
      parameter (n1= 93525)
      parameter (n2= 93260)
      parameter (n3= 93179)
      parameter (n4= 93312)
      parameter (n5= 92695)
      parameter (n6= 93324)
      parameter (n7= 93255)
      parameter (n8= 92893)
      parameter (n9= 93337)
      parameter (n10=93408)
      integer d1(n),d2(n),d3(n),d4(n),d5(n),d6(n),d7(n),d8(n),d9(n)
      integer d10(n),d11(n)
      integer e1(n),e2(n),e3(n),e4(n),e5(n),e6(n),e7(n),e8(n),e9(n)
      integer e10(n),e11(n)
      dimension f1(n),f2(n),f3(n),f4(n),f5(n),f6(n),f7(n),f8(n),f9(n)
      dimension f10(n),f11(n)
      dimension c1(n),c2(n),c3(n),c4(n),c5(n),c6(n),c7(n),c8(n),c9(n)
      dimension c10(n),c11(n)
      integer a1(n),a2(n),a3(n),a4(n),a5(n),a6(n),a7(n),a8(n),a9(n)
      integer a10(n),a11(n)
      integer b1(n),b2(n),b3(n),b4(n),b5(n),b6(n),b7(n),b8(n),b9(n)
      integer b10(n),b11(n)
c
       open(20,file='locdiff.100.0',  status='old')
       open(30,file='locdiff.100.1',  status='old')
       open(40,file='locdiff.100.2',  status='old')
       open(50,file='locdiff.100.3',  status='old')
       open(60,file='locdiff.100.4',  status='old')
       open(70,file='locdiff.100.5',  status='old')
       open(80,file='locdiff.100.6',  status='old')
       open(90,file='locdiff.100.7',  status='old')
       open(100,file='locdiff.100.8', status='old')
       open(110,file='locdiff.100.9', status='old')
       open(120,file='check.893', status='old')
c
       open(300,file='file0',status='unknown')
       open(310,file='file1',status='unknown')
       open(320,file='file2',status='unknown')
       open(330,file='file3',status='unknown')
       open(340,file='file4',status='unknown')
       open(350,file='file5',status='unknown')
       open(360,file='file6',status='unknown')
       open(370,file='file7',status='unknown')
       open(380,file='file8',status='unknown')
       open(390,file='file9',status='unknown')

       do i = 1,n1
          read(20,*)a1(i),b1(i),c1(i)
       enddo
          rewind(20)
          close(20)
c         
       do i = 1,n2
             read(30,*)a2(i),b2(i),c2(i)
       enddo
       rewind (30)
       close  (30)
c
       do i = 1,n3
          read(40,*)a3(i),b3(i),c3(i)
       enddo
       rewind (40)
       close  (40)
c
       do i = 1,n4
          read(50,*)a4(i),b4(i),c4(i)
       enddo
       rewind (50)
       close  (50)
c
       do i = 1,n5
             read(60,*)a5(i),b5(i),c5(i)
       enddo
       rewind (60)
       close  (60)
c
       do i = 1,n6
          read(70,*)a6(i),b6(i),c6(i)
       enddo
       rewind (70)
       close  (70)
c
       do i = 1,n7
          read(80,*)a7(i),b7(i),c7(i)
       enddo
       rewind (80)
       close  (80)
c
       do i = 1,n8
          read(90,*)a8(i),b8(i),c8(i)
       enddo
       rewind (90)
       close  (90)
c
       do i = 1,n9
          read(100,*)a9(i),b9(i),c9(i)
       enddo
       rewind (100)
       close  (100)
c
       do i = 1,n10
          read(110,*)a10(i),b10(i),c10(i)
        enddo
       rewind (110)
       close  (110)
c
       do i = 1,n
          read(120,*)d1(i),e1(i),f1(i)
       enddo
       rewind (120)
       do i = 1,n
          read(120,*)d2(i),e2(i),f2(i)
       enddo
       rewind (120)
       do i = 1,n
          read(120,*)d3(i),e3(i),f3(i)
       enddo
       rewind (120)
       do i = 1,n
          read(120,*)d4(i),e4(i),f4(i)
       enddo
       rewind (120)
       do i = 1,n
          read(120,*)d5(i),e5(i),f5(i)
       enddo
       rewind (120)
       do i = 1,n
          read(120,*)d6(i),e6(i),f6(i)
       enddo
       rewind (120)
       do i = 1,n
          read(120,*)d7(i),e7(i),f7(i)
       enddo
       rewind (120)
       do i = 1,n
          read(120,*)d8(i),e8(i),f8(i)
       enddo
       rewind (120)
       do i = 1,n
          read(120,*)d9(i),e9(i),f9(i)
       enddo
       rewind (120)
       do i = 1,n
          read(120,*)d10(i),e10(i),f10(i)
       enddo
       rewind (120)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc       
       do l = 1,n
          do m = 1,n1
             if (d1(l).eq.a1(m).and.e1(l).eq.b1(m)) then
                d1(l)=a1(m)
                e1(l)=b1(m)
                f1(l)=c1(m)
             endif
          enddo
           write(300,*)d1(l),e1(l),f1(l)
       enddo
c 
       do l = 1,n
          do m = 1,n2
             if (d2(l).eq.a2(m).and.e2(l).eq.b2(m)) then
                d2(l)=a2(m)
                e2(l)=b2(m)
                f2(l)=c2(m)
             endif
          enddo
           write(310,*)d2(l),e2(l),f2(l)
       enddo
c
       do l = 1,n
          do m = 1,n3
             if (d3(l).eq.a3(m).and.e3(l).eq.b3(m)) then
                d3(l)=a3(m)
                e3(l)=b3(m)
                f3(l)=c3(m)
             endif
          enddo
           write(320,*)d3(l),e3(l),f3(l)
       enddo
c 
       do l = 1,n
          do m = 1,n4
            if (d4(l).eq.a4(m).and.e4(l).eq.b4(m)) then
                d4(l)=a4(m)
                e4(l)=b4(m)
                f4(l)=c4(m)
             endif
          enddo
           write(330,*)d4(l),e4(l),f4(l)
       enddo
c 
       do l = 1,n
          do m = 1,n5
            if (d5(l).eq.a5(m).and.e5(l).eq.b5(m)) then
                d5(l)=a5(m)
                e5(l)=b5(m)
                f5(l)=c5(m)
             endif
          enddo
           write(340,*)d5(l),e5(l),f5(l)
       enddo
c
       do l = 1,n
          do m = 1,n6
            if (d6(l).eq.a6(m).and.e6(l).eq.b6(m)) then
                d6(l)=a6(m)
                e6(l)=b6(m)
                f6(l)=c6(m)
             endif
          enddo
           write(350,*)d6(l),e6(l),f6(l)
       enddo
c 
       do l = 1,n
          do m = 1,n7
            if (d7(l).eq.a7(m).and.e7(l).eq.b7(m)) then
                d7(l)=a7(m)
                e7(l)=b7(m)
                f7(l)=c7(m)
             endif
          enddo
           write(360,*)d7(l),e7(l),f7(l)
       enddo
c 
       do l = 1,n
          do m = 1,n8
            if (d8(l).eq.a8(m).and.e8(l).eq.b8(m)) then
                d8(l)=a8(m)
                e8(l)=b8(m)
                f8(l)=c8(m)
             endif
          enddo
           write(370,*)d8(l),e8(l),f8(l)
        enddo
c
        do l = 1,n
           do m = 1,n9
             if (d9(l).eq.a9(m).and.e9(l).eq.b9(m)) then
                 d9(l)=a9(m)
                 e9(l)=b9(m)
                 f9(l)=c9(m)
              endif
           enddo
           write(380,*)d9(l),e9(l),f9(l)
        enddo
c
        do l = 1,n
           do m = 1,n10
             if (d10(l).eq.a10(m).and.e10(l).eq.b10(m)) then
                 d10(l)=a10(m)
                 e10(l)=b10(m)
                 f10(l)=c10(m)
              endif
           enddo
           write(390,*)d10(l),e10(l),f10(l)
        enddo
ccccccccccccccccccccccccccccccccccccccccccccccccccc
        end

