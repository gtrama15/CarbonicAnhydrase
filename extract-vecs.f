C This code extracts the eigenvectors from the charmm output file 
C for n atoms and m modes
        Implicit real *8 (a-h,o-z)
c       parameter (n=6001,m=6001*3)
        parameter (n=5995,m=5995*3)
        dimension x(n*3),y(n*3),z(n*3)
        Open(1,file='vector.inp')
        Open(2,file='vector.out')
            j = 0
c
15       do 30  i = 1, n
             read(1,*)x(i),y(i),z(i)
             write(2,200)x(i)
             write(2,200)y(i)
             write(2,200)z(i)
30      continue

                j = j +1
               If(j.lt.m) goto 15
c100     format(3f13.6)
c100     format(10x,2i5,1x,a4,5x,a4,4x,3f10.5)
100     format(20x,3f8.3)
c100    format(10x,2i5,1x,a4,1x,a4,3f10.5)
200     format(f8.3)
        END

