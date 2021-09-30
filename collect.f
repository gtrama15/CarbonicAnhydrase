       implicit real*8 (a-h,o-z)
       parameter(n=893*893)
       dimension c1(n)
       integer a1(n),b1(n)
c
        open(15,file='t-aver-locdiff-50To500.txt')
        open(50,file='EF5000000.txt')
c
        do i=1,248232
           read(15,*)a1(i),b1(i),c1(i)
            if (c1(i).gt.5000000)then
               if(a1(i).ne.b1(i)) then
           write(50,103)a1(i),b1(i),c1(i)
           endif
           endif
        enddo
c
103    Format(i5,1x,i5,1x,f16.1)
       END

