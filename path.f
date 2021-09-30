       implicit real*8 (a-h,o-z)
       parameter(n=923*923)
       dimension c1(n)
       integer a1(n),b1(n)
c
        open(15,file='t-aver-locdiff-50To500.txt')
c
        do i=1,248232
           read(15,*)a1(i),b1(i),c1(i)
c          if(a1(i).ne.b1(i)) then
c          if(a1(i).ne.b1(i).and.a1(i).eq.214.or.b1(i).eq.214) then
            if (c1(i).gt.100000)then
           write(*,103)a1(i),b1(i),c1(i)
c          endif
c          endif
           endif
        enddo
c
103    Format(i5,1x,i5,1x,f16.1)
       END

