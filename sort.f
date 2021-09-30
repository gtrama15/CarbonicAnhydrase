      open(1,file='t-aver-locdiff-50To500.txt',status='old')
c     open(1,file='full-locdiff.HIV1-RT.100.txt',status='old')
      open(2,file='diff.10000')
      open(3,file='diff.100000')
      open(4,file='diff.1000000')
c     do 1 i =1,851929
      do 1 i =1,248232
         read(1,*)ia,ib,c
         if(c.gt.9999.)write(2,*)ia,ib
         if(c.gt.99999.)write(3,*)ia,ib
         if(c.gt.999999.)write(4,*)ia,ib
1     continue
      end

