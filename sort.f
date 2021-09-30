      open(1,file='locdiff.100.0',status='old')
c     open(1,file='locdiff.100.0',status='old')
      open(2,file='diff.10')
      open(3,file='diff.100')
      open(4,file='diff.1000')
      do 1 i =1,93525
c     read(1,*)ia,ib,c
      read(1,*)a,b,c,d
      if(c.gt.9.)write(2,*)a,b
      if(c.gt.99.)write(3,*)a,b
      if(c.gt.999.)write(4,*)a,b
 1    continue
      end

