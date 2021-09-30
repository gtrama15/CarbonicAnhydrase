      implicit real*4(a-h,o-z)
      parameter (n=923)
c
       open(60,file='check.923',  status='unknown')
c
       c=0.0
c
        do i = 1,n
           do j= 1,n
              write(60,*)i,j,c
           enddo
        enddo
c
       end


