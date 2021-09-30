      implicit real*4(a-h,o-z)
      parameter (npt = 5995)
c      parameter (npt1 = 700)
      parameter (nrs = 893)
      dimension w(3*npt),ev(3*npt,3*npt)
      dimension x(npt),y(npt),z(npt), rd(npt),am(npt)
      dimension ww(3*npt),dh(3*npt,3*npt)
      dimension nr(0:npt), ndf(nrs), dhtx(nrs,nrs)
      dimension dhty(nrs,nrs), dhtz(nrs,nrs)
      dimension dhta(nrs,nrs)
      dimension xm(nrs), ym(nrs), zm(nrs), amt(nrs)
      open(1,file='vector.out', status='old')
      open(2,file='egfreq-u',   status='old')
      open(3,file='geometry.pdb',  status='old')
      open(4,file='Atom-mass',  status='old')
      open(7,file='hessian.out',status='old')
c  
      open(13,file='locdiff.500.9')
c 
      data xm/893*0./
      data ym/893*0./
      data zm/893*0./
      data amt/893*0./
      pi = acos(-1.0)  
      nrr = 60
      thrsh = 0.1
      cv = 418.**2
      anrm = 893.*892./2.
      vol = 4*pi*(20.e-11)**3/3./572.
       cnst = 0.01*(4.18e+6)/3.
          c = 3.0d-2 
          w0 = 2.*pi*c*w0n
          wwin = 10.d0
          wfo = pi/(2.*pi*c*(2.*wwin))/12.
          wfo2 = pi/12.
          nr(0) = 0
c         iwf = 4996
          iwi = 5719
          iwf = 5719
c         iwf = 46
          do 99 ll = 1, nrs
           do 99 mm = 1, nrs
             dhta(ll,mm) = 0.
             dhtx(ll,mm) = 0.
             dhty(ll,mm) = 0.
 99          dhtz(ll,mm) = 0.
              do 22 i = 1,npt
                  read(4,*)am(i)
c  
                  read(3,100)nr(i),axx,ayy,azz
c                 write(14,100) nr(i), axx, ayy, azz
                  x(i) = axx
                  y(i) = ayy
                  z(i) = azz
                  rd(i) = sqrt(x(i)*x(i)+y(i)*y(i)+z(i)*z(i))   
              xm(nr(i)) = xm(nr(i)) + am(i)*x(i)
              ym(nr(i)) = ym(nr(i)) + am(i)*y(i)
              zm(nr(i)) = zm(nr(i)) + am(i)*z(i)
              amt(nr(i)) = amt(nr(i)) + am(i)
 22           continue
       
c********************
               do jj = 1, 3*npt
                  read(2,*)wt
                  w(jj) = 2.*pi*c*wt
                  ww(jj) = wt
                  do ii = 1, 3*npt
                      iii = (ii-1)/3 + 1
                      read(1,*)evv
                      ev(ii,jj) = evv
                enddo
             enddo
c
              do 2 jj = 1, 3*npt
                  do 2 ii = 1, 3*npt
                      read(7,*)dh(jj,ii)
                      dh(jj,ii) = dh(jj,ii)/420
                      dh(ii,jj) = dh(jj,ii)
 2                continue
c
              do 10 i = iwi, iwf
              sij2 = 0.
              sij2x = 0.
              sij2y = 0.
              sij2z = 0.
              jmx = 1
              djtat = 0.
              do 11 j = iwi-nrr, iwf+nrr
              vx = 0.
              vy = 0.
              vz = 0.
              dhxmx = 0.
              dhymx = 0.
              dhzmx = 0.
              if(j.eq.i)goto 11
               wdiff = abs(ww(i) - ww(j))
               if(wdiff.gt.wwin)goto 11
                iflg = 0
                do 12 la = 1, 3*npt
                 do 13 lb = 1, 3*npt
                 m = (la-1)/3+1
                 n = (lb-1)/3+1
              vv = sqrt(am(m)*am(n))*ev(la,j)*dh(la,lb)*ev(lb,i)
                 vva = dh(la,lb)/float(3*npt)
                 vx = vx + vv*(x(m)-x(n))
                 vy = vy + vv*(y(m)-y(n))
                 vz = vz + vv*(z(m)-z(n))
                 dhxa = abs(vva*(x(m)-x(n)))
                 dhya = abs(vva*(y(m)-y(n)))
                 dhza = abs(vva*(z(m)-z(n)))
                 dhx = (vv*(x(m)-x(n)))
                 dhy = (vv*(y(m)-y(n)))
                 dhz = (vv*(z(m)-z(n)))
                 dhtx(nr(m),nr(n)) = dhtx(nr(m),nr(n)) + dhx
                 dhty(nr(m),nr(n)) = dhty(nr(m),nr(n)) + dhy 
                 dhtz(nr(m),nr(n)) = dhtz(nr(m),nr(n)) + dhz 
                 dhta(nr(m),nr(n)) = dhta(nr(m),nr(n)) 
     .                + dhxa + dhya + dhza
  13             continue
  12            continue
c
                fr = 0.5*(w(i)+w(j))/sqrt(w(i)*w(j))
                sij2 = sij2 + fr*(vx*vx + vy*vy + vz*vz)
  11           continue
c
               dw = cv*wfo*sij2/(w(i)*w(i))
               djtot = 0.
               ddjtot = 0.
               do 39 jj = 1, nrs
                do 38 ii = 1, jj
                djx = dhtx(jj,ii)
                dix = dhtx(ii,jj)
                djy = dhty(jj,ii)
                diy = dhty(ii,jj)
                djz = dhtz(jj,ii)
                diz = dhtz(ii,jj)
                if(jj.ne.ii)then
             ddjx = dhtx(jj,ii)/abs(xm(jj)/amt(jj)-xm(ii)/amt(ii))
             ddix = dhtx(ii,jj)/abs(xm(jj)/amt(jj)-xm(ii)/amt(ii))
             ddjy = dhty(jj,ii)/abs(ym(jj)/amt(jj)-ym(ii)/amt(ii))
             ddiy = dhty(ii,jj)/abs(ym(jj)/amt(jj)-ym(ii)/amt(ii))
             ddjz = dhtz(jj,ii)/abs(zm(jj)/amt(jj)-zm(ii)/amt(ii))
             ddiz = dhtz(ii,jj)/abs(zm(jj)/amt(jj)-zm(ii)/amt(ii))
c
             ddst = sqrt((xm(jj)/amt(jj)-xm(ii)/amt(ii))**2
     .                +(ym(jj)/amt(jj)-ym(ii)/amt(ii))**2
     .                +(zm(jj)/amt(jj)-zm(ii)/amt(ii))**2)
                else
                 ddst = 0.
                endif
                if(ii.ne.jj)then
                sij2x = sij2x + (djx + dix)
                sij2y = sij2y + (djy + diy)
                sij2z = sij2z + (djz + diz)
                  else
                sij2x = sij2x + (djx)
                sij2y = sij2y + (djy)
                sij2z = sij2z + (djz)
                 endif
                djtot = djtot + cnst*(djx+dix)/ww(iwi)
                if(ii.ne.jj)then
             djt = cnst*ww(iwi)*sqrt((djx + dix)**2+(djy + diy)**2
     .       +(djz + diz)**2)/(ww(iwi-nrr)-ww(iwi+nrr))/2.
c       ddjt=anrm*cv*wfo2*((djx+dix)**2+(djy + diy)**2+(djz+diz)**2)
        ddjt=anrm*cv*wfo2*((djx+dix)**2+(djy + diy)**2
     .   +(djz+diz)**2)/abs(w(iwi+nrr)-w(iwi-nrr))/(w(i)*w(i))
                   else
             djt = cnst*ww(iwi)*sqrt((djx)**2+(djy)**2+
     .                 (djz)**2)/(ww(iwi-nrr)-ww(iwi+nrr))/2.
         ddjt=anrm*cv*wfo2*((djx)**2+(djy)**2+(djz)**2)
c         ddjt=cv*wfo2*((djx)**2+(djy)**2+(djz)**2)
     .         /abs(w(iwi+nrr)-w(iwi-nrr))/(w(i)*w(i))
                endif
                if(ddjt.gt.thrsh)then
                   write(13,*)jj,ii,ddjt,ddst
                endif
                dja = dhta(jj,ii)
                dia = dhta(ii,jj)
                djta = max(dja,dia)/20.
                if(ii.ne.jj)then
                  djtat = djtat + djt/(572.*572.-572.)
                endif
                ddjtot = ddjtot + ddjt/anrm
 38             continue
 39             continue
c
                sij2n = sij2x**2 + sij2y**2 + sij2z**2
                dw = dw*wfo2/wfo/(w(iwi-nrr)-w(iwi+nrr))
  10          continue
c  100     format(a4,3x,i4,1x,a3,2x,a3,3x,i3,5x,3(1x,f7.3),2x,f4.2,2x,f4.2)
c 100       format(7x,i4,19x,3(1x,f7.3))
100     format(22x,i4,4x,3(1x,f7.3))
c 100     format(1x,i4,4x,3(1x,f7.3))
c 100     format(10x,i10,20x,3(6x,f14.10))
c 100     format(5x,i5,10x,3f10.5)
             end 
