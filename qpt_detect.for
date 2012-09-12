      real*8 function fmax(MM,N,tmin,tmax,q,dc,nhat,fast)
      implicit none
      include 'qpt_include.for'
      integer*4 i,imax,j,j1,j2,k,k1,k2,m,MM,N,tmin,tmax,q
      integer*4 nhat(0:ntmax-1)
      integer fast
      real*8 dc(0:N-q-1),fmnMM(0:npmax-1,0:ntmax-1),tmp,fmax0,fmnMMmax
c Computes F_max
      j1=max(0,n-mm*tmax)
      j2=min(tmax-q,n-q-(mm-1)*tmin)
      do j=j1,j2 
        fmnMM(0,j)=dc(j)
C       write(6,*) fmnMM(m-1,j)
      enddo
C      write(6,*) 'In fmax',MM,j1,j2
      do m=2,MM
c Compute omega:
        j1=max((m-1)*tmin,n-(mm-m+1)*tmax)
        j2=min(-q+m*tmax,n-q-(mm-m)*tmin)
C        write(6,*) m,j1,j2
         do j=j1,j2
c now, compute k1 & k2:
           k1=max((m-2)*tmin,j-tmax)
           k2=min((m-1)*tmax-q,j-tmin)
           fmnMMmax=-1d30
           do k=k1,k2
             if (fmnMM(m-2,k).gt.fmnMMmax) then
               fmnMMmax = fmnMM(m-2,k)
             endif
           enddo
           fmnMM(m-1,j)=dc(j)+fmnMMmax
         enddo
C           write(6,*) fmnMM(m-1,j),m,j,dc(j)
      enddo
C      write(6,*) 'In fmax',fmnMMmax
      fmax0=-1d30
      do j=j1,j2
        if(fmnMM(MM-1,j).gt.fmax0) then 
          imax=j
          fmax0=fmnMM(MM-1,j)
        endif
      enddo
c Now determine optimum values:
      if(fast.eq.0) then
        nhat(MM-1)=imax
        do m=MM-1,1,-1
          k1=max((m-1)*tmin,nhat(m)-tmax)
          k2=min(m*tmax-q,nhat(m)-tmin)
          tmp=-1d30
          do k=k1,k2
            if(fmnMM(m-1,k).gt.tmp) then
              imax=k
              tmp=fmnMM(m-1,k)
            endif
          enddo
          nhat(m-1)=imax
        enddo
      endif
      fmax=fmax0
C      write(6,*) 'In fmax',2
      return
      end

      subroutine qpt_detect (data,ndata,MM,tmin,tmax,q,nhat,smax,
     &     flag,mbest)
      implicit none
      include 'qpt_include.for'
      integer*4 ndata,mm,tmin,tmax,q,nhat(0:ntmax-1)
      integer*4 i,j,imax,mmax, mmin,flag, mbest,fast
      integer*4 nhatMM(0:npmax-1,0:ntmax-1)
      real*8 smaxMM(0:ntmax-1),mean,smax,sum,fmax
      real*8 data(0:ndata-1),dc(0:ndata-q-1)
c Uses the Kel'Manov & Jeon algorithm for detection of
c quasi-periodic transits
c Compute the mean:
C      sum = 0d0
C      do  i=0,ndata-1
C        sum= sum + data(i)
C      enddo
C      write(6,*) sum
C      sum= sum/dble(ndata)
C Subtract the data from the mean (this turns transit dips
C into bumps):
C      do  i=0,ndata-1
C        data(i) = sum-data(i)
C      enddo
C      write(6,*) sum
C Now, convolve the data with the transit width, q.
C If flag mod 2 = 1, then convolve; otherwise assume pre-convolved:
      if(mod(flag,2).eq.1) then 
C        write(6,*) flag
        do i=0,ndata-q-1
          dc(i)=0d0
          do j=0,q-1
            dc(i)=dc(i)+data(i+j)
C            write(6,*) i,j,dc(i),data(i+j),data(i+j+1),dc(i+1)
          enddo
        enddo
      else
C        write(6,*) flag
        do i=0,ndata-q-1
          dc(i)=data(i)
        enddo
      endif
C      write(6,*) flag
c Minimum and maximum number of transits given
c the duration of the data, q & (tmin,tmax)
      mmin=floor(dble(ndata+q-1)/dble(tmax))
      mmax=floor(dble(ndata-q)/dble(tmin))+1
C      write(6,*) mmin,mmax
c Loop over the number of transits, MM:
      smax=-1d30
      do MM=mmin,mmax
c Optimize the likelihood for a given number of transits:
C        write(6,*) ndata,MM,tmin,tmax,q,smaxMM(MM-mmin)
        fast=(flag-mod(flag,2))/2
        smaxMM(MM-mmin)=fmax(MM,ndata,tmin,tmax,q,dc,nhat,fast)
c I commented the following line out since now dc is chi-square:
c        smaxMM(MM-mmin)=smaxMM(MM-mmin)/sqrt(dble(MM*q))
C        write(6,*) ndata,MM,tmin,tmax,q,smaxMM(MM-mmin)
        if(smaxMM(MM-mmin).gt.smax) then
           smax=smaxMM(MM-mmin)
           imax=MM-mmin
           mbest=MM
        endif
        if(fast.eq.0) then  
          do i=0,MM-1  
            nhatMM(MM-mmin,i)=nhat(i)
          enddo
        endif
      enddo
      if(fast.eq.0) then
        MM=mmin+imax
        do j=0,ntmax-1 
          if(j.lt.MM) then 
            nhat(j)=nhatMM(imax,j)
          else
            nhat(j)=0
          endif
        enddo
      endif
      return
      end
