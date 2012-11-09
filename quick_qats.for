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

C     ============================================================
C     Subroutine gn
C     ============================================================
      subroutine gn(ndata,g,d,DeltaMin,DeltaMax,q,count_points_in_g)
      implicit none
      include 'qpt_include.for'
C     ============================================================
C     1.  Define variables
C     ============================================================
C     ============================================================
C     1.1 Loop variables
C     ============================================================
      integer*4 j
C     ============================================================
C     1.2 Input variables
C     ============================================================
      integer*4 ndata
      real*8 g(0:ndata-1)
      real*8 d(0:ndata-1)
      integer*4 DeltaMin
      integer*4 DeltaMax
      integer*4 q
C     ============================================================
C     1.3 Internal variables
C     ============================================================
      integer*4 N
      real*8 BIG
      integer*4 count_points_in_g
C     ============================================================
C     2.  Initialize variables
C     ============================================================
      N=ndata
      BIG=2000d0
      do j=0,ndata-1
         g(j)=-BIG
      enddo
      count_points_in_g=0
C     ============================================================
C     3.  Calculate g
C     ============================================================
      do j=0,ndata-1
c         write(6,*) '---'
c         write(6,*) floor(1d0*j/DeltaMin)-floor(1d0*(j+q-1)/DeltaMax)
c         write(6,*) floor(1d0*(N-q-j)/DeltaMin)-
c     &        floor(1d0*(N-j-1)/DeltaMax)
c         write(6,*) '---'
         if ((floor(1d0*j/DeltaMin)-floor(1d0*(j+q-1)/DeltaMax).ge.0)
     &   .and.
     &   (floor(1d0*(N-q-j)/DeltaMin)-floor(1d0*(N-j-1)/DeltaMax).ge.0))
     &   then 
            g(j)=d(j)
            count_points_in_g=count_points_in_g+1
         endif
      enddo
C     ============================================================
C     4.  Return
C     ============================================================
      return
      end

C     ============================================================
C     subroutine index_max
C     finds the maximum value in an array, between the indices
C     index_start and index_end.  maxind is set to the offset
C     between the index of maximum and the index_start, for
C     consistency with Josh's IDL implementation of quick_qats.
C     ============================================================
      subroutine index_max(
     &     array,
     &     array_length,
     &     maxval,
     &     maxind,
     &     index_start,
     &     index_end
     &     )
      implicit none
C     ============================================================
C     1.  Declare variables
C     ============================================================
      integer*4 array_length
      real*8 array(0:array_length-1)
      real*8 maxval
      integer*4 maxind
      integer*4 index_start
      integer*4 index_end
      integer*4 i
C     ============================================================
C     2.  Initialize variables
C     ============================================================
      maxval=array(index_start)
      maxind=index_start
c      write(6,*) 'In index_max_debug',maxval,index_start,index_end
C     ============================================================
C     3.  Loop through array to find maximum value
C     ============================================================
      do i=index_start,index_end
c         write(6,*) 'In index_max_debug',i,array(i)
         if (array(i).gt.maxval) then
            maxval=array(i)
            maxind=i
         endif
      enddo
c     Offset the maxind for consistency with Josh's code
c      write(6,*) 'In index_max_debug',maxind
      maxind=maxind-index_start
c      write(6,*) 'In index_max_debug',maxind
      return
      end

C     ============================================================
C     function maxgammaN
C     ============================================================
      real*8 function maxgammaN(ndata,G,i,DeltaMin,DeltaMax,q,N,index)
cndata,g,d,DeltaMin,DeltaMax,q,count_points_in_g)
      implicit none
      include 'qpt_include.for'
      integer*4 ndata,i,DeltaMin,DeltaMax,q,N
      integer*8 index
      real*8 G(0:ndata-1)
      integer*4 a
      integer*4 ap
      integer*4 b
      integer*4 bm
      integer*4 lb
      real*8 mm
C     ============================================================
C     1.  Initialize variables
C     ============================================================
      a = 0
      ap = DeltaMax-q
      b = N-q
      bm = N-DeltaMax
C     ============================================================
C     2.  Check through the various cases
C     ============================================================
      if (i-DeltaMax.gt.a) then
         lb=i-DeltaMax
      else
         lb=a
      endif
      if (ap.ge.a+DeltaMin-1) then
         if ((i.ge.a).and.(i.le.a+DeltaMin-1)) then
            index = a-1
            maxgammaN = 0
            return
         endif
         if ((i.ge.a+DeltaMin).and.(i.le.ap)) then
c            mm = max([0,G(lb:(i-DeltaMin))],A)
            call index_max(G,ndata,mm,A,lb,i-DeltaMin)
            if (mm.lt.0) then
               mm = 0
               A = 0
            else
               A = A+1
            endif
            if (A.eq.0) then
               index = -1
            else
               index = lb+A-1
            endif
            maxgammaN = mm
            return
         endif
         if ((i.ge.ap+1).and.(i.le.b)) then
c            mm = max(G(lb:(i-DeltaMin)),A)
            call index_max(G,ndata,mm,A,lb,i-DeltaMin)
            index = A+lb
            maxgammaN = mm
            return
         endif
      else     
         if ((i.ge.a).and.(i.le.ap)) then
            index = a-1
            maxgammaN = 0
            return
         endif
         if ((i.ge.ap+1).and.(i.le.a+DeltaMin-1)) then
            index = -2
            maxgammaN = 0
            return
         endif
         if ((i.ge.a+DeltaMin).and.(i.le.b)) then
c           mm = max(G(lb:(i-DeltaMin)),A)
            call index_max(G,ndata,mm,A,lb,i-DeltaMin)
            index = A+lb
            maxgammaN = mm
            return
         endif
      endif
C     ============================================================
C     3.  Return
C     ============================================================
      return
      end


C     ============================================================
C     function maxgammaN_debug
C     ============================================================
      real*8 function maxgammaN_debug(
     &     ndata,G,i,DeltaMin,DeltaMax,q,N,index)
cndata,g,d,DeltaMin,DeltaMax,q,count_points_in_g)
      implicit none
      include 'qpt_include.for'
      integer*4 ndata,i,DeltaMin,DeltaMax,q,N
      integer*8 index
      real*8 G(0:ndata-1)
      integer*4 a
      integer*4 ap
      integer*4 b
      integer*4 bm
      integer*4 lb
      real*8 mm
C     ============================================================
C     1.  Initialize variables
C     ============================================================
      a = 0
      ap = DeltaMax-q
      b = N-q
      bm = N-DeltaMax
C     ============================================================
C     2.  Check through the various cases
C     ============================================================
c      write(6,*) 'In maxgammaN_debug 1',index
      if (i-DeltaMax.gt.a) then
         lb=i-DeltaMax
      else
         lb=a
      endif
c      write(6,*) 'In maxgammaN_debug 2',index,lb,ap,a+DeltaMin-1,a
      if (ap.ge.a+DeltaMin-1) then
c         write(6,*) 'In maxgammaN_debug 3',index,lb,ap,a+DeltaMin-1,a
         if ((i.ge.a).and.(i.le.a+DeltaMin-1)) then
            index = a-1
            maxgammaN_debug = 0
c            write(6,*) 'In maxgammaN_debug 4',index,
c     &           lb,ap,a+DeltaMin-1,maxgammaN_debug
            return
         endif
         if ((i.ge.a+DeltaMin).and.(i.le.ap)) then
c            mm = max([0,G(lb:(i-DeltaMin))],A)
            call index_max(G,ndata,mm,A,lb,i-DeltaMin)
            if (mm.lt.0) then
               mm = 0
               A = 0
            else
               A = A+1
            endif
            if (A.eq.0) then
               index = -1
            else
               index = lb+A-1
            endif
            maxgammaN_debug = mm
c            write(6,*) 'In maxgammaN_debug 5',index,
c     &           lb,ap,a+DeltaMin-1,maxgammaN_debug
            return
         endif
c         write(6,*) 'In maxgammaN_debug 6',i,ap+1,i,b,a
         if ((i.ge.ap+1).and.(i.le.b)) then
c            mm = max(G(lb:(i-DeltaMin)),A)
c            write(6,*) 'In maxgammaN_debug 7',a,lb,i-DeltaMin
c            call index_max(G,ndata,mm,A,lb,i-DeltaMin)
            call index_max(G,ndata,mm,A,lb,i-DeltaMin)
c            write(6,*) 'In maxgammaN_debug 8',index,
c     &           lb,ap,a,DeltaMin,a+DeltaMin-1,mm
            index = A+lb
            maxgammaN_debug = mm
c            write(6,*) 'In maxgammaN_debug 9',index,
c     &           lb,ap,a+DeltaMin-1,maxgammaN_debug
            return
         endif
      else     
         if ((i.ge.a).and.(i.le.ap)) then
            index = a-1
            maxgammaN_debug = 0
            return
         endif
         if ((i.ge.ap+1).and.(i.le.a+DeltaMin-1)) then
            index = -2
            maxgammaN_debug = 0
            return
         endif
         if ((i.ge.a+DeltaMin).and.(i.le.b)) then
c           mm = max(G(lb:(i-DeltaMin)),A)
            call index_max(G,ndata,mm,A,lb,i-DeltaMin)
            index = A+lb
            maxgammaN_debug = mm
            return
         endif
      endif
C     ============================================================
C     3.  Return
C     ============================================================
      return
      end

      subroutine quick_qats (data,ndata,DeltaMin,DeltaMax,q,nhat,smax)
      implicit none
      include 'qpt_include.for'
      integer*4 ndata,DeltaMin,DeltaMax,q,nhat(0:ntmax-1)
      integer*4 i,j,imax,mmax, mmin,flag, mbest,fast
      integer*4 nhatMM(0:npmax-1,0:ntmax-1)
      real*8 smaxMM(0:ntmax-1),mean,smax,sum,fmax
      real*8 data(0:ndata-1),dc(0:ndata-q-1)
      real*8 gI(0:ndata-1)
      integer*4 N
      real*8 BIG
      integer*4 a
      integer*4 ap
      integer*4 b
      integer*4 bm
      real*8 G(0:ndata-1)
      real*8 maxgammaN
      integer*8 index
      real*8 Gmax
      integer*4 mu
      integer*4 mus(0:ndata-1)
      real*8 mm
      integer*4 M
      real*8 maxgammaN_debug

      call gn (ndata,gI,data,DeltaMin,DeltaMax,q,N)
      BIG=2000d0
      a=0
      ap=DeltaMax-q
      b=N-q
      bm=N-DeltaMax

      G=gI
      do i=0,N-1
         if (ap.ge.a+DeltaMin-1) then 
            G(i)=G(i)+maxgammaN(ndata,G,i,DeltaMin,DeltaMax,q,N,index)
         endif
         if (ap.lt.a+DeltaMin-1) then
            if (((i.ge.a).and.(i.le.ap)).or.
     &           ((i.ge.(a+DeltaMin)).and.(i.le.b))) then 
               G(i)=G(i)+
     &              maxgammaN(ndata,G,i,DeltaMin,DeltaMax,q,N,index)
            endif
            if ((i.ge.ap+1).and.(i.le.a+DeltaMin-1)) then 
               G(i) = -BIG
            endif
         endif
c         if (i.eq.801) write(6,*) BIG
c         if (i.eq.802) write(6,*) G(i)
      enddo

c      Gmax = max(G(bm:b),mu)
      call index_max(G,ndata,Gmax,mu,bm,b)

      mu = mu + bm

      mus(0) = mu
      i = 0
      do while(mu.ne.a-1)
         i = i+1
         index = mu     
c         if (i.eq.25) then
            mm = maxgammaN(ndata,G,mu,DeltaMin,DeltaMax,q,N,index)
            write(6,*) i,mm,index,a-1
c         else
c            mm = maxgammaN(ndata,G,mu,DeltaMin,DeltaMax,q,N,index)
c         endif
         mu = index
         mus(i) = mu
      enddo

      M = i-1

c      write(6,*) DeltaMin,N

c        smaxMM(MM-mmin)=fmax(MM,ndata,tmin,tmax,q,dc,nhat,fast)
c$$$c Uses the Kel'Manov & Jeon algorithm for detection of
c$$$c quasi-periodic transits
c$$$c Compute the mean:
c$$$C      sum = 0d0
c$$$C      do  i=0,ndata-1
c$$$C        sum= sum + data(i)
c$$$C      enddo
c$$$C      write(6,*) sum
c$$$C      sum= sum/dble(ndata)
c$$$C Subtract the data from the mean (this turns transit dips
c$$$C into bumps):
c$$$C      do  i=0,ndata-1
c$$$C        data(i) = sum-data(i)
c$$$C      enddo
c$$$C      write(6,*) sum
c$$$C Now, convolve the data with the transit width, q.
c$$$C If flag mod 2 = 1, then convolve; otherwise assume pre-convolved:
c$$$      if(mod(flag,2).eq.1) then 
c$$$C        write(6,*) flag
c$$$        do i=0,ndata-q-1
c$$$          dc(i)=0d0
c$$$          do j=0,q-1
c$$$            dc(i)=dc(i)+data(i+j)
c$$$C            write(6,*) i,j,dc(i),data(i+j),data(i+j+1),dc(i+1)
c$$$          enddo
c$$$        enddo
c$$$      else
c$$$C        write(6,*) flag
c$$$        do i=0,ndata-q-1
c$$$          dc(i)=data(i)
c$$$        enddo
c$$$      endif
c$$$C      write(6,*) flag
c$$$c Minimum and maximum number of transits given
c$$$c the duration of the data, q & (tmin,tmax)
c$$$      mmin=floor(dble(ndata+q-1)/dble(tmax))
c$$$      mmax=floor(dble(ndata-q)/dble(tmin))+1
c$$$C      write(6,*) mmin,mmax
c$$$c Loop over the number of transits, MM:
c$$$      smax=-1d30
c$$$      do MM=mmin,mmax
c$$$c Optimize the likelihood for a given number of transits:
c$$$C        write(6,*) ndata,MM,tmin,tmax,q,smaxMM(MM-mmin)
c$$$        fast=(flag-mod(flag,2))/2
c$$$        smaxMM(MM-mmin)=fmax(MM,ndata,tmin,tmax,q,dc,nhat,fast)
c$$$c I commented the following line out since now dc is chi-square:
c$$$c        smaxMM(MM-mmin)=smaxMM(MM-mmin)/sqrt(dble(MM*q))
c$$$C        write(6,*) ndata,MM,tmin,tmax,q,smaxMM(MM-mmin)
c$$$        if(smaxMM(MM-mmin).gt.smax) then
c$$$           smax=smaxMM(MM-mmin)
c$$$           imax=MM-mmin
c$$$           mbest=MM
c$$$        endif
c$$$        if(fast.eq.0) then  
c$$$          do i=0,MM-1  
c$$$            nhatMM(MM-mmin,i)=nhat(i)
c$$$          enddo
c$$$        endif
c$$$      enddo
c$$$      if(fast.eq.0) then
c$$$        MM=mmin+imax
c$$$        do j=0,ntmax-1 
c$$$          if(j.lt.MM) then 
c$$$            nhat(j)=nhatMM(imax,j)
c$$$          else
c$$$            nhat(j)=0
c$$$          endif
c$$$        enddo
c$$$      endif
      return
      end
