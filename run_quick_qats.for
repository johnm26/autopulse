        program run_quick_qats
        implicit none
        include 'qpt_include.for'
        integer*4 i,j,ntime,flag,mm,tmin,tmax,q,p1,p2,mbest
        real*8 f,time(0:ntimemax-1),flux(0:ntimemax-1),
     &         sig_flux(0:ntimemax-1),smax,smax_max
        integer*4 nhat(0:ntmax-1),nhatbest(0:ntmax-1)

        character*13 inputdata

        inputdata='lightcurve.in'
	open(unit=8,file=inputdata)
        read(8,*) ntime,f,q,p1,p2,flag
        do i=0,ntime-1
          read(8,*) time(i),flux(i),sig_flux(i)
C          write(6,*) time(i),flux(i),sig_flux(i)
        enddo
        close(unit=8)
C        flag = 0
C        p1=64
CC        p1=3360
C        p2=14400
CC        q=24
C        q=8
C        f=0.0075
        tmin=p1
        tmax=ceiling(tmin*(1d0+f))
        open(unit=7,file='qats_spectrum.txt',position='append')
        smax_max=0d0
C        write(6,*) tmin,tmax,p1,p2
c        do while(tmin.le.p2) 
cDEBUGGING START
        do while(tmin.lt.ceiling(p1*(1d0+f))) 
cDEBUGGING END
C          write(6,*) tmin,tmax,mm,q,smax
          do j=0,ntmax-1
            nhat(j)=0d0
          enddo
          call quick_qats (flux,ntime,tmin,tmax,q,nhat,smax)
c          call qpt_detect (flux,ntime+q,mm,tmin,tmax,q,nhat,smax,
c     &     flag,mbest)
C          smax=smax/sqrt(dble(q*mbest))
          write(7,*) tmin,tmax,mm,q,smax,mbest
c          write(6,*) tmin,tmax,mm,q,smax
          if(smax.gt.smax_max) then
            smax_max=smax
            do j=0,ntmax-1
              if(j.lt.mbest) then 
                nhatbest(j)=nhat(j)
              else
                nhatbest(j)=0d0
              endif
            enddo
          endif
          tmin=tmax
          tmax=ceiling(tmin*(1d0+f/2d0))
        enddo
        open(unit=9,file='transit_times.txt')
        do j=0,ntmax-1
          if(nhatbest(j).ne.0) then 
            write(9,'(I6)') nhatbest(j)
          endif
        enddo
        close(unit=9)
        close(unit=7)
        stop
        end
