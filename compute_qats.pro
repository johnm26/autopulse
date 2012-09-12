; For a given KID, finds all of the files, unzips, detrends,
; runs QATS, saves & outputs spectrum:

pro compute_qats,kid0,f,prange
q=0
; Now, find all of the KOIs for masking:
restore,'koi_data01.sav'
;KID             LONG      = Array[3559]
;KOI             FLOAT     = Array[3559]
;PERIOD          DOUBLE    = Array[3559]
;T0              DOUBLE    = Array[3559]
;TDUR            FLOAT     = Array[3559]
nkid=n_elements(kid0)
spawn,'gfortran -O3 -c qpt_detect.for -o qpt_detect.o'
spawn,'gfortran -O3 test_qpt.for qpt_detect.o -o test_qpt'

for ikid=0,nkid-1 do begin
  kids=strcompress(kid0[ikid],/remove_all)
  print,'Working on KID: ',kids
  indx=where(long(kid0[ikid]) eq long(kid))
  if(indx[0] ge 0) then begin
    nplanet=n_elements(indx)
; Identify all of the fits files associated with this KID:
;    spawn,'ls *'+kids+'*gz > fits_gz_list.txt'
;    readcol,'fits_gz_list.txt',fnamegz,format='a'
; gunzip the files:
;    for i=0,n_elements(fnamegz)-1 do spawn,'gunzip '+fnamegz[i]
    spawn,'ls DATA/*'+kids+'*fits > fits_list.txt'
; Run the qats algorithm:
    pdot=dblarr(nplanet)
    print,'Removing: ',KOI[indx],t0[indx],period[indx],tdur[indx]
    spawn,'ls depth_distribution.sav',result
    if(result eq '') then begin
      fit_transit,kids,t0[indx]+54900d0,period[indx],pdot,tdur[indx]/24d0
    endif
    restore,'depth_distribution.sav'
    chisq_array[where(chisq_array lt 0d0)]=0d0
    ntime=n_elements(time)
    gap0=median(time[1:ntime-1L]-time[0:ntime-2L])
    pmin=round(prange[0]/gap0) & pmax=round(prange[1]/gap0) 
    flag=0 & nperiod=alog10(prange[1]/prange[0])/alog10(1d0+f/2d0)
    ncadence=long(round((max(time)-min(time))/gap0) +1L) & sigma=1d-4
    cadence=round((time-min(time))/gap0)
    timetotal=min(time)+(max(time)-min(time))*dindgen(ncadence)/double(ncadence-1L)
    sntot=dblarr(2,ndepth,ndur,nperiod)
    for idepth=1,ndepth-1 do begin
;    for idepth=9,9 do begin
;      for iq=8,8 do begin
;      for iq=13,13 do begin
      for iq=0,ndur-1 do begin
        spawn,'rm -f qats_spectrum.txt'
        ftotal=dblarr(ncadence)
        ftotal[cadence-min(cadence)]=chisq_array[idepth,iq,*]
        openw,1,'lightcurve.in'
        printf,1,ncadence,f,q,pmin,pmax,flag
        for i=0L,ncadence-1L do printf,1,timetotal[i],ftotal[i],sigma
        close,1
        spawn,'./test_qpt'
        readcol,'qats_spectrum.txt',tminnew,tmaxnew,mmnew,qnew,smaxnew,mbestnew,/silent
        pgrid=(tminnew+tmaxnew)*.5d0*gap0
        sntot[0,idepth,iq,0:n_elements(smaxnew)-1]=pgrid
        sntot[1,idepth,iq,0:n_elements(smaxnew)-1]=smaxnew
        snr=smaxnew*(pgrid/100d0)^.5d0
        ;plot,pgrid,snr,/xl,ys=1
        i0=where(snr eq max(snr))
        print,idepth,iq,pgrid[i0[0]],smaxnew[i0[0]]
      endfor
    endfor
    save,time,fsap,f,sntot,pmin,pmax,depth,ndepth,tdur,ndur,filename='qats_depth_dur_'+kids+'.sav'
    spawn,'mv depth_distribution.sav depth_distribution_'+kids+'.sav'
    print,'Finished'
    c=get_kbrd(1)
; Now, re-gzip these files:
;    readcol,'fits_list.txt',fname,format='a'
;    for i=0,n_elements(fname)-1 do spawn,'gzip '+fname[i]
  endif else begin
     print,'Trouble identifying KID ',kid0[ikid],indx
  endelse
endfor
return
end
