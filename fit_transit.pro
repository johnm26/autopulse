pro fit_transit, $
                 kid, $
                 t0, $
                 period, $
                 pdot, $
                 td, $
                 db_time=db_time, $
                 db_flux=db_flux, $
                 db_err_flux=db_err_flux, $
                 db_quarter=db_quarter, $
                 db_channel=db_channel
; 8/23/2012 This routine fits a transit light curve with a box-car
; transit shape of a specified depth (multiplied by a polynomial).  
; A delta-chi-square is computed relative to a fit with a single
; polynomial; this will be fed into QATS to search for planets.
; Remaining questions to figure out:  1) how do I pick a window?
; 2) how do I pick the polynomial order (AIC, BIC, F-test, etc)?
; 3) how do I detect jumps that were not eliminated by the CBV?

;;1.  Set up internal variables
@const
;;2.1  Reading data from fits files
;@read_data
;;2.2  Applying CBV's to SQL database light curves
status=make_cbv_corrected_lightcurve( $
                                      time=db_time, $
                                      flux=db_flux, $
                                      err_flux=db_err_flux, $
                                      channel=db_channel, $
                                      quarter=db_quarter, $
                                      cbv_data_dir='./DATA/', $
                                      log_lun=log_lun $
                                    )
;;Compare CBV results
;time=time-55000d0
db_time=db_time-55000d0
;plot,time,fflat,color=-1,psym=3,charsize=2,xtitle='time',ytitle='cbvsap_flux',title='KID 1432214'
;oplot,db_time,db_flux/median(db_flux),color=255,psym=3
;;Assign database-extracted light curve into the business end
;;variables
time=db_time
fflat=db_flux
sig=db_err_flux
;;Resume Eric's unmodified code
t0=t0-55000d0
; Baseline 'window' before & after transit for fitting polynomial:
window=1d0
; Range of depths of the transit to search over:
depth=[0d0,3.2d-5,4d-5,5d-5,6.4d-5,8d-5,.000100,.000128,.000160,.000200,.000256,.000320,.000400,.000512,.000640,.000800,.001000]
tdur=[1d0,1.25,1.5,1.75,2d0,2.5,3d0,3.5,4d0,4.5d0,5d0,5.5d0,6d0,6.5,7d0,7.5,8d0,8.5,9d0,9.5,10d0]/24d0
ndepth=n_elements(depth)
ndur=n_elements(tdur)
fsap=fflat
ssap=sig

nt=n_elements(time)
gap=where((shift(reform(time),-1)-reform(time)) gt 0.5d0)
;gap=[gap,1634,5158]
gap=gap[sort(gap)]
gap1=[0,gap+1]
gap2=[gap,nt-1]
nseg=n_elements(gap1)


; Mask known planets by dividing out best-fit periodic model:
;restore,'koi701_01_05_model.sav'
;fsap = fsap / modtot

goto,skipmaskplanet
n0=floor((min(time)-t0)/period)
ntrans=ceil((max(time)-min(time))/period)+1
nplanet=n_elements(period)
print,'Number of planets: ',nplanet
mask_planet=bytarr(nplanet,n_elements(time))
for ip=0,nplanet-1 do begin
  if(period[ip] gt 0d0) then begin
    nnp=n0[ip]+dindgen(ntrans[ip])
    ttp=t0[ip]+period[ip]*nnp+pdot[ip]*nnp^2
    for it=0,ntrans[ip]-1 do mask_planet[ip,*]=(mask_planet[ip,*] or abs(time-ttp[it]) lt 1.1d0*td[ip])
;    plot,((time-t0[ip]+period[ip]/2d0) mod period[ip])+period[ip]/2d0,fflat/median(fflat,100),ys=1,ps=3,yr=[.998,1.002],col=256L^3-1L
;    oplot,((time-t0[ip]+period[ip]/2d0) mod period[ip])+period[ip]/2d0,fflat/median(fflat,100)*(mask_planet[ip,*] eq 0),col=255,ps=3
;    char=get_kbrd(1)
  endif else begin
    mask_planet[ip,*]=0
  endelse
endfor
skipmaskplanet:

; Flag & remove outliers:

; Read in bad points from data file:
readcol,'bad_regions.txt',bad_start,bad_end,format='f,f'
nbad=n_elements(bad_start)
mask=bytarr(n_elements(time))
for ibad=0,nbad-1 do begin
;  for ip=0,nplanet-1 do begin
;    mask_planet[ip,*]=mask_planet[ip,*] or (time gt bad_start[ibad] and time lt bad_end[ibad])
;  endfor
    mask=mask or (time gt bad_start[ibad] and time lt bad_end[ibad])
endfor
mask=mask eq 0
mask= mask and (abs(fsap-median(fsap,20)) lt 5d-4)

;char=get_kbrd(1)

chisq_array=dblarr(ndepth,ndur,nt)
size_array=intarr(ndur,nt)
ord=2
for iseg=0,nseg-1 do begin
  i1=gap1[iseg] & i2=gap2[iseg]  
  for itime=i1,i2 do begin
    for idur=0,ndur-1 do begin
      indx=i1+where((time[i1:i2]-time[itime]-tdur[idur]) lt window and $
            (time[i1:i2]-time[itime]) gt -window and (mask[i1:i2] eq 1))
      iout=i1+where(((((time[i1:i2]-time[itime]-tdur[idur]) lt window) and ((time[i1:i2]-time[itime]-tdur[idur] gt 0d0))) or $
            (((time[i1:i2]-time[itime]) gt -window) and ((time[i1:i2]-time[itime]) lt 0d0))) and (mask[i1:i2] eq 1))
      iin=where((time[indx] ge time[itime]) and (time[indx] le (time[itime]+tdur[idur])))
      if(n_elements(iout) gt ord+2 and n_elements(iin) ge 1) then begin
        ttmp=time[indx]-time[itime]
        ntmp=double(n_elements(indx))
        size_array[idur,itime]=ntmp
        for idepth=0,ndepth-1 do begin
          model=1d0-depth[idepth]*((time[indx] ge time[itime]) and (time[indx] le (time[itime]+tdur[idur])))
          flux_model=fsap[indx]/model
          flux=fsap[indx]
          coeff0=poly_fit(ttmp,flux,ord,/double,measure_errors=ssap[indx],chisq=chi0,yfit=yfit0)
          coeff=poly_fit(ttmp,flux_model,ord,/double,measure_errors=ssap[indx],chisq=chi,yfit=yfit)
          if(idepth eq 0) then chisq_array[idepth,idur,itime]=chi else chisq_array[idepth,idur,itime]=chi0-chi
;print,idepth,idur,itime,chisq_array[idepth,idur,itime]
          if(chi0-chi gt 20d0) then begin
;            plot,ttmp,flux,ys=1,ps=4
;            oplot,ttmp,poly(ttmp,coeff0),col=255
;           oplot,ttmp,poly(ttmp,coeff)*model,col=255L*256L
;           char=get_kbrd(1)
          endif
        endfor
      endif
    endfor
    if(itime mod 1000 eq 0) then print,'completed: ',itime*ndur*ndepth/double(nt*ndepth*ndur)*100d0,'% steps'
  endfor
  for idur=0,ndur-1 do begin
    inz=where(reform(double(size_array[idur,i1:i2])) ne 0d0)
    resid=sqrt(chisq_array[0,idur,i1+inz]/size_array[idur,i1+inz])
    for idepth=1,ndepth-1 do chisq_array[idepth,idur,i1:i2]= chisq_array[idepth,idur,i1:i2]/median(resid)^2
  endfor
endfor
save,chisq_array,depth,ndepth,tdur,ndur,size_array,time,fsap,ssap,mask,filename='depth_distribution.sav'
;save,/all,filename='depth_distribution'+kid+'.sav'
return
end
