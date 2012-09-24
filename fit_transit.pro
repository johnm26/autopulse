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
                 db_channel=db_channel, $
                 read_lightcurve_from_local_fitsfile=read_lightcurve_from_local_fitsfile
; 8/23/2012 This routine fits a transit light curve with a box-car
; transit shape of a specified depth (multiplied by a polynomial).  
; A delta-chi-square is computed relative to a fit with a single
; polynomial; this will be fed into QATS to search for planets.
; Remaining questions to figure out:  1) how do I pick a window?
; 2) how do I pick the polynomial order (AIC, BIC, F-test, etc)?
; 3) how do I detect jumps that were not eliminated by the CBV?

;;=============================================================================
;;1.  Set up internal variables
;;=============================================================================
@const
;;=============================================================================
;;2.1  Reading data from fits files
;;=============================================================================
if keyword_set(read_lightcurve_from_local_fitsfile) then begin
@read_data
    time=time-55000d0
endif else begin
;;=============================================================================
;;2.2  Applying CBV's to SQL database light curves
;;=============================================================================
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
    db_time=db_time-55000d0
;plot,time,fflat,color=-1,psym=3,charsize=2,xtitle='time',ytitle='cbvsap_flux',title='KID 1432214'
;oplot,db_time,db_flux/median(db_flux),color=255,psym=3
;;Assign database-extracted light curve into the business end
;;variables
    time=db_time
    fflat=db_flux
    sig=db_err_flux
endelse

;;=============================================================================
;; Plot to verify the corrected light curve to be fit looks good by eye
;;=============================================================================
set_plot,'X'
plot,time,fflat,psym=3,title='Corrected light curve to be fit.',ystyle=1
wait,1
;;=============================================================================
;;Resume Eric's unmodified code
;;=============================================================================
;;=============================================================================
;;3. Set up internal variables
;;=============================================================================
t0=t0-55000d0
fsap=fflat
ssap=sig
;;=============================================================================
;;3.1 Baseline 'window' before & after transit for fitting polynomial:
;;=============================================================================
window=1d0
;;=============================================================================
;;3.2 Range of depths of the transit to search over:
;;=============================================================================
;depth=[0d0,3.2d-5,4d-5,5d-5,6.4d-5,8d-5,.000100,.000128,.000160,.000200,.000256,.000320,.000400,.000512,.000640,.000800,.001000]
depth=[0d0,.000100,.001]
tdur=[1d0,1.25,1.5,1.75,2d0,2.5,3d0,3.5,4d0,4.5d0,5d0,5.5d0,6d0,6.5,7d0,7.5,8d0,8.5,9d0,9.5,10d0]/24d0
ndepth=n_elements(depth)
ndur=n_elements(tdur)
;;=============================================================================
;;3.3 Find temporal gaps in data taking:
;;=============================================================================
nt=n_elements(time)
gap=where((shift(reform(time),-1)-reform(time)) gt 0.5d0)
;stop
;gap=[gap,1634,5158]
;gap=[gap,1635,5530,41460]
;gap=[gap,10]
gap=gap[sort(gap)]
gap1=[0,gap+1]
gap2=[gap,nt-1]
nseg=n_elements(gap1)


;;=============================================================================
;;4. Mask known planets by dividing out best-fit periodic model:
;;=============================================================================
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

;;=============================================================================
;;5.  Flag & remove outliers:
;;=============================================================================

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

;;=============================================================================
;;6.  Loop over light curve segments and do three kinds of fits to each
;;segment:  polynomial, polynomial+pulse (gridded), polynomial+step
;;=============================================================================
;;=============================================================================
;;6.1  Set up variables:
;;=============================================================================
chisq_array=dblarr(ndepth,ndur,nt)
chisq_array_poly=dblarr(ndepth,ndur,nt)
chisq_array_polypulse=dblarr(ndepth,ndur,nt)
chisq_array_polystep=dblarr(ndepth,ndur,nt)
size_array=intarr(ndur,nt)
ord=2
;;=============================================================================
;;6.2  Commence the loop over segments:
;;=============================================================================
;for iseg=1,nseg-1 do begin
;    print,'******DEBUG:  starting at segment 1 instead of segment 0 for step fit testing******'
;    print,'******Restore to starting at segment 0 when done******'
for iseg=0,nseg-1 do begin
    i1=gap1[iseg] & i2=gap2[iseg]
;;=============================================================================
;;6.2.1  Commence the loop over window position within the segment
;;=============================================================================
    for itime=i1,i2 do begin
        print,iseg,itime
;;=============================================================================
;;6.2.1.1  For the maximum possible size of the window at this
;;position, do a polynomial+step fit (this fit takes a long time,
;;which is why we are going to do it outside the pulse duration loop,
;;which uses a more tailored window).
;;=============================================================================
;        indx=i1+where((time[i1:i2]-time[itime]-max(tdur)) lt window and $
;                      (time[i1:i2]-time[itime]) gt -window and (mask[i1:i2] eq 1))
        
;;=============================================================================
;;6.2.1.2  Commence the loops over pulse durations and depths
;;=============================================================================
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
                ;;Do best fit for polynomial alone for this window.  Ideally,
                ;;it would be outside the duration loop, but the duration loop
                ;;looks like it has side effects on the window of data to fit
                ;;to.
                flux=fsap[indx]
                coeff0=poly_fit(ttmp,flux,ord,/double,measure_errors=ssap[indx],chisq=chi0,yfit=yfit0)
                ;;Do best fit for polynomial multiplier and depth/height of
                ;;step.  Ideally, it would be outside the duration loop, but
                ;;the duration loop looks like it has side effects on the
                ;;window of data to fit to.
                status=calc_stepfit(data_x=double(ttmp),data_y=double(fsap[indx]),measure_errors=double(ssap[indx]),poly_order=ord,chisq=chi_stepfit,yfit=yfit_stepfit)
                ;;Do best fit over depths for transit pulse
                for idepth=0,ndepth-1 do begin
                    model=1d0-depth[idepth]*((time[indx] ge time[itime]) and (time[indx] le (time[itime]+tdur[idur])))
                    flux_model=fsap[indx]/model
                    coeff=poly_fit(ttmp,flux_model,ord,/double,measure_errors=ssap[indx],chisq=chi,yfit=yfit)
                    ;;Save chisq results for later analysis
                    chisq_array_poly[idepth,idur,itime]=chi0
                    chisq_array_polypulse[idepth,idur,itime]=chi
                    chisq_array_polystep[idepth,idur,itime]=chi_stepfit
                    ;;Save best chisq for passing up to compute_qats         
                    if(idepth eq 0) then begin
                        chisq_array[idepth,idur,itime]=chi
                    endif else begin
                        value_best_chi=min([chi,chi_stepfit],index_best_chi)
                        if index_best_chi eq 0 then begin
;!!!Need to add dimensions to chisq_array so we can go back after the fact and
;plot the chi values of all three kinds of fits and analyze what
;happened to all three as a function of time.  EA, BLL, 24Sep2012.
                            chisq_array[idepth,idur,itime]=chi0-chi
                        endif else begin
                            ;;Want case where step func. gives better fit (lower
                            ;;chisq) to yield a negative value in chisq_array:
                            chisq_array[idepth,idur,itime]=chi_stepfit-chi
                        endelse
                    endelse
;print,idepth,idur,itime,chisq_array[idepth,idur,itime]
;+Debug plotting
                                ;if(chi0-chi gt 20d0) then begin
                                ;if(chi0-chi gt 20d0 and chi_stepfit-chi gt 20d0) then begin
                    if(chi0-chi gt 20d0) then begin
                        !p.multi=[0,1,2]
                        time_for_plot=time[indx]
                        plot,[time_for_plot,time_for_plot,time_for_plot],[flux,poly(ttmp,coeff0),poly(ttmp,coeff)*model],ys=1,/nodata,charsize=2
                        annotstr=['chi_poly:'+string(chi0),'chi_polypulse:'+string(chi),'chi_polystep:'+string(chi_stepfit)]
                        legend,annotstr
                        oplot,time_for_plot,flux,psym=4
                        oplot,time_for_plot,poly(ttmp,coeff0),col=255
                        oplot,time_for_plot,poly(ttmp,coeff)*model,col=255L*256L
                        oplot,time_for_plot,yfit_stepfit,col=255L*256L*256L
                        plot,indx,flux,psym=3,ystyle=1,charsize=2
                        wait,0.01
                                ;stop
                        ;;char=get_kbrd(1)
                    endif
;-
                endfor
            endif
        endfor
        if(itime mod 100 eq 0) then print,'completed: ',itime*ndur*ndepth/double(nt*ndepth*ndur)*100d0,'% steps'
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
