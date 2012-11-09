pro fit_transit, $
                 kid, $
                 t0=t0, $
                 period=period, $
                 pdot=pdot, $
                 tdur=td, $
                 db_time=db_time, $
                 db_flux=db_flux, $
                 db_err_flux=db_err_flux, $
                 db_quarter=db_quarter, $
                 db_channel=db_channel, $
                 do_make_planetmask=do_make_planetmask, $
                 do_read_lightcurve_from_local_fitsfile=do_read_lightcurve_from_local_fitsfile, $
                 working_dir=working_dir, $
                 common_data_root_dir=common_data_root_dir, $
                 fit_transit_donefile_name=fit_transit_donefile_name
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
;;1.1 Range of depths of the transit to search over:
;;=============================================================================
depth=[0d0,3.2d-5,4d-5,5d-5,6.4d-5,8d-5,.000100,.000128,.000160,.000200,.000256,.000320,.000400,.000512,.000640,.000800,.001000,.01,.024]
;depth=[0d0,.001,.024]
ndepth=n_elements(depth)
;;=============================================================================
;;1.2 Range of durations of the transit to search over:
;;=============================================================================
tdur=[1d0,1.25,1.5,1.75,2d0,2.5,3d0,3.5,4d0,4.5d0,5d0,5.5d0,6d0,6.5,7d0,7.5,8d0,8.5,9d0,9.5,10d0]/24d0
ndur=n_elements(tdur)
;;=============================================================================
;;2.1  Reading data from fits files
;;=============================================================================
if keyword_set(do_read_lightcurve_from_local_fitsfile) then begin
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
                                          cbv_data_dir=common_data_root_dir+'/DATA/', $
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
;;2.3 Plot to verify that the corrected light curve to be fit looks good by eye
;;=============================================================================
if keyword_set(do_screen_plotting) then begin
    set_plot,'X'
    plot,time,fflat,psym=3,title='Corrected light curve to be fit.',ystyle=1
    wait,1
endif
;;=============================================================================
;;2.4 Visualize the FFT of the light curve
;;=============================================================================
;;=============================================================================
;;2.4.1 Pick the longest non-gappy segment of light curve available
;;=============================================================================
time_interval_matching_threshold=0.00001 ;;allowed slop in days for exposures to count as normal
time_interval=time[1:n_elements(time)-1]-time[0:n_elements(time)-2]
median_time_interval=median(time_interval)
index_time_interval_gaps=where(abs(time_interval-median_time_interval) gt time_interval_matching_threshold,count_time_interval_gaps)
uninterrupted_length_between_gaps=index_time_interval_gaps[1:n_elements(index_time_interval_gaps)-1]-index_time_interval_gaps[0:n_elements(index_time_interval_gaps)-2]
;;=============================================================================
;;2.4.1.2 Sort to get an assortment of the longest regions to test with
;;BIC polynomial order chooser.
;;=============================================================================
index_uninterrupted_length_between_gaps_sorted=reverse(sort(uninterrupted_length_between_gaps))
;;=============================================================================
;;2.4.2. Loop through a handful of the longest uninterrupted regions,
;;and pick the median polynomial order to progress
;;=============================================================================
number_of_bic_test_regions=20L
bic_chosen_polynomial_orders=lonarr(number_of_bic_test_regions)
for i=0L,number_of_bic_test_regions-1 do begin
;max_uninterrupted_length_between_gaps=max(uninterrupted_length_between_gaps,index_max_uninterrupted_length_between_gaps)
    max_uninterrupted_length_between_gaps=uninterrupted_length_between_gaps[index_uninterrupted_length_between_gaps_sorted[i]]
    index_max_uninterrupted_length_between_gaps=index_uninterrupted_length_between_gaps_sorted[i]
    index_start_nogap_region=index_time_interval_gaps[index_max_uninterrupted_length_between_gaps]+1
    index_end_nogap_region=index_time_interval_gaps[index_max_uninterrupted_length_between_gaps+1]-1
    time_nogap_region=time[index_start_nogap_region:index_end_nogap_region]
    fflat_nogap_region=fflat[index_start_nogap_region:index_end_nogap_region]
    sig_nogap_region=sig[index_start_nogap_region:index_end_nogap_region]
;;=============================================================================
;;2.4.2 Run the FFT on the non-gappy data
;;=============================================================================
    npoints=n_elements(time_nogap_region)
    delta=median( shift(reform(time_nogap_region),-1)-reform(time_nogap_region) )
;t=time
;;g_t_anal=fflat_nogap_region
;;print,'***DEBUG:  inserting a fake sine signal to see if the FFT picks it up cleanly'
    g_t_anal=fflat_nogap_region ;;*sin(4.*!DPI*time_nogap_region)
;;2.4.2.1 Use a sigma clip to filter out any transits...
;;sigma_g_t_anal=robust_sigma(g_t_anal)
;;
    u=lindgen(npoints)
    f=(((u+npoints/2) mod npoints) -(npoints/2))/(delta*npoints)
    g_f_fft=npoints*delta*fft(g_t_anal)
    index_positive_frequency = where(f ge 0)
    f_pos=f[index_positive_frequency]
    g_f_fft_pos=abs(g_f_fft[index_positive_frequency])
    if keyword_set(do_screen_plotting) then begin
        !p.multi=[0,1,2]
        plot,time_nogap_region,g_t_anal,/ys,xtitle='Time (d)',ytitle='Relative flux',title=kid
        plot,f_pos,g_f_fft_pos,psym=-4,xtitle='freq (cycles/day)',ytitle='power',title='power spectrum',xrange=[0,max(f)],/xs,/ylog
        !p.multi=[0,1,1]
;om=(((u+npoints/2) mod npoints) -(npoints/2))/(delta*npoints)
;scargle,t,g_t_anal,om,px
;    stop
        wait,1
    endif
;;=============================================================================
;;2.4.3 Select frequency with the highest power (ignoring the zero frequency)
;;=============================================================================
    f_pos_nozero=f_pos[1:n_elements(f_pos)-1]
    g_f_fft_pos_nozero=g_f_fft_pos[1:n_elements(f_pos)-1]
    dummy=max(g_f_fft_pos_nozero,index_peak_frequency)
    peak_period=1.0/f_pos_nozero[index_peak_frequency]
;;=============================================================================
;;3.3 Set up 'window' before & after transit for fitting polynomial:
;;=============================================================================
;window=1d0
    time_nogap_region_range=max(time_nogap_region) - min(time_nogap_region)
;print,'******',(time_nogap_region_range-max(tdur))/2.0
;print,'******',peak_period/2.0
    window=min([ (time_nogap_region_range-max(tdur))/2.0 ,peak_period/2.0])
    single_fit_time_baseline=max(tdur)+2.0*window
    index_start_nogap_region_trimmed=index_start_nogap_region
    dummy=min(abs(time-time[index_start_nogap_region]-single_fit_time_baseline),index_end_nogap_region_trimmed)
    time_nogap_region_trimmed=time[index_start_nogap_region_trimmed:index_end_nogap_region_trimmed]
    fflat_nogap_region_trimmed=fflat[index_start_nogap_region_trimmed:index_end_nogap_region_trimmed]
    sig_nogap_region_trimmed=sig[index_start_nogap_region_trimmed:index_end_nogap_region_trimmed]
    time_nogap_region_trimmed=( (time_nogap_region_trimmed-min(time_nogap_region_trimmed)) / (max(time_nogap_region_trimmed)-min(time_nogap_region_trimmed)) )
;;=============================================================================
;;2.5  Set up the polynomial detrending order
;;=============================================================================
    bic_chosen_polynomial_orders[i]=calc_best_poly_deg_using_bic( $
                                      data_x=time_nogap_region_trimmed, $
                                      data_y=fflat_nogap_region_trimmed, $
                                      data_err_y=sig_nogap_region_trimmed, $
                                      plot_to_screen=1, $
                                      max_trial_poly_deg=20L $
                                    )
;    stop
endfor
;;=============================================================================
;;2.6 Pick the median of all the choices of polynomial order to
;;proceed with (other choices to explore are the mode, or the maximum;
;;the maximum would be susceptible to outliers).
;;=============================================================================
ord=long(median(bic_chosen_polynomial_orders))
print,'Selected polynomial detrending order = '+strtrim(string(ord),2)
n_hist_bic_choices=histogram(bic_chosen_polynomial_orders,location=bic_choices)
plot,$
  bic_choices,$
  n_hist_bic_choices,$
  psym=10,$
  xtitle='BIC-selected poly order',$
  ytitle='N',$
  title='Distribution of polynomial orders selected by BIC for various segments'
oplot,$
  [ord,ord],$
  [-99999,99999],$
  color=255
xyouts,0.7,0.7,'Order selected: '+strtrim(string(ord),2),/normal,charsize=2
;stop
;;=============================================================================
;;Resume Eric's unmodified code
;;=============================================================================
;;=============================================================================
;;3. Set up internal variables
;;=============================================================================
fsap=fflat
ssap=sig
;;=============================================================================
;;3.4 Find temporal gaps in data taking:
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
if keyword_set(do_make_planetmask) then begin
    print,systime()+'|FIT_TRANSIT|Making planet mask'
    t0=t0-55000d0
    n0=floor((min(time)-t0)/period)
    ntrans=ceil((max(time)-min(time))/period)+1
    nplanet=n_elements(period)
;stop
;nplanet=1
;print,'***DEBUG:  Attempt to mask the first planet but leave the second.  Alter both the mask array and the loop for this.  Restore afterwards.'
    print,'Number of planets to be masked: ',nplanet
    mask_planet=bytarr(nplanet,n_elements(time))
    for ip=0,nplanet-1 do begin
        if(period[ip] gt 0d0) then begin
            nnp=n0[ip]+dindgen(ntrans[ip])
            ttp=t0[ip]+period[ip]*nnp+pdot[ip]*nnp^2
            for it=0,ntrans[ip]-1 do mask_planet[ip,*]=(mask_planet[ip,*] or abs(time-ttp[it]) lt 1.1d0*td[ip])
            if keyword_set(do_screen_plotting) then begin
                plot,((time-t0[ip]+period[ip]/2d0) mod period[ip])+period[ip]/2d0,fflat/median(fflat,100),ys=1,ps=3,yr=[.998,1.002],col=-1,title='Planet masking'
                oplot,((time-t0[ip]+period[ip]/2d0) mod period[ip])+period[ip]/2d0,fflat/median(fflat,100)*(mask_planet[ip,*] eq 0),col=255*256L,ps=3
                legend,['retained','masked'],color=[255*256L,-1]
                wait,1
;;    char=get_kbrd(1)
            endif
        endif else begin
            mask_planet[ip,*]=0
        endelse
    endfor
endif

;;=============================================================================
;;5.  Flag & remove outliers:
;;=============================================================================
;;5.1 Initialize mask:
mask=bytarr(n_elements(time))
;;5.2 Exclude known bad sections
;;5.2.1 Read in known bad points from data file:
readcol,common_data_root_dir+'bad_regions.txt',bad_start,bad_end,format='f,f'
nbad=n_elements(bad_start)
for ibad=0,nbad-1 do begin
;    if keyword_set(do_make_planetmask) then begin
;        for ip=0,nplanet-1 do begin
;            mask_planet[ip,*]=mask_planet[ip,*] or (time gt bad_start[ibad] and time lt bad_end[ibad])
;        endfor
;    endif else begin
    mask=mask or (time gt bad_start[ibad] and time lt bad_end[ibad])
;    endelse
endfor
;;5.2.2 Invert 0's into 1's and vice versa:
mask=mask eq 0
;;5.3 Mask out known planet transits, if desired
if keyword_set(do_make_planetmask) then begin
    mask_planet=mask_planet eq 0
    for ip=0,nplanet-1 do begin
        mask=mask*reform(mask_planet[ip,*])
    endfor
endif
;;5.4 Mask outliers beyond outlier_threshold of the median
;outlier_threshold=5d-4
outlier_threshold=1.0
if max(depth) gt outlier_threshold then begin
    err_msg = SYSTIME(/UTC) + "|ERROR|FIT_TRANSIT|One or more of the pulse depths to be tested exceeded the absolute outlier rejection threshold.  Iterating over the depths in excess of the threshold will cause unstable chisq results.  Please either increase the outlier_threshold or limit the depth array to smaller depths before proceeding."
    PRINT, err_msg
    ;;PRINTF, log_lun, err_msg
    SAVE, FILENAME=working_dir+'error_FIT_TRANSIT.sav', /ALL
    stop
endif
mask= mask and (abs(fsap-median(fsap,20)) lt outlier_threshold)

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
;should I use the same order over the whole lc, or should we attempt
;to evaluate locally?
;ord=2
;;=============================================================================
;;6.2  Commence the loop over segments:
;;=============================================================================
;for iseg=16,nseg-1 do begin
;    print,'******DEBUG:  starting at segment 1 instead of segment 0 for step fit testing******'
;    print,'******Restore to starting at segment 0 when done******'
for iseg=0,nseg-1 do begin
    i1=gap1[iseg] & i2=gap2[iseg]
;stop
;;=============================================================================
;;6.2.1  Commence the loop over window position within the segment
;;=============================================================================
    for itime=i1,i2 do begin
;    for itime=i1,i2 do begin
;        print,iseg,itime
;;=============================================================================
;;6.2.1.1  For the maximum possible size of the window at this
;;position, do a polynomial+step fit.  This fit takes a long time
;;compared to a plain polynomial fit, which is why we are going to do
;;it outside the pulse duration loop, which uses a more tailored
;;window.  Because the fit will be done at the maximum window size, we
;;will have to offset the fit result later to match each more tailored
;;window (this means recalculating the chi-squared in the new window).
;;=============================================================================
        indx_maxwindow=i1+where((time[i1:i2]-time[itime]-max(tdur)) lt window and $
                                (time[i1:i2]-time[itime]) gt -window and (mask[i1:i2] eq 1))
        iout=i1+where(((((time[i1:i2]-time[itime]-max(tdur)) lt window) and ((time[i1:i2]-time[itime]-max(tdur) gt 0d0))) or $
                       (((time[i1:i2]-time[itime]) gt -window) and ((time[i1:i2]-time[itime]) lt 0d0))) and (mask[i1:i2] eq 1))
        iin=where((time[indx_maxwindow] ge time[itime]) and (time[indx_maxwindow] le (time[itime]+max(tdur))))
        if(n_elements(iout) gt ord+2 and n_elements(iin) ge 1) then begin
            ttmp_maxwindow=time[indx_maxwindow]-time[itime]
            ntmp_maxwindow=double(n_elements(indx_maxwindow))
            ;;Do best fit for polynomial multiplier and depth/height of
            ;;step.
            status=calc_stepfit(data_x=double(ttmp_maxwindow),data_y=double(fsap[indx_maxwindow]),measure_errors=double(ssap[indx_maxwindow]),poly_order=ord,chisq=chi_stepfit_maxwindow,yfit=yfit_stepfit_maxwindow)
        endif
;;=============================================================================
;;6.2.1.2  Commence the loops over pulse durations and depths
;;=============================================================================
        for idur=0,ndur-1 do begin
;;=============================================================================
;;6.2.1.2.1 Define a data window matching this pulse duration
;;=============================================================================
            indx=i1+where((time[i1:i2]-time[itime]-tdur[idur]) lt window and $
                          (time[i1:i2]-time[itime]) gt -window and (mask[i1:i2] eq 1))
            iout=i1+where(((((time[i1:i2]-time[itime]-tdur[idur]) lt window) and ((time[i1:i2]-time[itime]-tdur[idur] gt 0d0))) or $
                           (((time[i1:i2]-time[itime]) gt -window) and ((time[i1:i2]-time[itime]) lt 0d0))) and (mask[i1:i2] eq 1))
            iin=where((time[indx] ge time[itime]) and (time[indx] le (time[itime]+tdur[idur])))
            if(n_elements(iout) gt ord+2 and n_elements(iin) ge 1) then begin
                ttmp=time[indx]-time[itime]
                ntmp=double(n_elements(indx))
                size_array[idur,itime]=ntmp
;;=============================================================================
;;6.2.1.2.2  Do best fit for polynomial alone for this window.
;;Ideally, it would be outside the duration loop, but the duration
;;loop looks like it has side effects on the window of data to fit to.
;;=============================================================================
                flux=fsap[indx]
;                coeff0=poly_fit(ttmp,flux,ord,/double,measure_errors=ssap[indx],chisq=chi0,yfit=yfit0)
                coeff0=calc_polyfit_regress( $
                                             data_x=ttmp, $
                                             data_y=flux, $
                                             measure_errors=ssap[indx], $
                                             poly_order=ord, $
                                             chisq=chi0, $
                                             yfit=yfit0, $
                                             plot_to_screen=0 $
                                           )
;;=============================================================================
;;6.2.1.2.2.1 Recalculate the chi-squared for the polynomial+step fit, when confined to this window
;;=============================================================================
;;6.2.1.2.2.1.1 Find where indx appears in indx_maxwindow:
                match2,indx,indx_maxwindow,indx_sub,indx_maxwindow_sub
                indx_match_success=where(indx_sub ne -1,count_match_success)
                if count_match_success ne n_elements(indx) then begin
                    err_msg = SYSTIME(/UTC) + "|ERROR|FIT_TRANSIT|count_match_success is not equal to the number of elements in indx."
                    PRINT, err_msg
                    ;;PRINTF, log_lun, err_msg
                    SAVE, FILENAME=working_dir+'error_FIT_TRANSIT.sav', /ALL
                    stop
                endif
;;6.2.1.2.2.1.2 Calculate the chi-squared:
                yfit_stepfit=yfit_stepfit_maxwindow[indx_sub]
                chi_stepfit=total( ((yfit_stepfit-flux)/ssap[indx])^2 )
;;=============================================================================
;;6.2.1.2.3 Commence loop over pulse depth and do best fit over depths for transit pulse
;;=============================================================================
                for idepth=0,ndepth-1 do begin
                    model=1d0-depth[idepth]*((time[indx] ge time[itime]) and (time[indx] le (time[itime]+tdur[idur])))
                    flux_model=fsap[indx]/model
;                    coeff=poly_fit(ttmp,flux_model,ord,/double,measure_errors=ssap[indx],chisq=chi,yfit=yfit)
                    coeff=calc_polyfit_regress( $
                                                data_x=ttmp, $
                                                data_y=flux_model, $
                                                measure_errors=ssap[indx], $
                                                poly_order=ord, $
                                                chisq=chi, $
                                                yfit=yfit, $
                                                plot_to_screen=0 $
                                              )
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
                    if(chi0-chi gt 20d0) and keyword_set(do_screen_plotting) then begin
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
                        ;;stop
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
err_flux=db_err_flux
save, $
  chisq_array, $
;  chisq_array_poly, $
;  chisq_array_polypulse, $
;  chisq_array_polystep, $
depth, $
  ndepth, $
  tdur, $
  ndur, $
  size_array, $
  time, $
  fsap, $
  ssap, $
  mask, $
  err_flux, $
  filename=working_dir+'depth_distribution.sav'
spawn,'touch '+working_dir+fit_transit_donefile_name
;save,/all,filename='depth_distribution'+kid+'.sav'
return
end
