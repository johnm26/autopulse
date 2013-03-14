; For a given KID, finds all of the files, unzips, detrends,
; runs QATS, saves & outputs spectrum:
;; @example:  IDL>  compute_qats_check_transit,1432214,0.005,[1.0,300.0],mask_planet=0,working_dir='/astro/net/astro-agol/blevlee/CODE/condor/test3/test_working_dir/',common_data_root_dir='/astro/net/astro-agol/blevlee/CODE/IDL/KEPLER_REDUX/autopulse/'
;; @example2: IDL>  compute_qats_check_transit,757076,0.005,[1.0,300.0],mask_planet=0,working_dir='/astro/store/student-scratch1/bvegaff/QATSruns/KOI_chiSq/test_new_data/output/007/00757076/check_transits/',common_data_root_dir='/astro/users/bvegaff/testGit/autopulse/',single_depth_dur=[9,10],plot_transit_detrends=1

;NOTE- for plot_transit_detrends keyword to work correctly, need a list of the start-of-transit cadences in transit_times.txt.$
;To get the cadences right, first run with your single_depth_dur to make the file, then run with the same single_depth_dur, and plot_transit_detrends=1.


pro compute_qats_check_transit, $
                  kid0, $
                  f, $
                  prange, $
                  mask_planet=mask_planet, $
                  working_dir=working_dir, $
                  common_data_root_dir=common_data_root_dir, $
                  kid_fits_filenames=kid_fits_filenames, $
                  single_depth_dur=single_depth_dur, $ ;currently set-up as positions in our depth and duration arrays
                  mask_peak_transit_cadences=mask_peak_transit_cadences,$
                  plot_transit_detrends=plot_transit_detrends ;1 is true, must have a single_depth_dur set as well
                  
;1.  Set up internal variables
if keyword_set(kid_fits_filenames) then begin
    do_read_lightcurve_from_local_fitsfile_orig=1
endif else begin
    do_read_lightcurve_from_local_fitsfile_orig=0
endelse

;TESTING:
IF keyword_set(kid_fits_filenames) then print,'fitslist = '+STRING(do_read_lightcurve_from_local_fitsfile_orig),kid_fits_filenames

if keyword_set(mask_planet) then begin
    do_make_planetmask_master_orig=1
endif else begin
    do_make_planetmask_master_orig=0
endelse
if keyword_set(working_dir) then begin
    working_dir=working_dir+PATH_SEP()
endif else begin
    working_dir='./'
endelse
if keyword_set(common_data_root_dir) then begin
    common_data_root_dir=common_data_root_dir+PATH_SEP()
endif else begin
    common_data_root_dir='./'
endelse
sql_macro_tmpfile_name=working_dir+'tmpMacroMySQL.sql'
sql_queryresult_tmpfile_name=working_dir+'tmpResultMySQL.tab'
sql_query_donefile_name='sql_query_donefile'
fit_transit_donefile_name='fit_transit_donefile'
tt=0
ephem=0
q=1 ;;07Nov2012 q had been equal to 0 for Eric's FORTRAN version of qpt_detect.  However, Josh's quick_qats code crashes by running one beyond an array if q=0.  Try setting q=1.  !!!MAKE SURE ERIC'S VERSION RETURNS THE SAME RESULT AS BEFORE AFTER THIS CHANGE!!!
; Now, find all of the KOIs for masking:
restore,common_data_root_dir+'koi_data01.sav'
;KID             LONG      = Array[3559]
;KOI             FLOAT     = Array[3559]
;PERIOD          DOUBLE    = Array[3559]
;T0              DOUBLE    = Array[3559]
;TDUR            FLOAT     = Array[3559]
nkid=n_elements(kid0)

;2.  Check that the FORTRAN QATS executable has been precompiled, since
;you can't compile inside the IDL Virtual Machine
;if ~file_test('test_qpt') then begin
;    print,systime()+'|ERROR|test_qpt was not found.  Please compile a copy in the common_data_root_dir: '+common_data_root_dir
;    return
spawn,'gfortran -O3 -c qpt_detect.for -o qpt_detect.o'
spawn,'gfortran -O3 test_qpt.for qpt_detect.o -o test_qpt'
spawn,'gfortran -O3 -c quick_qats.for -o quick_qats.o'
spawn,'gfortran -O3 run_quick_qats.for quick_qats.o -o run_quick_qats'
;endif

;2.  Loop over Kepler ID list
for ikid=0,nkid-1 do begin
    kids=strcompress(kid0[ikid],/remove_all)
    print,'Working on KID: ',kids
    indx=where(long(kid0[ikid]) eq long(kid))
    if(indx[0] ge 0) then begin
        print,'KID found within saved ephemerides ',kid0[ikid],indx
        nplanet=n_elements(indx)
; Identify all of the fits files associated with this KID:
;    spawn,'ls *'+kids+'*gz > fits_gz_list.txt'
;    readcol,'fits_gz_list.txt',fnamegz,format='a'
; gunzip the files:
;    for i=0,n_elements(fnamegz)-1 do spawn,'gunzip '+fnamegz[i]
        spawn,'ls '+common_data_root_dir+'DATA/*'+kids+'*fits > fits_list.txt'
        pdot=dblarr(nplanet)
        print,'KOI info: ',KOI[indx],'t0',t0[indx],'period',period[indx],'tdur',tdur[indx]
        t0_curr=t0[indx]+54900d0
        period_curr=period[indx]
        pdot_curr=pdot
        tdur_curr=tdur[indx]/24d0
        do_read_lightcurve_from_local_fitsfile=do_read_lightcurve_from_local_fitsfile_orig
        do_make_planetmask=do_make_planetmask_master_orig
    endif else begin
        print,'Ephemeris file did not contain KID ',kid0[ikid],indx
        print,'planetmask was: ',do_make_planetmask_master_orig
;        print,'Therefore must read data from database.  No local file reading permitted.'
;        do_read_lightcurve_from_local_fitsfile=0
        do_make_planetmask=0
        do_read_lightcurve_from_local_fitsfile=do_read_lightcurve_from_local_fitsfile_orig
    endelse
;2.1 Check whether fit_transit previously finished and saved its work
;here.  If it is already accomplished, we can skip ahead.
    result=''
    if file_test(working_dir+fit_transit_donefile_name) then spawn,'ls '+working_dir+'depth_distribution.sav',result
    if(result eq '') then begin
;2.2 By default, read from Kepler SQL database (but don't do it if the
;local reading keyword is set)
        if (do_read_lightcurve_from_local_fitsfile EQ 0) then begin
;;;2.3 Clean up old temporary files
;;        file_delete, $
;;          sql_macro_tmpfile_name, $
;;          sql_queryresult_tmpfile_name, $
;;          /allow_nonexistent
;;;2.3.1 Double-check that delete is finished;  if not, wait.
;;        while file_test(sql_macro_tmpfile_name) do begin
;;           wait,0.1
;;        endwhile
;;        while file_test(sql_queryresult_tmpfile_name) do begin
;;            wait,1
;;        endwhile

;2.3 Check whether light curve has already been fetched from SQL
;database.  If not, then proceed with the querying.
;+START DEBUG:  a section to ensure I don't delete all of my existing .tab
;files in old versions while I rerun on those while waiting for the
;SQL database update.  BLL, 02Nov2012.
;            spawn,'touch '+working_dir+sql_query_donefile_name
;            while ~file_test(working_dir+sql_query_donefile_name) do begin
;                print,'.'
;                wait,0.1
;            endwhile
;-END DEBUG
            if ~file_test(working_dir+sql_query_donefile_name) then begin
;2.3.1 Form the SQL query
                status=make_sql_query_macro( $
                                             in_kepler_id=kids, $
                                             out_scriptfile_name=sql_macro_tmpfile_name, $
                                             log_lun=log_lun $
                                           )
                IF SIZE(status, /TYPE) EQ 7 THEN BEGIN
                    err_msg = SYSTIME(/UTC) + "|ERROR|compute_qats|Halting on main level due to error status passed up from make_sql_query_macro."
                    PRINT, err_msg
                    PRINTF, log_lun, err_msg
                    stop
                ENDIF
;2.3.2 Execute the SQL query
                status=make_query_to_sql_database_by_macro( $
                                                            in_scriptfile_name=sql_macro_tmpfile_name, $
                                                            out_queryresultfile_name=sql_queryresult_tmpfile_name, $
                                                            log_lun=log_lun $
                                                          )
                IF SIZE(status, /TYPE) EQ 7 THEN BEGIN
                    err_msg = SYSTIME(/UTC) + "|ERROR|compute_qats|Halting on main level due to error status passed up from make_query_to_sql_database_by_macro."
                    PRINT, err_msg
                    PRINTF, log_lun, err_msg
                    stop
                ENDIF
;2.3.2.1 Write a donefile to signal when SQL query was successful
                spawn,'touch '+working_dir+sql_query_donefile_name
            endif
;2.4 Parse the query result
            status=make_parsed_lightcurve_from_queryresult( $
                                                            in_queryresultfile_name=sql_queryresult_tmpfile_name, $
                                                            min_lines_required_in_queryresultfile=3, $
                                                            out_time=time, $
                                                            out_flux=flux, $
                                                            out_err_flux=err_flux, $
                                                            out_quarter=quarter, $
                                                            out_channel=channel, $
                                                            log_lun=log_lun $
                                                          )
            IF SIZE(status, /TYPE) EQ 7 THEN BEGIN
                err_msg = SYSTIME(/UTC) + "|ERROR|compute_qats|Halting on main level due to error status passed up from make_parsed_lightcurve_from_queryresult."
                PRINT, err_msg
                PRINTF, log_lun, err_msg
                stop
            ENDIF
        endif

;2.5 Run fit_transit on the parsed light curve 
        temptime = systime(1);CHECKING TIME PER RUNNNNNNNNNNNN
        fit_transit_check_transit, $
          kids, $
          t0=t0_curr, $
          period=period_curr, $
          pdot=pdot_curr, $
          tdur=tdur_curr, $
          db_time=time, $
          db_flux=flux, $
          db_err_flux=err_flux, $
          db_channel=channel, $
          db_quarter=quarter, $
          do_make_planetmask=do_make_planetmask, $
          do_read_lightcurve_from_local_fitsfile=do_read_lightcurve_from_local_fitsfile, $
          working_dir=working_dir, $
          common_data_root_dir=common_data_root_dir, $
          fit_transit_donefile_name=fit_transit_donefile_name, $
          kid_fits_filenames=kid_fits_filenames, $
          single_depth_dur=single_depth_dur, $
          mask_peak_transit_cadences=mask_peak_transit_cadences,$
          plot_transit_detrends=plot_transit_detrends
        print,'time to run fit transit' + string(systime(1)-temptime) ;CHECKING RUN TIMMEEEEE
    endif
    
    temptime = systime(1)   ;CHECKING RUN TIMMEEEEE
    restore,working_dir+'depth_distribution.sav'
    chisq_array[where(chisq_array lt 0d0)]=0d0
    ntime=n_elements(time)
    gap0=median(time[1:ntime-1L]-time[0:ntime-2L])
    pmin=round(prange[0]/gap0) & pmax=round(prange[1]/gap0) 
    flag=0 & nperiod=alog10(prange[1]/prange[0])/alog10(1d0+f/2d0)
    ncadence=long(round((max(time)-min(time))/gap0) +1L) & sigma=1d-4
    cadence=round((time-min(time))/gap0)
    timetotal=min(time)+(max(time)-min(time))*dindgen(ncadence)/double(ncadence-1L)
;    sntot=dblarr(4,ndepth,ndur,nperiod)
    expected_total_delta_chiSq = dblarr(ndepth,ndur)
    peak_period = dblarr(ndepth,ndur)
    peak_signal = dblarr(ndepth,ndur)
	sntrim = dblarr(ndepth,ndur,nperiod)
    total_calculated_signal = dblarr(ndepth,ndur)
    good_transit_count = dblarr(ndepth,ndur)
    max_dist_from_med = dblarr(ndepth,ndur)
    frac_signal_from_max = dblarr(ndepth,ndur)
    frac_signal_from_2 = dblarr(ndepth,ndur)
    max_chisq_rat = dblarr(ndepth,ndur)

    chisq_rat_max=0d0
    chisq_diff_max=0d0
    set_plot,'ps'
    device,filename=working_dir+'kid'+kids+'_qats.ps'
    cd,working_dir,current=current_dir
    for idepth=1,ndepth-1 do begin
;    for idepth=12,12 do begin
;      for iq=12,12 do begin
        for iq=0,ndur-1 do begin
            print,systime(/UTC)+'|Trying depth #'+strtrim(string(idepth),2)+' of '+strtrim(string(ndepth),2)+', duration #'+strtrim(string(iq),2)+' of '+strtrim(string(ndur),2)
            index_detected_points=where(chisq_array[idepth,iq,*] gt 0,count_detected_points)
                                ;plot,chisq_array_polypulse[2,10,*]
                                ;stop
            if count_detected_points gt 0 then begin
                spawn,'\rm -f '+working_dir+'qats_spectrum.txt'
                ftotal=dblarr(ncadence)
                ftotal[cadence-min(cadence)]=chisq_array[idepth,iq,*]
                openw,1,working_dir+'lightcurve.in'
                printf,1,ncadence,f,q,pmin,pmax,flag
                for i=0L,ncadence-1L do printf,1,timetotal[i],ftotal[i],sigma
                close,1
print,systime(/UTC)+'|Starting FORTRAN version of test_qpt...'
                spawn,'\cp -f '+common_data_root_dir+'test_qpt '+working_dir
                spawn,working_dir+'test_qpt'
                print,systime(/UTC)+'|...finished FORTRAN version of test_qpt.'
;print,systime(/UTC)+'|Starting FORTRAN version of run_quick_qats...'
;                spawn,'\cp -f '+common_data_root_dir+'run_quick_qats '+working_dir
;                spawn,working_dir+'run_quick_qats'
;print,systime(/UTC)+'|...finished FORTRAN version of run_quick_qats.'
;stop
;print,systime(/UTC)+'|Starting IDL QUICK_QATS version of test_qpt...'
;                 status=test_qpt( $
;                                  fractionalperiodwindow_f=f, $
;                                  boxwidth_q=q, $
;                                  n_cadences=ncadence, $
;                                  searchperiod_npoints_lo=pmin, $
;                                  searchperiod_npoints_hi=pmax, $
;                                  flag=flag, $
;                                  x_values=timetotal, $
;                                  y_values=ftotal, $
;                                  yerr_values=sigma, $
;                                  working_dir=working_dir $
;                               )
;print,systime(/UTC)+'|...finished IDL QUICK_QATS version of test_qpt...'
;stop
;                    spawn,'rm -f '+working_dir+'test_qpt '
                readcol,working_dir+'qats_spectrum.txt',tminnew,tmaxnew,mmnew,qnew,smaxnew,mbestnew,/silent
                pgrid=(tminnew+tmaxnew)*.5d0*gap0
                ngrid=n_elements(smaxnew)
;                sntot[0,idepth,iq,0:ngrid-1]=pgrid
;                sntot[1,idepth,iq,0:ngrid-1]=smaxnew
				sntrim[idepth,iq,0:ngrid-1]=smaxnew
                coeff=robust_poly_fit(alog10(pgrid),median(alog10(smaxnew),10),5)
                chisq_rat=smaxnew/10.^poly(alog10(pgrid),coeff)
                chisq_diff=smaxnew-10.^poly(alog10(pgrid),coeff)
;                sntot[2,idepth,iq,0:ngrid-1]=chisq_rat
;                sntot[3,idepth,iq,0:ngrid-1]=chisq_diff
                !p.multi=[0,1,3]
                i0=where(chisq_rat eq max(chisq_rat))
                if(max(chisq_rat) gt chisq_rat_max and max(chisq_diff[i0]) gt chisq_diff_max) then begin
                    chisq_rat_max=max(chisq_rat)
                    chisq_diff_max=chisq_diff[i0[0]]
                    datamax=[idepth,depth[idepth],iq,tdur[iq],pgrid[i0[0]],smaxnew[i0[0]]]
                endif
                max_chisq_rat[idepth,iq] = max(chisq_rat)
                plot,alog10(pgrid),chisq_rat
                    
                ;stuff Ben added:
                ;Calculates expected signal of transits $
                ;that match the peak signal for the given depth/duration.
                ;Can compare to actual peak signal. if a match, maybe some verification?
                openw,lun,working_dir+'lightcurve.in',/get_lun
                printf,lun,ncadence,f,q,long(tminnew[i0[0]]),long(tminnew[i0[0]]),flag
                for i=0L,ncadence-1L do printf,lun,timetotal[i],ftotal[i],sigma
                close,lun
                    free_lun,lun
                spawn,working_dir+'test_qpt'
                readcol,working_dir+'transit_times.txt',start_of_transit_cadences,format='l'
                start_of_transit_times = timetotal[start_of_transit_cadences] ;TESTINGGGG
                ;start_of_transit_times = time[start_of_transit_cadences]
                print,'QATS strongest signal start of transit times: ',start_of_transit_times
                ;print,'chi_sq values around a supposed transit: ',chisq_array[1,0,where((time ge start_of_transit_times[0]-0.01) and (time le start_of_transit_times[0]+0.01))]
                for i=0,n_elements(start_of_transit_cadences)-1 do begin
                    print,'chi_sq in QATS input around supposed transit ',i,': ',ftotal[start_of_transit_cadences[i]-10:start_of_transit_cadences[i]+10]
                endfor
                ;stuff Ben + Chris added:
                ;Returns the cadences that are in the transit (of our peak QATS signal for the input depth and dur)
                if keyword_set(single_depth_dur) then begin
                    openw,lun,working_dir+'transit_cadences.txt',/get_lun,/append
                    buffer_time = 0.5/24. ;half an hour, the buffer around our transit mask (we have duration uncertainty)
                    for i=0,n_elements(start_of_transit_cadences)-1 do begin
                        ;in_transit_cadences = where((time ge start_of_transit_times[i]-buffer_time) and (time le start_of_transit_times[i]+tdur[iq]+buffer_time))
                        in_transit_cadences_i = where((time ge start_of_transit_times[i]-0.005) and (time le start_of_transit_times[i]+0.005))
                        printf,lun,in_transit_cadences_i
                    endfor
                    close,lun
                    free_lun,lun
                endif


                expected_total_delta_chiSq[idepth,iq] = 0.0
                peak_period[idepth,iq] = tminnew[i0[0]]*gap0
                peak_signal[idepth,iq] = smaxnew[i0[0]]
                transit_delta_chiSq = dblarr(n_elements(start_of_transit_cadences))
                for i=0,n_elements(start_of_transit_cadences)-1 do begin

                    ;Calculating expected transit signals
                    in_transit_cadences = where((time ge start_of_transit_times[i]) and (time le start_of_transit_times[i]+tdur[iq]))
                    if in_transit_cadences[0] eq -1 then begin
                        expected_transit_delta_chiSq=0.0
                    endif else begin
                        expected_transit_delta_chiSq=0.0
                        for j=0,n_elements(in_transit_cadences)-1 do begin
                            if sigma_array_polypulse[idepth,iq,in_transit_cadences[j]] ne 0.0 then begin
                                expected_transit_delta_chiSq += (depth[idepth]/sigma_array_polypulse[idepth,iq,in_transit_cadences[j]])^2.
                            endif
                        endfor
                    endelse
                    expected_total_delta_chiSq[idepth,iq] += expected_transit_delta_chiSq

                    ;Calculating sigma(transit event signals),
                    ;And see if any of the signals are very far off from expected.
                    ;If only one or two transits have a strong signal, it's probably not real
                    if in_transit_cadences[0] eq -1 then begin 
                        transit_delta_chiSq[i] = -1.0
                    endif else begin
                        transit_delta_chiSq[i] = ftotal[start_of_transit_cadences[i]]
                    endelse
                endfor
                
                observed_transit_delta_chiSq = transit_delta_chiSq[where(transit_delta_chiSq ne -1.0)]
                robust_sig = robust_sigma(observed_transit_delta_chiSq)
                median_delta_chiSq = median(observed_transit_delta_chiSq)
                max_delta_chiSq = max(observed_transit_delta_chiSq)
                second_delta_chiSq = max(observed_transit_delta_chiSq[where(observed_transit_delta_chiSq ne max_delta_chiSq)])

                total_calculated_signal[idepth,iq] = total(observed_transit_delta_chiSq) ;should be same as that produced by QATS
                good_transit_count[idepth,iq] = n_elements(observed_transit_delta_chiSq) ;If only 3, then having two transits dominate the signal not so strange...
                max_dist_from_med[idepth,iq] = (max_delta_chiSq - median_delta_chiSq) / robust_sig ;If very large, confirms that this transit event probably outlier
                frac_signal_from_max[idepth,iq] = max_delta_chiSq / total_calculated_signal[idepth,iq] ;If very close to one, maybe suspicious signal?
                frac_signal_from_2[idepth,iq] = (second_delta_chiSq+max_delta_chiSq) / total_calculated_signal[idepth,iq] ;If very close to one, maybe suspicious signal?



;will be getting rid of this stuff because the new cut involves comparing the delta chi squared of the peak to the expected value
;more than one ; means the line was already commented out
;                if(smaxnew[i0[0]] gt 40d0 and chisq_diff[i0[0]] gt 10d0) then begin
;                    plot,pgrid,chisq_rat,/xl
;                    print,idepth,depth[idepth],iq,tdur[iq],pgrid[i0[0]],smaxnew[i0[0]],chisq_rat[i0[0]],chisq_diff[i0[0]]
;                    openw,1,working_dir+'lightcurve.in'
;                    printf,1,ncadence,f,q,long(tminnew[i0[0]]),long(tminnew[i0[0]]),flag
;                    for i=0L,ncadence-1L do printf,1,timetotal[i],ftotal[i],sigma
;                    close,1
;                    spawn,working_dir+'test_qpt'
;                    readcol,working_dir+'transit_times.txt',ntt,format='l'
;                    tt=timetotal[ntt]
;                    ephem=poly_fit(dindgen(n_elements(ntt)),tt,1,/double)
;                    print,ephem,tt,ftotal[ntt],stddev(tt-ephem[0]-ephem[1]*dindgen(n_elements(ntt))),stddev(ftotal(ntt))^2/mean(ftotal(ntt))
;;        c=get_kbrd(1)
;;         wait,.005
;                    plot,time-tt[0],chisq_array[idepth,iq,*],xr=[-2,2],yr=[0,max(chisq_array[idepth,iq,*])]
;                    for i=1,n_elements(ntt)-1 do oplot,time-tt[i],chisq_array[idepth,iq,*]
;;        c=get_kbrd(1)
;;        wait,.005
;                    is_first_plot=1
;                    for i=0,n_elements(ntt)-1 do begin
;                        iin=where(abs(time-tt[i]) lt 2d0,count_in_transit)
;;;if(iin[0] ge 0) then 
;                        if count_in_transit gt 0 then begin
;                            if is_first_plot eq 1 then begin
;                                y_plot_halfheight=max([3.0*depth[idepth],median(err_flux)])
;                                plot, $
;                                  time-tt[i],$
;                                  fsap/median(fsap[iin]), $
;                                  ys=1, $
;                                  xr=[-1,1], $
;                                  psym=6, $
;                                  symsize=0.25, $
;                                  thick=2, $
;                                  yr=[1.0-y_plot_halfheight,1.0+y_plot_halfheight], $ ;[.999,1.001], $
;                                  tit='Depth= '+string(depth[idepth]*1d6,format='(f8.1)')+ $
;                                  ' ppm; Duration= '+string(tdur[iq]*24d0,format='(f6.1)')+ $
;                                  ' hr; f= '+string(f,format='(f6.3)')+ $
;                                  '; t0= '+string(ephem[0],format='(f10.5)')+ $
;                                  '; Period= '+string(ephem[1],format='(f10.5)')
;                                is_first_plot=0
;                            endif else begin
;                                oplot,time-tt[i],fsap/median(fsap[iin]),psym=6,symsize=0.25,thick=2
;                            endelse
;                        endif
;                    endfor
;                endif
;;        c=get_kbrd(1)
;;        wait,.005
;;        endif
            endif else begin
                print,'|WARN|COMPUTE_QATS|No transit fits were deemed better than a systematics fit at this depth/duration combo. idepth:'+string(idepth)+' iq:'+string(iq)
            endelse
        endfor
    endfor
    print,'time for QATS part of code: '+string(systime(1)-temptime) ;CHECKING RUN TIMMEEEEE
    cd,current_dir
    device,/close
;    save,time,fsap,f,sntot,pmin,pmax,depth,ndepth,tdur,ndur,ephem,tt,datamax,filename=working_dir+'qats_depth_dur_'+kids+'.sav'
    OpenW,lun,working_dir+'expected_vs_actual_qats_peaks.txt',/get_lun
    printf,lun,'#depth    tdur(hrs)    peak_period normpeak_sig  peak_signal  peak_expect '+ $
    'calcd_sig good_trans max-med %sig_max %sig_2'
    for i=0,ndepth-1 do begin
        for j=0,ndur-1 do begin
            printf,lun,depth[i],tdur[j]*24.,peak_period[i,j],max_chisq_rat[i,j],peak_signal[i,j],expected_total_delta_chiSq[i,j],$
            total_calculated_signal[i,j],good_transit_count[i,j],max_dist_from_med[i,j],$
            frac_signal_from_max[i,j],frac_signal_from_2[i,j],FORMAT='(4(F11.6,x),3(F12.3,x),4(F9.3))'
	    endfor
    endfor
    Close,lun
        free_lun,lun
    OpenW,lun,working_dir+'qats_trim.txt',/get_lun
	for i=0,ndepth-1 do begin
		for j=0,ndur-1 do begin
			printf,lun,sntrim[i,j,0:nperiod-1],FORMAT='(2290(F10.3,x))'
		endfor
	endfor
	Close,lun
        free_lun,lun
;    spawn,'mv '+working_dir+'depth_distribution.sav '+working_dir+'depth_distribution_'+kids+'.sav'
    print,'Finished'
    cd,current_dir
;        c=get_kbrd(1)
; Now, re-gzip these files:
;    readcol,'fits_list.txt',fname,format='a'
;    for i=0,n_elements(fname)-1 do spawn,'gzip '+fname[i]
;    spawn,'gzip '+working_dir+'depth_distribution_'+kids+'.sav'
;    spawn,'gzip '+working_dir+'kid'+kids+'_qats.ps'
;   spawn,'python '+common_data_root_dir+'input_txt_to_db.py '+working_dir+'qats_trim.txt '+kids
;	spawn,'rm '+working_dir+'qats_trim.txt'
    spawn,'gzip '+working_dir+'qats_trim.txt'
;	spawn,'rm '+working_dir+'depth_distribution.sav'
;	spawn,'rm '+working_dir+'kid'+kids+'_qats.ps'
;    spawn,'rm '+working_dir+'transit_times.txt'
;    spawn,'rm '+working_dir+'lightcurve.in'
;    spawn,'rm '+working_dir+'qats_spectrum.txt'
;    spawn,'rm '+working_dir+'tmpResultMySQL.tab'
    spawn,'rm '+working_dir+'*donefile*'
    spawn,'touch '+working_dir+'donefile'
endfor
return
end
