; For a given KID, finds all of the files, unzips, detrends,
; runs QATS, saves & outputs spectrum:
;; @example:  IDL>  compute_qats,1432214,0.005,[1.0,300.0],mask_planet=0,working_dir='/astro/net/astro-agol/blevlee/CODE/condor/test3/test_working_dir',common_data_root_dir='/astro/net/astro-agol/blevlee/CODE/IDL/KEPLER_REDUX/autopulse'
pro compute_qats, $
                  kid0, $
                  f, $
                  prange, $
                  mask_planet=mask_planet, $
                  working_dir=working_dir, $
                  common_data_root_dir=common_data_root_dir
;1.  Set up internal variables
do_read_lightcurve_from_local_fitsfile_orig=0
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
tt=0
ephem=0
q=0
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
        print,'Removing: ',KOI[indx],t0[indx],period[indx],tdur[indx]
        t0_curr=t0[indx]+54900d0
        period_curr=period[indx]
        pdot_curr=pdot
        tdur_curr=tdur[indx]/24d0
        do_read_lightcurve_from_local_fitsfile=do_read_lightcurve_from_local_fitsfile_orig
        do_make_planetmask=do_make_planetmask_master_orig
    endif else begin
        print,'Ephemeris file did not contain KID ',kid0[ikid],indx
        print,'Therefore must read data from database.  No local file reading permitted.'
        do_read_lightcurve_from_local_fitsfile=0
        do_make_planetmask=0
    endelse
; Run the qats algorithm:
        spawn,'ls '+working_dir+'depth_distribution.sav',result
;2.2 Clean up old temporary files
        file_delete, $
          sql_macro_tmpfile_name, $
          sql_queryresult_tmpfile_name, $
          /allow_nonexistent
;2.3 By default, read from Kepler SQL database (but don't do it if the
;local reading keyword is set)
        if ~keyword_set(do_read_lightcurve_from_local_fitsfile) then begin
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
;2.3.3 Parse the query result
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

                                ;+Order arrays by time
                                ;index_time_ascending=sort(time)
                                ;time=time[index_time_ascending]
                                ;flux=flux[index_time_ascending]
                                ;err_flux=err_flux[index_time_ascending]
                                ;quarter=quarter[index_time_ascending]
                                ;channel=channel[index_time_ascending]
                                ;-

                                ;+Debug:  trimming off the initial jump
                                ;index_time_leadjunk=where(time lt 187.0)
                                ;time=time[max(index_time_leadjunk):*]
                                ;flux=flux[max(index_time_leadjunk):*]
                                ;err_flux=err_flux[max(index_time_leadjunk):*]
                                ;channel=channel[max(index_time_leadjunk):*]
                                ;quarter=quarter[max(index_time_leadjunk):*]
                                ;-End debug

        if(result eq '') then begin
            fit_transit, $
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
              common_data_root_dir=common_data_root_dir
        endif

        restore,working_dir+'depth_distribution.sav'
        chisq_array[where(chisq_array lt 0d0)]=0d0
        ntime=n_elements(time)
        gap0=median(time[1:ntime-1L]-time[0:ntime-2L])
        pmin=round(prange[0]/gap0) & pmax=round(prange[1]/gap0) 
        flag=0 & nperiod=alog10(prange[1]/prange[0])/alog10(1d0+f/2d0)
        ncadence=long(round((max(time)-min(time))/gap0) +1L) & sigma=1d-4
        cadence=round((time-min(time))/gap0)
        timetotal=min(time)+(max(time)-min(time))*dindgen(ncadence)/double(ncadence-1L)
        sntot=dblarr(4,ndepth,ndur,nperiod)
        chisq_rat_max=0d0
        chisq_diff_max=0d0
        set_plot,'ps'
        device,filename=working_dir+'kid'+kids+'_qats.ps'
        cd,working_dir,current=current_dir
        for idepth=1,ndepth-1 do begin
;    for idepth=1,16 do begin
;    for idepth=9,9 do begin
;      for iq=8,8 do begin
;      for iq=13,13 do begin
            for iq=0,ndur-1 do begin
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
                    spawn,'\cp -f '+common_data_root_dir+'test_qpt '+working_dir
                    spawn,working_dir+'test_qpt'
;                    spawn,'rm -f '+working_dir+'test_qpt '
                    readcol,working_dir+'qats_spectrum.txt',tminnew,tmaxnew,mmnew,qnew,smaxnew,mbestnew,/silent
                    pgrid=(tminnew+tmaxnew)*.5d0*gap0
                    ngrid=n_elements(smaxnew)
                    sntot[0,idepth,iq,0:ngrid-1]=pgrid
                    sntot[1,idepth,iq,0:ngrid-1]=smaxnew
                    coeff=robust_poly_fit(alog10(pgrid),median(alog10(smaxnew),10),5)
                    chisq_rat=smaxnew/10.^poly(alog10(pgrid),coeff)
                    chisq_diff=smaxnew-10.^poly(alog10(pgrid),coeff)
                    sntot[2,idepth,iq,0:ngrid-1]=chisq_rat
                    sntot[3,idepth,iq,0:ngrid-1]=chisq_diff
                    !p.multi=[0,1,3]
                    i0=where(chisq_rat eq max(chisq_rat))
                    if(max(chisq_rat) gt chisq_rat_max and max(chisq_diff[i0]) gt chisq_diff_max) then begin
                        chisq_rat_max=max(chisq_rat)
                        chisq_diff_max=chisq_diff[i0[0]]
                        datamax=[idepth,depth[idepth],iq,tdur[iq],pgrid[i0[0]],smaxnew[i0[0]]]
                    endif
                    if(smaxnew[i0[0]] gt 40d0 and chisq_diff[i0[0]] gt 10d0) then begin
                        plot,pgrid,chisq_rat,/xl
                        print,idepth,depth[idepth],iq,tdur[iq],pgrid[i0[0]],smaxnew[i0[0]],chisq_rat[i0[0]],chisq_diff[i0[0]]
                        openw,1,working_dir+'lightcurve.in'
                        printf,1,ncadence,f,q,long(tminnew[i0[0]]),long(tminnew[i0[0]]),flag
                        for i=0L,ncadence-1L do printf,1,timetotal[i],ftotal[i],sigma
                        close,1
                        spawn,working_dir+'test_qpt'
                        readcol,working_dir+'transit_times.txt',ntt,format='l'
                        tt=timetotal[ntt]
                        ephem=poly_fit(dindgen(n_elements(ntt)),tt,1,/double)
                        print,ephem,tt,ftotal[ntt],stddev(tt-ephem[0]-ephem[1]*dindgen(n_elements(ntt))),stddev(ftotal(ntt))^2/mean(ftotal(ntt))
;        c=get_kbrd(1)
;          wait,.005
                        plot,time-tt[0],chisq_array[idepth,iq,*],xr=[-2,2],yr=[0,max(chisq_array[idepth,iq,*])]
                        for i=1,n_elements(ntt)-1 do oplot,time-tt[i],chisq_array[idepth,iq,*]
;        c=get_kbrd(1)
;        wait,.005
                        plot,time-tt[0],fsap/median(fsap[where(abs(time-tt[0]) lt 2d0)]),ys=1,xr=[-1,1],psym=6,symsize=0.25,thick=2,yr=[.999,1.001],tit='Depth= '+string(depth[idepth]*1d6,format='(f5.1)')+' ppm; Duration= '+string(tdur[iq]*24d0,format='(f5.1)')+' hr; f= '+string(f,format='(f6.3)')+'; t0= '+string(ephem[0],format='(f10.5)')+'; Period= '+string(ephem[1],format='(f10.5)')
                        for i=1,n_elements(ntt)-1 do begin
                            iin=where(abs(time-tt[i]) lt 2d0)
                            if(iin[0] ge 0) then oplot,time-tt[i],fsap/median(fsap[iin]),psym=6,symsize=0.25,thick=2
                        endfor
                    endif
;        c=get_kbrd(1)
;        wait,.005
;        endif
                endif else begin
                    print,'|WARN|COMPUTE_QATS|No transit fits were deemed better than a systematics fit at this depth/duration combo. idepth:'+string(idepth)+' iq:'+string(iq)
                endelse
            endfor
        endfor
        cd,current_dir
        device,/close
        save,time,fsap,f,sntot,pmin,pmax,depth,ndepth,tdur,ndur,ephem,tt,datamax,filename=working_dir+'qats_depth_dur_'+kids+'.sav'
        spawn,'mv '+working_dir+'depth_distribution.sav '+working_dir+'depth_distribution_'+kids+'.sav'
        print,'Finished'
        cd,current_dir
;        c=get_kbrd(1)
; Now, re-gzip these files:
;    readcol,'fits_list.txt',fname,format='a'
;    for i=0,n_elements(fname)-1 do spawn,'gzip '+fname[i]
endfor
return
end
