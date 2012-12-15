;readcol,'fits_list.txt',fname,format='a'
fname=kid_fits_filenames
nfile=n_elements(fname)
is_first_valid_quarter=1
for iname=0,nfile-1 do begin
  fits_read,fname[iname],tab,htab,exten=1
  channel=fxpar(htab,'CHANNEL')
print,'******channel******',channel
  quarter=fxpar(htab,'QUARTER')
  print,fxpar(htab,'MODULE'),fxpar(htab,'OUTPUT')
  cbv_read,quarter,channel,cbv
  if size(cbv,/type) ne 8 then begin
      err_msg = SYSTIME(/UTC) + "|WARN|read_data.pro|No CBV data was available for quarter="+string(quarter)+", channel="+string(channel)+", so we are excluding this segment of light curve."
      PRINT, err_msg
  endif else begin
;;3.3  Concatenate each quarter's values together
;;3.3.1  Initialize the concatenation with the first available quarter
      if is_first_valid_quarter then begin
          vec1=cbv.vector_1
          vec2=cbv.vector_2
          vec3=cbv.vector_3
          vec4=cbv.vector_4
          vec5=cbv.vector_5
          vec6=cbv.vector_6
          vec7=cbv.vector_7
          vec8=cbv.vector_8
          qtr=intarr(n_elements(vec1))+quarter
          time=tbget(htab,tab,'TIME')
          ftmp=tbget(htab,tab,'SAP_FLUX')
          fcor=ftmp/median(ftmp)
          stmp=tbget(htab,tab,'SAP_FLUX_ERR')
          inan=where(finite(stmp) eq 0)
          if(inan[0] eq 0) then stmp[inan]=median(stmp[where(finite(stmp) eq 1)])
          sig=stmp/median(ftmp)
          cadencetmp=tbget(htab,tab,'CADENCENO')
          cadence=cadencetmp
          is_first_valid_quarter=0
;;3.3.2  Continue the concatenation by joining on to the previous quarters
      endif else begin
          vec1=[vec1,cbv.vector_1]
          vec2=[vec2,cbv.vector_2]
          vec3=[vec3,cbv.vector_3]
          vec4=[vec4,cbv.vector_4]
          vec5=[vec5,cbv.vector_5]
          vec6=[vec6,cbv.vector_6]
          vec7=[vec7,cbv.vector_7]
          vec8=[vec8,cbv.vector_8]
          qtr=[qtr,intarr(n_elements(cbv.vector_1))+quarter]
          time=[time,tbget(htab,tab,'TIME')]
          ftmp=tbget(htab,tab,'SAP_FLUX')
          fcor=[fcor,ftmp/median(ftmp)]
          stmp=tbget(htab,tab,'SAP_FLUX_ERR')
          inan=where(finite(stmp) eq 0)
          if(inan[0] eq 0) then stmp[inan]=median(stmp[where(finite(stmp) eq 1)])
          sig=[sig,stmp/median(ftmp)]
          cadencetmp=tbget(htab,tab,'CADENCENO')
          cadence=[cadence,cadencetmp]
      endelse
  endelse
endfor
help,fcor
;stop
indx=where(finite(fcor) eq 1)
time=time[indx]+54833d0
fcor=fcor[indx]
sig=sig[indx]
vec1=vec1[indx]
vec2=vec2[indx]
vec3=vec3[indx]
vec4=vec4[indx]
vec5=vec5[indx]
vec6=vec6[indx]
vec7=vec7[indx]
vec8=vec8[indx]
qtr = qtr[indx]
weight=1d0/sig^2
ntime=n_elements(time)
fflat=fcor
correction=fcor
for iqtr=min(qtr),max(qtr) do begin
  jqtr=where(qtr eq iqtr)
  if(jqtr[0] ge 0) then begin
    fqtr=fcor[jqtr]
    medqtr=median(fqtr)
    fqtr=fqtr/medqtr-1d0
    coeff=regress(transpose([[vec1[jqtr]],[vec2[jqtr]],[vec3[jqtr]],[vec4[jqtr]],[vec5[jqtr]]]),fqtr)
    correction1=medqtr*(coeff[0]*vec1[jqtr]+coeff[1]*vec2[jqtr]+coeff[2]*vec3[jqtr]+coeff[3]*vec4[jqtr]+coeff[4]*vec5[jqtr])
    fqtr=fcor[jqtr]-correction1+median(correction1)
;    fqtr=fcor[jqtr]
    fflat[jqtr]=fqtr/median(fqtr)
    correction[jqtr]=correction1-median(correction1)
;  plot,time[jqtr]-55000d0,fqtr/median(fqtr),psym=3,ys=1,yr=[.998,1.002]
;  oplot,time[jqtr]-55000d0,fqtr/median(fqtr),psym=3,col=255 & char=get_kbrd(1)
  endif
endfor
;plot,time-54900d0,fflat,ys=1,psym=3
