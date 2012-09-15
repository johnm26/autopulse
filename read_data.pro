readcol,'fits_list.txt',fname,format='a'
nfile=n_elements(fname)
for iname=0,nfile-1 do begin
  fits_read,fname[iname],tab,htab,exten=1
  channel=fxpar(htab,'CHANNEL')
print,'******channel******',channel
  quarter=fxpar(htab,'QUARTER')
  print,fxpar(htab,'MODULE'),fxpar(htab,'OUTPUT')
  cbv_read,quarter,channel,cbv
  if(iname eq 0) then vec1=cbv.vector_1 else vec1=[vec1,cbv.vector_1]
  if(iname eq 0) then vec2=cbv.vector_2 else vec2=[vec2,cbv.vector_2]
  if(iname eq 0) then vec3=cbv.vector_3 else vec3=[vec3,cbv.vector_3]
  if(iname eq 0) then vec4=cbv.vector_4 else vec4=[vec4,cbv.vector_4]
  if(iname eq 0) then vec5=cbv.vector_5 else vec5=[vec5,cbv.vector_5]
  if(iname eq 0) then vec6=cbv.vector_6 else vec6=[vec6,cbv.vector_6]
  if(iname eq 0) then vec7=cbv.vector_7 else vec7=[vec7,cbv.vector_7]
  if(iname eq 0) then vec8=cbv.vector_8 else vec8=[vec8,cbv.vector_8]
  if(iname eq 0) then qtr=intarr(n_elements(vec1))+quarter else qtr=[qtr,intarr(n_elements(cbv.vector_1))+quarter]
  if(iname eq 0) then time=tbget(htab,tab,'TIME') else time=[time,tbget(htab,tab,'TIME')]
  ftmp=tbget(htab,tab,'SAP_FLUX')
  if(iname eq 0) then fcor=ftmp/median(ftmp) else fcor=[fcor,ftmp/median(ftmp)]
  stmp=tbget(htab,tab,'SAP_FLUX_ERR')
  inan=where(finite(stmp) eq 0)
  if(inan[0] eq 0) then stmp[inan]=median(stmp[where(finite(stmp) eq 1)])
  if(iname eq 0) then sig=stmp/median(ftmp) else sig=[sig,stmp/median(ftmp)]
  cadencetmp=tbget(htab,tab,'CADENCENO')
  if(iname eq 0) then cadence=cadencetmp else cadence=[cadence,cadencetmp]
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
for iqtr=1,12 do begin
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
plot,time-54900d0,fflat,ys=1,psym=3
