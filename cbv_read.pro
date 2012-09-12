pro cbv_read,quarter,channel,cbv
; CBV filenames:
path='./DATA/'
fname=[$
'kplr2009131105131-q00-d14_lcbv.fits',$
'kplr2009166043257-q01-d14_lcbv.fits',$
'kplr2009259160929-q02-d14_lcbv.fits',$
'kplr2009350155506-q03-d14_lcbv.fits',$
'kplr2010078095331-q04-d14_lcbv.fits',$
'kplr2010174085026-q05-d08_lcbv.fits',$
'kplr2010265121752-q06-d09_lcbv.fits',$
'kplr2010355172524-q07-d10_lcbv.fits',$
'kplr2011073133259-q08-d11_lcbv.fits',$
'kplr2011177032512-q09-d12_lcbv.fits',$
'kplr2011271113734-q10-d13_lcbv.fits',$
'kplr2012004120508-q11-d15_lcbv.fits',$
'kplr2012088054726-q12-d17_lcbv.fits']
cbv=mrdfits(path+fname[quarter],channel,h)
print,fxpar(h,'EXTNAME')
return
end
