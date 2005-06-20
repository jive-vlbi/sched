#!csh

mv accsrc.f   accsrc.f1  ; sed s/ERROR/ERRLOG/g accsrc.f1   >  accsrc.f   
mv antpos.f   antpos.f1  ; sed s/ERROR/ERRLOG/g antpos.f1   >  antpos.f   
mv badlo.f    badlo.f1   ; sed s/ERROR/ERRLOG/g badlo.f1    >  badlo.f    
mv chkcode.f  chkcode.f1 ; sed s/ERROR/ERRLOG/g chkcode.f1  >  chkcode.f  
mv chksc1.f   chksc1.f1  ; sed s/ERROR/ERRLOG/g chksc1.f1   >  chksc1.f   
mv chkvla.f   chkvla.f1  ; sed s/ERROR/ERRLOG/g chkvla.f1   >  chkvla.f   
mv crdvla.f   crdvla.f1  ; sed s/ERROR/ERRLOG/g crdvla.f1   >  crdvla.f   
mv dopfq.f    dopfq.f1   ; sed s/ERROR/ERRLOG/g dopfq.f1    >  dopfq.f    
mv fileopen.f fileopen.f1; sed s/ERROR/ERRLOG/g fileopen.f1 >  fileopen.f 
mv getfset.f  getfset.f1 ; sed s/ERROR/ERRLOG/g getfset.f1  >  getfset.f  
mv getpset.f  getpset.f1 ; sed s/ERROR/ERRLOG/g getpset.f1  >  getpset.f  
mv getptlo.f  getptlo.f1 ; sed s/ERROR/ERRLOG/g getptlo.f1  >  getptlo.f  
mv getset.f   getset.f1  ; sed s/ERROR/ERRLOG/g getset.f1   >  getset.f   
mv getsta.f   getsta.f1  ; sed s/ERROR/ERRLOG/g getsta.f1   >  getsta.f   
mv gettim.f   gettim.f1  ; sed s/ERROR/ERRLOG/g gettim.f1   >  gettim.f   
mv headpos.f  headpos.f1 ; sed s/ERROR/ERRLOG/g headpos.f1  >  headpos.f  
mv horchk.f   horchk.f1  ; sed s/ERROR/ERRLOG/g horchk.f1   >  horchk.f   
mv listfreq.f listfreq.f1; sed s/ERROR/ERRLOG/g listfreq.f1 >  listfreq.f 
mv mtaltaz.f  mtaltaz.f1 ; sed s/ERROR/ERRLOG/g mtaltaz.f1  >  mtaltaz.f  
mv mtequat.f  mtequat.f1 ; sed s/ERROR/ERRLOG/g mtequat.f1  >  mtequat.f  
mv mtxyew.f   mtxyew.f1  ; sed s/ERROR/ERRLOG/g mtxyew.f1   >  mtxyew.f   
mv mtxyns.f   mtxyns.f1  ; sed s/ERROR/ERRLOG/g mtxyns.f1   >  mtxyns.f   
mv optcells.f optcells.f1; sed s/ERROR/ERRLOG/g optcells.f1 >  optcells.f 
mv optcsar.f  optcsar.f1 ; sed s/ERROR/ERRLOG/g optcsar.f1  >  optcsar.f  
mv optcsub.f  optcsub.f1 ; sed s/ERROR/ERRLOG/g optcsub.f1  >  optcsub.f  
mv optsch.f   optsch.f1  ; sed s/ERROR/ERRLOG/g optsch.f1   >  optsch.f   
mv optupt.f   optupt.f1  ; sed s/ERROR/ERRLOG/g optupt.f1   >  optupt.f   
mv pcalfq.f   pcalfq.f1  ; sed s/ERROR/ERRLOG/g pcalfq.f1   >  pcalfq.f   
mv pkfinish.f pkfinish.f1; sed s/ERROR/ERRLOG/g pkfinish.f1 >  pkfinish.f 
mv plmark.f   plmark.f1  ; sed s/ERROR/ERRLOG/g plmark.f1   >  plmark.f   
mv plotmap.f  plotmap.f1 ; sed s/ERROR/ERRLOG/g plotmap.f1  >  plotmap.f  
mv plotsta.f  plotsta.f1 ; sed s/ERROR/ERRLOG/g plotsta.f1  >  plotsta.f  
mv pn3db.f    pn3db.f1   ; sed s/ERROR/ERRLOG/g pn3db.f1    >  pn3db.f    
mv pseti.f    pseti.f1   ; sed s/ERROR/ERRLOG/g pseti.f1    >  pseti.f    
mv rdpeak.f   rdpeak.f1  ; sed s/ERROR/ERRLOG/g rdpeak.f1   >  rdpeak.f   
mv rdset.f    rdset.f1   ; sed s/ERROR/ERRLOG/g rdset.f1    >  rdset.f    
mv rfreq.f    rfreq.f1   ; sed s/ERROR/ERRLOG/g rfreq.f1    >  rfreq.f    
mv rotvlba.f  rotvlba.f1 ; sed s/ERROR/ERRLOG/g rotvlba.f1  >  rotvlba.f  
mv sattim.f   sattim.f1  ; sed s/ERROR/ERRLOG/g sattim.f1   >  sattim.f   
mv schday.f   schday.f1  ; sed s/ERROR/ERRLOG/g schday.f1   >  schday.f   
mv sched.f    sched.f1   ; sed s/ERROR/ERRLOG/g sched.f1    >  sched.f    
mv schfiles.f schfiles.f1; sed s/ERROR/ERRLOG/g schfiles.f1 >  schfiles.f 
mv schgeo.f   schgeo.f1  ; sed s/ERROR/ERRLOG/g schgeo.f1   >  schgeo.f   
mv schin.f    schin.f1   ; sed s/ERROR/ERRLOG/g schin.f1    >  schin.f    
mv schopt.f   schopt.f1  ; sed s/ERROR/ERRLOG/g schopt.f1   >  schopt.f   
mv schpre.f   schpre.f1  ; sed s/ERROR/ERRLOG/g schpre.f1   >  schpre.f   
mv schtim.f   schtim.f1  ; sed s/ERROR/ERRLOG/g schtim.f1   >  schtim.f   
mv scndup.f   scndup.f1  ; sed s/ERROR/ERRLOG/g scndup.f1   >  scndup.f   
mv setband.f  setband.f1 ; sed s/ERROR/ERRLOG/g setband.f1  >  setband.f  
mv seterr.f   seterr.f1  ; sed s/ERROR/ERRLOG/g seterr.f1   >  seterr.f   
mv settrk.f   settrk.f1  ; sed s/ERROR/ERRLOG/g settrk.f1   >  settrk.f   
mv snap.f     snap.f1    ; sed s/ERROR/ERRLOG/g snap.f1     >  snap.f     
mv srclst.f   srclst.f1  ; sed s/ERROR/ERRLOG/g srclst.f1   >  srclst.f   
mv srfinish.f srfinish.f1; sed s/ERROR/ERRLOG/g srfinish.f1 >  srfinish.f 
mv srread.f   srread.f1  ; sed s/ERROR/ERRLOG/g srread.f1   >  srread.f   
mv stafrd.f   stafrd.f1  ; sed s/ERROR/ERRLOG/g stafrd.f1   >  stafrd.f   
mv stmsg.f    stmsg.f1   ; sed s/ERROR/ERRLOG/g stmsg.f1    >  stmsg.f    
mv stread.f   stread.f1  ; sed s/ERROR/ERRLOG/g stread.f1   >  stread.f   
mv sumope.f   sumope.f1  ; sed s/ERROR/ERRLOG/g sumope.f1   >  sumope.f   
mv suvopt.f   suvopt.f1  ; sed s/ERROR/ERRLOG/g suvopt.f1   >  suvopt.f   
mv timej.f    timej.f1   ; sed s/ERROR/ERRLOG/g timej.f1    >  timej.f    
mv times.f    times.f1   ; sed s/ERROR/ERRLOG/g times.f1    >  times.f    
mv toggle.f   toggle.f1  ; sed s/ERROR/ERRLOG/g toggle.f1   >  toggle.f   
mv tppack.f   tppack.f1  ; sed s/ERROR/ERRLOG/g tppack.f1   >  tppack.f   
mv tptpns.f   tptpns.f1  ; sed s/ERROR/ERRLOG/g tptpns.f1   >  tptpns.f   
mv uvopt.f    uvopt.f1   ; sed s/ERROR/ERRLOG/g uvopt.f1    >  uvopt.f    
mv uvqual.f   uvqual.f1  ; sed s/ERROR/ERRLOG/g uvqual.f1   >  uvqual.f   
mv vlbabws.f  vlbabws.f1 ; sed s/ERROR/ERRLOG/g vlbabws.f1  >  vlbabws.f  
mv vlbachg.f  vlbachg.f1 ; sed s/ERROR/ERRLOG/g vlbachg.f1  >  vlbachg.f  
mv vlba.f     vlba.f1    ; sed s/ERROR/ERRLOG/g vlba.f1     >  vlba.f     
mv vlbaini.f  vlbaini.f1 ; sed s/ERROR/ERRLOG/g vlbaini.f1  >  vlbaini.f  
mv vlbaint.f  vlbaint.f1 ; sed s/ERROR/ERRLOG/g vlbaint.f1  >  vlbaint.f  
mv vlbasu.f   vlbasu.f1  ; sed s/ERROR/ERRLOG/g vlbasu.f1   >  vlbasu.f   
