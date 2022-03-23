      SUBROUTINE SETCOP( FROM, TO )
C
C     Routine for SCHED that copies all information except the station
C     name from one setup group to another.  This is to allow users
C     to specify several stations with generic information but have 
C     SCHED treat them separately as it fills out defaults based on
C     the frequency catalog and other information.  SETCOP is used
C     by SETEXPND.
C
C     The input group is index FROM.  It is copied to TO.
C     If NSET is less than TO, it will be set to TO.
C
C     I guess this would be a good place for a C structure.  Maybe 
C     someday in FORTRAN 90.  But I need to be sure the compilers
C     are everywhere before I do that.
C
C     Double check that all variables in schset.inc that should be
C     copied are copied.  Found a couple missing! - 24mar2000 RCW.
C
C     Don't copy IFREQNUM, IFREQIF, SETSTA, ISETSTA
C     as they won't be set yet and/or would be wrong anyway.
C     These all depend on the station and the reason for copying
C     the setup is to get one for another station.
C     Also don't copy the frequency and pcal set information.  It
C     is not organized by setup group.
C
      INCLUDE  'sched.inc'
      INCLUDE  'schset.inc'
C
      INTEGER   FROM, TO, I, J, IF
C --------------------------------------------------------------------
      IF( TO .GT. NSET ) NSET = TO
C
C     Go copy.  Variables are in the order that they appear in 
C     schset.inc, other than that multidimension arrays are in
C     loops below.
C
      ISETNUM(TO)  = ISETNUM(FROM)            
      SETNAME(TO)  = SETNAME(FROM)
C
C     Reals
C
      AZCOLIM(TO)  = AZCOLIM(FROM)  
      ELCOLIM(TO)  = ELCOLIM(FROM)  
      PTINCR(TO)   = PTINCR(FROM)             
      PTOFF(TO)    = PTOFF(FROM)
      SAMPRATE(TO) = SAMPRATE(FROM)           
      SPEEDH(TO)   = SPEEDH(FROM)             
      SPEEDL(TO)   = SPEEDL(FROM)             
      FANOUT(TO)   = FANOUT(FROM)             
      SPEEDUP(TO)  = SPEEDUP(FROM)            
      BESTOVER(TO) = BESTOVER(FROM)
      TOTBW(TO)    = TOTBW(FROM)
C
C     Integers
C
      NCHAN(TO)    = NCHAN(FROM)              
      PERIOD(TO)   = PERIOD(FROM)             
      LEVEL(TO)    = LEVEL(FROM)              
      TAPEMODE(TO) = TAPEMODE(FROM)           
      TPMODE(TO)   = TPMODE(FROM)           
      SWTCHDUR(TO) = SWTCHDUR(FROM)           
C
C     Logicals
C
      DUALPOL(TO)  = DUALPOL(FROM)
      FRSWITCH(TO) = FRSWITCH(FROM)           
      DUALX(TO)    = DUALX(FROM)              
      MODETEST(TO) = MODETEST(FROM)
      USED(TO)     = USED(FROM)
C
C     Characters
C
      BAND(TO)     = BAND(FROM)
      NOISEFRQ(TO) = NOISEFRQ(FROM)     
      LOGGING(TO)  = LOGGING(FROM)          
      FORMAT(TO)   = FORMAT(FROM)           
      DBE(TO)      = DBE(FROM)
      FIRMFILE(TO) = FIRMFILE(FROM)
      SPCAL(TO)    = SPCAL(FROM)            
      RCHAN(TO)    = RCHAN(FROM)            
      LCHAN(TO)    = LCHAN(FROM)            
      LCP50CM(TO)  = LCP50CM(FROM)          
      RCP50CM(TO)  = RCP50CM(FROM)          
      BARREL(TO)   = BARREL(FROM)           
      M4PATCH(TO)  = M4PATCH(FROM)
C
C     Derived quantities.
C
      MINTRAK(TO)  = MINTRAK(FROM)
      MAXTRAK(TO)  = MAXTRAK(FROM)
      MINTBPS(TO)  = MINTBPS(FROM)
      MAXTBPS(TO)  = MAXTBPS(FROM)
      TBPS(TO)     = TBPS(FROM)
      TOTBPS(TO)   = TOTBPS(FROM)
      VLBAMKIV(TO) = VLBAMKIV(FROM)
      RECUSED(TO)  = RECUSED(FROM)
C
C     Multi-dimension arrays that need loops.
C
      DO IF = 1, 4
         IFDIST(IF,TO) = IFDIST(IF,FROM)         
         NOISE(IF,TO)  = NOISE(IF,FROM)          
         FE(IF,TO)     = FE(IF,FROM)             
      END DO
C
      DO I = 1, 4
         STRING(I,TO) = STRING(I,FROM)        
      END DO
C
      DO I = 1, 3
         SYNTH(I,TO)  = SYNTH(I,FROM)            
      END DO  
C
      DO I = 1, MCHAN
         FREQREF(I,TO) = FREQREF(I,FROM)          
         FIRSTLO(I,TO) = FIRSTLO(I,FROM)      
         FIFMIN(I,TO)  = FIFMIN(I,FROM)
         FIFMAX(I,TO)  = FIFMAX(I,FROM)
         BBSYN(I,TO)   = BBSYN(I,FROM)        
         BBSYN2(I,TO)  = BBSYN2(I,FROM)       
         CORINV(I,TO)  = CORINV(I,FROM)
         BBFILT(I,TO)  = BBFILT(I,FROM)       
         BITS(I,TO)    = BITS(I,FROM)         
         BBC(I,TO)     = BBC(I,FROM)          
         SIDEBD(I,TO)  = SIDEBD(I,FROM)     
         CRDSIDE(I,TO) = CRDSIDE(I,FROM)     
         POL(I,TO)     = POL(I,FROM)        
         IFCHAN(I,TO)  = IFCHAN(I,FROM)     
         ALTIFC(I,TO)  = ALTIFC(I,FROM)
         NETSIDE(I,TO) = NETSIDE(I,FROM)    
         SIDE1(I,TO)   = SIDE1(I,FROM)      
         SFCHAN(I,TO)  = SFCHAN(I,FROM)
         SGCHAN(I,TO)  = SGCHAN(I,FROM)
         VFESYN(I,TO)  = VFESYN(I,FROM)
         DO J = 1, MTPMOD
            TRACK(I,J,TO) = TRACK(I,J,FROM)      
         END DO
      END DO
C
      DO I = 1, MAXPC
         PCALFR1(I,TO) = PCALFR1(I,FROM)     
         PCALFR2(I,TO) = PCALFR2(I,FROM)     
         PCALX1(I,TO)  = PCALX1(I,FROM)    
         PCALX2(I,TO)  = PCALX2(I,FROM)    
      END DO
C
      RETURN
      END

