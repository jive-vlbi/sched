C$Procedure ZZCVSTAT ( Constant velocity state )
 
      SUBROUTINE ZZCVSTAT ( ET, REF, CENTER, STATE )
 
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines. Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Umbrella routine for constant velocity state "put" and 
C     "extrapolate" routines.
C
C$ Disclaimer
C
C     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
C     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
C     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
C     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
C     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
C     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
C     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
C     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
C     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
C     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
C
C     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
C     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
C     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
C     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
C     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
C     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
C
C     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
C     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
C     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
C     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
C
C$ Required_Reading
C
C     SPK
C
C$ Keywords
C
C     EPHEMERIS
C
C$ Declarations

      IMPLICIT NONE
      
      DOUBLE PRECISION      ET
      CHARACTER*(*)         REF
      INTEGER               CENTER
      DOUBLE PRECISION      STATE ( 6 )

C$ Brief_I/O
C
C     Variable  I/O  Entry points
C     --------  ---  --------------------------------------------------
C     ET         I   ZZCVSSTA, ZZCVXSTA
C     REF        I   ZZCVSSTA, ZZCVXSTA
C     CENTER    I-O  ZZCVSSTA, ZZCVXSTA
C     STATE     I-O  ZZCVSSTA, ZZCVXSTA
C
C$ Detailed_Input
C
C     See the headers of the entry points for descriptions of
C     their inputs.
C                                 
C$ Detailed_Output
C
C     See the headers of the entry points for descriptions of
C     their outputs.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     See the headers of the entry points for descriptions of
C     their exceptions.
C
C$ Files
C
C     See the entry points ZZCVXSTA.
C
C$ Particulars
C
C     This suite of routines stores and extrapolates a constant-velocity
C     ephemeris. There are two entry points:
C
C        ZZCVSSTA  {Store ephemeris parameters}
C        ZZCVXSTA  {Extrapolate state}
C
C     The "store" entry point stores parameters that specify a constant-
C     velocity ephemeris. The routine accepts as inputs an initial state
C     vector, a epoch, a center of motion, and a reference frame name.
C
C     The "extrapolate" entry point extrapolates the input state to a
C     given epoch ET via the linear formula
C
C        Final_state =      Initial_position 
C                      +  ( ET - Initial_ET ) * Initial_velocity
C
C     Following extrapolation, the final state is transformed to the
C     output reference frame.
C
C$ Examples
C
C     See usage in CVOSTA, CVTSTA.
C
C$ Restrictions
C
C     1) This routine can store data for only one 
C        constant-velocity ephemeris.
C     
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman    (JPL)
C
C$ Version
C
C-    SPICELIB Version 2.0.0, 11-JAN-2012 (NJB)
C
C-&
 

C
C     SPICELIB functions
C
      LOGICAL               FAILED
      LOGICAL               RETURN

C
C     Local parameters
C      
      INTEGER               FRNMLN
      PARAMETER           ( FRNMLN = 32 )

C
C     Local variables
C     
      CHARACTER*(FRNMLN)    SVREF

      DOUBLE PRECISION      DELTA
      DOUBLE PRECISION      SVET
      DOUBLE PRECISION      SVSTAT ( 6 )
      DOUBLE PRECISION      XSTATE ( 6 )
      DOUBLE PRECISION      XF     ( 6, 6 )

      INTEGER               SVCTR

C
C     Saved variables
C
      SAVE                  SVCTR
      SAVE                  SVET
      SAVE                  SVREF
      SAVE                  SVSTAT

C
C     This routine should never be called.
C
      CALL CHKIN  ( 'ZZCVSTAT'          )
      CALL SIGERR ( 'SPICE(BOGUSENTRY)' )
      CALL CHKOUT ( 'ZZCVSTAT'          )
      RETURN





C$Procedure ZZCVXSTA ( Constant velocity state, fetch state )
 
      ENTRY ZZCVXSTA ( ET, REF, CENTER, STATE )
  
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines. Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Compute target state relative to its center of motion at
C     a specified time, in a specified reference frame.
C    
C$ Disclaimer
C
C     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
C     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
C     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
C     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
C     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
C     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
C     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
C     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
C     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
C     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
C
C     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
C     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
C     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
C     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
C     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
C     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
C
C     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
C     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
C     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
C     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
C
C$ Required_Reading
C
C     FRAMES
C     SPK
C     TIME
C
C$ Keywords
C
C     EPHEMERIS
C
C$ Declarations
C 
C     DOUBLE PRECISION      ET
C     CHARACTER*(*)         REF
C     INTEGER               CENTER
C     DOUBLE PRECISION      STATE ( 6 )
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     ET         I   Observer epoch.
C     REF        I   Inertial reference frame of output state.
C     CENTER     O   ID code of center of motion.
C     STATE      O   State vector.
C
C$ Detailed_Input
C
C     ET          is the ephemeris time, expressed as seconds past
C                 J2000 TDB, at which to evaluate the constant-velocity
C                 ephemeris defined by the latest call to ZZCVSSTA.
C
C     REF         is name of a reference frame with respect to which
C                 the output state vector STATE is to be expressed.
C
C                 Case and leading and trailing blanks in REF are not
C                 significant.
C
C$ Detailed_Output
C
C     CENTER      is the NAIF ID code of the center of motion
C                 associated with the output state vector.
C
C     STATE       is a state vector. The contents of STATE are
C                 compatible with output states returned by SPKEZR: the
C                 first three components of STARG represent the x-, y-
C                 and z-components of the target's position; the last
C                 three components form the corresponding velocity
C                 vector.
C
C                 Units are always km and km/sec.
C
C                 STATE is obtained by applying linear extrapolation
C                 to the state stored by the latest call to ZZCVSSTA.
C                 The extrapolation is performed using the difference
C                 between ET and the epoch stored with the saved state.
C
C                 STATE contains the result of this extrapolation,
C                 expressed in the reference frame designated by the
C                 input REF. This frame's orientation is evaluated at
C                 the epoch ET.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If REF is not recognized, the error will be diagnosed by
C         routines in the call tree of this routine.
C
C     2)  If the loaded kernels provide insufficient data to compute
C         the requested state vector, the deficiency will be diagnosed
C         by a routine in the call tree of this routine.
C
C     3)  If an error occurs while reading a kernel file, the error
C         will be diagnosed by a routine in the call tree of this
C         routine.
C
C$ Files
C
C     Appropriate kernels must be loaded by the calling program before
C     this routine is called.
C
C     The following data may be required to transform the stored state
C     to the output frame REF:
C
C        - PCK data: rotation data will be needed for 
C          for any frame in the chain connecting the frame
C          of the stored state to that of the output state.
C
C        - Frame data: any frame definition required to compute
C          the transformation to the output frame must be known
C          to SPICE. If the definition is not built in, it must
C          be provided via a frame kernel.
C
C        - CK data: if a CK frame is required to compute the
C          transformation to the output frame, at least one CK file
C          will be needed.
C
C        - SCLK data: if a CK file is needed, an associated SCLK
C          kernel is required to enable conversion between encoded SCLK
C          (used to time-tag CK data) and barycentric dynamical time
C          (TDB).
C
C     In all cases, kernel data are normally loaded once per program
C     run, NOT every time this routine is called.
C
C$ Particulars
C
C     This routine returns the state vector stored by the entry point
C
C        ZZCVSSTA
C
C     after transforming the state to the reference frame REF.
C     
C$ Examples
C
C     See usage in CVOSTA, CVTSTA.
C
C$ Restrictions
C
C     1) This routine can compute states based only on the most
C        recently stored state vector.
C     
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman    (JPL)
C
C$ Version
C
C-    SPICELIB Version 2.0.0, 11-JAN-2012 (NJB)
C
C-&

      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'ZZCVXSTA' )

C
C     Extrapolate the saved state to the input time.
C
      DELTA = ET - SVET

      CALL VLCOM ( 1.D0, SVSTAT(1), DELTA, SVSTAT(4), XSTATE )

      CALL VEQU  ( SVSTAT(4), XSTATE(4) )

C
C     Convert the extrapolated state to the requested frame
C     at ET.
C
      CALL SXFORM ( SVREF, REF, ET, XF )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'ZZCVXSTA' )
         RETURN
      END IF

      CALL MXVG ( XF, XSTATE, 6, 6,  STATE )

C
C     Set the output center of motion argument as well.
C
      CENTER = SVCTR

      CALL CHKOUT ( 'ZZCVXSTA' )
      RETURN
 






C$Procedure ZZCVSSTA ( Constant velocity state, store parameters )
 
      ENTRY ZZCVSSTA ( STATE, CENTER, ET, REF )
  
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines. Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Save an object's state and the center, time, and frame associated
C     with the state.
C
C$ Disclaimer
C
C     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
C     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
C     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
C     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
C     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
C     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
C     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
C     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
C     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
C     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
C
C     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
C     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
C     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
C     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
C     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
C     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
C
C     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
C     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
C     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
C     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
C
C$ Required_Reading
C
C     FRAMES
C     SPK
C     TIME
C
C$ Keywords
C
C     EPHEMERIS
C
C$ Declarations
C 
C     DOUBLE PRECISION      STATE ( 6 )
C     INTEGER               CENTER
C     DOUBLE PRECISION      ET
C     CHARACTER*(*)         REF
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     STATE      I   State vector.
C     CENTER     I   ID code of center of motion.
C     ET         I   Observer epoch.
C     REF        I   Inertial reference frame of output state.
C
C$ Detailed_Input
C
C     STATE       is a state vector. The contents of STATE are
C                 compatible with output states returned by SPKEZR.
C
C     CENTER      is the NAIF ID code of the center of motion associated
C                 with the input state vector.
C
C     ET          is the ephemeris time, expressed as seconds past
C                 J2000 TDB, associated with the input state.
C
C     REF         is name of a reference frame with respect to which
C                 the input STATE is expressed. This routine simply
C                 stores REF, so there are no restrictions on the 
C                 contents of this string. However, at the time
C                 the companion entry point ZZCVXSTA is called, the
C                 frame designated by REF must be recognized by the
C                 SPICE system.
C
C$ Detailed_Output
C
C     None. This routine operates by side effects.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine stores its inputs so that they can be
C     used for linear extrapolation by the entry point 
C
C        ZZCVXSTA
C     
C$ Examples
C
C     See usage in CVOSTA, CVTSTA.
C
C$ Restrictions
C
C     1) This routine can store data for only one 
C        constant-velocity ephemeris.
C     
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman    (JPL)
C
C$ Version
C
C-    SPICELIB Version 2.0.0, 11-JAN-2012 (NJB)
C
C-&
 
      IF ( RETURN() ) THEN
         RETURN
      END IF

C
C     No SPICE errors are detected here, so we don't check in.
C      
C
C     Save all inputs.
C
      CALL MOVED ( STATE, 6, SVSTAT )

      SVCTR = CENTER
      SVET  = ET
      SVREF = REF

      RETURN
      END 



