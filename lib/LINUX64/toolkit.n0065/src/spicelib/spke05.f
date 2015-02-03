C$Procedure SPKE05 ( Evaluate SPK record, type 5 )
 
      SUBROUTINE SPKE05 ( ET, RECORD, STATE )
 
C$ Abstract
C
C     Evaluate a single SPK data record from a segment of type 5
C     (two body propagation between discrete state vectors).
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
 
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      RECORD   ( * )
      DOUBLE PRECISION      STATE    ( 6 )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     ET         I   Target epoch.
C     RECORD     I   Data record.
C     STATE      O   State (position and velocity).
C
C$ Detailed_Input
C
C     ET          is a target epoch, specified as ephemeris seconds past
C                 J2000, at which a state vector is to be computed.
C
C     RECORD      is a data record which, when evaluated at epoch ET,
C                 will give the state (position and velocity) of some
C                 body, relative to some center, in some inertial
C                 reference frame.
C
C                 The structure of RECORD is:
C
C                     RECORD(1)
C                        .            state of the body at epoch 1.
C                        .
C                        .
C                     RECORD(6)
C
C                     RECORD(7)
C                        .
C                        .            state of the body at epoch 2.
C                        .
C                     RECORD(12)
C                     RECORD(13)      epoch 1 in seconds past 2000.
C                     RECORD(14)      epoch 2 in seconds past 2000.
C                     RECORD(15)      GM for the center of motion.
C
C                 Epoch 1 and epoch 2 are the times in the segment that
C                 bracket ET.  If ET is less than the first time in the
C                 segment then both epochs 1 and 2 are equal to the
C                 first time.  And if ET is greater than the last time
C                 then, epochs 1 and 2 are set equal to this last time.
C
C$ Detailed_Output
C
C     STATE       is the state produced by evaluating RECORD at ET.
C                 Units are km and km/sec.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If there is a problem propagating, subject to the laws of two
C        body motion, either of the states from RECORD to the requested
C        time ET, an error will be signalled by the routine PROP2B.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine interpolates a state from the two reference states
C     contained in RECORD.
C
C     It is assumed that this routine is used in conjunction with
C     the routine SPKR05 as shown here:
C
C        CALL SPKR05 ( HANDLE, DESCR, ET, RECORD         )
C        CALL SPKE05 (                ET, RECORD, STATE  )
C
C     Where it is known in advance that the HANDLE, DESCR pair points
C     to a type 05 data segment.
C
C$ Examples
C
C     The SPKEnn routines are almost always used in conjunction with
C     the corresponding SPKRnn routines, which read the records from
C     SPK files.
C
C     The data returned by the SPKRnn routine is in its rawest form,
C     taken directly from the segment.  As such, it will be meaningless
C     to a user unless he/she understands the structure of the data type
C     completely.  Given that understanding, however, the SPKRnn
C     routines might be used to examine raw segment data before
C     evaluating it with the SPKEnn routines.
C
C
C     C
C     C     Get a segment applicable to a specified body and epoch.
C     C
C           CALL SPKSFS ( BODY, ET, HANDLE, DESCR, IDENT, FOUND )
C
C     C
C     C     Look at parts of the descriptor.
C     C
C           CALL DAFUS ( DESCR, 2, 6, DCD, ICD )
C           CENTER = ICD( 2 )
C           REF    = ICD( 3 )
C           TYPE   = ICD( 4 )
C
C           IF ( TYPE .EQ. 5 ) THEN
C
C              CALL SPKR05 ( HANDLE, DESCR, ET, RECORD )
C                  .
C                  .  Look at the RECORD data.
C                  .
C              CALL SPKE05 ( ET, RECORD, STATE )
C                  .
C                  .  Check out the evaluated state.
C                  .
C           END IF
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman    (JPL)
C     K.R. Gehringer  (JPL)
C     J.M. Lynch      (JPL)
C     W.L. Taber      (JPL)
C     I.M. Underwood  (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.2.0, 31-AUG-2005 (NJB)
C
C        Updated to remove non-standard use of duplicate arguments
C        in VADD call.
C
C-    SPICELIB Version 1.1.0, 29-FEB-1996 (KRG)
C
C        The declaration for the SPICELIB function PI is now
C        preceded by an EXTERNAL statement declaring PI to be an 
C        external function. This removes a conflict with any
C        compilers that have a PI intrinsic function.
C
C-    SPICELIB Version 1.0.0, 01-APR-1992 (JML) (WLT) (IMU)
C
C-&
 
C$ Index_Entries
C
C     evaluate type_5 spk segment
C
C-&

C$ Revisions
C
C-    SPICELIB Version 1.2.0, 31-AUG-2005 (NJB)
C
C        Updated to remove non-standard use of duplicate arguments
C        in VADD call.
C
C-    SPICELIB Version 1.1.0, 29-FEB-1996 (KRG)
C
C        The declaration for the SPICELIB function PI is now
C        preceded by an EXTERNAL statement declaring PI to be an 
C        external function. This removes a conflict with any
C        compilers that have a PI intrinsic function.
C
C-    SPICELIB Version 1.0.0, 01-APR-1992 (JML) (WLT) (IMU)
C
C-&

 
C
C     SPICELIB functions
C
      EXTERNAL              PI
      DOUBLE PRECISION      PI

      LOGICAL               RETURN
 
C
C     Local variables
C
      DOUBLE PRECISION      ARG
      DOUBLE PRECISION      DARGDT
      DOUBLE PRECISION      DENOM
      DOUBLE PRECISION      DWDT
      DOUBLE PRECISION      GM
      DOUBLE PRECISION      NUMER
      DOUBLE PRECISION      PV       ( 6, 2 )
      DOUBLE PRECISION      S1       ( 6 )
      DOUBLE PRECISION      S2       ( 6 )
      DOUBLE PRECISION      T1
      DOUBLE PRECISION      T2
      DOUBLE PRECISION      VCOMP    ( 3 )
      DOUBLE PRECISION      VEL      ( 3 )
      DOUBLE PRECISION      W
 

C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SPKE05' )
      END IF
 
C
C     Unpack the record, for easier reading.
C
      CALL MOVED ( RECORD, 12, PV )
 
      T1 = RECORD(13)
      T2 = RECORD(14)
      GM = RECORD(15)
 
C
C     Evaluate the two states. Call them s_1(t) and s_2(t).
C     Let the position and velocity components be: p_1, v_1, p_2, v_2.
C
C     The final position is a weighted average.
C
C     Let
C
C        W(t) =  0.5 + 0.5*COS( PI*(t-t1)/(t2-t1) )
C
C     then
C
C        p  = W(t)*p_1(t) + (1 - W(t))*p_2(t)
C        v  = W(t)*v_1(t) + (1 - W(t))*v_2(t) + W'(t)*(p_1(t) - p_2(t))
C
C     If t1 = t2, the state is just s(t1).
C
C
C     Note: there are a number of weighting schemes we could have
C     used.  This one has the nice property that
C
C     The graph of W is symmetric about the point
C
C
C        ( (t1+t2)/2,  W( (t1+t2)/2 )
C
C     The range of W is from 1 to 0.  And the derivative of W is
C     symmetric and zero at both t1 and t2.
C
C
      IF ( T1 .NE. T2 ) THEN
 
         CALL PROP2B ( GM, PV(1,1), ET - T1, S1 )
         CALL PROP2B ( GM, PV(1,2), ET - T2, S2 )
 
         NUMER  = ET - T1
         DENOM  = T2 - T1
         ARG    = NUMER*PI()/DENOM
         DARGDT =       PI()/DENOM
 
         W      =  0.5D0 + 0.5D0 * COS( ARG )
         DWDT   =        - 0.5D0 * SIN( ARG ) * DARGDT
 
         CALL VLCOMG ( 6,  W,    S1,       1.D0-W, S2,     STATE    )
         CALL VLCOM  (     DWDT, S1,       -DWDT,  S2,     VCOMP    )
         CALL VADD   (           STATE(4),         VCOMP,  VEL      )
         CALL VEQU   (           VEL,                      STATE(4) )

      ELSE
 
         CALL PROP2B ( GM, PV(1,1), ET - T1, STATE )
 
      END IF
 
      CALL CHKOUT ( 'SPKE05' )
      RETURN
      END
