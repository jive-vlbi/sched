C$Procedure SPKPVN ( S/P Kernel, position and velocity in native frame )
 
      SUBROUTINE SPKPVN ( HANDLE, DESCR, ET, REF, STATE, CENTER )
 
C$ Abstract
C
C     Return the state (position and velocity) of a target body
C     relative to some center of motion.
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

      INCLUDE 'spkrec.inc'

      INTEGER               HANDLE
      DOUBLE PRECISION      DESCR    ( 5 )
      DOUBLE PRECISION      ET
      INTEGER               REF
      DOUBLE PRECISION      STATE    ( 6 )
      INTEGER               CENTER
 
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   File handle.
C     DESCR      I   Segment descriptor.
C     ET         I   Target epoch.
C     REF        O   Target reference frame.
C     STATE      O   Position, velocity.
C     CENTER     O   Center of state.
C     MAXREC     P   Maximum length of records returned by SPKRnn.
C
C$ Detailed_Input
C
C     HANDLE,
C     DESCR       are the file handle assigned to a SPK file, and the
C                 descriptor for a segment within the file. Together
C                 they determine the ephemeris data from which the
C                 state of the body is to be computed.
C
C     ET          is the epoch (ephemeris time) at which the state
C                 is to be computed.
C
C$ Detailed_Output
C
C     REF         is the id-code of the reference frame to
C                 which the vectors returned by the routine belong.
C
C     STATE       contains the position and velocity, at epoch ET,
C                 for whatever body is covered by the specified segment.
C                 STATE has six elements:  the first three contain the
C                 body's position; the last three contain the body's
C                 velocity.  These vectors are rotated into the
C                 specified  reference frame, the origin of
C                 which is located at the center of motion for the
C                 body (see CENTER, below).  Units are always km and
C                 km/sec.
C
C     CENTER      is the integer ID code of the center of motion for
C                 the state.
C
C$ Parameters
C
C     MAXREC      is the maximum length of a record returned by any of
C                 data type-specific routines SPKRnn, which are called
C                 by SPKPVN (see Particulars).
C
C$ Exceptions
C
C     1) If the segment type is not supported by the current
C        version of SPKPVN, the error 'SPICE(SPKTYPENOTSUPP)'
C        is signaled.
C
C
C$ Files
C
C     See argument HANDLE.
C
C$ Particulars
C
C     SPKPVN is the most basic of the SPK readers, the reader upon
C     which SPKPV and SPKGEO, etc. are built. It should not normally
C     be called directly except in cases where some optimization is
C     required. (That is, where the calling program has prior knowledge
C     of the center-barycenter shifts to be performed, or a non-standard
C     method of determining the files and segments to be used when
C     computing states.)
C
C     This is the only reader which makes distinctions between the
C     various segment types in the SPK format. The complete list
C     of types currently supported is shown below.
C
C        Type   Description
C        ----   -----------------------
C           1   Difference Lines
C           2   Chebyshev (P)
C           3   Chebyshev (P,V)
C           5   Two body propagation between discrete states
C           8   Lagrange interpolation, equally spaced discrete states
C           9   Lagrange interpolation, unequally spaced discrete states
C          12   Hermite interpolation, equally spaced discrete states
C          13   Hermite interpolation, unequally spaced discrete states
C          14   Chebyshev Unequally spaced
C          15   Precessing Ellipse
C          17   Equinoctial Elements
C          18   ESOC/DDID Hermite/Lagrange Interpolation
C          19   ESOC/DDID Piecewise Interpolation
C          20   Chebyshev (V)
C          21   Extended Modified Difference Array
C
C     SPKPVN is the only reader that needs to be changed in order to
C     add a new segment type to the SPK format.  If a new data type is
C     added, the following steps should be taken:
C
C     1) Write two new routines, SPKRnn and SPKEnn, to read and
C        evaluate, respectively, a record from a data type nn segment.
C
C     2) Insert a new case into the body of SPKPVN to accommodate the
C        new type.
C
C     3) If necessary, adjust the parameter MAXREC, above, so that it
C        is large enough to encompass the maximum size of a record
C        returned by SPKRnn and passed to SPKEnn.
C
C        The maximum record lengths for each data type currently
C        supported are as follows:
C
C                  Data type       Maximum record length
C                  ---------       ---------------------
C                      1                    71
C                      2                    87
C                      3                   171
C                      5                    15
C                      8                   171
C                      9                   197
C                     12                    87
C                     13                    99
C                     14                 Variable
C                     15                    16
C                     17                    12
C                     18                   198
C                     19                   198
C                     20                   159
C                     21                   112
C
C$ Examples
C
C     In the following code fragment, an entire SPK file is searched
C     for segments containing a particular epoch. For each one found,
C     the body, center, segment identifier, and range at the epoch
C     are printed out.
C
C        CALL DAFOPR ( 'TEST.SPK', HANDLE )
C        CALL DAFBFS (             HANDLE )
C
C        CALL DAFFNA ( FOUND  )
C
C        DO WHILE ( FOUND )
C           CALL DAFGS ( DESCR )
C           CALL DAFUS ( DESCR, 2, 6, DC, IC )
C
C           IF ( DC(1) .LE. ET  .AND.  ET .LE. DC(2) ) THEN
C              CALL SPKPVN ( HANDLE, DESCR, ET, REF, STATE, CENTER )
C              CALL DAFGN  ( IDENT )
C              CALL FRMNAM ( REF, FRAME )
C              WRITE (*,*)
C              WRITE (*,*) 'Body   = ', IC(1)
C              WRITE (*,*) 'Center = ', CENTER,
C              WRITE (*,*) 'ID     = ', IDENT
C              WRITE (*,*) 'Frame  = ', FRAME
C              WRITE (*,*) 'Range  = ', VNORM ( STATE )
C           END IF
C
C           CALL DAFFNA ( FOUND )
C        END DO
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
C     W.L. Taber      (JPL)
C
C$ Version
C
C-    SPICELIB Version 4.0.0,  23-DEC-2013 (NJB)
C
C        Added support for types 19, 20 and 21. Added header 
C        comments giving description for types 18, 19,
C        and 21. Removed header reference to type 4.
C
C-    SPICELIB Version 3.0.0,  16-AUG-2002 (NJB)
C
C        Added support for type 18.  This routine now uses the
C        include file spkrec.inc to declare the record size.
C
C        Corrected header comments giving record sizes for types
C        8, 9, 12, 13.
C
C-    SPICELIB Version 2.0.0,  06-NOV-1999 (NJB)
C
C        Added support for types 12 and 13.
C
C-    SPICELIB Version 1.1.0,  7-JAN-1997 (WLT)
C
C        Added support for type 17.
C
C-    SPICELIB Version 1.0.0, 19-SEP-1995 (WLT)
C
C
C-&
 
C$ Index_Entries
C
C     position and velocity from ephemeris
C     spk file position and velocity
C
C-&
 
 
C$ Revisions
C
C-    SPICELIB Version 1.1.0,  7-JAN-1997 (WLT)
C
C        Added support for type 17.
C
C
C-&
 
 
 
 
C
C     SPICELIB functions
C
      LOGICAL               RETURN
      LOGICAL               FAILED
 
C
C     Some local space is needed in which to return records, and
C     into which to unpack the segment descriptor.
C


      DOUBLE PRECISION      RECORD   ( MAXREC )
 
      DOUBLE PRECISION      DC       (      2 )
      INTEGER               IC       (      6 )
      INTEGER               RECSIZ
      INTEGER               TYPE
 
C
C     Local Parameters
C
      INTEGER               NSTATE
      PARAMETER           ( NSTATE = 6 )
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SPKPVN' )
      END IF
 
C
C     Unpacking the segment descriptor will tell us the center,
C     reference frame, and data type for this segment.
C
      CALL DAFUS ( DESCR, 2, 6, DC, IC )
 
      CENTER = IC(2)
      REF    = IC(3)
      TYPE   = IC(4)
 
C
C     Each data type has a pair of routines to read and evaluate
C     records for that data type. These routines are the only ones
C     that actually look inside the segments.
C
C     By the time we have more than 100 data types, we should be
C     allowed to use longer variable names.
C
      IF ( TYPE .EQ. 01 ) THEN
         CALL SPKR01 ( HANDLE, DESCR, ET, RECORD         )
         CALL SPKE01 (                ET, RECORD, STATE  )
 
      ELSE IF ( TYPE .EQ. 02 ) THEN
         CALL SPKR02 ( HANDLE, DESCR, ET, RECORD         )
         CALL SPKE02 (                ET, RECORD, STATE  )
 
      ELSE IF ( TYPE .EQ. 03 ) THEN
         CALL SPKR03 ( HANDLE, DESCR, ET, RECORD         )
         CALL SPKE03 (                ET, RECORD, STATE  )
 
 
C
C     Type 04 is not officially part of the library.
C
C     ELSE IF ( TYPE .EQ. 04 ) THEN
C         CALL SPKR04 ( HANDLE, DESCR, ET, RECORD         )
C         CALL SPKE04 (                ET, RECORD, STATE  )
 
      ELSE IF ( TYPE .EQ. 05 ) THEN
         CALL SPKR05 ( HANDLE, DESCR, ET, RECORD         )
         CALL SPKE05 (                ET, RECORD, STATE  )
 
      ELSE IF ( TYPE .EQ. 08 ) THEN
         CALL SPKR08 ( HANDLE, DESCR, ET, RECORD         )
         CALL SPKE08 (                ET, RECORD, STATE  )
 
      ELSE IF ( TYPE .EQ. 09 ) THEN
         CALL SPKR09 ( HANDLE, DESCR, ET, RECORD         )
         CALL SPKE09 (                ET, RECORD, STATE  )
 
      ELSE IF ( TYPE .EQ. 10 ) THEN
         CALL SPKR10 ( HANDLE, DESCR, ET, RECORD         )
         CALL SPKE10 (                ET, RECORD, STATE  )
 
      ELSE IF ( TYPE .EQ. 12 ) THEN
         CALL SPKR12 ( HANDLE, DESCR, ET, RECORD         )
         CALL SPKE12 (                ET, RECORD, STATE  )
 
      ELSE IF ( TYPE .EQ. 13 ) THEN
         CALL SPKR13 ( HANDLE, DESCR, ET, RECORD         )
         CALL SPKE13 (                ET, RECORD, STATE  )
 
      ELSE IF ( TYPE .EQ. 14 ) THEN
C
C        Fetch the number of Chebyshev coefficients, compute the record
C        size needed, and signal an error if there is not enough storage
C        in RECORD. The number of coefficients is the first constant
C        value in the generic segment.
C
         CALL SGFCON ( HANDLE, DESCR, 1, 1, RECORD(1) )
 
         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'SPKPVN' )
            RETURN
         END IF
 
         RECSIZ = NSTATE * INT(RECORD(1)) + 3
 
         IF ( RECSIZ .GT. MAXREC ) THEN
 
            CALL SETMSG ( 'Storage for # double precision numbers'
     .      //            ' is needed for an SPK data record and'
     .      //            ' only # locations were available. Update'
     .      //            ' the parameter MAXREC in the subroutine'
     .      //            ' SPKPVN and notify the NAIF group of this'
     .      //            ' problem.'                                )
            CALL ERRINT ( '#', RECSIZ                                )
            CALL ERRINT ( '#', MAXREC                                )
            CALL SIGERR ( 'SPICE(SPKRECTOOLARGE)'                    )
            CALL CHKOUT ( 'SPKPVN'                                    )
            RETURN
 
         END IF
 
         CALL SPKR14 ( HANDLE, DESCR, ET, RECORD         )
         CALL SPKE14 (                ET, RECORD, STATE  )
 
      ELSE IF ( TYPE .EQ. 15 ) THEN
         CALL SPKR15 ( HANDLE, DESCR, ET, RECORD         )
         CALL SPKE15 (                ET, RECORD, STATE  )
 
      ELSE IF ( TYPE .EQ. 17 ) THEN
         CALL SPKR17 ( HANDLE, DESCR, ET, RECORD         )
         CALL SPKE17 (                ET, RECORD, STATE  )
 
      ELSE IF ( TYPE .EQ. 18 ) THEN
         CALL SPKR18 ( HANDLE, DESCR, ET, RECORD         )
         CALL SPKE18 (                ET, RECORD, STATE  )
 
      ELSE IF ( TYPE .EQ. 19) THEN
         CALL SPKR19 ( HANDLE, DESCR, ET, RECORD         )         
         CALL SPKE19 (                ET, RECORD, STATE  )

      ELSE IF ( TYPE .EQ. 20) THEN
         CALL SPKR20 ( HANDLE, DESCR, ET, RECORD         )         
         CALL SPKE20 (                ET, RECORD, STATE  )

      ELSE IF ( TYPE .EQ. 21) THEN
         CALL SPKR21 ( HANDLE, DESCR, ET, RECORD         )         
         CALL SPKE21 (                ET, RECORD, STATE  )
 
      ELSE
         CALL SETMSG ( 'SPK type # is not supported in your '
     .   //            'version of the SPICE library.  You will '
     .   //            'need to upgrade your version of the '
     .   //            'library to make use of ephemerides that '
     .   //            'contain this SPK data type. ' )
         CALL ERRINT ( '#', TYPE               )
         CALL SIGERR ( 'SPICE(SPKTYPENOTSUPP)' )
         CALL CHKOUT ( 'SPKPVN'                 )
         RETURN
      END IF
 
 
      CALL CHKOUT ( 'SPKPVN' )
      RETURN
      END
