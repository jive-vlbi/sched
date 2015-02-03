C$Procedure      SPKR08 ( Read SPK record from segment, type 8 )
 
      SUBROUTINE SPKR08 ( HANDLE, DESCR, ET, RECORD )
      IMPLICIT NONE
 
C$ Abstract
C
C     Read a single SPK data record from a segment of type 8
C     (equally spaced discrete states, interpolated by Lagrange
C     polynomials).
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
C     TIME
C
C$ Keywords
C
C     EPHEMERIS
C
C$ Declarations
 
      INTEGER               HANDLE
      DOUBLE PRECISION      DESCR    ( 5 )
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      RECORD   ( * )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   File handle.
C     DESCR      I   Segment descriptor.
C     ET         I   Target epoch.
C     RECORD     O   Data record.
C
C$ Detailed_Input
C
C     HANDLE,
C     DESCR       are the file handle and segment descriptor for
C                 a SPK segment of type 8.
C
C     ET          is a target epoch, for which a data record from
C                 a specific segment is required.
C
C$ Detailed_Output
C
C     RECORD      is a set of data from the specified segment which,
C                 when evaluated at epoch ET, will give the state
C                 (position and velocity) of some body, relative
C                 to some center, in some inertial reference frame.
C
C                 The structure of the record is as follows:
C
C                    +----------------------+
C                    | number of states (n) |
C                    +----------------------+
C                    | start epoch          |
C                    +----------------------+
C                    | step size            |
C                    +----------------------+
C                    | state 1 (6 elts.)    |
C                    +----------------------+
C                    | state 2 (6 elts.)    |
C                    +----------------------+
C                                .
C                                .
C                                .
C                    +----------------------+
C                    | state n (6 elts.)    |
C                    +----------------------+
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     This routine follows the pattern established in the lower-numbered
C     SPK data type readers of not explicitly performing error
C     diagnoses.  Exceptions are listed below nonetheless.
C
C     1)  If the input HANDLE does not designate a loaded SPK file, the
C         error will be diagnosed by routines called by this routine.
C
C     2) If the segment specified by DESCR is not of data type 08,
C        the error 'SPICE(WRONGSPKTYPE)' is signalled.
C
C     3)  If the input ET value is not within the range specified
C         in the segment descriptor, the error SPICE(TIMEOUTOFBOUNDS)
C         is signalled.
C
C$ Files
C
C     See argument HANDLE.
C
C$ Particulars
C
C     See the SPK Required Reading file for a description of the
C     structure of a data type 8 segment.
C
C$ Examples
C
C     The data returned by the SPKRnn routine is in its rawest form,
C     taken directly from the segment.  As such, it will be meaningless
C     to a user unless he/she understands the structure of the data type
C     completely.  Given that understanding, however, the SPKRxx
C     routines might be used to "dump" and check segment data for a
C     particular epoch.
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
C           IF ( TYPE .EQ. 8 ) THEN
C              CALL SPKR08 ( HANDLE, DESCR, ET, RECORD )
C                  .
C                  .  Look at the RECORD data.
C                  .
C           END IF
C
C$ Restrictions
C
C     1)  Correctness of inputs must be ensured by the caller of
C         this routine.
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
C-    SPICELIB Version 2.1.0, 07-SEP-2001 (EDW)
C
C        Replaced DAFRDA call with DAFGDA.
C        Added IMPLICIT NONE.
C
C-    SPICELIB Version 2.0.0, 06-NOV-1999 (NJB)
C
C        Data type check was relaxed to enable reading type 12
C        segments.
C
C-    SPICELIB Version 1.0.1, 24-OCT-1994 (NJB)
C
C        In-line comment concerning transpose of state data was
C        removed.
C
C-    SPICELIB Version 1.0.0, 14-AUG-1993 (NJB)
C
C-&
 
 
C$ Index_Entries
C
C     read record from type_8 spk segment
C
C-&
 
C$ Revisions
C
C-&
 
 
C
C     SPICELIB functions
C
      LOGICAL               ODD
      LOGICAL               RETURN
 
C
C     Local parameters
C
      INTEGER               ND
      PARAMETER           ( ND     = 2 )
 
      INTEGER               NI
      PARAMETER           ( NI     = 6 )
 
      INTEGER               STATSZ
      PARAMETER           ( STATSZ = 6 )
 
      INTEGER               CTRLSZ
      PARAMETER           ( CTRLSZ = 4 )
 
      INTEGER               STAIDX
      PARAMETER           ( STAIDX = 4 )
 
C
C     Local variables
C
      DOUBLE PRECISION      CONTRL ( CTRLSZ  )
      DOUBLE PRECISION      DC     ( ND )
      DOUBLE PRECISION      START
      DOUBLE PRECISION      STEP
 
      INTEGER               BEGIN
      INTEGER               DEGREE
      INTEGER               END
      INTEGER               FIRST
      INTEGER               GRPSIZ
      INTEGER               IC     ( NI )
      INTEGER               LAST
      INTEGER               LOW
      INTEGER               N
      INTEGER               NEAR
      INTEGER               TYPE
 
C
C     Use discovery check-in.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF
 
 
C
C     Unpack the segment descriptor, and get the start and end addresses
C     of the segment.
C
      CALL DAFUS ( DESCR, 2, 6, DC, IC )
 
      TYPE  = IC( 4 )
      BEGIN = IC( 5 )
      END   = IC( 6 )
 
C
C     Make sure that this really is a type 8 or type 12 data segment.
C
      IF (  ( TYPE .NE. 08 ) .AND. ( TYPE .NE. 12 )  ) THEN
         CALL CHKIN ( 'SPKR08'                                        )
         CALL SETMSG( 'You are attempting to locate type 8 or type ' //
     .                '12 data in a type # data segment.'             )
         CALL ERRINT( '#',  TYPE                                      )
         CALL SIGERR( 'SPICE(WRONGSPKTYPE)'                           )
         CALL CHKOUT( 'SPKR08'                                        )
         RETURN
      END IF
 
C
C     Check the request time against the bounds in the segment
C     descriptor.
C
      IF (  ( ET .LT. DC(1) )  .OR.  ( ET .GT. DC(2) )  )  THEN
         CALL CHKIN ( 'SPKR08'                                      )
         CALL SETMSG( 'Request time # is outside of descriptor '   //
     .                'bounds # : #.'                               )
         CALL ERRDP ( '#',  ET                                      )
         CALL ERRDP ( '#',  DC(1)                                   )
         CALL ERRDP ( '#',  DC(2)                                   )
         CALL SIGERR( 'SPICE(TIMEOUTOFBOUNDS)'                      )
         CALL CHKOUT( 'SPKR08'                                      )
         RETURN
 
      END IF
 
C
C     The type 8 segment structure is described by this diagram from
C     the SPK Required Reading:
C
C        +-----------------------+
C        | State 1               |
C        +-----------------------+
C        | State 2               |
C        +-----------------------+
C                    .
C                    .
C                    .
C        +-----------------------+
C        | State N               |
C        +-----------------------+
C        | Epoch of state 1 (ET) |
C        +-----------------------+
C        | Step size             |
C        +-----------------------+
C        | Polynomial degree     |
C        +-----------------------+
C        | Number of states      |
C        +-----------------------+
C
C
C     We'll need the last four items before we can determine which
C     states make up our output record.
C
C
      CALL DAFGDA ( HANDLE, END-3, END, CONTRL )
 
      START   =  CONTRL(1)
      STEP    =  CONTRL(2)
      DEGREE  =  NINT ( CONTRL(3) )
      N       =  NINT ( CONTRL(4) )
 
      GRPSIZ  =  DEGREE + 1
 
C
C     We'll now select the set of states that define the interpolating
C     polynomials.  The cases of odd and even GRPSIZ are handled
C     separately.
C
      IF ( ODD(GRPSIZ) ) THEN
C
C        Find the index of the state whose epoch is closest to the
C        input epoch.  Find the first and last indices in the record
C        of the (GRPSIZ-1)/2 states on either side of this central
C        state.
C
         NEAR   =  NINT ( ( ET - START ) / STEP )  +  1
 
         FIRST  =  MIN (  MAX( 1,  NEAR - (DEGREE/2) ),  N-DEGREE )
         LAST   =  FIRST  +  DEGREE
 
 
      ELSE
C
C        Find the index of the last state whose epoch is less than or
C        equal to that of the input epoch.  Find the first and last
C        indices in the record of the set of GRPSIZ consecutive states
C        having this state as the (GRPSIZ/2)th one.
C
         LOW    =  INT ( ( ET - START ) / STEP )  +  1
 
         FIRST  =  MIN (  MAX( 1,  LOW - DEGREE/2 ),  N-DEGREE )
         LAST   =  FIRST  +  DEGREE
 
      END IF
 
C
C     Put the size of the group of states, the epoch of the first
C     state in the record, and the step size into the output record.
C
      RECORD(1) =  GRPSIZ
      RECORD(2) =  START  +  (FIRST-1)*STEP
      RECORD(3) =  STEP
 
C
C     Read the states.
C
      CALL DAFGDA ( HANDLE,
     .              BEGIN + (FIRST-1)*STATSZ,
     .              BEGIN +      LAST*STATSZ  -  1,
     .              RECORD(STAIDX)                  )
 
      RETURN
      END
