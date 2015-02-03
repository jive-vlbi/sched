C$Procedure      SPKE08 ( S/P Kernel, evaluate, type 8 )
 
      SUBROUTINE SPKE08 ( ET, RECORD, STATE )
 
C$ Abstract
C
C     Evaluate a single SPK data record from a segment of type 8
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

      IMPLICIT NONE

      INCLUDE 'spkrec.inc'
 
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      RECORD ( * )
      DOUBLE PRECISION      STATE  ( 6 )
 
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
C     ET          is a target epoch, at which a state vector is to
C                 be computed.
C
C     RECORD      is a data record which, when evaluated at epoch ET,
C                 will give the state (position and velocity) of some
C                 body, relative to some center, in some inertial
C                 reference frame.  Normally, the caller of this routine
C                 will obtain RECORD by calling SPKR08.
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
C$ Detailed_Output
C
C     STATE       is the state. In order, the elements are
C
C                    X, Y, Z, X', Y', and Z'
C
C                 Units are km and km/sec.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  The caller of this routine must ensure that the input record
C         is appropriate for the supplied ET value. Otherwise,
C         arithmetic overflow may result.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     The exact format and structure of type 8 (equally spaced discrete
C     states, interpolated by Lagrange polynomials) segments are
C     described in the SPK Required Reading file.
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
C           IF ( TYPE .EQ. 8 ) THEN
C
C              CALL SPKR08 ( HANDLE, DESCR, ET, RECORD )
C                  .
C                  .  Look at the RECORD data.
C                  .
C              CALL SPKE08 ( ET, RECORD, STATE )
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
C     N.J. Bachman   (JPL)
C
C$ Version
C
C-    SPICELIB Version 2.0.0, 10-DEC-2013 (NJB)
C
C        RECORD is now strictly an input; it is not overwritten by this
C        routine. Formerly RECORD was used as a workspace array.
C
C-    SPICELIB Version 1.1.0, 25-AUG-2005 (NJB)
C
C        Updated to remove non-standard use of duplicate arguments
C        in XPOSEG and LGRESP calls.
C
C-    SPICELIB Version 1.0.0, 14-AUG-1993 (NJB)
C
C-&

C$ Index_Entries
C
C     evaluate type_8 spk segment
C
C-&

C$ Revisions
C
C-    SPICELIB Version 1.1.0, 25-AUG-2005 (NJB)
C
C        Updated to remove non-standard use of duplicate arguments
C        in XPOSEG and LGRESP calls.
C
C-& 


 
C
C     SPICELIB functions
C
      DOUBLE PRECISION      LGRESP
      LOGICAL               RETURN
 
C
C     Local parameters
C
C
C     Size of a state vector:
C
      INTEGER               STATSZ
      PARAMETER           ( STATSZ = 6 )
 
C
C     Indices of input record elements:
C
C        -- size
C        -- start epoch
C        -- step size
C        -- start of state information
C
      INTEGER               SIZIDX
      PARAMETER           ( SIZIDX = 1 )
 
      INTEGER               BEGIDX
      PARAMETER           ( BEGIDX = 2 )
 
      INTEGER               STPIDX
      PARAMETER           ( STPIDX = 3 )
 
      INTEGER               STAIDX
      PARAMETER           ( STAIDX = 4 )
 
C
C     Local variables
C
      DOUBLE PRECISION      LOCREC ( MAXREC )
      DOUBLE PRECISION      WORK   ( MAXREC )

      INTEGER               I
      INTEGER               N
      INTEGER               YSTART

C
C     Saved values
C
C     Save arrays to prevent stack overflow problems on some
C     platforms.
C
      SAVE                  LOCREC
      SAVE                  WORK

C
C     Initial values
C
C
C     Initialize the workspace array to suppress compiler warnings.
C
      DATA                  WORK / MAXREC * 0.D0 /
 
C
C     Use discovery check-in.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF
 
C
C     We'll transpose the state information in the input record
C     so that contiguous pieces of it can be shoved directly into the
C     interpolation routine LGRESP. 
C
      N   =  NINT( RECORD(SIZIDX) )
 
      CALL XPOSEG ( RECORD(STAIDX), STATSZ, N, LOCREC )

C
C     We interpolate each state component in turn.
C
      DO I = 1, STATSZ
 
         YSTART    =   1  +  ( (I-1) * N )
 
         STATE(I)  =   LGRESP ( N,
     .                          RECORD(BEGIDX),
     .                          RECORD(STPIDX),
     .                          LOCREC(YSTART),
     .                          WORK,
     .                          ET              )
      END DO
 
      RETURN
      END
