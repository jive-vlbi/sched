C$Procedure      SPKE13 ( S/P Kernel, evaluate, type 13 )
 
      SUBROUTINE SPKE13 ( ET, RECORD, STATE )
 
C$ Abstract
C
C     Evaluate a single data record from a type 13 SPK segment.
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
      
      INTEGER               MAXREC
      PARAMETER           ( MAXREC = 129 )
 
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      RECORD   ( * )
      DOUBLE PRECISION      STATE    ( 6 )

C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     MAXREC     P   Maximum size of SPK record.  See SPKPVN.
C     ET         I   Epoch for which a state is desired.
C     RECORD     I   Record from a type 13 SPK segment valid for ET.
C     STATE      O   State (position and velocity) at epoch ET.
C
C$ Detailed_Input
C
C     ET             is the epoch for which a state vector is desired.
C
C     RECORD         is a record from a type 13 SPK segment which, when
C                    evaluated at epoch ET, will give the state
C                    (position and velocity) of some body, relative to
C                    some center, in some inertial reference frame.
C
C                    The structure of the record is as follows:
C
C                       +----------------------+
C                       | number of states (n) |
C                       +----------------------+
C                       | state 1 (6 elts.)    |
C                       +----------------------+
C                       | state 2 (6 elts.)    |
C                       +----------------------+
C                                   .
C                                   .
C                                   .
C                       +----------------------+
C                       | state n (6 elts.)    |
C                       +----------------------+
C                       | epochs 1--n          |
C                       +----------------------+
C
C$ Detailed_Output
C
C     STATE    is the state vector at epoch ET. Its contents are, in
C              order, X, Y, Z, X', Y', and Z'. Units are km and km/sec.
C
C$ Parameters
C
C     MAXREC   is the maximum size of SPK record.  See the SPICELIB 
C              routine SPKPVN for details.
C
C$ Exceptions
C
C     None.  This routine assumes that the input record is valid.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     The exact format and structure of type 13 (unequally spaced
C     discrete states, evaluated by Hermite interpolation) SPK segments
C     is described in the SPK Required Reading.
C
C$ Examples
C
C     The SPKEnn routines are almost always used in conjunction with
C     the corresponding SPKRnn routines, which read the records from
C     SPK files.
C
C     The data returned by the SPKRnn routine is in a raw form, taken
C     directly from the segment.  As such, it will be not be directly
C     useful to a user unless they have a complete understanding of the
C     structure of the data type.  Given that understanding, however,
C     the SPKRnn routines could be used to "dump" and check segment data
C     for a particular epoch before evaluating the record to obtain a
C     state vector, as in the example which follows.
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
C
C           CENTER = ICD( 2 )
C           REF    = ICD( 3 )
C           TYPE   = ICD( 4 )
C
C           IF ( TYPE .EQ. 13 ) THEN
C
C              CALL SPKR13 ( HANDLE, DESCR, ET, RECORD )
C                  .
C                  .  Look at the RECORD data.
C                  .
C              CALL SPKE13 ( ET, RECORD, STATE )
C                  .
C                  .  Check out the evaluated state.
C                  .
C           END IF
C
C$ Restrictions
C
C     1)  This routine assumes that the input record is valid.  Any
C         checking of the input data is assumed to have been performed
C         when the source SPK file was created.
C
C$ Literature_References
C
C     NAIF Document 168.0, "S- and P- Kernel (SPK) Specification and
C     User's Guide"
C
C$ Author_and_Institution
C
C     N.J. Bachman   (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 25-FEB-2000 (NJB)
C
C-&


C$ Index_Entries
C
C     evaluate type_13 spk segment
C
C-&
 
C
C     SPICELIB functions
C
      LOGICAL               RETURN
 
C
C     Local parameters
C
      INTEGER               SIZIDX
      PARAMETER           ( SIZIDX = 1 )
 
C
C     Local variables
C
      DOUBLE PRECISION      LOCREC ( MAXREC )
      DOUBLE PRECISION      WORK   ( MAXREC * 2,  2 )
      
      INTEGER               FROM
      INTEGER               I
      INTEGER               J
      INTEGER               N
      INTEGER               TO
      INTEGER               XSTART

      
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF
      
      CALL CHKIN ( 'SPKE13' )
 
 
      N =  NINT( RECORD(SIZIDX) )
  
C
C     We interpolate each state component in turn.
C
      XSTART   =   2  +  6 * N 
 
      DO I = 1, 3
 
         DO J = 1, N
C
C           For the Jth input state vector, copy the Ith position and
C           velocity components into the local record buffer LOCREC.
C
            FROM         = 1 + 6*(J-1) + I
            TO           =     2* J    - 1
            
            LOCREC(TO  ) = RECORD ( FROM     )
            LOCREC(TO+1) = RECORD ( FROM + 3 )
            
         END DO

C
C        Interpolate the Ith position and velocity components of the
C        state.
C        
         CALL HRMINT ( N, 
     .                 RECORD(XSTART),
     .                 LOCREC,
     .                 ET,           
     .                 WORK,
     .                 STATE(I  ),
     .                 STATE(I+3)      ) 

      END DO
 
 
      CALL CHKOUT ( 'SPKE13' )
      RETURN
      END
 
