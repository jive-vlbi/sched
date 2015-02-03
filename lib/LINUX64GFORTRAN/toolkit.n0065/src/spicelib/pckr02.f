C$Procedure PCKR02 ( PCK, read record from type 2 segment )

      SUBROUTINE PCKR02 ( HANDLE, DESCR, ET, RECORD )
      IMPLICIT NONE

C$ Abstract
C
C     Read a single PCK data record from a segment of type 2
C     (Chebyshev, 3-vector only).
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
C     PCK
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
C                 a PCK segment of type 2.
C
C     ET          is a target epoch, for which a data record from
C                 a specific segment is required.
C
C$ Detailed_Output
C
C     RECORD      is the record from the specified segment which,
C                 when evaluated at epoch ET, will give the Euler
C                 angles (orientation) of some body.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     None.
C
C$ Files
C
C     See argument HANDLE.
C
C$ Particulars
C
C     See the PCK Required Reading file for a description of the
C     structure of a data type 2 (Chebyshev polynomials, Euler
C     angles only) segment.
C
C$ Examples
C
C     The data returned  is in its rawest form, taken directly from
C     the segment.  As such, it will be meaningless to a user unless
C     he/she understands the structure of the data type completely.
C     Given that understanding, however, the PCKRxx routines might be
C     used to "dump" and check segment data for a particular epoch.
C
C
C     C
C     C     Get a segment applicable to a specified body and epoch.
C     C
C           CALL PCKSFS ( BODY, ET, HANDLE, DESCR, IDENT, FOUND )
C
C     C
C     C     Look at parts of the descriptor.
C     C
C           CALL DAFUS ( DESCR, ND, NI, DCD, ICD )
C           REF    = ICD( NR )
C           TYPE   = ICD( NT )
C
C           IF ( TYPE .EQ. 2 ) THEN
C              CALL PCKR02 ( HANDLE, DESCR, ET, RECORD )
C                  .
C                  .  Look at the RECORD data.
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
C     K.S. Zukor  (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.1.1, 03-JAN-2014 (EDW)
C
C        Minor edits to Procedure; clean trailing whitespace.
C
C-    SPICELIB Version 1.1.0, 07-SEP-2001 (EDW)
C
C        Replaced DAFRDA call with DAFGDA.
C        Added IMPLICIT NONE.
C
C-    SPICELIB Version 1.0.0, 11-MAR-1993 (KSZ)
C
C-&

C$ Index_Entries
C
C     read record from type_2 pck segment
C
C-&



C
C     SPICELIB functions
C
      LOGICAL               RETURN

C
C     Parameters
C
      INTEGER               ND
      PARAMETER           ( ND      =   2 )

      INTEGER               NI
      PARAMETER           ( NI     =    5 )

C
C     Local variables
C

      DOUBLE PRECISION      DC       (   ND )
      DOUBLE PRECISION      INIT
      DOUBLE PRECISION      INTLEN

      INTEGER               BEGIN
      INTEGER               END
      INTEGER               IC       (   NI )
      INTEGER               NREC
      INTEGER               RECADR
      INTEGER               RECNO
      INTEGER               RECSIZ


C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'PCKR02' )
      END IF

C
C     Unpack the segment descriptor.
C
      CALL DAFUS ( DESCR, ND, NI, DC, IC )

      BEGIN = IC( NI-1 )
      END   = IC( NI )

C
C     The segment is made up of a number of logical records, each
C     having the same size, and covering the same length of time.
C
C     We can determine which record to return by comparing the input
C     epoch with the initial time of the segment and the length of the
C     interval covered by each record.  These final two constants are
C     located at the end of the segment, along with the size of each
C     logical record and the total number of records.
C
      CALL DAFGDA ( HANDLE, END-3, END, RECORD )

      INIT   = RECORD( 1 )
      INTLEN = RECORD( 2 )
      RECSIZ = INT( RECORD( 3 ) )
      NREC   = INT( RECORD( 4 ) )

      RECNO = INT( (ET - INIT) / INTLEN ) + 1
      RECNO = MIN( RECNO, NREC )

C
C     Compute the address of the desired record.
C
      RECADR = ( RECNO - 1 )*RECSIZ + BEGIN

C
C     Along with the record, return the size of the record.
C
      RECORD( 1 ) = RECORD( 3 )
      CALL DAFGDA ( HANDLE, RECADR, RECADR + RECSIZ - 1, RECORD( 2 ) )

      CALL CHKOUT ( 'PCKR02' )
      RETURN
      END
