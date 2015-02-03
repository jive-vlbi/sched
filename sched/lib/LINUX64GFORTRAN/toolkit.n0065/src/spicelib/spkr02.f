C$Procedure  SPKR02 ( SPK, read record from segment, type 2 )
 
      SUBROUTINE SPKR02 ( HANDLE, DESCR, ET, RECORD )
      IMPLICIT NONE
 
C$ Abstract
C
C     Read a single SPK data record from a segment of type 2
C     (Chebyshev, position only).
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
C     ET         I   Evaluation epoch.
C     RECORD     O   Data record.
C
C$ Detailed_Input
C
C     HANDLE,
C     DESCR       are the file handle and segment descriptor for
C                 an SPK segment of type 2.
C
C     ET          is an epoch for which a data record from the
C                 specified segment is required. ET is expressed as
C                 seconds past J2000 TDB.
C
C
C$ Detailed_Output
C
C     RECORD      is an array of data from the specified segment which,
C                 when evaluated at epoch ET, will give the state
C                 (position and velocity) of the target body identified
C                 by the input segment descriptor. The descriptor
C                 specifies the center of motion and reference frame of
C                 the state.
C
C                 The structure of the record is as follows:
C
C                    +--------------------------------------+
C                    | record size (excluding this element) |
C                    +--------------------------------------+
C                    | Coverage interval midpoint           |
C                    +--------------------------------------+
C                    | Coverage interval radius             |
C                    +--------------------------------------+
C                    | Coeffs for X position component      |
C                    +--------------------------------------+
C                    | Coeffs for Y position component      |
C                    +--------------------------------------+
C                    | Coeffs for Z position component      |
C                    +--------------------------------------+
C
C                 In the above record
C
C                    - Times are expressed as seconds past J2000 TDB.
C                    - Position components have units of km.
C
C                 RECORD must be declared by the caller with size large
C                 enough to accommodate the largest record that can be
C                 returned by this routine. See the INCLUDE file
C                 spkrec.inc for the correct record length.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) Any errors that occur while looking up SPK data will be
C        diagnosed by routines in the call tree of this routine.
C
C$ Files
C
C     See argument HANDLE.
C
C$ Particulars
C
C     See the SPK Required Reading file for a description of the
C     structure of a data type 2 (Chebyshev polynomials, position
C     only) segment.
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
C           IF ( TYPE .EQ. 2 ) THEN
C              CALL SPKR02 ( HANDLE, DESCR, ET, RECORD )
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
C     NAIF Document 168.0, "S- and P- Kernel (SPK) Specification and
C     User's Guide"
C
C$ Author_and_Institution
C
C     I.M. Underwood  (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.1.1, 18-JAN-2014 (NJB)
C
C        Enhanced header and in-line documentation.
C
C-    SPICELIB Version 1.1.0, 07-SEP-2001 (EDW)
C
C        Replaced DAFRDA call with DAFGDA.
C        Added IMPLICIT NONE.
C
C-    SPICELIB Version 1.0.3, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.2, 23-AUG-1991 (HAN)
C
C        SPK02 was removed from the Required_Reading section of the
C        header. The information in the SPK02 Required Reading file
C        is now part of the SPK Required Reading file.
C
C-    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN)
C
C        Literature references added to the header.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
C
C-&
 
C$ Index_Entries
C
C     read record from type_2 spk segment
C
C-&
 
 
 
C
C     SPICELIB functions
C
      LOGICAL               RETURN
 
C
C     Local variables
C
 
      DOUBLE PRECISION      DC       (   2 )
      DOUBLE PRECISION      INIT
      DOUBLE PRECISION      INTLEN
 
      INTEGER               IC       (   6 )
      INTEGER               BEGIN
      INTEGER               END
      INTEGER               RECSIZ
      INTEGER               NREC
      INTEGER               RECNO
      INTEGER               RECADR
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SPKR02' )
      END IF
 
C
C     Unpack the segment descriptor.
C
      CALL DAFUS ( DESCR, 2, 6, DC, IC )
 
      BEGIN = IC( 5 )
      END   = IC( 6 )
 
C
C     The segment is made up of a number of logical records, each
C     having the same size, and covering the same length of time.
C
C     We can determine which record to return using the input epoch,
C     the initial time of the first record's coverage interval, and the
C     length of the interval covered by each record. These constants
C     are located at the end of the segment, along with the size of
C     each logical record and the total number of records.
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
 
      CALL CHKOUT ( 'SPKR02' )
      RETURN
      END
