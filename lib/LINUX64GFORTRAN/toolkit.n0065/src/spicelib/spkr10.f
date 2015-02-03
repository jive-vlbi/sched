C$Procedure SPKR10 ( SPK, read record from SPK type 10 segment )
 
      SUBROUTINE SPKR10 ( HANDLE, DESCR, ET, RECORD )
 
C$ Abstract
C
C     Read a single SPK data record from a segment of type 10
C     (NORAD two line element sets).
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
      INCLUDE               'sgparam.inc'
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
C                 a SPK segment of type 10.
C
C     ET          is a target epoch, for which a data record from
C                 a specific segment is required.
C
C$ Detailed_Output
C
C     RECORD      is the record from the specified segment which,
C                 when evaluated at epoch ET, will give the state
C                 (position and velocity) of some body, relative
C                 to some center, in some inertial reference frame.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) It is assumed that the descriptor and handle supplied are
C        for a properly constructed type 10 segment.  No checks are
C        performed to ensure this.
C
C     2) All errors are diagnosed by routines in the call tree
C        of this routine.
C
C$ Files
C
C     See argument HANDLE.
C
C$ Particulars
C
C     See the SPK Required Reading file for a description of the
C     structure of a data type 10 segment.
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
C           IF ( TYPE .EQ. 1 ) THEN
C              CALL SPKR10 ( HANDLE, DESCR, ET, RECORD )
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
C     W.L. Taber  (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.1.0, 09-MAR-2009 (EDW)
C
C        Remove declaration of unused varaible DOINT.
C
C-    SPICELIB Version 1.0.0, 05-JAN-1994 (WLT)
C
C-&
 
C$ Index_Entries
C
C     read record from type_10 spk segment
C
C-&
 
 
 
C
C     SPICELIB functions
C
      LOGICAL               RETURN
 
C
C     Local variables
C
      INTEGER               NCONST
      PARAMETER           ( NCONST = 8  )
 
      INTEGER               NELEMS
      PARAMETER           ( NELEMS = 10 )
C
C     We have 2 nutation/obliquity terms and their rates giving us
C     four angle components for each packet.
C
      INTEGER               NANGS
      PARAMETER           ( NANGS =   4 )
C
C     BEGEL1 is the location in the record where the first
C     two-line element set will begin.
C
      INTEGER               BEGEL1
      PARAMETER           ( BEGEL1 = NCONST + 1      )
C
C     BEGEL2 is the location in the record where the second
C     two-line element set will begin.
C
      INTEGER               BEGEL2
      PARAMETER           ( BEGEL2 = BEGEL1 + NELEMS + NANGS )
C
C     ENSET1 and ENSET2 are the locations in the record where the
C     last element of set 1 and set 2 will be located.
C
      INTEGER               ENSET1
      PARAMETER           ( ENSET1 = BEGEL1 + NELEMS - 1 )
 
      INTEGER               ENSET2
      PARAMETER           ( ENSET2 = BEGEL2 + NELEMS - 1 )
 
 
      DOUBLE PRECISION      VALUE
 
 
      INTEGER               ENDS    ( 2 )
      INTEGER               FROM
      INTEGER               INDX
      INTEGER               NEPOCH
      INTEGER               TO
      INTEGER               PUTELM
      INTEGER               GETELM
      INTEGER               I
      INTEGER               SET1
      INTEGER               SET2
 
      LOGICAL               FOUND
 
      SAVE
  
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF
 
      CALL CHKIN ( 'SPKR10' )
 
C
C     Fetch the constants and store them in the first part of
C     the output RECORD.
C
      CALL SGFCON ( HANDLE, DESCR, 1,  NCONST, RECORD(1) )
 
C
C     Locate the time in the file closest to the input ET.
C
      CALL SGFRVI ( HANDLE, DESCR, ET, VALUE, INDX, FOUND )
 
C
C     Determine which pair of element sets to choose so that
C     they will bracket ET.
C
      IF ( ET .LE. VALUE ) THEN
 
         FROM = MAX( INDX-1, 1 )
         TO   = INDX
 
      ELSE
 
         CALL SGMETA ( HANDLE, DESCR, NREF, NEPOCH )
         FROM = INDX
         TO   = MIN( INDX+1, NEPOCH  )
 
      END IF
 
C
C     Fetch the element sets
C
      CALL SGFPKT ( HANDLE, DESCR, FROM, TO, RECORD(BEGEL1), ENDS )
C
C     If the size of the packets is not 14, this is an old style
C     two-line element set without nutation information.  We simply
C     set all of the angles to zero.
C
      IF ( ENDS(1) .EQ. NELEMS ) THEN
C
C        First shift the elements to their proper locations in RECORD
C        so there will be room to fill in the zeros.
C
         PUTELM = ENSET2
         GETELM = ENSET1 + NELEMS
 
         DO WHILE ( GETELM .GT. ENSET1 )
 
            RECORD(PUTELM) = RECORD(GETELM)
            PUTELM         = PUTELM - 1
            GETELM         = GETELM - 1
 
         END DO
 
         SET1 = ENSET1 + 1
         SET2 = ENSET2 + 1
 
         DO I = 1, NANGS
            RECORD(SET1) = 0.0D0
            RECORD(SET2) = 0.0D0
            SET1         = SET1 + 1
            SET2         = SET2 + 1
         END DO
 
      END IF
 
C
C     If we only got one element set, ET  was either before the
C     first one in the segment or after the last one in the
C     segment.  We simply copy the one fetched a second time so
C     that the record is properly constructed.
C
      IF ( FROM .EQ. TO ) THEN
         CALL MOVED ( RECORD(BEGEL1), NELEMS + NANGS, RECORD(BEGEL2) )
      END IF
 
      CALL CHKOUT ( 'SPKR10' )
      RETURN
      END
