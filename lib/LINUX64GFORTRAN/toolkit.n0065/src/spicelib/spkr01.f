C$Procedure      SPKR01 ( Read SPK record from segment, type 1 )
 
      SUBROUTINE SPKR01 ( HANDLE, DESCR, ET, RECORD )
      IMPLICIT NONE
 
C$ Abstract
C
C     Read a single SPK data record from a segment of type 1
C     (Difference Lines).
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
C     ET         I   Target epoch.
C     RECORD     O   Data record.
C
C$ Detailed_Input
C
C     HANDLE,
C     DESCR       are the file handle and segment descriptor for
C                 a SPK segment of type 1.
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
C     None.
C
C$ Files
C
C     See argument HANDLE.
C
C$ Particulars
C
C     See the SPK Required Reading file for a description of the
C     structure of a data type 1 segment.
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
C              CALL SPKR01 ( HANDLE, DESCR, ET, RECORD )
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
C-    SPICELIB Version 1.1.0, 07-SEP-2001 (EDW)
C
C        Replaced DAFRDA call with DAFGDA.
C
C-    SPICELIB Version 1.0.3, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.2, 23-AUG-1991 (HAN)
C
C        SPK01 was removed from the Required_Reading section of the
C        header. The information in the SPK01 Required Reading file
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
C     read record from type_1 spk segment
C
C-&
 
 
 
C
C     SPICELIB functions
C
      INTEGER               LSTLTD
      LOGICAL               RETURN
 
C
C     Local variables
C
 
      DOUBLE PRECISION      DC       (   2 )
      INTEGER               IC       (   6 )
      INTEGER               BEGIN
      INTEGER               END
 
      DOUBLE PRECISION      DATA     ( 100 )
      INTEGER               NREC
      INTEGER               NDIR
      INTEGER               OFFD
      INTEGER               OFFE
      INTEGER               OFF
 
      INTEGER               RECNO
      INTEGER               OFFR
      INTEGER               I
 
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SPKR01' )
      END IF
 
C
C     Unpack the segment descriptor.
C
      CALL DAFUS ( DESCR, 2, 6, DC, IC )
 
      BEGIN = IC(5)
      END   = IC(6)
 
C
C     Get the number of records in the segment. From that, we can
C     compute
C
C        NDIR      The number of directory epochs.
C
C        OFFD      The offset of the first directory epoch.
C
C        OFFE      The offset of the first epoch.
C
C
C     the number of directory epochs.
C
      CALL DAFGDA ( HANDLE, END, END, DATA )
      NREC = INT ( DATA(1) )
 
      NDIR  = NREC / 100
      OFFD  = END - NDIR - 1
      OFFE  = END - NDIR - NREC - 1
 
C
C     What we want is the record number: once we have that, we can
C     compute the offset of the record from the beginning of the
C     segment, grab it, and go. But how to find it?
C
C     Ultimately, we want the first record whose epoch is greater
C     than or equal to ET. If there are 100 or fewer records, all
C     the record epochs can be examined in a single group.
C
      IF ( NREC .LE. 100 ) THEN
         CALL DAFGDA ( HANDLE, OFFE+1, OFFE+NREC, DATA )
         RECNO = LSTLTD ( ET, NREC, DATA ) + 1
 
         OFFR = (BEGIN - 1) + (RECNO - 1) * 71
         CALL DAFGDA ( HANDLE, OFFR+1, OFFR+71, RECORD )
 
         CALL CHKOUT ( 'SPKR01' )
         RETURN
      END IF
 
C
C     Searching directories is a little more difficult.
C
C     The directory contains epochs 100, 200, and so on. Once we
C     find the first directory epoch greater than or equal to ET,
C     we can grab the corresponding set of 100 record epochs, and
C     search them.
C
      DO I = 1, NDIR
         CALL DAFGDA ( HANDLE, OFFD+I, OFFD+I, DATA )
 
         IF ( DATA(1) .GE. ET ) THEN
            OFF = OFFE + (I - 1) * 100
            CALL DAFGDA ( HANDLE, OFF+1, OFF+100, DATA )
 
            RECNO = ((I - 1) * 100) + LSTLTD ( ET, 100, DATA ) + 1
 
            OFFR = (BEGIN - 1) + (RECNO - 1) * 71
            CALL DAFGDA ( HANDLE, OFFR+1, OFFR+71, RECORD )
 
            CALL CHKOUT ( 'SPKR01' )
            RETURN
         END IF
      END DO
 
C
C     If ET is greater than the final directory epoch, we want one
C     of the final records.
C
      I = MOD ( NREC, 100 )
 
      CALL DAFGDA ( HANDLE, END-NDIR-I, END-NDIR-1, DATA )
      RECNO = (NDIR * 100) + LSTLTD ( ET, I, DATA ) + 1
 
      OFFR = (BEGIN - 1) + (RECNO - 1) * 71
      CALL DAFGDA ( HANDLE, OFFR+1, OFFR+71, RECORD )
 
 
      CALL CHKOUT ( 'SPKR01' )
      RETURN
      END
