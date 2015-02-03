C$Procedure      SPKR21 ( Read SPK record from segment, type 21 )
 
      SUBROUTINE SPKR21 ( HANDLE, DESCR, ET, RECORD )
      IMPLICIT NONE
 
C$ Abstract
C
C     Read a single SPK data record from a segment of type 21
C     (Extended Difference Lines).
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
 
      INCLUDE 'spk21.inc'

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
C                 a SPK segment of type 21.
C
C     ET          is an epoch for which a data record from a specific
C                 segment is required. The epoch is represented as
C                 seconds past J2000 TDB.
C
C$ Detailed_Output
C
C     RECORD      is a data record which, when evaluated at epoch ET,
C                 will give the state (position and velocity) of an
C                 ephemeris object, relative to its center of motion,
C                 in an inertial reference frame.
C
C                 The contents of RECORD are as follows:
C
C                    RECORD(1):         The difference table size per
C                                       Cartesian component. Call this
C                                       size MAXDIM; then the difference
C                                       line (MDA) size DLSIZE is
C
C                                         ( 4 * MAXDIM ) + 11
C                                    
C                    RECORD(2)
C                       ...
C                    RECORD(1+DLSIZE):  An extended difference line.
C                                       The contents are:
C
C                       Dimension  Description
C                       ---------  ----------------------------------
C                       1          Reference epoch of difference line
C                       MAXDIM     Stepsize function vector
C                       1          Reference position vector,  x
C                       1          Reference velocity vector,  x
C                       1          Reference position vector,  y
C                       1          Reference velocity vector,  y
C                       1          Reference position vector,  z
C                       1          Reference velocity vector,  z
C                       MAXDIM,3   Modified divided difference
C                                  arrays (MDAs)
C                       1          Maximum integration order plus 1
C                       3          Integration order array
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If the maximum table size of the input record exceeds 
C        MAXTRM, the error SPICE(DIFFLINETOOLARGE) is signaled.
C
C     2) Any errors that occur while reading SPK data will be
C        diagnosed by routines in the call tree of this routine.
C
C$ Files
C
C     See argument HANDLE.
C
C$ Particulars
C
C     See the SPK Required Reading file for a description of the
C     structure of a data type 21 segment.
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
C              CALL SPKR21 ( HANDLE, DESCR, ET, RECORD )
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
C     N.J. Bachman    (JPL)
C     I.M. Underwood  (JPL)
C     E.D. Wright     (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 16-JAN-2014 (NJB) (FTK) (WLT) (IMU)
C
C-&
 
C$ Index_Entries
C
C     read record from type_21 spk segment
C
C-&
 
 
 
C
C     SPICELIB functions
C
      INTEGER               LSTLTD
      LOGICAL               RETURN
 
C
C     Local parameters
C
      INTEGER               BUFSIZ
      PARAMETER           ( BUFSIZ = 100 )

C
C     Local variables
C 
      DOUBLE PRECISION      DATA   ( BUFSIZ )
      DOUBLE PRECISION      DC     ( 2 )

      INTEGER               BEGIN
      INTEGER               DFLSIZ
      INTEGER               END
      INTEGER               I
      INTEGER               IC     ( 6 )
      INTEGER               MAXDIM
      INTEGER               NDIR
      INTEGER               NREC
      INTEGER               OFF
      INTEGER               OFFD
      INTEGER               OFFE
      INTEGER               OFFR
      INTEGER               RECNO
  
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'SPKR21' )

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
C     We'll fetch the difference table dimension as well.
C
      CALL DAFGDA ( HANDLE, END-1, END, DATA )

      NREC   = NINT ( DATA(2) ) 
      NDIR   = NREC / BUFSIZ
      OFFD   = END  - NDIR - 2
      OFFE   = OFFD - NREC
 
      MAXDIM = NINT ( DATA(1) )

      IF ( MAXDIM .GT. MAXTRM ) THEN

         CALL SETMSG ( 'The input record has a maximum table '
     .   //            'dimension of #, while the maximum '
     .   //            'supported by this routine is #. It is '
     .   //            'possible that this problem is due to '
     .   //            'your SPICE Toolkit being out of date.' )
         CALL ERRINT ( '#',  MAXDIM                            )
         CALL ERRINT ( '#',  MAXTRM                            )
         CALL SIGERR ( 'SPICE(DIFFLINETOOLARGE)'               )
         CALL CHKOUT ( 'SPKR21'                                )
         RETURN

      END IF

C
C     The difference line dimension per component is the
C     first element of the output record.
C
      RECORD( 1 ) = MAXDIM

C
C     Set the difference line size.
C
      DFLSIZ = ( 4 * MAXDIM ) + 11

C
C     What we want is the record number: once we have that, we can
C     compute the offset of the record from the beginning of the
C     segment, grab it, and go. But how to find it?
C
C     Ultimately, we want the first record whose epoch is greater
C     than or equal to ET. If there are BUFSIZ or fewer records, all
C     the record epochs can be examined in a single group.
C
      IF ( NREC .LE. BUFSIZ ) THEN

         CALL DAFGDA ( HANDLE, OFFE+1, OFFE+NREC, DATA )
         RECNO = LSTLTD ( ET, NREC, DATA ) + 1
 
         OFFR = (BEGIN - 1) + (RECNO - 1) * DFLSIZ
         CALL DAFGDA ( HANDLE, OFFR+1, OFFR+DFLSIZ, RECORD(2) )
 
         CALL CHKOUT ( 'SPKR21' )
         RETURN

      END IF
 
C
C     Searching directories is a little more difficult.
C
C     The directory contains epochs BUFSIZ, 2*BUFSIZ, and so on. Once
C     we find the first directory epoch greater than or equal to ET, we
C     can grab the corresponding set of BUFSIZ record epochs, and
C     search them.
C
      DO I = 1, NDIR

         CALL DAFGDA ( HANDLE, OFFD+I, OFFD+I, DATA )
 
         IF ( DATA(1) .GE. ET ) THEN

            OFF = OFFE + (I - 1) * BUFSIZ
            CALL DAFGDA ( HANDLE, OFF+1, OFF+BUFSIZ, DATA )
 
            RECNO = ((I - 1) * BUFSIZ) + LSTLTD ( ET, BUFSIZ, DATA ) + 1
 
            OFFR = (BEGIN - 1) + (RECNO - 1) * DFLSIZ
            CALL DAFGDA ( HANDLE, OFFR+1, OFFR+DFLSIZ, RECORD(2) )
 
            CALL CHKOUT ( 'SPKR21' )
            RETURN

         END IF

      END DO
 
C
C     If ET is greater than the final directory epoch, we want one
C     of the final records.
C
      I = MOD ( NREC, BUFSIZ )
 
      CALL DAFGDA ( HANDLE, END-NDIR-I-1, END-NDIR-2, DATA )

      RECNO = (NDIR * BUFSIZ) + LSTLTD ( ET, I, DATA ) + 1
 
      OFFR = (BEGIN - 1) + (RECNO - 1) * DFLSIZ

      CALL DAFGDA ( HANDLE, OFFR+1, OFFR+DFLSIZ, RECORD(2) )
 
 
      CALL CHKOUT ( 'SPKR21' )
      RETURN
      END
