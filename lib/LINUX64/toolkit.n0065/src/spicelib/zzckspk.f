C$Procedure      ZZCKSPK ( SPK or CK )
 
      SUBROUTINE ZZCKSPK ( HANDLE, CKSPK )
 
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     This routine determines whether or not a DAF file attached to
C     the supplied handle is an SPK, CK or unknown file.
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
C      None.
C
C$ Keywords
C
C      PRIVATE
C
C$ Declarations
 
      IMPLICIT NONE
      INTEGER               HANDLE
      CHARACTER*(*)         CKSPK
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     HANDLE     I   the handle of a DAF file open for read access.
C     CKSPK      O   the type of the DAF file (SPK,CK or ?)
C
C$ Detailed_Input
C
C     HANDLE     is the handle of a DAF file open for read.
C
C$ Detailed_Output
C
C     CKSPK      is a string containing one of the following 3 values
C                'SPK', 'CK' or '?'
C
C$ Parameters
C
C     None.
C
C$ Files
C
C     None.
C
C$ Exceptions
C
C     None.
C
C$ Particulars
C
C     This routine examines the first segment of a DAF that is
C     a candidate for being an SPK or CK and returns a diagnosis
C     of the type of the file.
C
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     None.
C
C$ Author_and_Institution
C
C     W.L. Taber      (JPL)
C
C$ Literature_References
C
C     None.
C
C$ Version
C
C-    SPICELIB Version 1.1.0, 07-SEP-2001 (EDW)
C
C        Replaced DAFRDA call with DAFGDA.
C        Added IMPLICIT NONE.
C
C-    SPICELIB Version 1.0.0, 03-DEC-1999 (WLT)
C
C-&
 
      LOGICAL               RETURN
      LOGICAL               FAILED
 
 
C
C     Local parameters
C
C
C     The following parameters point to the various slots in the
C     integer portion of the DAF descriptor where the values are
C     located.
C
      INTEGER               BODID
      PARAMETER           ( BODID  =  1 )
 
      INTEGER               CENID
      PARAMETER           ( CENID  = BODID  + 1 )
 
      INTEGER               SPKFRM
      PARAMETER           ( SPKFRM = CENID  + 1 )
 
      INTEGER               SPKTYP
      PARAMETER           ( SPKTYP = SPKFRM + 1 )
 
      INTEGER               START
      PARAMETER           ( START  = SPKTYP + 1 )
 
      INTEGER               FINISH
      PARAMETER           ( FINISH = START  + 1 )
 
      INTEGER               CKRATE
      PARAMETER           ( CKRATE = SPKTYP     )
 
      INTEGER               CKTYPE
      PARAMETER           ( CKTYPE = SPKFRM )
 
C
C     These parameters give the number of integer and double precision
C     components of the descriptor for SPK and CK files.
C
      INTEGER               ND
      PARAMETER           ( ND = 2 )
 
      INTEGER               NI
      PARAMETER           ( NI = 6 )
C
C     The size of a summary.
C
      INTEGER               SUMSIZ
      PARAMETER           ( SUMSIZ = ND + ((NI+1)/2) )
 
 
      DOUBLE PRECISION      CHCKTM
      DOUBLE PRECISION      DC    ( ND )
      DOUBLE PRECISION      LASTDP
      DOUBLE PRECISION      FRSTTM
      DOUBLE PRECISION      SUM   ( SUMSIZ )
      DOUBLE PRECISION      TIMES ( 2 )
 
      INTEGER               ANGVEL
      INTEGER               FIRST
      INTEGER               FROM
      INTEGER               IC    ( NI )
      INTEGER               LAST
      INTEGER               NCK2
      INTEGER               NSPK
      INTEGER               SIZE
      INTEGER               THISND
      INTEGER               THISNI
      INTEGER               TO
      INTEGER               TYPE
 
      LOGICAL               CK2OK
      LOGICAL               FOUND
      LOGICAL               SPKOK
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      END IF
 
      CALL CHKIN ( 'ZZCKSPK' )
C
C     Make sure the values of ND and NI associated with this file
C     have the correct values.
C
      CALL DAFHSF ( HANDLE, THISND, THISNI )
 
      IF (     THISND .NE. ND
     .    .OR. THISNI .NE. NI ) THEN
         CKSPK = '?'
         CALL CHKOUT ( 'ZZCKSPK' )
         RETURN
      END IF
 
C
C     We've got the correct values for ND and NI, examine the descriptor
C     for the first array.
C
      CALL DAFBFS ( HANDLE )
      CALL DAFFNA ( FOUND  )
 
      IF ( FAILED () ) THEN
         CKSPK = '?'
         CALL CHKOUT ( 'ZZCKSPK' )
         RETURN
      END IF
 
C
C     If we don't find any segments, we don't have a clue about
C     the file type.
C
      IF ( .NOT. FOUND ) THEN
 
         CKSPK = '?'
         CALL CHKOUT ( 'ZZCKSPK' )
         RETURN
 
      END IF
C
C     Unpack the summary record.
C
      CALL DAFGS ( SUM                 )
      CALL DAFUS ( SUM, ND, NI, DC, IC )
 
C
C     Look at the slot where the angular velocity flag would
C     be located if this is a CK file.
C
      ANGVEL = IC(CKRATE)
      TYPE   = IC(CKTYPE)
 
C
C     Test 1. The value of ANGVEL may do the trick
C     right at the start.
C
      IF ( ANGVEL .EQ. 0 ) THEN
         CKSPK   = 'CK'
         CALL CHKOUT ( 'ZZCKSPK' )
         RETURN
      END IF
 
      IF ( ANGVEL .GT. 1 ) THEN
         CKSPK   = 'SPK'
         CALL CHKOUT ( 'ZZCKSPK' )
         RETURN
      END IF
 
C
C     Test 2. If this is an SPK file, it has a type 01 segment.
C     See if this is something orbiting the solar system
C     barycenter.
C
      IF ( IC(CENID) .EQ. 0 ) THEN
         CKSPK   = 'SPK'
         CALL CHKOUT ( 'ZZCKSPK' )
         RETURN
      END IF
 
C
C     Test 3. This is the super test.  Compute the size of the
C     segment and fetch the last d.p. from the segment.
C
 
      FIRST  = IC(START)
      LAST   = IC(FINISH)
      SIZE   = LAST  - FIRST +  1
C
C     Check the size of the array to see if it has any chance
C     of being an SPK and if it does get the number of MDA records.
C
      CALL ZZSIZEOK ( SIZE-1, 72, 100, 0, SPKOK, NSPK )
 
      IF ( .NOT. SPKOK ) THEN
         CKSPK = 'CK'
         CALL CHKOUT ( 'ZZCKSPK' )
         RETURN
      END IF
 
 
      CALL DAFGDA ( HANDLE, LAST, LAST, LASTDP )
C
C     See if the last number in the file is the allowed number of
C     MDA records.  If not, this must be a CK segment.
C
      IF ( LASTDP .NE. DBLE(NSPK) ) THEN
         CKSPK   = 'CK'
         CALL CHKOUT ( 'ZZCKSPK' )
         RETURN
      END IF
C
C     If we are still here, the last d.p. in the segment matches the
C     expected number of MDA records.  If the potential CK type is
C     not 2, we must have an SPK file.
C
      IF ( TYPE .NE. 2 ) THEN
         CKSPK = 'SPK'
         CALL CHKOUT ( 'ZZCKSPK' )
         RETURN
      END IF
C
C     We are getting down to the nitty gritty here. See if the
C     size is compatible with a type 02 C-kernel.
C
      CALL ZZSIZEOK ( SIZE, 10, 100, 1, CK2OK, NCK2 )
 
      IF ( .NOT. CK2OK ) THEN
         CKSPK = 'SPK'
         CALL CHKOUT ( 'ZZCKSPK' )
         RETURN
      END IF
 
C
C     So much for being nice. We need to examine the structure of the
C     actual data in the segment.  There are two cases to consider:
C     when there is 1 or fewer type 02 CK directory records and when
C     there is more than 1.  Note that to get to this point there must
C     be at least 1 directory value if this is a CK type 02 segment.
C     (To see this check the sizes when ZZSIZEOK returns TRUE for
C     both type 1 SPK and type 02 CK. The only such sizes in which
C     there the number CK type 02 directory values is one or fewer
C     are SIZE = 1081, 1441, and 1801 which correspond to (NSPK,NCK2) =
C     (15,108), (20,144), (25, 180).  In all of these cases there is
C     exactly 1 ck type 02 directory value.)
C
      IF ( NCK2 .LT. 201 ) THEN
C
C        Recall that MDA record contains its stop time as the first
C        entry of the record.  These epochs show up duplicated in the
C        epochs portion of the segment.
C
C        If this is a type 01 SPK segment, there are no directory
C        records and the first epoch shows up in the slot NSPK before
C        the last slot of the segment.  If it is a type 02 CK segment
C        the last stop tick shows up in this slot.  We need to look
C        at this value to see what's up.
C
         CALL DAFGDA ( HANDLE, LAST-NSPK, LAST-NSPK, FRSTTM )
 
C
C        Now (under the assumption that we have an SPK segment) look
C        up the epoch from the last MDA record--- the NSPK'th
C        record.  This epoch must be greater than the first epoch
C        in the array of epochs.
         FROM = FIRST + (NSPK-1)*71
         TO   = FROM
         CALL DAFGDA ( HANDLE, FROM, TO, CHCKTM )
 
C
C        If this is a type 02 segment.  The value we just picked out
C        will come from the array of stop ticks.  The array of stop
C        ticks is non-decreasing so:
C
         IF ( CHCKTM .GT. FRSTTM ) THEN
            CKSPK = 'SPK'
         ELSE
            CKSPK = 'CK'
         END IF
 
      ELSE
C
C        In this case there are at least 2 directory records if we
C        have a CK.  We read the last potential tick value and the
C        first potential directory value..  Note that the last potential
C        stop tick must be greater than the first potential directory
C        record.
C
         FROM = LAST - (NCK2-1)/100
         TO   = FROM + 1
 
         CALL DAFGDA ( HANDLE, FROM, TO, TIMES )
C
C        If we happen to have a TYPE 01 SPK segment we've just
C        read two consecutive values from the epochs sub-array of the
C        segment. Here's a sketch of why this is so:
C
C          The number of directory records for a CK type 02 segment is
C          (NCK2-1)/100  which is the same as SIZE/1001.
C
C          The number of directory records for an SPK type 01 segment is
C          (NSPK-1)/100  which is the same as SIZE/7201.
C
C          The number of stop ticks for type 02 CK is NCK2 ~ SIZE/10
C
C          The number of epochs for a type 01 SPK is  NSPK ~ SIZE/72
C
C        so NSPK directories < NCK2 directories < NCK2 directories + 1
C        < NSPK + NSPK directories < NCK2.  Consequently, the
C        two values just read are either the last stop tick and the
C        first CK directory value or two consecutive epochs.
C        In the first case TIMES(1) > TIMES(2), in the later case
C        we have TIMES(1) < TIMES(2)
C
         IF ( TIMES(1) .GT. TIMES(2) ) THEN
            CKSPK = 'CK'
         ELSE
            CKSPK = 'SPK'
         END IF
 
      END IF
 
      CALL CHKOUT ( 'ZZCKSPK' )
      RETURN
 
      END
