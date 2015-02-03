C$Procedure      SPKR15 ( Read SPK record from segment, type 15 )
 
      SUBROUTINE SPKR15 ( HANDLE, DESCR, ET, RECORD )
      IMPLICIT NONE
 
C$ Abstract
C
C     This routine reads a single spk data record from a segment of
C     type 15 (Precessing Conic Propagation).
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
      DOUBLE PRECISION      DESCR (5)
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      RECORD(*)
 
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
C                 a SPK segment of type 15.
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
C$ Files
C
C     See argument HANDLE.
C
C$ Exceptions
C
C     1) If the segment specified by DESCR is not a type 15 segment
C        the error 'SPICE(WRONGSPKTYPE)' will be signalled.
C
C     2) A type 15 segment should have exactly 16 values.  If this
C        is not the case the error 'SPICE(MALFORMEDSEGMENT)' is
C        signalled.
C
C$ Particulars
C
C     This routine reads all of the data from a type 15 SPK segment.
C
C     The structure of the data retrieved in RECORD is:
C
C         RECORD(1)             epoch of the orbit elements at periapse
C                               in ephemeris seconds past J2000.
C         RECORD(2)-RECORD(4)   unit trajectory pole vector
C         RECORD(5)-RECORD(7)   unit periapsis vector
C         RECORD(8)             semi-latus rectum---p in the
C                               equation:
C
C                               r = p/(1 + ECC*COS(Nu))
C
C         RECORD(9)             eccentricity
C         RECORD(10)            J2 processing flag describing
C                               what J2 corrections are to be
C                               applied when the orbit is
C                               propagated.
C
C                                Value       Meaning
C                                -----  -----------------------------
C                                1      Regress line of nodes only.
C                                2      Precess line of apsides only.
C                                3      Don't use J2 corrections.
C                                Other  Regress line of nodes
C                                       and precess line of apsides.
C
C         RECORD(11)-RECORD(13) unit central body pole vector
C         RECORD(14)            central body GM
C         RECORD(15)            central body J2
C         RECORD(16)            central body radius
C
C     Except for J2, units are radians, km, seconds.
C
C
C$ Examples
C
C     The data returned by the SPKRnn routine is in its rawest form,
C     taken directly from the segment.  As such, it will be meaningless
C     to a user unless he/she understands the structure of the data type
C     completely.  Given that understanding, however, the SPKRnn
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
C           IF ( TYPE .EQ. 15 ) THEN
C              CALL SPKR15 ( HANDLE, DESCR, ET, RECORD )
C                  .
C                  .  Look at the RECORD data.
C                  .
C           END IF
C
C
C$ Restrictions
C
C     None.
C
C$ Author_and_Institution
C
C      S.   Schlaifer  (JPL)
C      W.L. Taber      (JPL)
C
C$ Literature_References
C
C      None.
C
C$ Version
C
C-    SPICELIB Version 1.1.0, 07-SEP-2001 (EDW)
C
C        Replaced DAFRDA call with DAFGDA.
C        Added IMPLICIT NONE.
C
C-    SPICELIB Version 1.0.0, 15-NOV-1994 (WLT) (SS)
C
C-&
 
C$ Index_Entries
C
C     read record from type_15 spk segment
C
C-&
 
C
C     SPICELIB Funcions
C
      LOGICAL               RETURN
 
C
C     Local Variables
C
      INTEGER               ND
      PARAMETER           ( ND = 2 )
 
      INTEGER               NI
      PARAMETER           ( NI = 6 )
 
C
C     The differnce between the first and last address of a type 15
C     segment should be 15.
C
      INTEGER               DIFF
      PARAMETER           ( DIFF = 15 )
 
      DOUBLE PRECISION      DC  ( ND )
      INTEGER               IC  ( NI )
 
      INTEGER               BEGIN
      INTEGER               END
      INTEGER               TYPE
 
C
C     Standard Spice Error Handling.
C
      IF (RETURN() ) THEN
         RETURN
      END IF
 
      CALL CHKIN('SPKR15')
C
C     Unpack the segment descriptor.
C
      CALL DAFUS( DESCR, ND, NI, DC, IC )
 
      TYPE  = IC(4)
      BEGIN = IC(5)
      END   = IC(6)
C
C     Make sure that this really is a type 15 data segment.
C
      IF (TYPE .NE. 15) THEN
 
         CALL SETMSG ('You are attempting to locate type 15 ' //
     .                'data in a type # data segment.')
         CALL ERRINT ('#', TYPE                       )
         CALL SIGERR ('SPICE(WRONGSPKTYPE)'           )
         CALL CHKOUT ('SPKR15'                        )
         RETURN
 
      END IF
 
C
C     Since it doesn't cost much we make sure that the segment has
C     the correct amount of data.
C
      IF ( END - BEGIN .NE. DIFF ) THEN
 
         CALL SETMSG ( 'A type 15 segment should contain exactly '
     .   //            '16 double precision values.  The segment '
     .   //            'supplied had #.  The segment is badly '
     .   //            'formed. ' )
 
         CALL ERRINT ( '#',   END-BEGIN+1         )
         CALL SIGERR ( 'SPICE(MALFORMEDSEGMENT)'  )
         CALL CHKOUT ( 'SPKR15'                   )
         RETURN
 
      END IF
 
C
C     Read the data for the record.
C
      CALL DAFGDA ( HANDLE, BEGIN, END, RECORD )
      CALL CHKOUT ('SPKR15')
      RETURN
      END
