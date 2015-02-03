C$Procedure      ZZFDAT ( Initialize built-in frame names and ID codes )
 
      SUBROUTINE ZZFDAT ( NCOUNT, MAXBFR, NAME,   IDCODE, 
     .                    CENTER, TYPE,   TYPID,  CENTRD, 
     .                    BNMLST, BNMPOL, BNMNMS, BNMIDX,
     .                    BIDLST, BIDPOL, BIDIDS, BIDIDX )

C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     This routine initializes the table of built-in frame names and
C     their ID codes and hashes and order arrays providing efficient
C     access to these tables.
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
C     None.
C
C$ Keywords
C
C     PRIVATE
C
C$ Declarations
 
      IMPLICIT NONE

      INCLUDE              'frmtyp.inc'
      INCLUDE              'ninert.inc'
      INCLUDE              'nninrt.inc'

      INTEGER               LBPOOL
      PARAMETER           ( LBPOOL = -5 )

      INTEGER               NCOUNT
      INTEGER               MAXBFR
      CHARACTER*(*)         NAME   (          * )
      INTEGER               IDCODE (          * )
      INTEGER               CENTER (          * )
      INTEGER               TYPE   (          * )
      INTEGER               TYPID  (          * )
      INTEGER               CENTRD (          * )
      INTEGER               BNMLST (          * )
      INTEGER               BNMPOL ( LBPOOL : * )
      CHARACTER*(*)         BNMNMS (          * )
      INTEGER               BNMIDX (          * )
      INTEGER               BIDLST (          * )
      INTEGER               BIDPOL ( LBPOOL : * )
      INTEGER               BIDIDS (          * )
      INTEGER               BIDIDX (          * )

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     NCOUNT     I   Input checking variable.
C     MAXBFR     I   build-in frame hash size.
C     NAME       O   array containing the names of all built-in frames
C     IDCODE     O   array containing the IDs of all built-in frames
C     CENTER     O   array containing the centers of the built-in frames
C     TYPE       O   array containing the types of the built-in frames
C     TYPID      O   array containing the subtype id
C     CENTRD     O   an order vector for CENTER
C     BNMLST     O   frame name-based hash head node pointer list
C     BNMPOL     O   frame name-based hash node collision list
C     BNMNMS     O   frame name-based hash item list
C     BNMIDX     O   frame name-based hash index storage array 
C     BIDLST     O   frame ID-based hash head node pointer list
C     BIDPOL     O   frame ID-based hash node collision list
C     BIDIDS     O   frame ID-based hash item list
C     BIDIDX     O   frame ID-based hash index storage array 
C
C$ Detailed_Input
C
C     NCOUNT      is the number of names that the calling routine
C                 expects to receive. It should have the value of
C                 NNAMES which is given below for NNAMES.  If this is
C                 not the case then the error 'SPICE(VERSIONMISMATCH1)'
C                 is signaled.
C
C     MAXBFR      is the build-in frame hash size. It should have a
C                 value greater than or equal to the value of NNAMES.
C                 If this is not the case then the error
C                 'SPICE(VERSIONMISMATCH2)' is signaled.
C
C                 If everything has been properly called, compiled and
C                 linked these errors should never be signaled. If
C                 either is signaled, it indicates that either a
C                 calling sequence, or version mismatch has occurred.
C
C$ Detailed_Output
C
C     The six arrays described below (NAME...CORDER) should be declared
C     with the same dimensions --- NCOUNT.
C
C     NAME        is an array of the official SPICE names for the
C                 recognized frames (both inertial and non-inertial)
C
C     IDCODE      is an array parallel to NAME of SPICE ID codes for
C                 the various frames.
C
C     CENTER      is an array parallel to NAME of body ID codes for
C                 the centers of frames.
C
C     TYPE        is an array parallel to NAME of inertial frame types
C                 for the various frames.  These include INERTL, PCK,
C                 CK, etc.
C
C     TYPID       is an array parallel to NAME of the ID code for the
C                 frame within the TYPE of the frame.  Once the class
C                 of the frame has been identified by TYPE, TYPID is
C                 used to access the information specific about this
C                 frame.
C
C     CENTRD      is an order vector for the array CENTER.  The value
C                 CENTER(CORDER(I)) is the I'th IDCODE when ordered
C                 from smallest to largest.
C
C     The eight hash array below (BMM*, BID*) should be declared with
C     the same upper dimensions --- MAXBFR.
C
C     BNMLST
C     BNMPOL
C     BNMNMS      are the frame name-based hash head node pointer, node
C                 collision, and item lists. Together they return the
C                 index of the element in the BNMIDX index storage
C                 array corresponding to a given built-in frame name.
C
C     BNMIDX      is the frame name-based hash index storage array 
C                 containing at the index determined by the hash for a
C                 given name the index corresponding to the same name
C                 in the NAME, IDCODE, CENTER, TYPE, and TYPID arrays.
C
C     BIDLST
C     BIDPOL
C     BIDIDS      are the frame ID-based hash head node pointer, node
C                 collision, and item lists. Together they return the
C                 index of the element in the BNMIDX index storage
C                 array corresponding to a given built-in frame ID.
C
C     BIDIDX      is the frame ID-based hash index storage array 
C                 containing at the index determined by the hash for a
C                 given ID the index corresponding to the same ID
C                 in the NAME, IDCODE, CENTER, TYPE, and TYPID arrays.
C
C$ Parameters
C
C     See include files.
C
C$ Exceptions
C
C     1) If the input number of frames NCOUNT is not equal to the 
C        number of frames determined from include files, the error
C        'SPICE(BUG)' will be signaled.
C
C     2) If the input hash size MAXBFR is not greater or equal to the 
C        number of frames determined from include files, the error
C        'SPICE(BUG2)' will be signaled.
C
C     If everything has been properly called, compiled and linked these
C     errors should never be signaled. If either is signaled, it
C     indicates that either a calling sequence, or version mismatch has
C     occurred.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine establishes the built-in SPICE reference frames and
C     their ID-codes. In addition it returns the order vector for the
C     frame center IDs and hashes for the frame names and IDs.
C
C     This is a private routine intended solely as a support routine
C     for the SPICE routine FRAMEX.
C
C$ Examples
C
C     This routine should typically be called as part of an
C     initialization portion of FRAMEX entry points.
C
C        LOGICAL               FIRST
C        SAVE                  FIRST
C
C        DATA                  FIRST / .TRUE. /
C
C
C        IF ( FIRST ) THEN
C
C           FIRST = .FALSE.
C           CALL ZZFDAT ( NCOUNT, MAXBFR, NAME,   IDCODE, 
C       .                 CENTER, TYPE,   TYPID,  CENTRD, 
C       .                 BNMLST, BNMPOL, BNMNMS, BNMIDX,
C       .                 BIDLST, BIDPOL, BIDIDS, BIDIDX )
C
C        END IF
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
C     B.V. Semenov    (JPL)
C     W.L. Taber      (JPL)
C     F.S. Turner     (JPL)
C     E.D. Wright     (JPL)
C
C$ Version
C
C-    SPICELIB Version 5.0.0, 08-AUG-2013 (BVS)
C
C        Changed routine's calling sequence by dropping name and ID
C        order vectors and adding name- and ID-based hashes and
C        modified it to initialize hashes instead of the order arrays.
C
C-    SPICELIB Version 4.3.0, 11-OCT-2011 (BVS)
C
C        Added the following PCK frames:
C
C           IAU_CERES
C           IAU_PALLAS
C           IAU_LUTETIA
C           IAU_DAVIDA
C           IAU_STEINS
C
C-    SPICELIB Version 4.2.0, 11-MAY-2010 (BVS)
C
C        Added the following PCK frames:
C
C           IAU_BORRELLY
C           IAU_TEMPEL_1
C           IAU_VESTA
C           IAU_ITOKAWA
C
C-    SPICELIB Version 4.1.0, 12-DEC-2002 (BVS)
C
C        Added PCK frames for new Jovian satellites:
C
C           IAU_CALLIRRHOE
C           IAU_THEMISTO
C           IAU_MAGACLITE
C           IAU_TAYGETE
C           IAU_CHALDENE
C           IAU_HARPALYKE
C           IAU_KALYKE
C           IAU_IOCASTE
C           IAU_ERINOME
C           IAU_ISONOE
C           IAU_PRAXIDIKE
C
C-    SPICELIB Version 4.0.1, 18-OCT-2002 (EDW)
C
C        Corrected the erroneous frame values for IAU_PAN.
C        Minor edits to the header.
C
C-    SPICELIB Version 4.0.0, 02-AUG-2002 (FST)
C
C        The frames IAU_PAN, IAU_GASPRA, IAU_IDA, and IAU_EROS
C        were added to the list of recognized frames.
C
C-    SPICELIB Version 3.1.1, 20-APR-1999 (WLT)
C
C        Changed the variable name TYPEID to TYPID in the calling
C        sequence to avoid having to take special measures in the f2c
C        conversion process.
C
C-    SPICELIB Version 3.1.0, 11-SEP-1997 (WLT)
C
C        The error condition check early in the routine
C        did not use the exception handling subsystem correctly.
C        This has been fixed.
C
C-    SPICELIB Version 3.0.0, 02-JUN-1997 (WLT)
C
C        The calling sequence changed.  ZZFDAT now also returns
C        an order vector for the CENTERs of the frames.
C
C-    SPICELIB Version 2.0.0, 03-APR-1997 (WLT)
C
C        The frames ITRF93 and EARTH_FIXED were added to the
C        list of recognized frames.
C
C-    SPICELIB Version 1.1.0, 14-OCT-1996 (WLT)
C
C        Changed declarations so that the variables NINERT and
C        NNINRT are included instead of being declared locally.
C
C-    SPICELIB Version 1.0.0, 19-SEP-1995 (WLT)
C
C-&
 
      INTEGER               NON
      PARAMETER           ( NON    = NNINRT )
 
      INTEGER               NNAMES
      PARAMETER           ( NNAMES = NINERT + NON )
 
C
C     To add to the list of recognized frames,
C
C     1. Determine whether or not the frame is inertial.
C
C        Inertial Case.
C
C        A. Be sure that the routine CHGIRF has been modified to
C           reflect the new frame and set NINERT (above) equal to
C           the number of recognized inertial frames give by CHGIRF.
C
C        Non Inertial Case.
C
C        A. Locate the last non-inertial frame in the lengthy list
C           below.
C
C        B. Add the frame name to the array NAME.  Add the IDCODE
C           to the array IDCODE. (Unless there is a compelling reason
C           to do otherwise this should just be the next integer in
C           the sequence of ID codes.  The mixture of old and new code
C           should look something like this:
C
C              Last bit of old assignments
C
C                 NAME   ( NINERT + NON ) = last name in the old routine
C                 IDCODE ( NINERT + NON ) = 10000 + NON
C
C              Your new assignment
C
C                 NAME   ( NINERT + NEXT ) = your name
C                 IDCODE ( NINERT + NEXT ) = 10000 + NEXT
C
C           where
C
C              NON  = the value of the parameter above
C              NEXT = NON + 1
C
C        C. Modify the value of the parameter NON above to reflect the
C           new number of non-inertial frames.
C
C     2. Update the version and date routine.
C
C     3. Update the routines that call this routine so that they
C        will be expecting the correct number of names and ID codes
C        to be returned.
C

C
C     Local variables.
C
 
      INTEGER               I
      INTEGER               ITEM

      LOGICAL               NEW


C
C     Perform the consistency checks first.
C
      IF ( NCOUNT .NE. NNAMES ) THEN
         CALL CHKIN ( 'ZZFDAT' )
         CALL SETMSG( 'There is an inconsistency between the '
     .   //           'version of the routine calling ZZFDAT and '
     .   //           'the current version of ZZFDAT. Check to '
     .   //           'make sure that you have the most current '
     .   //           'versions of ZZFDAT and the routines that '
     .   //           'make use of it.' )
         CALL SIGERR ( 'SPICE(VERSIONMISMATCH1)'  )
         CALL CHKOUT ( 'ZZFDAT' )
         RETURN
      END IF

      IF ( MAXBFR .LT. NNAMES ) THEN
         CALL CHKIN ( 'ZZFDAT' )
         CALL SETMSG( 'There is an inconsistency between the '
     .   //           'version of the routine calling ZZFDAT and '
     .   //           'the current version of ZZFDAT. Check to '
     .   //           'make sure that you have the most current '
     .   //           'versions of ZZFDAT and the routines that '
     .   //           'make use of it.' )
         CALL SIGERR ( 'SPICE(VERSIONMISMATCH2)' )
         CALL CHKOUT ( 'ZZFDAT' )
         RETURN
      END IF

C
C     Inertial Frames Section
C
C     Fetch the names of the inertial frames from CHGIRF.
C
      DO I = 1, NINERT
         IDCODE( I ) = I
         CENTER( I ) = 0
         TYPE  ( I ) = INERTL
         TYPID ( I ) = I
         CALL IRFNAM ( I, NAME(I) )
      END DO
 
C
C     Non-Inertial Frames Section.
C
      NAME   ( NINERT +   1 ) = 'IAU_MERCURY_BARYCENTER'
      IDCODE ( NINERT +   1 ) =  10001
      CENTER ( NINERT +   1 ) =  1
      TYPID  ( NINERT +   1 ) =  1
      TYPE   ( NINERT +   1 ) =  PCK
 
      NAME   ( NINERT +   2 ) = 'IAU_VENUS_BARYCENTER'
      IDCODE ( NINERT +   2 ) =  10002
      CENTER ( NINERT +   2 ) =  2
      TYPID  ( NINERT +   2 ) =  2
      TYPE   ( NINERT +   2 ) =  PCK
 
      NAME   ( NINERT +   3 ) = 'IAU_EARTH_BARYCENTER'
      IDCODE ( NINERT +   3 ) =  10003
      CENTER ( NINERT +   3 ) =  3
      TYPID  ( NINERT +   3 ) =  3
      TYPE   ( NINERT +   3 ) =  PCK
 
      NAME   ( NINERT +   4 ) = 'IAU_MARS_BARYCENTER'
      IDCODE ( NINERT +   4 ) =  10004
      CENTER ( NINERT +   4 ) =  4
      TYPID  ( NINERT +   4 ) =  4
      TYPE   ( NINERT +   4 ) =  PCK
 
      NAME   ( NINERT +   5 ) = 'IAU_JUPITER_BARYCENTER'
      IDCODE ( NINERT +   5 ) =  10005
      CENTER ( NINERT +   5 ) =  5
      TYPID  ( NINERT +   5 ) =  5
      TYPE   ( NINERT +   5 ) =  PCK
 
      NAME   ( NINERT +   6 ) = 'IAU_SATURN_BARYCENTER'
      IDCODE ( NINERT +   6 ) =  10006
      CENTER ( NINERT +   6 ) =  6
      TYPID  ( NINERT +   6 ) =  6
      TYPE   ( NINERT +   6 ) =  PCK
 
      NAME   ( NINERT +   7 ) = 'IAU_URANUS_BARYCENTER'
      IDCODE ( NINERT +   7 ) =  10007
      CENTER ( NINERT +   7 ) =  7
      TYPID  ( NINERT +   7 ) =  7
      TYPE   ( NINERT +   7 ) =  PCK
 
      NAME   ( NINERT +   8 ) = 'IAU_NEPTUNE_BARYCENTER'
      IDCODE ( NINERT +   8 ) =  10008
      CENTER ( NINERT +   8 ) =  8
      TYPID  ( NINERT +   8 ) =  8
      TYPE   ( NINERT +   8 ) =  PCK
 
      NAME   ( NINERT +   9 ) = 'IAU_PLUTO_BARYCENTER'
      IDCODE ( NINERT +   9 ) =  10009
      CENTER ( NINERT +   9 ) =  9
      TYPID  ( NINERT +   9 ) =  9
      TYPE   ( NINERT +   9 ) =  PCK
 
      NAME   ( NINERT +  10 ) = 'IAU_SUN'
      IDCODE ( NINERT +  10 ) =  10010
      CENTER ( NINERT +  10 ) =  10
      TYPID  ( NINERT +  10 ) =  10
      TYPE   ( NINERT +  10 ) =  PCK
 
      NAME   ( NINERT +  11 ) = 'IAU_MERCURY'
      IDCODE ( NINERT +  11 ) =  10011
      CENTER ( NINERT +  11 ) =  199
      TYPID  ( NINERT +  11 ) =  199
      TYPE   ( NINERT +  11 ) =  PCK
 
      NAME   ( NINERT +  12 ) = 'IAU_VENUS'
      IDCODE ( NINERT +  12 ) =  10012
      CENTER ( NINERT +  12 ) =  299
      TYPID  ( NINERT +  12 ) =  299
      TYPE   ( NINERT +  12 ) =  PCK
 
      NAME   ( NINERT +  13 ) = 'IAU_EARTH'
      IDCODE ( NINERT +  13 ) =  10013
      CENTER ( NINERT +  13 ) =  399
      TYPID  ( NINERT +  13 ) =  399
      TYPE   ( NINERT +  13 ) =  PCK
 
      NAME   ( NINERT +  14 ) = 'IAU_MARS'
      IDCODE ( NINERT +  14 ) =  10014
      CENTER ( NINERT +  14 ) =  499
      TYPID  ( NINERT +  14 ) =  499
      TYPE   ( NINERT +  14 ) =  PCK
 
      NAME   ( NINERT +  15 ) = 'IAU_JUPITER'
      IDCODE ( NINERT +  15 ) =  10015
      CENTER ( NINERT +  15 ) =  599
      TYPID  ( NINERT +  15 ) =  599
      TYPE   ( NINERT +  15 ) =  PCK
 
      NAME   ( NINERT +  16 ) = 'IAU_SATURN'
      IDCODE ( NINERT +  16 ) =  10016
      CENTER ( NINERT +  16 ) =  699
      TYPID  ( NINERT +  16 ) =  699
      TYPE   ( NINERT +  16 ) =  PCK
 
      NAME   ( NINERT +  17 ) = 'IAU_URANUS'
      IDCODE ( NINERT +  17 ) =  10017
      CENTER ( NINERT +  17 ) =  799
      TYPID  ( NINERT +  17 ) =  799
      TYPE   ( NINERT +  17 ) =  PCK
 
      NAME   ( NINERT +  18 ) = 'IAU_NEPTUNE'
      IDCODE ( NINERT +  18 ) =  10018
      CENTER ( NINERT +  18 ) =  899
      TYPID  ( NINERT +  18 ) =  899
      TYPE   ( NINERT +  18 ) =  PCK
 
      NAME   ( NINERT +  19 ) = 'IAU_PLUTO'
      IDCODE ( NINERT +  19 ) =  10019
      CENTER ( NINERT +  19 ) =  999
      TYPID  ( NINERT +  19 ) =  999
      TYPE   ( NINERT +  19 ) =  PCK
 
      NAME   ( NINERT +  20 ) = 'IAU_MOON'
      IDCODE ( NINERT +  20 ) =  10020
      CENTER ( NINERT +  20 ) =  301
      TYPID  ( NINERT +  20 ) =  301
      TYPE   ( NINERT +  20 ) =  PCK
 
      NAME   ( NINERT +  21 ) = 'IAU_PHOBOS'
      IDCODE ( NINERT +  21 ) =  10021
      CENTER ( NINERT +  21 ) =  401
      TYPID  ( NINERT +  21 ) =  401
      TYPE   ( NINERT +  21 ) =  PCK
 
      NAME   ( NINERT +  22 ) = 'IAU_DEIMOS'
      IDCODE ( NINERT +  22 ) =  10022
      CENTER ( NINERT +  22 ) =  402
      TYPID  ( NINERT +  22 ) =  402
      TYPE   ( NINERT +  22 ) =  PCK
 
      NAME   ( NINERT +  23 ) = 'IAU_IO'
      IDCODE ( NINERT +  23 ) =  10023
      CENTER ( NINERT +  23 ) =  501
      TYPID  ( NINERT +  23 ) =  501
      TYPE   ( NINERT +  23 ) =  PCK
 
      NAME   ( NINERT +  24 ) = 'IAU_EUROPA'
      IDCODE ( NINERT +  24 ) =  10024
      CENTER ( NINERT +  24 ) =  502
      TYPID  ( NINERT +  24 ) =  502
      TYPE   ( NINERT +  24 ) =  PCK
 
      NAME   ( NINERT +  25 ) = 'IAU_GANYMEDE'
      IDCODE ( NINERT +  25 ) =  10025
      CENTER ( NINERT +  25 ) =  503
      TYPID  ( NINERT +  25 ) =  503
      TYPE   ( NINERT +  25 ) =  PCK
 
      NAME   ( NINERT +  26 ) = 'IAU_CALLISTO'
      IDCODE ( NINERT +  26 ) =  10026
      CENTER ( NINERT +  26 ) =  504
      TYPID  ( NINERT +  26 ) =  504
      TYPE   ( NINERT +  26 ) =  PCK
 
      NAME   ( NINERT +  27 ) = 'IAU_AMALTHEA'
      IDCODE ( NINERT +  27 ) =  10027
      CENTER ( NINERT +  27 ) =  505
      TYPID  ( NINERT +  27 ) =  505
      TYPE   ( NINERT +  27 ) =  PCK
 
      NAME   ( NINERT +  28 ) = 'IAU_HIMALIA'
      IDCODE ( NINERT +  28 ) =  10028
      CENTER ( NINERT +  28 ) =  506
      TYPID  ( NINERT +  28 ) =  506
      TYPE   ( NINERT +  28 ) =  PCK
 
      NAME   ( NINERT +  29 ) = 'IAU_ELARA'
      IDCODE ( NINERT +  29 ) =  10029
      CENTER ( NINERT +  29 ) =  507
      TYPID  ( NINERT +  29 ) =  507
      TYPE   ( NINERT +  29 ) =  PCK
 
      NAME   ( NINERT +  30 ) = 'IAU_PASIPHAE'
      IDCODE ( NINERT +  30 ) =  10030
      CENTER ( NINERT +  30 ) =  508
      TYPID  ( NINERT +  30 ) =  508
      TYPE   ( NINERT +  30 ) =  PCK
 
      NAME   ( NINERT +  31 ) = 'IAU_SINOPE'
      IDCODE ( NINERT +  31 ) =  10031
      CENTER ( NINERT +  31 ) =  509
      TYPID  ( NINERT +  31 ) =  509
      TYPE   ( NINERT +  31 ) =  PCK
 
      NAME   ( NINERT +  32 ) = 'IAU_LYSITHEA'
      IDCODE ( NINERT +  32 ) =  10032
      CENTER ( NINERT +  32 ) =  510
      TYPID  ( NINERT +  32 ) =  510
      TYPE   ( NINERT +  32 ) =  PCK
 
      NAME   ( NINERT +  33 ) = 'IAU_CARME'
      IDCODE ( NINERT +  33 ) =  10033
      CENTER ( NINERT +  33 ) =  511
      TYPID  ( NINERT +  33 ) =  511
      TYPE   ( NINERT +  33 ) =  PCK
 
      NAME   ( NINERT +  34 ) = 'IAU_ANANKE'
      IDCODE ( NINERT +  34 ) =  10034
      CENTER ( NINERT +  34 ) =  512
      TYPID  ( NINERT +  34 ) =  512
      TYPE   ( NINERT +  34 ) =  PCK
 
      NAME   ( NINERT +  35 ) = 'IAU_LEDA'
      IDCODE ( NINERT +  35 ) =  10035
      CENTER ( NINERT +  35 ) =  513
      TYPID  ( NINERT +  35 ) =  513
      TYPE   ( NINERT +  35 ) =  PCK
 
      NAME   ( NINERT +  36 ) = 'IAU_THEBE'
      IDCODE ( NINERT +  36 ) =  10036
      CENTER ( NINERT +  36 ) =  514
      TYPID  ( NINERT +  36 ) =  514
      TYPE   ( NINERT +  36 ) =  PCK
 
      NAME   ( NINERT +  37 ) = 'IAU_ADRASTEA'
      IDCODE ( NINERT +  37 ) =  10037
      CENTER ( NINERT +  37 ) =  515
      TYPID  ( NINERT +  37 ) =  515
      TYPE   ( NINERT +  37 ) =  PCK
 
      NAME   ( NINERT +  38 ) = 'IAU_METIS'
      IDCODE ( NINERT +  38 ) =  10038
      CENTER ( NINERT +  38 ) =  516
      TYPID  ( NINERT +  38 ) =  516
      TYPE   ( NINERT +  38 ) =  PCK
 
      NAME   ( NINERT +  39 ) = 'IAU_MIMAS'
      IDCODE ( NINERT +  39 ) =  10039
      CENTER ( NINERT +  39 ) =  601
      TYPID  ( NINERT +  39 ) =  601
      TYPE   ( NINERT +  39 ) =  PCK
 
      NAME   ( NINERT +  40 ) = 'IAU_ENCELADUS'
      IDCODE ( NINERT +  40 ) =  10040
      CENTER ( NINERT +  40 ) =  602
      TYPID  ( NINERT +  40 ) =  602
      TYPE   ( NINERT +  40 ) =  PCK
 
      NAME   ( NINERT +  41 ) = 'IAU_TETHYS'
      IDCODE ( NINERT +  41 ) =  10041
      CENTER ( NINERT +  41 ) =  603
      TYPID  ( NINERT +  41 ) =  603
      TYPE   ( NINERT +  41 ) =  PCK
 
      NAME   ( NINERT +  42 ) = 'IAU_DIONE'
      IDCODE ( NINERT +  42 ) =  10042
      CENTER ( NINERT +  42 ) =  604
      TYPID  ( NINERT +  42 ) =  604
      TYPE   ( NINERT +  42 ) =  PCK
 
      NAME   ( NINERT +  43 ) = 'IAU_RHEA'
      IDCODE ( NINERT +  43 ) =  10043
      CENTER ( NINERT +  43 ) =  605
      TYPID  ( NINERT +  43 ) =  605
      TYPE   ( NINERT +  43 ) =  PCK
 
      NAME   ( NINERT +  44 ) = 'IAU_TITAN'
      IDCODE ( NINERT +  44 ) =  10044
      CENTER ( NINERT +  44 ) =  606
      TYPID  ( NINERT +  44 ) =  606
      TYPE   ( NINERT +  44 ) =  PCK
 
      NAME   ( NINERT +  45 ) = 'IAU_HYPERION'
      IDCODE ( NINERT +  45 ) =  10045
      CENTER ( NINERT +  45 ) =  607
      TYPID  ( NINERT +  45 ) =  607
      TYPE   ( NINERT +  45 ) =  PCK
 
      NAME   ( NINERT +  46 ) = 'IAU_IAPETUS'
      IDCODE ( NINERT +  46 ) =  10046
      CENTER ( NINERT +  46 ) =  608
      TYPID  ( NINERT +  46 ) =  608
      TYPE   ( NINERT +  46 ) =  PCK
 
      NAME   ( NINERT +  47 ) = 'IAU_PHOEBE'
      IDCODE ( NINERT +  47 ) =  10047
      CENTER ( NINERT +  47 ) =  609
      TYPID  ( NINERT +  47 ) =  609
      TYPE   ( NINERT +  47 ) =  PCK
 
      NAME   ( NINERT +  48 ) = 'IAU_JANUS'
      IDCODE ( NINERT +  48 ) =  10048
      CENTER ( NINERT +  48 ) =  610
      TYPID  ( NINERT +  48 ) =  610
      TYPE   ( NINERT +  48 ) =  PCK
 
      NAME   ( NINERT +  49 ) = 'IAU_EPIMETHEUS'
      IDCODE ( NINERT +  49 ) =  10049
      CENTER ( NINERT +  49 ) =  611
      TYPID  ( NINERT +  49 ) =  611
      TYPE   ( NINERT +  49 ) =  PCK
 
      NAME   ( NINERT +  50 ) = 'IAU_HELENE'
      IDCODE ( NINERT +  50 ) =  10050
      CENTER ( NINERT +  50 ) =  612
      TYPID  ( NINERT +  50 ) =  612
      TYPE   ( NINERT +  50 ) =  PCK
 
      NAME   ( NINERT +  51 ) = 'IAU_TELESTO'
      IDCODE ( NINERT +  51 ) =  10051
      CENTER ( NINERT +  51 ) =  613
      TYPID  ( NINERT +  51 ) =  613
      TYPE   ( NINERT +  51 ) =  PCK
 
      NAME   ( NINERT +  52 ) = 'IAU_CALYPSO'
      IDCODE ( NINERT +  52 ) =  10052
      CENTER ( NINERT +  52 ) =  614
      TYPID  ( NINERT +  52 ) =  614
      TYPE   ( NINERT +  52 ) =  PCK
 
      NAME   ( NINERT +  53 ) = 'IAU_ATLAS'
      IDCODE ( NINERT +  53 ) =  10053
      CENTER ( NINERT +  53 ) =  615
      TYPID  ( NINERT +  53 ) =  615
      TYPE   ( NINERT +  53 ) =  PCK
 
      NAME   ( NINERT +  54 ) = 'IAU_PROMETHEUS'
      IDCODE ( NINERT +  54 ) =  10054
      CENTER ( NINERT +  54 ) =  616
      TYPID  ( NINERT +  54 ) =  616
      TYPE   ( NINERT +  54 ) =  PCK
 
      NAME   ( NINERT +  55 ) = 'IAU_PANDORA'
      IDCODE ( NINERT +  55 ) =  10055
      CENTER ( NINERT +  55 ) =  617
      TYPID  ( NINERT +  55 ) =  617
      TYPE   ( NINERT +  55 ) =  PCK
 
      NAME   ( NINERT +  56 ) = 'IAU_ARIEL'
      IDCODE ( NINERT +  56 ) =  10056
      CENTER ( NINERT +  56 ) =  701
      TYPID  ( NINERT +  56 ) =  701
      TYPE   ( NINERT +  56 ) =  PCK
 
      NAME   ( NINERT +  57 ) = 'IAU_UMBRIEL'
      IDCODE ( NINERT +  57 ) =  10057
      CENTER ( NINERT +  57 ) =  702
      TYPID  ( NINERT +  57 ) =  702
      TYPE   ( NINERT +  57 ) =  PCK
 
      NAME   ( NINERT +  58 ) = 'IAU_TITANIA'
      IDCODE ( NINERT +  58 ) =  10058
      CENTER ( NINERT +  58 ) =  703
      TYPID  ( NINERT +  58 ) =  703
      TYPE   ( NINERT +  58 ) =  PCK
 
      NAME   ( NINERT +  59 ) = 'IAU_OBERON'
      IDCODE ( NINERT +  59 ) =  10059
      CENTER ( NINERT +  59 ) =  704
      TYPID  ( NINERT +  59 ) =  704
      TYPE   ( NINERT +  59 ) =  PCK
 
      NAME   ( NINERT +  60 ) = 'IAU_MIRANDA'
      IDCODE ( NINERT +  60 ) =  10060
      CENTER ( NINERT +  60 ) =  705
      TYPID  ( NINERT +  60 ) =  705
      TYPE   ( NINERT +  60 ) =  PCK
 
      NAME   ( NINERT +  61 ) = 'IAU_CORDELIA'
      IDCODE ( NINERT +  61 ) =  10061
      CENTER ( NINERT +  61 ) =  706
      TYPID  ( NINERT +  61 ) =  706
      TYPE   ( NINERT +  61 ) =  PCK
 
      NAME   ( NINERT +  62 ) = 'IAU_OPHELIA'
      IDCODE ( NINERT +  62 ) =  10062
      CENTER ( NINERT +  62 ) =  707
      TYPID  ( NINERT +  62 ) =  707
      TYPE   ( NINERT +  62 ) =  PCK
 
      NAME   ( NINERT +  63 ) = 'IAU_BIANCA'
      IDCODE ( NINERT +  63 ) =  10063
      CENTER ( NINERT +  63 ) =  708
      TYPID  ( NINERT +  63 ) =  708
      TYPE   ( NINERT +  63 ) =  PCK
 
      NAME   ( NINERT +  64 ) = 'IAU_CRESSIDA'
      IDCODE ( NINERT +  64 ) =  10064
      CENTER ( NINERT +  64 ) =  709
      TYPID  ( NINERT +  64 ) =  709
      TYPE   ( NINERT +  64 ) =  PCK
 
      NAME   ( NINERT +  65 ) = 'IAU_DESDEMONA'
      IDCODE ( NINERT +  65 ) =  10065
      CENTER ( NINERT +  65 ) =  710
      TYPID  ( NINERT +  65 ) =  710
      TYPE   ( NINERT +  65 ) =  PCK
 
      NAME   ( NINERT +  66 ) = 'IAU_JULIET'
      IDCODE ( NINERT +  66 ) =  10066
      CENTER ( NINERT +  66 ) =  711
      TYPID  ( NINERT +  66 ) =  711
      TYPE   ( NINERT +  66 ) =  PCK
 
      NAME   ( NINERT +  67 ) = 'IAU_PORTIA'
      IDCODE ( NINERT +  67 ) =  10067
      CENTER ( NINERT +  67 ) =  712
      TYPID  ( NINERT +  67 ) =  712
      TYPE   ( NINERT +  67 ) =  PCK
 
      NAME   ( NINERT +  68 ) = 'IAU_ROSALIND'
      IDCODE ( NINERT +  68 ) =  10068
      CENTER ( NINERT +  68 ) =  713
      TYPID  ( NINERT +  68 ) =  713
      TYPE   ( NINERT +  68 ) =  PCK
 
      NAME   ( NINERT +  69 ) = 'IAU_BELINDA'
      IDCODE ( NINERT +  69 ) =  10069
      CENTER ( NINERT +  69 ) =  714
      TYPID  ( NINERT +  69 ) =  714
      TYPE   ( NINERT +  69 ) =  PCK
 
      NAME   ( NINERT +  70 ) = 'IAU_PUCK'
      IDCODE ( NINERT +  70 ) =  10070
      CENTER ( NINERT +  70 ) =  715
      TYPID  ( NINERT +  70 ) =  715
      TYPE   ( NINERT +  70 ) =  PCK
 
      NAME   ( NINERT +  71 ) = 'IAU_TRITON'
      IDCODE ( NINERT +  71 ) =  10071
      CENTER ( NINERT +  71 ) =  801
      TYPID  ( NINERT +  71 ) =  801
      TYPE   ( NINERT +  71 ) =  PCK
 
      NAME   ( NINERT +  72 ) = 'IAU_NEREID'
      IDCODE ( NINERT +  72 ) =  10072
      CENTER ( NINERT +  72 ) =  802
      TYPID  ( NINERT +  72 ) =  802
      TYPE   ( NINERT +  72 ) =  PCK
 
      NAME   ( NINERT +  73 ) = 'IAU_NAIAD'
      IDCODE ( NINERT +  73 ) =  10073
      CENTER ( NINERT +  73 ) =  803
      TYPID  ( NINERT +  73 ) =  803
      TYPE   ( NINERT +  73 ) =  PCK
 
      NAME   ( NINERT +  74 ) = 'IAU_THALASSA'
      IDCODE ( NINERT +  74 ) =  10074
      CENTER ( NINERT +  74 ) =  804
      TYPID  ( NINERT +  74 ) =  804
      TYPE   ( NINERT +  74 ) =  PCK
 
      NAME   ( NINERT +  75 ) = 'IAU_DESPINA'
      IDCODE ( NINERT +  75 ) =  10075
      CENTER ( NINERT +  75 ) =  805
      TYPID  ( NINERT +  75 ) =  805
      TYPE   ( NINERT +  75 ) =  PCK
 
      NAME   ( NINERT +  76 ) = 'IAU_GALATEA'
      IDCODE ( NINERT +  76 ) =  10076
      CENTER ( NINERT +  76 ) =  806
      TYPID  ( NINERT +  76 ) =  806
      TYPE   ( NINERT +  76 ) =  PCK
 
      NAME   ( NINERT +  77 ) = 'IAU_LARISSA'
      IDCODE ( NINERT +  77 ) =  10077
      CENTER ( NINERT +  77 ) =  807
      TYPID  ( NINERT +  77 ) =  807
      TYPE   ( NINERT +  77 ) =  PCK
 
      NAME   ( NINERT +  78 ) = 'IAU_PROTEUS'
      IDCODE ( NINERT +  78 ) =  10078
      CENTER ( NINERT +  78 ) =  808
      TYPID  ( NINERT +  78 ) =  808
      TYPE   ( NINERT +  78 ) =  PCK
 
      NAME   ( NINERT +  79 ) = 'IAU_CHARON'
      IDCODE ( NINERT +  79 ) =  10079
      CENTER ( NINERT +  79 ) =  901
      TYPID  ( NINERT +  79 ) =  901
      TYPE   ( NINERT +  79 ) =  PCK
 
C
C     This is for the first new PCK frame---the high precision earth
C     frame ITRF93.
C
      NAME   ( NINERT +  80 ) = 'ITRF93'
      IDCODE ( NINERT +  80 ) =  13000
      CENTER ( NINERT +  80 ) =  399
      TYPID  ( NINERT +  80 ) =  3000
      TYPE   ( NINERT +  80 ) =  PCK
 
C
C     This if for the alias frame EARTH BODYFIXED.  This is a TK
C     class frame.  To use it a FRAME kernel must be loaded via
C     FURNSH.
C
      NAME   ( NINERT +  81 ) = 'EARTH_FIXED'
      IDCODE ( NINERT +  81 ) =  10081
      CENTER ( NINERT +  81 ) =  399
      TYPID  ( NINERT +  81 ) =  10081
      TYPE   ( NINERT +  81 ) =  TK
 
C
C     Frames introduced into the generic NAIF PCK
C     system as referenced from the 1997 IAU report.
C
      NAME   ( NINERT +  82 ) =  'IAU_PAN'
      IDCODE ( NINERT +  82 ) =  10082
      CENTER ( NINERT +  82 ) =  618
      TYPID  ( NINERT +  82 ) =  618
      TYPE   ( NINERT +  82 ) =  PCK
 
      NAME   ( NINERT +  83 ) =  'IAU_GASPRA'
      IDCODE ( NINERT +  83 ) =  10083
      CENTER ( NINERT +  83 ) =  9511010
      TYPID  ( NINERT +  83 ) =  9511010
      TYPE   ( NINERT +  83 ) =  PCK
 
      NAME   ( NINERT +  84 ) =  'IAU_IDA'
      IDCODE ( NINERT +  84 ) =  10084
      CENTER ( NINERT +  84 ) =  2431010
      TYPID  ( NINERT +  84 ) =  2431010
      TYPE   ( NINERT +  84 ) =  PCK
 
C
C     Frame referenced from the Eros orientation model in the 2000 IAU
C     report.
C
      NAME   ( NINERT +  85 ) =  'IAU_EROS'
      IDCODE ( NINERT +  85 ) =  10085
      CENTER ( NINERT +  85 ) =  2000433
      TYPID  ( NINERT +  85 ) =  2000433
      TYPE   ( NINERT +  85 ) =  PCK

C
C     Frames for Jovian satellites approved by IAU in late 2002.
C 
      NAME   ( NINERT +  86 ) =  'IAU_CALLIRRHOE'
      IDCODE ( NINERT +  86 ) =  10086
      CENTER ( NINERT +  86 ) =  517
      TYPID  ( NINERT +  86 ) =  517
      TYPE   ( NINERT +  86 ) =  PCK
 
      NAME   ( NINERT +  87 ) =  'IAU_THEMISTO'
      IDCODE ( NINERT +  87 ) =  10087
      CENTER ( NINERT +  87 ) =  518
      TYPID  ( NINERT +  87 ) =  518
      TYPE   ( NINERT +  87 ) =  PCK
 
      NAME   ( NINERT +  88 ) =  'IAU_MAGACLITE'
      IDCODE ( NINERT +  88 ) =  10088
      CENTER ( NINERT +  88 ) =  519
      TYPID  ( NINERT +  88 ) =  519
      TYPE   ( NINERT +  88 ) =  PCK
 
      NAME   ( NINERT +  89 ) =  'IAU_TAYGETE'
      IDCODE ( NINERT +  89 ) =  10089
      CENTER ( NINERT +  89 ) =  520
      TYPID  ( NINERT +  89 ) =  520
      TYPE   ( NINERT +  89 ) =  PCK
 
      NAME   ( NINERT +  90 ) =  'IAU_CHALDENE'
      IDCODE ( NINERT +  90 ) =  10090
      CENTER ( NINERT +  90 ) =  521
      TYPID  ( NINERT +  90 ) =  521
      TYPE   ( NINERT +  90 ) =  PCK
 
      NAME   ( NINERT +  91 ) =  'IAU_HARPALYKE'
      IDCODE ( NINERT +  91 ) =  10091
      CENTER ( NINERT +  91 ) =  522
      TYPID  ( NINERT +  91 ) =  522
      TYPE   ( NINERT +  91 ) =  PCK
 
      NAME   ( NINERT +  92 ) =  'IAU_KALYKE'
      IDCODE ( NINERT +  92 ) =  10092
      CENTER ( NINERT +  92 ) =  523
      TYPID  ( NINERT +  92 ) =  523
      TYPE   ( NINERT +  92 ) =  PCK
 
      NAME   ( NINERT +  93 ) =  'IAU_IOCASTE'
      IDCODE ( NINERT +  93 ) =  10093
      CENTER ( NINERT +  93 ) =  524
      TYPID  ( NINERT +  93 ) =  524
      TYPE   ( NINERT +  93 ) =  PCK
 
      NAME   ( NINERT +  94 ) =  'IAU_ERINOME'
      IDCODE ( NINERT +  94 ) =  10094
      CENTER ( NINERT +  94 ) =  525
      TYPID  ( NINERT +  94 ) =  525
      TYPE   ( NINERT +  94 ) =  PCK
 
      NAME   ( NINERT +  95 ) =  'IAU_ISONOE'
      IDCODE ( NINERT +  95 ) =  10095
      CENTER ( NINERT +  95 ) =  526
      TYPID  ( NINERT +  95 ) =  526
      TYPE   ( NINERT +  95 ) =  PCK
 
      NAME   ( NINERT +  96 ) =  'IAU_PRAXIDIKE'
      IDCODE ( NINERT +  96 ) =  10096
      CENTER ( NINERT +  96 ) =  527
      TYPID  ( NINERT +  96 ) =  527
      TYPE   ( NINERT +  96 ) =  PCK

C
C     Frames for comets and asteroids, for which rotation constants
C     were added in 2006 IAU Report.
C 
      NAME   ( NINERT +  97 ) =  'IAU_BORRELLY'
      IDCODE ( NINERT +  97 ) =  10097
      CENTER ( NINERT +  97 ) =  1000005
      TYPID  ( NINERT +  97 ) =  1000005
      TYPE   ( NINERT +  97 ) =  PCK

      NAME   ( NINERT +  98 ) =  'IAU_TEMPEL_1'
      IDCODE ( NINERT +  98 ) =  10098
      CENTER ( NINERT +  98 ) =  1000093
      TYPID  ( NINERT +  98 ) =  1000093
      TYPE   ( NINERT +  98 ) =  PCK

      NAME   ( NINERT +  99 ) =  'IAU_VESTA'
      IDCODE ( NINERT +  99 ) =  10099
      CENTER ( NINERT +  99 ) =  2000004
      TYPID  ( NINERT +  99 ) =  2000004
      TYPE   ( NINERT +  99 ) =  PCK

      NAME   ( NINERT + 100 ) =  'IAU_ITOKAWA'
      IDCODE ( NINERT + 100 ) =  10100
      CENTER ( NINERT + 100 ) =  2025143
      TYPID  ( NINERT + 100 ) =  2025143
      TYPE   ( NINERT + 100 ) =  PCK

C
C     Frames for asteroids, for which rotation constants were added in
C     2009 IAU Report.
C 
      NAME   ( NINERT + 101 ) =  'IAU_CERES'
      IDCODE ( NINERT + 101 ) =  10101
      CENTER ( NINERT + 101 ) =  2000001
      TYPID  ( NINERT + 101 ) =  2000001
      TYPE   ( NINERT + 101 ) =  PCK

      NAME   ( NINERT + 102 ) =  'IAU_PALLAS'
      IDCODE ( NINERT + 102 ) =  10102
      CENTER ( NINERT + 102 ) =  2000002
      TYPID  ( NINERT + 102 ) =  2000002
      TYPE   ( NINERT + 102 ) =  PCK

      NAME   ( NINERT + 103 ) =  'IAU_LUTETIA'
      IDCODE ( NINERT + 103 ) =  10103
      CENTER ( NINERT + 103 ) =  2000021
      TYPID  ( NINERT + 103 ) =  2000021
      TYPE   ( NINERT + 103 ) =  PCK

      NAME   ( NINERT + 104 ) =  'IAU_DAVIDA'
      IDCODE ( NINERT + 104 ) =  10104
      CENTER ( NINERT + 104 ) =  2000511
      TYPID  ( NINERT + 104 ) =  2000511
      TYPE   ( NINERT + 104 ) =  PCK

      NAME   ( NINERT + 105 ) =  'IAU_STEINS'
      IDCODE ( NINERT + 105 ) =  10105
      CENTER ( NINERT + 105 ) =  2002867
      TYPID  ( NINERT + 105 ) =  2002867
      TYPE   ( NINERT + 105 ) =  PCK

C
C     Below is a template to use for adding another non-inertial
C     frame. Copy it, fill in the new values and then leave
C     a new template for the next person who needs to modify this
C     routine.
C
C     NAME   ( NINERT + 106 ) =  name
C     IDCODE ( NINERT + 106 ) =  10101
C     CENTER ( NINERT + 106 ) =  center
C     TYPID  ( NINERT + 106 ) =  type ID code
C     TYPE   ( NINERT + 106 ) =  type (INERTL, PCK, etc. )
C

C
C     Generate order vector for centers (used by CIDFRM).
C 
      CALL ORDERI ( CENTER, NNAMES, CENTRD )

C
C     Initialize build-in frame name- and ID-based hashes.
C
      CALL ZZHSCINI ( MAXBFR, BNMLST, BNMPOL )
      CALL ZZHSIINI ( MAXBFR, BIDLST, BIDPOL )

C
C     Register all built-in frames in the frame name- and ID-based
C     hashes.
C
      DO I = 1, NNAMES

         CALL ZZHSCADD ( BNMLST, BNMPOL, BNMNMS, NAME(I),   ITEM, NEW )
         BNMIDX( ITEM ) = I

         CALL ZZHSIADD ( BIDLST, BIDPOL, BIDIDS, IDCODE(I), ITEM, NEW )
         BIDIDX( ITEM ) = I

      END DO
 
      RETURN
      END
