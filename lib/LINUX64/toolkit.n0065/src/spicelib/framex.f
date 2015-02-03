C$Procedure FRAMEX ( FRAMe EXpert )
 
      SUBROUTINE FRAMEX ( CNAME, FRNAME, FRCODE, CENT,
     .                    CLASS, CLSSID, FOUND )
 
C$ Abstract
C
C     This is an umbrella routine for the entry points available for
C     manipulating different reference frames. It should not be called
C     directly.
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
C     FRAMES
C
C$ Keywords
C
C     FRAMES
C
C$ Declarations
 
      IMPLICIT NONE

      INCLUDE              'ninert.inc'
      INCLUDE              'nninrt.inc'
      INCLUDE              'frmtyp.inc'
      INCLUDE              'zzctr.inc'

      CHARACTER*(*)         CNAME
      CHARACTER*(*)         FRNAME
      INTEGER               FRCODE
      INTEGER               CENT
      INTEGER               CLASS
      INTEGER               CLSSID
      LOGICAL               FOUND
 
C$ Brief_I/O
C
C     VARIABLE  I/O  ENTRY POINT
C     --------  ---  --------------------------------------------------
C     CNAME      I   CNMFRM
C     FRNAME    I/O  NAMFRM, FRMNAM, CCIFRM
C     FRCODE    I/O  NAMFRM, FRMNAM, FRINFO, CIDFRM, CCIFRM
C     CENT      I/O  FRINFO, CIDFRM, CCIFRM
C     CLASS     I/O  FRINFO, CCIFRM
C     CLSSID    I/O  FRINFO, CCIFRM
C     FOUND      O   FRINFO
C
C
C$ Detailed_Input
C
C     See individual entry points for details concerning inputs.
C
C$ Detailed_Output
C
C     See individual entry points for details concerning inputs.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If this routine is called directly the error
C       'SPICE(BOGUSENTRY)' will be signaled.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This is an umbrella routine that comprises the SPICE
C     interface to the reference frame transformation software.
C
C     There are 6 entry points.
C
C     NAMFRM  converts string to the ID codes used by low level
C             SPICE software
C
C     FRMNAM  converts frame ID codes to the more familiar names
C             used to describe various reference frames.
C
C     FRINFO  returns the center associated with a reference frame.
C
C     CIDFRM  given the ID code of an object, returns the bodyfixed
C             frame associated with it.
C
C     CNMFRM  given the name of an object, returns the bodyfixed
C             frame associated with it.
C
C     CCIFRM  given a frame's class and class ID, returns
C             the frame's ID code, name, and center.
C
C$ Examples
C
C     Suppose that you needed to transform between two reference
C     frames on the basis of their names and that you wanted to
C     correct for light time to the center of the second frame
C     as seen from an observer with ID code OBS.
C
C     The code fragment below illustrates how you could use the
C     entry points gathered in this routine to retrieve the
C     state transformation matrix.
C
C
C        First convert names to frame ID codes.
C
C        CHARACTER*(26)        NAME1
C        CHARACTER*(26)        NAME2
C
C        INTEGER               FRAME1
C        INTEGER               FRAME2
C        INTEGER               CENT
C        INTEGER               OBS
C
C        DOUBLE PRECISION      ET
C        DOUBLE PRECISION      LT
C
C        DOUBLE PRECISION      STATE ( 6 )
C        DOUBLE PRECISION      XFORM ( 6, 6 )
C
C
C        First we use the entry points NAMFRM to convert the frame
C        names to ID codes.
C
C        CALL NAMFRM ( NAME1, FRAME1 )
C        CALL NAMFRM ( NAME2, FRAME2 )
C
C        Next we determine the center of the second frame
C
C        CALL FRINFO ( FRAME2, CENT, CLASS, CLSSID, FOUND )
C
C        Determine the light time to the center of the second frame.
C
C        CALL SPKGEO ( CENT,  ET, 'J2000',  OBS, STATE, LT )
C
C        Finally get the state transformation from FRAME1 to FRAME2
C        at time ET - LT
C
C        CALL FRMCHG ( FRAME1, FRAME2, ET-LT, XFORM )
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
C     N.J. Bachman    (JPL)
C     B.V. Semenov    (JPL)
C     W.L. Taber      (JPL)
C
C$ Version
C
C-    SPICELIB Version 5.2.0, 08-AUG-2012 (BVS) 
C
C        The routine was updated to be more efficient by using hashes
C        instead kernel POOL look-ups for kernel POOL frames and by
C        using hases instead of ordered array searches for built-in
C        frames. 
C
C        Bux fix: CCIFRM entry point logic was corrected to examine the
C        built-in frames before looking at the kernel POOL frames.
C
C-    SPICELIB Version 5.1.1, 09-FEB-2011 (NJB) 
C
C        Bug fix: corrected logic in entry point CIDFRM for
C        object-frame association for case where the assigned frame
C        value is denoted by a frame code. 
C
C        Fixed typo in FRAMEX header.
C
C-    SPICELIB Version 5.0.1, 17-MAR-2009 (EDW) 
C
C        Entry point NAMFRM: Typo correction in Required_Reading, 
C        changed FRAME to FRAMES.
C
C-    SPICELIB Version 5.0.0, 05-NOV-2007 (NJB)
C
C        Entry point CCIFRM (map frame class and class ID
C        to frame ID code, name, and center) has been added.
C
C-    SPICELIB Version 4.0.0, 13-SEP-2005 (NJB)
C
C        Entry point FRINFO is no longer error-free. Various frame
C        definition errors that were previously ignored are now
C        diagnosed.
C
C        Entry point FRINFO has been updated to support specification
C        of frame center by name or ID code. Previously only ID codes
C        could be used to identify frame centers. 
C 
C-    SPICELIB Version 3.2.0, 20-DEC-2004 (BVS)
C
C        Added parameter incorporating maximum body name length and set
C        it to the same value as MAXL from zzbodtrn.inc. Used this
C        parameter to declare local variable that holds frame center 
C        name (LCNAME).
C
C        In FRINFO entry: removed special handling of the frame IDs
C        less than -999. If they cannot be ``resolved'' using kernel
C        pool keywords, the frame is NOT declared CK-based with center
C        ID derived by dividing frame ID by a 1000 and class ID
C        assigned the frame ID anymore. In the current practice with
C        multitude of TK frames with IDs set instrument IDs this
C        default behavior is simply not valid.
C
C-    SPICELIB Version 3.1.0, 28-NOV-2002 (NJB)
C
C        Bug fix: updated CNMFRM so a TK frame specified by name and
C        designated as an object's preferred frame via kernel pool
C        assignments is found, and so that the correct name of this
C        frame is returned. 
C
C-    SPICELIB Version 3.0.1, 25-JUN-1999 (WLT)
C
C        Extended documentation of entry point CNMFRM and
C        corrected example for that entry point.
C
C-    SPICELIB Version 3.0.0, 03-JUN-1997 (WLT)
C
C        The entry points CIDFRM and CNMFRM were added so that
C        user's may determine the frame-id and name to associated
C        with a planetary object.
C
C-    SPICELIB Version 2.0.0, 04-APR-1997 (WLT)
C
C        The routine was upgraded to reflect that a block of
C        frame ID codes have been reserved for use by the DSN.
C        ID codes 13001 to 13999 have been set aside for DSN
C        models for the orientation of the earth. These frames
C        are all PCK frames. Moreover, the PCK ID code to
C        use with these frames is simply the Frame-Code minus 10000.
C        All of these frames are centered at the earth (body 399).
C
C-    SPICELIB Version 1.1.0, 14-OCT-1996 (WLT)
C
C       The values NINERT and NNINRT are included instead of
C       being declared locally.
C
C-    SPICELIB Version 1.0.0, 18-SEP-1995 (WLT)
C
C-&
 
C$ Index_Entries
C
C     Frame Transformation
C
C-&
 
C
C     SPICELIB Functions
C
      INTEGER               BSCHOI

      LOGICAL               FAILED
      LOGICAL               RETURN
 
C
C     Local parameters
C
      INTEGER               NON
      PARAMETER           ( NON    = NNINRT )
 
      INTEGER               NCOUNT
      PARAMETER           ( NCOUNT = NINERT + NON )
 
      INTEGER               ROOM
      PARAMETER           ( ROOM   = 8 )
 
      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

C
C     Body name length. The value BDNMLN used here must be the 
C     same as the value of MAXL defined in the INCLUDE file
C
C        zzbodtrn.inc
C
C     Current value of MAXL = 36.
C
      INTEGER               BDNMLN
      PARAMETER           ( BDNMLN = 36 )

C
C     Frame name length.
C
      INTEGER               FRNMLN
      PARAMETER           ( FRNMLN = 32 )

C
C     Kernel variable name length.
C
      INTEGER               KVNMLN
      PARAMETER           ( KVNMLN = 32 )

C
C     Kernel variable buffer size.
C
      INTEGER               KVBSZ
      PARAMETER           ( KVBSZ  = 100 )

C
C     Local Variables
C
      CHARACTER*(1)         DATTYP

      CHARACTER*(BDNMLN)    LCNAME

      CHARACTER*(FRNMLN)    LCFRAM
      CHARACTER*(FRNMLN)    NAME   ( NCOUNT )
      CHARACTER*(FRNMLN)    PNAME

      CHARACTER*(KVNMLN)    KVBUFF ( KVBSZ )
      CHARACTER*(KVNMLN)    LOOK2
      CHARACTER*(KVNMLN)    LOOKUP

      CHARACTER*(LNSIZE)    LINE   ( ROOM )
 
      INTEGER               CENTER ( NCOUNT )
      INTEGER               CENTRD ( NCOUNT )
      INTEGER               I
      INTEGER               ID
      INTEGER               IDCODE ( NCOUNT )
      INTEGER               ITEM
      INTEGER               KVCLID
      INTEGER               KVCLSS
      INTEGER               N
      INTEGER               START
      INTEGER               TYPE   ( NCOUNT )
      INTEGER               TYPEID ( NCOUNT )
      INTEGER               VALUES ( ROOM )

      LOGICAL               FIRST
      LOGICAL               FND
      LOGICAL               GOTIT

C
C     POOL state counter.
C
      INTEGER               PULCTR ( CTRSIZ )

C
C     Lower bound of collision lists in hashes.
C
      INTEGER               LBPOOL
      PARAMETER           ( LBPOOL = -5 )

C
C     The size of ID-based hash for kernel POOL frames. 
C
C     Since defining a valid kernel POOL frame takes at least 5
C     keywords and integer hash dimension must be a prime number, this
C     size should be set to the first prime number greater than POOL's
C     MAXVAR / 5 + 1. 
C
C     For the current POOL MAXVAR set to 26003, such number is 5209.
C
      INTEGER               MAXKFR
      PARAMETER           ( MAXKFR = 5209 )
      
C
C     Name-based hash for kernel pool frames. KNMLST, KNMPOL, and
C     KNMNMS provide an index in the frame ID array KNMIDS at which the
C     ID for the frame with a given name is stored.
C
      INTEGER               KNMLST  (          MAXKFR )
      INTEGER               KNMPOL  ( LBPOOL : MAXKFR )
      CHARACTER*(FRNMLN)    KNMNMS  (          MAXKFR )
      INTEGER               KNMIDS  (          MAXKFR )

C
C     ID-based hash for kernel pool frames. KIDLST, KIDPOL, and KIDIDS
C     provide the index in the kernel frame attiribut arrays KNAME,
C     KCENT, KCLASS, and KCLSID at which the attributes of the frame 
C     with a given ID are stored.
C
      INTEGER               KIDLST  (          MAXKFR )
      INTEGER               KIDPOL  ( LBPOOL : MAXKFR )
      INTEGER               KIDIDS  (          MAXKFR )

      CHARACTER*(FRNMLN)    KNAME   (          MAXKFR )
      INTEGER               KCENT   (          MAXKFR )
      INTEGER               KCLASS  (          MAXKFR )
      INTEGER               KCLSID  (          MAXKFR )

      LOGICAL               LUPDTE
      LOGICAL               LNEW

C
C     The size of hashes for built-in frames.
C
C     Since integer hash dimension must be a prime number it cannot be
C     computed in a parameter statement from the inertial and
C     non-inertial frame counts provided in the include files. Instead
C     it should be set manually to the first prime number greater than
C     or equal to NCOUNT. 
C
C     For the current NCOUNT equal to 126, such number is 127.
C
      INTEGER               MAXBFR
      PARAMETER           ( MAXBFR = 127 )

C
C     Name-based hash for built-in frames. BNMLST, BNMPOL, and BNMNMS
C     provide the index in BNMIDX which stores the index for the frame
C     attributes in the built-in frame attiributes arrays IDCODE, NAME,
C     CENTER, TYPE, and TYPEID.
C
      INTEGER               BNMLST  (          MAXBFR )
      INTEGER               BNMPOL  ( LBPOOL : MAXBFR )
      CHARACTER*(FRNMLN)    BNMNMS  (          MAXBFR )
      INTEGER               BNMIDX  (          MAXBFR )

C
C     ID-based hash for built-in frames. BIDLST, BIDPOL, and BIDIDS
C     provide an index in BIDIDX which stores the index for the frame
C     attributes in the built-in frame attiributes arrays IDCODE, NAME,
C     CENTER, TYPE, and TYPEID.
C
      INTEGER               BIDLST  (          MAXBFR )
      INTEGER               BIDPOL  ( LBPOOL : MAXBFR )
      INTEGER               BIDIDS  (          MAXBFR )
      INTEGER               BIDIDX  (          MAXBFR )

C
C     Saved variables
C
C     Because we need to save almost everything we save everything
C     rather than taking a chance and accidentally leaving something
C     off the list.
C
      SAVE

C
C     Initial values
C
 
      DATA                  FIRST  / .TRUE. /
 
      CALL CHKIN  ( 'FRAMEX' )
      CALL SETMSG ( 'A call has been made to the umbrella '
     .//            'routine FRAMEX. This routine doesn''t do '
     .//            'anything. It acts only as an umbrella '
     .//            'routine for its entry points. This call '
     .//            'probably indicates a misunderstanding in '
     .//            'programming. ' )
      CALL SIGERR ( 'SPICE(BOGUSENTRY)' )
      CALL CHKOUT ( 'FRAMEX' )
      RETURN
 
 
 
C$Procedure NAMFRM ( frame NAMe to FRaMe id )
 
      ENTRY NAMFRM ( FRNAME, FRCODE )
 
C$ Abstract
C
C     Look up the frame ID code associated with a string.
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
C     FRAMES
C
C$ Keywords
C
C     FRAMES
C
C$ Declarations
C
C     CHARACTER*(*)         FRNAME
C     INTEGER               FRCODE
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     FRNAME     I   The name of some reference frame
C     FRCODE     O   The SPICE ID code of the frame.
C
C$ Detailed_Input
C
C     FRNAME      is a character string that stands for some
C                 reference frame (either inertial or non-inertial).
C
C                 Leading blanks in FRNAME are ignored. And the
C                 case of the letters in FRNAME are insignificant.
C
C                 Note that all legitimate frame names contain
C                 26 or fewer characters.
C
C$ Detailed_Output
C
C     FRCODE      is the SPICE integer code used for internal
C                 representation of the named reference frame.
C
C                 If the name input through FRNAME is not recognized
C                 FRCODE will be returned with a value of zero.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If the input name is not recognized, FRCODE will be
C        returned with a value of 0.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This is a low level interface routine intended primarily for
C     use within the SPK and CK systems to assist in the transformation
C     to user specified reference frames.
C
C     The routine first consults a stored list of reference frame
C     names in an attempt to determine the appropriate reference
C     frame code.
C
C     If this search is unsuccessful, the routine then examines the
C     kernel pool to determine whether or not a variable of the
C     form
C
C        'FRAME_' // FRNAME
C
C        (where leading blanks of FRNAME are ignored)
C
C     is present. If it is and the number of values associated with the
C     name is 1, this value is taken to be the frame ID code.
C
C     Note: It is NOT possible to override the default names and
C     ID codes stored locally in this routine by placing an
C     appropriately variable in the kernel pool with a different
C     ID code. The predefined values always take precedence.
C
C     Consult the FRAMES required reading document for more details
C     about constructing your own frame definitions.
C
C$ Examples
C
C     Suppose that you needed to find the SPICE ID code for the
C     bodyfixed reference frame for Mars as modeled by the
C     IAU cartographic working group. Use the following code
C     to perform this task.
C
C        CALL NAMFRM ( 'IAU_MARS', FRCODE )
C
C        WRITE (*,*) 'The SPICE code for the Mars bodyfixed frame is: ',
C       .             FRCODE.
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
C     W.L. Taber      (JPL)
C     B.V. Semenov    (JPL) 
C     E.D. Wright     (JPL)
C
C$ Version
C
C-    SPICELIB Version 3.1.0, 08-AUG-2012 (BVS) 
C
C        The routine was updated to be more efficient by using hashes
C        instead kernel POOL look-ups for kernel POOL frames and by
C        using hases instead of ordered array searches for built-in
C        frames.
C
C-    SPICELIB Version 3.0.2, 17-MAR-2009 (EDW) 
C
C        Typo correction in Required_Reading, changed FRAME to FRAMES.
C
C-    SPICELIB Version 3.0.1, 25-JUN-1999 (WLT)
C
C        Extended documentation of entry point CNMFRM and
C        corrected example for that entry point.
C
C-    SPICELIB Version 3.0.0, 03-JUN-1997 (WLT)
C
C        The entry points CIDFRM and CNMFRM were added so that
C        user's may determine the frame-id and name to associated
C        with a planetary object.
C
C-    SPICELIB Version 2.0.0, 04-APR-1997 (WLT)
C
C        The routine was upgraded to reflect that a block of
C        frame ID codes have been reserved for use by the DSN.
C        ID codes 13001 to 13999 have been set aside for DSN
C        models for the orientation of the earth. These frames
C        are all PCK frames. Moreover, the PCK ID code to
C        use with these frames is simply the Frame-Code minus 10000.
C        All of these frames are centered at the earth (body 399).
C
C-    SPICELIB Version 1.1.0, 14-OCT-1996 (WLT)
C
C       The values NINERT and NNINRT are included instead of
C       being declared locally.
C
C-    SPICELIB Version 1.0.0, 18-SEP-1995 (WLT)
C
C-&
 
C$ Index_Entries
C
C     Frame name to frame ID code translation
C
C-&
 
      FRCODE = 0
C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      END IF

C
C     For efficiency, J2000 deserves special treatment.
C
      IF ( FRNAME .EQ. 'J2000' .OR. FRNAME .EQ. 'j2000') THEN
         FRCODE = 1
         RETURN
      END IF
 
      CALL CHKIN ( 'NAMFRM' )
 
C
C     Perform any needed first pass initializations.
C
      IF ( FIRST ) THEN
 
C
C        Initialize POOL state counter to the user value. 
C 
         CALL ZZCTRUIN ( PULCTR )

C
C        Initialize kernel POOL frame hashes.
C
         CALL ZZHSIINI ( MAXKFR, KIDLST, KIDPOL )
         CALL ZZHSCINI ( MAXKFR, KNMLST, KNMPOL )

C
C        Initialize built-in frame tables and hashes.
C
         CALL ZZFDAT   ( NCOUNT, MAXBFR, NAME,   IDCODE, 
     .                   CENTER, TYPE,   TYPEID, CENTRD,  
     .                   BNMLST, BNMPOL, BNMNMS, BNMIDX,
     .                   BIDLST, BIDPOL, BIDIDS, BIDIDX )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'NAMFRM' )
            RETURN
         END IF

         FIRST = .FALSE.

      END IF

C
C     Determine the location of the requested item in the array
C     of names.
C
      CALL LJUST ( FRNAME, PNAME )
      CALL UCASE ( PNAME,  PNAME )
 
      CALL ZZHSCCHK ( BNMLST, BNMPOL, BNMNMS, PNAME, ITEM )
      IF ( ITEM .NE. 0 ) THEN
         ITEM = BNMIDX( ITEM )
      END IF

C
C     If the name is in our hash, we can just look up its ID code in
C     the parallel array.
C
      IF ( ITEM .GT. 0 ) THEN
 
         FRCODE = IDCODE ( ITEM )
 
      ELSE

C
C        See if this frame is in the kernel pool frame name-based hash.
C        First reset the hash if POOL has changed.
C
         CALL ZZPCTRCK ( PULCTR, LUPDTE )

         IF ( LUPDTE ) THEN
            CALL ZZHSCINI ( MAXKFR, KNMLST, KNMPOL )
            CALL ZZHSIINI ( MAXKFR, KIDLST, KIDPOL )
         END IF

C
C        Check if this name is in the hash.
C
         CALL ZZHSCCHK ( KNMLST, KNMPOL, KNMNMS, PNAME, ITEM )

         IF ( ITEM .NE. 0 ) THEN
            
            FRCODE = KNMIDS ( ITEM ) 

         ELSE

C
C           The name wasn't in the hash, see if we can find this frame
C           in the kernel pool.
C
            CALL PREFIX ( 'FRAME_', 0,                  PNAME )
            CALL GIPOOL (  PNAME,   1, ROOM, N, VALUES, GOTIT )
 
            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'NAMFRM' )
               RETURN
            END IF

            IF ( N .EQ. 1 .AND. GOTIT ) THEN
               
               FRCODE = VALUES(1)
 
C
C              If we made it to this point, we successfully mapped the
C              kernel frame name to its ID. Add this pair to the
C              name-based hash.
C              
               CALL ZZHSCADD ( KNMLST, KNMPOL, KNMNMS, PNAME, ITEM, 
     .                                                        LNEW )

               IF ( .NOT. FAILED() .AND. ITEM .NE. 0 ) THEN

                  KNMIDS ( ITEM ) = FRCODE

               END IF
 
            ELSE
 
               FRCODE = 0
 
            END IF
 
         END IF
         
      END IF
 
      CALL CHKOUT ( 'NAMFRM' )
      RETURN
 
 
C$Procedure FRMNAM ( FRaMe id to frame NAMe )
 
      ENTRY FRMNAM ( FRCODE, FRNAME )
 
C$ Abstract
C
C     Retrieve the name of a reference frame associated with a SPICE ID
C     code.
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
C     FRAMES
C
C$ Keywords
C
C     FRAMES
C
C$ Declarations
C
C     INTEGER               FRCODE
C     CHARACTER*(*)         FRNAME
C
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     FRCODE     I   an integer code for a reference frame
C     FRNAME     O   the name associated with the reference frame.
C
C$ Detailed_Input
C
C     FRCODE      is an integer code for a reference frame.
C
C$ Detailed_Output
C
C     FRNAME      is the name associated with the reference frame.
C                 It will be returned left justified.
C
C                 If FRCODE is not recognized as the name of a
C                 known reference frame FRNAME will be returned
C                 as a blank.
C
C                 If FRNAME is not sufficiently long to hold the
C                 name, it will be truncated on the right.
C
C                 All reference frame names are 26 or fewer characters
C                 in length. Thus declaring FRNAME to be CHARACTER*(26)
C                 will ensure that the returned name will not be
C                 truncated.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If FRCODE is not recognized as the name of a
C        known reference frame FRNAME will be returned
C        as a blank.
C
C     2) If FRNAME is not sufficiently long to hold the
C        name, it will be truncated on the right.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine retrieves the name of a reference frame associated
C     with a SPICE frame ID code.
C
C     The ID codes stored locally are scanned for a match with FRCODE.
C     If a match is found, the name stored locally will be returned
C     as the name for the frame.
C
C     If FRCODE is not a member of the list of internally stored
C     ID codes, the kernel pool will be examined to see if the
C     variable
C
C        FRAME_idcode_NAME
C
C     is present (where idcode is the decimal character equivalent
C     of FRCODE). If the variable is located and it has both
C     character type and dimension 1, the string value of the
C     kernel pool variable is returned as the name of the reference
C     frame.
C
C     Note that because the local information is always examined
C     first and searches of the kernel pool are performed only
C     after exhausting local information, it is not possible to
C     override the local name for any reference frame that is
C     known by this routine.
C
C$ Examples
C
C     Suppose you needed to create a message concerning a reference
C     frame and wish to use the name of the frame in the message.
C     Suppose further that you have only the frame ID code at your
C     disposal. You can capture the frame name using this routine
C     as shown here.
C
C        CHARACTER*(26)        FRNAME
C
C        CALL FRMNAM ( FRCODE, FRNAME )
C
C        IF ( FRNAME .EQ. ' ' ) THEN
C           CALL INTSTR ( FRCODE, FRNAME )
C        END IF
C
C        WRITE (*,*) 'Concerning reference frame:', FRNAME
C
C        print the rest of your message.
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
C     W.L. Taber      (JPL)
C     B.V. Semenov    (JPL) 
C
C$ Version
C
C-    SPICELIB Version 3.1.0, 08-AUG-2012 (BVS) 
C
C        The routine was updated to be more efficient by using hashes
C        instead kernel POOL look-ups for kernel POOL frames and by
C        using hases instead of ordered array searches for built-in
C        frames.
C
C-    SPICELIB Version 3.0.1, 25-JUN-1999 (WLT)
C
C        Extended documentation of entry point CNMFRM and
C        corrected example for that entry point.
C
C-    SPICELIB Version 3.0.0, 03-JUN-1997 (WLT)
C
C        The entry points CIDFRM and CNMFRM were added so that
C        user's may determine the frame-id and name to associated
C        with a planetary object.
C
C-    SPICELIB Version 2.0.0, 04-APR-1997 (WLT)
C
C        The routine was upgraded to reflect that a block of
C        frame ID codes have been reserved for use by the DSN.
C        ID codes 13001 to 13999 have been set aside for DSN
C        models for the orientation of the earth. These frames
C        are all PCK frames. Moreover, the PCK ID code to
C        use with these frames is simply the Frame-Code minus 10000.
C        All of these frames are centered at the earth (body 399).
C
C-    SPICELIB Version 1.1.0, 14-OCT-1996 (WLT)
C
C       The values NINERT and NNINRT are included instead of
C       being declared locally.
C
C-    SPICELIB Version 1.0.0, 18-SEP-1995 (WLT)
C
C-&
 
C$ Index_Entries
C
C     Frame ID code to frame name translation
C
C-&
 
C
C     Standard SPICE error handling.
C
      FRNAME = ' '
 
      IF ( RETURN() ) THEN
         RETURN
      END IF
 
C
C     For efficiency, J2000 deserves special treatment.
C
      IF ( FRCODE .EQ. 1 ) THEN
         FRNAME = 'J2000'
         RETURN
      END IF

      CALL CHKIN ( 'FRMNAM' )
 
C
C     Perform any needed first pass initializations.
C
      IF ( FIRST ) THEN
 
C
C        Initialize POOL state counter to the user value. 
C 
         CALL ZZCTRUIN ( PULCTR )

C
C        Initialize kernel POOL frame hashes.
C
         CALL ZZHSIINI ( MAXKFR, KIDLST, KIDPOL )
         CALL ZZHSCINI ( MAXKFR, KNMLST, KNMPOL )

C
C        Initialize built-in frame tables and hashes.
C
         CALL ZZFDAT   ( NCOUNT, MAXBFR, NAME,   IDCODE, 
     .                   CENTER, TYPE,   TYPEID, CENTRD,  
     .                   BNMLST, BNMPOL, BNMNMS, BNMIDX,
     .                   BIDLST, BIDPOL, BIDIDS, BIDIDX )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'FRMNAM' )
            RETURN
         END IF

         FIRST = .FALSE.

      END IF

 
      CALL ZZHSICHK ( BIDLST, BIDPOL, BIDIDS, FRCODE, ITEM )
      IF ( ITEM .NE. 0 ) THEN
         ITEM = BIDIDX( ITEM )
      END IF

      IF ( ITEM .NE. 0 ) THEN
 
         FRNAME = NAME(ITEM)
 
      ELSE
 
C
C        See if this frame is in the kernel pool frame ID-based hash.
C        First reset the hash if POOL has changed.
C
         CALL ZZPCTRCK ( PULCTR, LUPDTE )

         IF ( LUPDTE ) THEN
            CALL ZZHSCINI ( MAXKFR, KNMLST, KNMPOL )
            CALL ZZHSIINI ( MAXKFR, KIDLST, KIDPOL )
         END IF

C
C        Check if this ID is in the hash.
C
         CALL ZZHSICHK ( KIDLST, KIDPOL, KIDIDS, FRCODE, ITEM )
         
         IF ( ITEM .NE. 0 ) THEN

            FRNAME = KNAME ( ITEM )

         ELSE

C
C           The ID wasn't in the hash, see if we can find this frame in
C           the kernel pool.
C
            PNAME      = 'FRAME_#_NAME'
            CALL REPMI (  PNAME, '#', FRCODE, PNAME )
 
            CALL GCPOOL ( PNAME, 1, ROOM, N, LINE, GOTIT )
 
            IF ( N .EQ. 1 .AND. GOTIT ) THEN
 
               CALL LJUST ( LINE(1), FRNAME )

C
C              Note that since we did not collect all needed
C              information about this frame, we will not try to add it
C              to the hash. This addition is done only by FRINFO.
C

            ELSE
 
               FRNAME = ' '
 
            END IF

         END IF
            
      END IF
 
      CALL CHKOUT ( 'FRMNAM' )
      RETURN
 
 
 
C$Procedure FRINFO ( FRame INFOrmation )
 
      ENTRY FRINFO ( FRCODE, CENT, CLASS, CLSSID, FOUND )
 
C$ Abstract
C
C     Retrieve the minimal attributes associated with a frame
C     needed for converting transformations to and from it.
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
C     FRAMES
C
C$ Keywords
C
C     FRAMES
C
C$ Declarations
C
C     IMPLICIT NONE
C     INTEGER               FRCODE
C     INTEGER               CENT
C     INTEGER               CLASS
C     INTEGER               CLSSID
C     LOGICAL               FOUND
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     FRCODE     I   the ID code for some frame
C     CENT       O   the center of the frame
C     CLASS      O   the class (type) of the frame
C     CLSSID     O   the ID code for the frame within its class.
C     FOUND      O   TRUE if the requested information is available.
C
C$ Detailed_Input
C
C     FRCODE      is the ID code for some reference frame.
C
C$ Detailed_Output
C
C     CENT        is the body ID code for the center of the reference
C                 frame (if such an ID code is appropriate).
C
C     CLASS       is the class or type of the frame. This identifies
C                 which subsystem will be used to perform frame
C                 transformations.
C
C     CLSSID      is the ID code used for the frame within its class.
C                 This may be different from the frame ID code.
C
C     FOUND       is TRUE if CENT, CLASS and CCODE are available.
C                 Otherwise, FOUND is returned with the value FALSE.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If a frame definition is encountered that does not define
C        a central body for the frame, the error will be diagnosed
C        by routines in the call tree of this routine.
C
C     2) If a frame definition is encountered that does not define
C        a class for the frame, the error will be diagnosed by routines
C        in the call tree of this routine.
C
C     3) If a frame definition is encountered that does not define
C        a class ID for the frame, the error will be diagnosed by
C        routines in the call tree of this routine.
C
C     4) If a kernel variable defining a frame name is found, but
C        that variable has dimension greater than 1, the error
C        SPICE(INVALIDDIMENSION) will be signaled.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This is a low level routine needed by state transformation
C     software to transform states and attitudes between different
C     reference frames.
C
C     The routine first examines local "hard-coded" information about
C     reference frames to see if the requested frame belongs to this
C     set. If it does that information is returned.
C
C     If the requested information is not stored locally, the routine
C     then examines the kernel pool to see if the requested information
C     is stored there. If it is and has the expected format, the data
C     is retrieved and returned.
C
C$ Examples
C
C     Suppose that you needed to determine the center of some
C     reference frame. The following code fragment illustrates
C     how to use this routine to determine this information.
C
C        CALL FRINFO ( FRCODE, CENT, CLASS, CLSSID, FOUND )
C
C        IF ( FOUND ) THEN
C
C           WRITE (*,*) 'The center of reference frame ', FRCODE
C           WRITE (*,*) 'has body ID code : ', CENT
C
C        ELSE
C
C           WRITE (*,*) 'There is insufficient data for frame ', FRCODE
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
C     N.J. Bachman    (JPL)
C     B.V. Semenov    (JPL)
C     W.L. Taber      (JPL)
C
C$ Version
C
C-    SPICELIB Version 4.1.0, 08-AUG-2012 (BVS) 
C
C        The routine was updated to be more efficient by using hashes
C        instead kernel POOL look-ups for kernel POOL frames and by
C        using hases instead of ordered array searches for built-in
C        frames.
C
C-    SPICELIB Version 4.0.0, 12-SEP-2005 (NJB)
C
C        Entry point FRINFO is no longer error-free. The following
C        errors are now diagnosed:
C
C           - Invalid dimension of frame name variable
C
C           - If a valid frame name assignment is present:
C
C              + Missing frame ID code assignment
C              + Missing class assignment
C              + Missing class ID assignment
C
C        Specification of frame center by name or ID is now supported.
C        Previously only ID codes could be used to identify frame
C        centers. Various frame definition errors that were previously
C        ignored are now diagnosed.      
C
C-    SPICELIB Version 3.1.0, 20-DEC-2004 (BVS)
C
C        Removed special handling of the frame IDs less than -999. If
C        they cannot be ``resolved'' using kernel pool keywords, the
C        frame is NOT declared CK-based with center ID derived by
C        dividing frame ID by a 1000 and class ID assigned the frame ID
C        anymore. In the current practice with multitude of TK frames
C        with IDs set instrument IDs this default behavior is simply
C        not valid.
C
C-    SPICELIB Version 3.0.1, 25-JUN-1999 (WLT)
C
C        Extended documentation of entry point CNMFRM and
C        corrected example for that entry point.
C
C-    SPICELIB Version 3.0.0, 03-JUN-1997 (WLT)
C
C        The entry points CIDFRM and CNMFRM were added so that
C        user's may determine the frame-id and name to associated
C        with a planetary object.
C
C-    SPICELIB Version 2.0.0, 04-APR-1997 (WLT)
C
C        The routine was upgraded to reflect that a block of
C        frame ID codes have been reserved for use by the DSN.
C        ID codes 13001 to 13999 have been set aside for DSN
C        models for the orientation of the earth. These frames
C        are all PCK frames. Moreover, the PCK ID code to
C        use with these frames is simply the Frame-Code minus 10000.
C        All of these frames are centered at the earth (body 399).
C
C-    SPICELIB Version 1.1.0, 14-OCT-1996 (WLT)
C
C       The values NINERT and NNINRT are included instead of
C       being declared locally.
C
C-    SPICELIB Version 1.0.0, 18-SEP-1995 (WLT)
C
C-&
 
C$ Index_Entries
C
C     Fetch reference frame attributes
C
C-&

C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      END IF
 
C
C     For efficiency, J2000 deserves special treatment.
C
      IF ( FRCODE .EQ. 1 ) THEN
         CENT   =  0
         CLASS  =  INERTL
         CLSSID =  1
         FOUND  = .TRUE.
         RETURN
      END IF

      CALL CHKIN ( 'FRINFO' )

C
C     Perform any needed first pass initializations.
C
      IF ( FIRST ) THEN
 
C
C        Initialize POOL state counter to the user value. 
C 
         CALL ZZCTRUIN ( PULCTR )

C
C        Initialize kernel POOL frame hashes.
C
         CALL ZZHSIINI ( MAXKFR, KIDLST, KIDPOL )
         CALL ZZHSCINI ( MAXKFR, KNMLST, KNMPOL )

C
C        Initialize built-in frame tables and hashes.
C
         CALL ZZFDAT   ( NCOUNT, MAXBFR, NAME,   IDCODE, 
     .                   CENTER, TYPE,   TYPEID, CENTRD,  
     .                   BNMLST, BNMPOL, BNMNMS, BNMIDX,
     .                   BIDLST, BIDPOL, BIDIDS, BIDIDX )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'FRINFO' )
            RETURN
         END IF

         FIRST = .FALSE.

      END IF

C
C     No frame information has been found yet.
C
      FOUND = .FALSE.
 
C
C     Determine the location of the requested item in the array
C     of ID codes.
C
      CALL ZZHSICHK ( BIDLST, BIDPOL, BIDIDS, FRCODE, ITEM )
      IF ( ITEM .NE. 0 ) THEN
         ITEM = BIDIDX( ITEM )
      END IF

C
C     If the name is in our hash, we can just look up its ID code in
C     the parallel array.
C
      IF ( ITEM .GT. 0 ) THEN
 
         CENT   =  CENTER(ITEM)
         CLASS  =  TYPE  (ITEM)
         CLSSID =  TYPEID(ITEM)
         FOUND  = .TRUE.
 
      ELSE

C
C        See if this frame is in the kernel pool frame ID-based hash.
C        First reset the hash if POOL has changed.
C
         CALL ZZPCTRCK ( PULCTR, LUPDTE )

         IF ( LUPDTE ) THEN
            CALL ZZHSCINI ( MAXKFR, KNMLST, KNMPOL )
            CALL ZZHSIINI ( MAXKFR, KIDLST, KIDPOL )
         END IF

C
C        Check if this ID is in the hash.
C
         CALL ZZHSICHK ( KIDLST, KIDPOL, KIDIDS, FRCODE, ITEM )
         
         IF ( ITEM .NE. 0 ) THEN

            CENT   =  KCENT  (ITEM)
            CLASS  =  KCLASS (ITEM)
            CLSSID =  KCLSID (ITEM)
            FOUND  = .TRUE.

         ELSE

C
C           The ID wasn't in the hash, see if we can find this frame in
C           the kernel pool.
C
            PNAME     = 'FRAME_#_NAME'
            CALL REPMI (  PNAME, '#', FRCODE, PNAME )
 
            CALL GCPOOL ( PNAME, 1, ROOM, N, LINE, GOTIT )
 
            IF ( GOTIT ) THEN

               IF ( N .GT. 1 ) THEN
C
C                 We have an array-valued variable that looks like
C                 a frame name. We consider this an error.
C
                  CALL SETMSG ( 'Kernel variable # is array-valued; ' //
     .                          'Frame name variables must be '       //
     .                          'scalar-valued.'                      )
                  CALL ERRCH  ( '#', PNAME                            )
                  CALL SIGERR ( 'SPICE(INVALIDDIMENSION)'             )
                  CALL CHKOUT ( 'FRINFO'                              )
                  RETURN

               END IF
 
               CALL LJUST ( LINE(1), LCFRAM )
C      
C              Start by looking up the central body of the frame. The
C              name of the kernel variable for the body could refer to
C              the frame by name or frame ID; the body itself could be
C              specified by name or body ID.
C
               CALL ZZDYNBID ( LCFRAM, FRCODE, 'CENTER', CENT )

               IF ( FAILED() ) THEN
                  CALL CHKOUT ( 'FRINFO' )
                  RETURN
               END IF

               FOUND = .TRUE.
 
C
C              FOUND has been set to indicate whether we found the
C              frame's center. If we did, CENT has been assigned.
C
C              Next look up the frame class and class ID.
C
               CALL ZZDYNVAI ( LCFRAM, FRCODE, 'CLASS',    1, N, VALUES)
               CLASS  = VALUES(1)

               CALL ZZDYNVAI ( LCFRAM, FRCODE, 'CLASS_ID', 1, N, VALUES)
               CLSSID = VALUES(1)

               IF ( FAILED() ) THEN
                  CALL CHKOUT ( 'FRINFO' )
                  RETURN
               END IF
 

C              If we made it to this point, we successfully collected
C              all items for this frame. Add this frame to the 
C              ID-based hash.
C              
               CALL ZZHSIADD ( KIDLST, KIDPOL, KIDIDS, FRCODE, ITEM, 
     .                                                         LNEW )

               IF ( .NOT. FAILED() .AND. ITEM .NE. 0 ) THEN

                  KNAME  (ITEM) = LCFRAM
                  KCENT  (ITEM) = CENT
                  KCLASS (ITEM) = CLASS
                  KCLSID (ITEM) = CLSSID

C
C                 Also, try to add this frame to the name-based hash.
C                  
                  CALL ZZHSCADD ( KNMLST, KNMPOL, KNMNMS, LCFRAM, 
     .                                                 ITEM, LNEW )

                  IF ( .NOT. FAILED() .AND. ITEM .NE. 0 ) THEN

                     KNMIDS ( ITEM ) = FRCODE

                  END IF
 
               END IF
 
            END IF

         END IF

C
C        In support of the DSN, NAIF has reserved a block of
C        ID codes for DSN specific frames  from 13000 to 13999.
C        These are always PCK based frames for the earth.
C        The PCK ID code is just FRCODE - 10000.
C
         IF      ( .NOT. FOUND
     .             .AND. FRCODE .GE. 13000
     .             .AND. FRCODE .LT. 14000 ) THEN
 
            CENT   = 399
            CLASS  = PCK
            CLSSID = FRCODE - 10000
            FOUND  = .TRUE.
 
         END IF
 
      END IF
 
      CALL CHKOUT ( 'FRINFO' )
      RETURN
 
 
C$Procedure CIDFRM ( Center ID to FRaMe id and name )
 
      ENTRY CIDFRM ( CENT, FRCODE, FRNAME, FOUND )
 
C$ Abstract
C
C     Retrieve frame ID code and name to associate with a frame center.
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
C     FRAMES
C
C$ Keywords
C
C     FRAMES
C
C$ Declarations
C
C     IMPLICIT NONE
C     INTEGER               CENT
C     INTEGER               FRCODE
C     CHARACTER*(*)         FRNAME
C     LOGICAL               FOUND
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     CENT       I   an object to associate a frame with.
C     FRCODE     O   the ID code of the frame associated with CENT
C     FRNAME     O   the name of the frame with ID FRCODE
C     FOUND      O   TRUE if the requested information is available.
C
C$ Detailed_Input
C
C     CENT        is the ID code for object for which there is a
C                 preferred reference frame.
C
C$ Detailed_Output
C
C     FRCODE      is the frame ID code to associate with the object
C                 specified by CENT.
C
C     FRNAME      is the name of the frame that should be associated
C                 with the object specified by CNAME. FRNAME should be
C                 declared as CHARACTER*(26) to ensure that it can
C                 contain the full name of the frame. If FRNAME does
C                 not have enough room to hold the full name of the
C                 frame, the name will be truncated on the right.
C
C     FOUND       is TRUE if the appropriate frame ID code and frame
C                 name can be determined. Otherwise FOUND is returned
C                 with the value FALSE.
C
C$ Parameters
C
C      None.
C
C$ Exceptions
C
C     1) If FRNAME is not have room to contain the frame name, the name
C        will be truncated on the right. ( Declaring FRNAME to be
C        CHARACTER*(26) will ensure that the name will not be
C        truncated.
C
C$ Files
C
C      None.
C
C$ Particulars
C
C     This routine allows the user to determine the frame that should
C     be associated with a particular object. For example, if you
C     need the frame to associate with the Io, you can call CIDFRM
C     to determine the frame name and ID code for the bodyfixed frame
C     of Io.
C
C     The preferred frame to use with an object is specified via one
C     of the kernel pool variables:
C
C         OBJECT_<cent>_FRAME
C
C     where <cent> is the decimal representation of the integer CENT.
C
C     For those PCK objects that have "built-in" frame names this
C     routine returns the corresponding "IAU" frame and frame ID code.
C
C$ Examples
C
C     Suppose that you want to determine the state of a target
C     in the preferred reference frame of some observer. This
C     routine can be used in conjunction with SPKEZ to compute
C     the state.
C
C        CALL CIDFRM ( OBS, FRCODE, FRNAME, FOUND )
C
C        IF ( .NOT. FOUND ) THEN
C
C           WRITE (*,*) 'The bodyfixed frame for object ', OBS
C           WRITE (*,*) 'could not be identified.'
C           STOP
C
C        END IF
C
C        CALL SPKEZ ( TARG, ET, FRNAME, ABCORR, OBS, STATE, LT )
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
C     N.J. Bachman    (JPL)
C     B.V. Semenov    (JPL)
C     W.L. Taber      (JPL)
C
C$ Version
C
C-    SPICELIB Version 3.2.0, 08-AUG-2012 (BVS) 
C
C        The routine was updated to be more efficient by using hashes
C        instead kernel POOL look-ups for kernel POOL frames and by
C        using hases instead of ordered array searches for built-in
C        frames.
C
C-    SPICELIB Version 3.1.1, 09-FEB-2011 (NJB) 
C
C        Bug fix: corrected logic for object-frame association for case
C        where the assigned frame value is denoted by a frame code.
C
C-    SPICELIB Version 3.0.1, 25-JUN-1999 (WLT)
C
C        Extended documentation of entry point CNMFRM and
C        corrected example for that entry point.
C
C-    SPICELIB Version 3.0.0, 03-JUN-1997 (WLT)
C
C        The entry points CIDFRM and CNMFRM were added so that
C        user's may determine the frame-id and name to associated
C        with a planetary object.
C
C-    SPICELIB Version 2.0.0, 04-APR-1997 (WLT)
C
C        The routine was upgraded to reflect that a block of
C        frame ID codes have been reserved for use by the DSN.
C        ID codes 13001 to 13999 have been set aside for DSN
C        models for the orientation of the earth. These frames
C        are all PCK frames. Moreover, the PCK ID code to
C        use with these frames is simply the Frame-Code minus 10000.
C        All of these frames are centered at the earth (body 399).
C
C-    SPICELIB Version 1.1.0, 14-OCT-1996 (WLT)
C
C       The values NINERT and NNINRT are included instead of
C       being declared locally.
C
C-    SPICELIB Version 1.0.0, 18-SEP-1995 (WLT)
C
C-&
 
C$ Index_Entries
C
C     Find the bodyfixed frame associated with an object
C
C-&
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      END IF
 
      CALL CHKIN ( 'CIDFRM' )
 
C
C     Perform any needed first pass initializations.
C
      IF ( FIRST ) THEN
 
C
C        Initialize POOL state counter to the user value. 
C 
         CALL ZZCTRUIN ( PULCTR )

C
C        Initialize kernel POOL frame hashes.
C
         CALL ZZHSIINI ( MAXKFR, KIDLST, KIDPOL )
         CALL ZZHSCINI ( MAXKFR, KNMLST, KNMPOL )

C
C        Initialize built-in frame tables and hashes.
C
         CALL ZZFDAT   ( NCOUNT, MAXBFR, NAME,   IDCODE, 
     .                   CENTER, TYPE,   TYPEID, CENTRD,  
     .                   BNMLST, BNMPOL, BNMNMS, BNMIDX,
     .                   BIDLST, BIDPOL, BIDIDS, BIDIDX )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'CIDFRM' )
            RETURN
         END IF

         FIRST = .FALSE.

      END IF

C
C     First look up in the kernel pool the frame associated with this
C     center.
C
      LOOKUP = 'OBJECT_#_FRAME'
 
      CALL REPMI  ( LOOKUP, '#',   CENT, LOOKUP )
      CALL DTPOOL ( LOOKUP, GOTIT, N,    DATTYP )
C
C     If we didn't find this object in the form OBJECT_<number>_FRAME
C     maybe it is present in the form OBJECT_<name>_FRAME. It's
C     worth a try.
C
      IF ( .NOT. GOTIT ) THEN
C
C        See if we can get the name for this center's ID code.
C
         CALL BODC2N ( CENT, LCNAME, GOTIT )
 
         IF ( GOTIT ) THEN
C
C           Construct and look up the alternative name in the
C           kernel pool.
C
            LOOKUP = 'OBJECT_#_FRAME'
 
            CALL REPMC  ( LOOKUP, '#', LCNAME, LOOKUP )
            CALL UCASE  ( LOOKUP,              LOOKUP )
            CALL DTPOOL ( LOOKUP, GOTIT, N,    DATTYP )
 
         END IF
 
      END IF
C
C     There are two cases. The user may specify either a name
C     or ID code for the frame to use to model the orientation of
C     an object. We assume they'll opt for the character string
C     form so we test that case first.
C
      IF ( GOTIT ) THEN
 
         IF ( DATTYP .EQ. 'C' ) THEN
 
            CALL GCPOOL ( LOOKUP, 1, 1, N,  PNAME, GOTIT )
C
C           We've got the name:  See if we have this in our handy hash
C           of built-in names.
C
            CALL ZZHSCCHK ( BNMLST, BNMPOL, BNMNMS, PNAME, ITEM )
            IF ( ITEM .NE. 0 ) THEN
               ITEM = BNMIDX( ITEM )
            END IF
 
            IF ( ITEM .GT. 0 ) THEN
 
               FRNAME = PNAME
               FRCODE = IDCODE ( ITEM )
               FOUND  = .TRUE.
 
            ELSE

C
C              See if this frame is in the kernel pool frame name-based
C              hash. First reset the hash if POOL has changed.
C
               CALL ZZPCTRCK ( PULCTR, LUPDTE )

               IF ( LUPDTE ) THEN
                  CALL ZZHSCINI ( MAXKFR, KNMLST, KNMPOL )
                  CALL ZZHSIINI ( MAXKFR, KIDLST, KIDPOL )
               END IF

C
C              Check if this name is in the hash.
C
               CALL ZZHSCCHK ( KNMLST, KNMPOL, KNMNMS, PNAME, ITEM )

               IF ( ITEM .NE. 0 ) THEN

                  FRNAME = PNAME
                  FRCODE = KNMIDS ( ITEM )
                  FOUND  = .TRUE.

               ELSE

C
C                 Nope. Look in the kernel pool for the data associated
C                 with this frame.
C
C                 Capture the frame name now, since we're going to
C                 modify PNAME.
C              
                  FRNAME = PNAME
 
                  CALL PREFIX ( 'FRAME_', 0,                  PNAME )
                  CALL GIPOOL (  PNAME,   1, ROOM, N, VALUES, GOTIT )
 
                  IF ( FAILED() ) THEN
                     CALL CHKOUT ( 'CIDFRM' )
                     RETURN
                  END IF

                  IF ( N .EQ. 1 .AND. GOTIT ) THEN
 
                     FRCODE = VALUES(1)
                     FOUND  = .TRUE.
 
C
C                    If we made it to this point, we successfully
C                    mapped the kernel frame name to its ID. Add this
C                    pair to the name-based hash.
C              
                     CALL ZZHSCADD ( KNMLST, KNMPOL, KNMNMS, FRNAME, 
     .                                                 ITEM, LNEW )

                     IF ( .NOT. FAILED() .AND. ITEM .NE. 0 ) THEN

                        KNMIDS ( ITEM ) = FRCODE

                     END IF
 
                  ELSE
 
                     FRCODE = 0
                     FRNAME = ' '
                     FOUND  = .FALSE.
 
                  END IF

               END IF
 
            END IF
 
 
         ELSE IF ( DATTYP .EQ. 'N' ) THEN
C
C           Ok. They decided to use the numeric form to specify
C           the frame ID. We need to figure out the name of the frame.
C           First we retrieve the frame ID they've loaded into the
C           kernel pool.
C
            CALL GIPOOL ( LOOKUP, 1, 1, N, VALUES, GOTIT )
 
            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'CIDFRM' )
               RETURN
            END IF

C
C           We've got the frame ID, see if we already know about this
C           ID code.
C
            CALL ZZHSICHK ( BIDLST, BIDPOL, BIDIDS, VALUES(1), ITEM )
            IF ( ITEM .NE. 0 ) THEN
               ITEM = BIDIDX( ITEM )
            END IF

            IF ( ITEM .NE. 0 ) THEN
C
C              Just look up the name and set the frame code.
C
               FRNAME = NAME  (ITEM)
               FRCODE = VALUES(1)
               FOUND  = .TRUE.
 
            ELSE

C
C              See if this frame is in the kernel pool frame ID-based
C              hash. First reset the hash if POOL has changed.
C
               CALL ZZPCTRCK ( PULCTR, LUPDTE )

               IF ( LUPDTE ) THEN
                  CALL ZZHSCINI ( MAXKFR, KNMLST, KNMPOL )
                  CALL ZZHSIINI ( MAXKFR, KIDLST, KIDPOL )
               END IF

C
C              Check if this ID is in the hash.
C
               CALL ZZHSICHK ( KIDLST, KIDPOL, KIDIDS, VALUES(1), ITEM )
         
               IF ( ITEM .NE. 0 ) THEN

                  FRNAME = KNAME ( ITEM )
                  FRCODE = VALUES(1)
                  FOUND  = .TRUE.

               ELSE

C
C                 It is not in the hash. See if it's in the kernel pool
C                 somewhere.
C
                  PNAME      = 'FRAME_#_NAME'
                  CALL REPMI (  PNAME, '#', VALUES(1),   PNAME )
                  CALL GCPOOL ( PNAME, 1, ROOM, N, LINE, GOTIT )
 
                  IF ( N .EQ. 1 .AND. GOTIT ) THEN
 
                     CALL LJUST ( LINE(1), FRNAME )
                     FRCODE = VALUES(1)
                     FOUND  = .TRUE.
 
C
C                    Note that since we did not collect all needed
C                    information about this frame, we will not try to
C                    add it to the hash. This addition is done only by
C                    FRINFO.
C
                  ELSE
 
                     FRCODE = VALUES(1)
                     FRNAME = ' '
                     FOUND  = .FALSE.
 
                  END IF

               END IF
 
            END IF
 
 
         END IF
C
C        One way or the other we've filled in the values at this
C        point. Nothing left to do but check out and return.
C
         CALL CHKOUT ( 'CIDFRM' )
         RETURN
 
      END IF
C
C     The only way to reach this point is if the user did not
C     specify via the kernel pool a frame to use for this center.
C
C     We have a special case for EARTH.
C
      IF ( CENT .EQ. 399 ) THEN
 
         FRCODE = 10013
         FRNAME = 'IAU_EARTH'
         FOUND  = .TRUE.
         CALL CHKOUT ( 'CIDFRM' )
         RETURN
 
      END IF
 
C
C     Determine the location of the requested item in the array
C     of centers.
C
      ITEM = BSCHOI ( CENT, NCOUNT, CENTER, CENTRD )
 
C
C     If the name is in our list, we can just look up its ID code and
C     name in the parallel array.
C
      IF ( ITEM .GT. 0 ) THEN
 
         FRCODE =  IDCODE(ITEM)
         FRNAME =  NAME  (ITEM)
         FOUND  = .TRUE.
 
      ELSE
C
C        There's nothing we can do now. We don't know what frame
C        might be associated with this object.
C
         FRNAME = ' '
         FRCODE =  0
         FOUND  = .FALSE.
 
      END IF
 
      CALL CHKOUT ( 'CIDFRM' )
      RETURN
 

C$Procedure CNMFRM ( Center NaMe to FRaMe id and name )
 
      ENTRY CNMFRM ( CNAME, FRCODE, FRNAME, FOUND )
 
C$ Abstract
C
C     Retrieve frame ID code and name to associate with an object.
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
C     FRAMES
C
C$ Keywords
C
C     FRAMES
C
C$ Declarations
C
C     IMPLICIT NONE
C     CHARACTER*(*)         CNAME
C     INTEGER               FRCODE
C     CHARACTER*(*)         FRNAME
C     LOGICAL               FOUND
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     CNAME      I   name of the object to find a frame for
C     FRCODE     O   the ID code of the frame associated with CNAME
C     FRNAME     O   the name of the frame with ID FRCODE
C     FOUND      O   TRUE if the requested information is available.
C
C$ Detailed_Input
C
C     CNAME       is the name for object for which there is a
C                 preferred reference frame
C
C$ Detailed_Output
C
C     FRCODE      is the frame ID code to associate with a the object
C                 specified by CNAME.
C
C     FRNAME      is the name of the frame that should be associated
C                 with the object specified by CNAME. FRNAME should be
C                 declared as CHARACTER*(26) to ensure that it can
C                 contain the full name of the frame. If FRNAME does
C                 not have enough room to hold the full name of the
C                 frame, the name will be truncated on the right.
C
C     FOUND       is TRUE if the appropriate frame ID code and frame
C                 name can be determined. Otherwise FOUND is returned
C                 with the value FALSE.
C
C$ Parameters
C
C      None.
C
C$ Exceptions
C
C     1) If FRNAME is not have room to contain the frame name, the name
C        will be truncated on the right. ( Declaring FRNAME to be
C        CHARACTER*(26) will ensure that the name will not be
C        truncated.
C
C$ Files
C
C      None.
C
C$ Particulars
C
C     This routine allows the user to determine the frame that should
C     be associated with a particular object. For example, if you
C     need the frame to associate with the Io, you can call CNMFRM
C     to determine the frame name and ID code for the bodyfixed frame
C     of Io.
C
C     The preferred frame to use with an object is specified via one
C     of the kernel pool variables:
C
C         OBJECT_<cname>_FRAME
C
C     where <cname> is the non-blank portion of the string CNAME.
C
C     For those PCK objects that have "built-in" frame names this
C     routine returns the corresponding "IAU" frame and frame ID code.
C
C$ Examples
C
C     Suppose that you want to determine the state of a target
C     in the preferred reference frame of some observer. This
C     routine can be used in conjunction with SPKEZR to compute
C     the state.
C
C        CALL CNMFRM ( OBSNAM, FRCODE, FRNAME, FOUND )
C
C        IF ( .NOT. FOUND ) THEN
C
C           WRITE (*,*) 'The bodyfixed frame for object ', OBSNAM
C           WRITE (*,*) 'could not be identified.'
C           STOP
C
C        END IF
C
C        CALL SPKEZR ( TARGET, ET, FRNAME, ABCORR, OBSNAM, STATE, LT )
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
C     N.J. Bachman    (JPL)
C     B.V. Semenov    (JPL)
C     W.L. Taber      (JPL)
C
C$ Version
C
C-    SPICELIB Version 3.2.0, 08-AUG-2012 (BVS) 
C
C        The routine was updated to be more efficient by using hashes
C        instead kernel POOL look-ups for kernel POOL frames and by
C        using hases instead of ordered array searches for built-in
C        frames.
C
C-    SPICELIB Version 3.1.0, 28-NOV-2002 (NJB)
C
C        Bug fix: updated this routine so a TK frame specified by name
C        and designated as an object's preferred frame via kernel pool
C        assignments is found, and so that the correct name of this
C        frame is returned.
C
C-    SPICELIB Version 3.0.1, 25-JUN-1999 (WLT)
C
C        Extended documentation of entry point CNMFRM and
C        corrected example for that entry point.
C
C-    SPICELIB Version 3.0.0, 03-JUN-1997 (WLT)
C
C        The entry points CIDFRM and CNMFRM were added so that
C        user's may determine the frame-id and name to associated
C        with a planetary object.
C
C-    SPICELIB Version 2.0.0, 04-APR-1997 (WLT)
C
C        The routine was upgraded to reflect that a block of
C        frame ID codes have been reserved for use by the DSN.
C        ID codes 13001 to 13999 have been set aside for DSN
C        models for the orientation of the earth. These frames
C        are all PCK frames. Moreover, the PCK ID code to
C        use with these frames is simply the Frame-Code minus 10000.
C        All of these frames are centered at the earth (body 399).
C
C
C-    SPICELIB Version 1.1.0, 14-OCT-1996 (WLT)
C
C       The values NINERT and NNINRT are included instead of
C       being declared locally.
C
C-    SPICELIB Version 1.0.0, 18-SEP-1995 (WLT)
C
C-&
 
C$ Index_Entries
C
C     Find the bodyfixed frame associated with an object
C
C-&

C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      END IF
 
      CALL CHKIN ( 'CNMFRM' )
 
C
C     Perform any needed first pass initializations.
C
      IF ( FIRST ) THEN
 
C
C        Initialize POOL state counter to the user value. 
C 
         CALL ZZCTRUIN ( PULCTR )

C
C        Initialize kernel POOL frame hashes.
C
         CALL ZZHSIINI ( MAXKFR, KIDLST, KIDPOL )
         CALL ZZHSCINI ( MAXKFR, KNMLST, KNMPOL )

C
C        Initialize built-in frame tables and hashes.
C
         CALL ZZFDAT   ( NCOUNT, MAXBFR, NAME,   IDCODE, 
     .                   CENTER, TYPE,   TYPEID, CENTRD,  
     .                   BNMLST, BNMPOL, BNMNMS, BNMIDX,
     .                   BIDLST, BIDPOL, BIDIDS, BIDIDX )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'CNMFRM' )
            RETURN
         END IF

         FIRST = .FALSE.

      END IF

C
C     First look up in the kernel pool the frame associated with this
C     center.
C
      LOOKUP = 'OBJECT_#_FRAME'
 
      CALL REPMC  ( LOOKUP, '#',   CNAME, LOOKUP )
      CALL UCASE  ( LOOKUP,               LOOKUP )
      CALL DTPOOL ( LOOKUP, GOTIT, N,     DATTYP )
 
C
C     If we didn't find this object in the form OBJECT_<name>_FRAME
C     maybe it is present in the form OBJECT_<number>_FRAME. It's
C     worth a try.
C
      IF ( .NOT. GOTIT ) THEN
C
C        See if we can get the name for this center's ID code.
C
         CALL BODN2C ( CNAME, ID, GOTIT )
 
         IF ( GOTIT ) THEN
C
C           Construct and look up the alternative name in the
C           kernel pool.
C
            LOOKUP = 'OBJECT_#_FRAME'
 
            CALL REPMI  ( LOOKUP, '#',   ID, LOOKUP )
            CALL DTPOOL ( LOOKUP, GOTIT, N,  DATTYP )
 
         END IF
 
      END IF
 
C
C     There are two cases. The user may specify either a name
C     or ID code for the frame to use to model the orientation of
C     an object. We assume they'll opt for the character string
C     form so we test that case first.
C
      IF ( GOTIT ) THEN
 
         IF ( DATTYP .EQ. 'C' ) THEN
 
            CALL GCPOOL ( LOOKUP, 1, 1, N,  PNAME, GOTIT )
C
C           We've got the name:  See if we have this in our handy hash
C           of built-in names.
C
            CALL ZZHSCCHK ( BNMLST, BNMPOL, BNMNMS, PNAME, ITEM )
            IF ( ITEM .NE. 0 ) THEN
               ITEM = BNMIDX( ITEM )
            END IF
 
            IF ( ITEM .GT. 0 ) THEN
 
               FRNAME = PNAME
               FRCODE = IDCODE ( ITEM )
               FOUND  = .TRUE.
 
            ELSE

C
C              See if this frame is in the kernel pool frame name-based
C              hash. First reset the hash if POOL has changed.
C
               CALL ZZPCTRCK ( PULCTR, LUPDTE )

               IF ( LUPDTE ) THEN
                  CALL ZZHSCINI ( MAXKFR, KNMLST, KNMPOL )
                  CALL ZZHSIINI ( MAXKFR, KIDLST, KIDPOL )
               END IF

C
C              Check if this name is in the hash.
C
               CALL ZZHSCCHK ( KNMLST, KNMPOL, KNMNMS, PNAME, ITEM )

               IF ( ITEM .NE. 0 ) THEN
            
                  FRNAME = PNAME
                  FRCODE = KNMIDS ( ITEM )
                  FOUND  = .TRUE.

               ELSE

C
C                 Nope. Look in the kernel pool for the data associated
C                 with this frame.
C
C                 Capture the frame name now, since we're going to
C                 modify PNAME.
C              
                  FRNAME = PNAME

                  CALL PREFIX ( 'FRAME_', 0,                  PNAME )
                  CALL GIPOOL (  PNAME,   1, ROOM, N, VALUES, GOTIT )
 
                  IF ( FAILED() ) THEN
                     CALL CHKOUT ( 'CNMFRM' )
                     RETURN
                  END IF

                  IF ( N .EQ. 1 .AND. GOTIT ) THEN
 
                     FRCODE = VALUES(1)
                     FOUND  = .TRUE.
 
C
C                    If we made it to this point, we successfully
C                    mapped the kernel frame name to its ID. Add this
C                    pair to the name-based hash.
C              
                     CALL ZZHSCADD ( KNMLST, KNMPOL, KNMNMS, FRNAME, 
     .                                                 ITEM, LNEW )

                     IF ( .NOT. FAILED() .AND. ITEM .NE. 0 ) THEN

                        KNMIDS ( ITEM ) = FRCODE

                     END IF
 
                  ELSE
 
                     FRCODE = 0
                     FRNAME = ' '
                     FOUND  = .FALSE.
 
                  END IF

               END IF
 
            END IF
 
 
         ELSE IF ( DATTYP .EQ. 'N' ) THEN
C
C           Ok. They decided to use the numeric form to specify
C           the frame ID. We need to figure out the name of the frame.
C           First we retrieve the frame ID they've loaded into the
C           kernel pool.
C
            CALL GIPOOL ( LOOKUP, 1, 1, N, VALUES, GOTIT )
 
            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'CNMFRM' )
               RETURN
            END IF

C
C           We've got the frame ID, see if we already know about this
C           ID code.
C
            CALL ZZHSICHK ( BIDLST, BIDPOL, BIDIDS, VALUES(1), ITEM )
            IF ( ITEM .NE. 0 ) THEN
               ITEM = BIDIDX( ITEM )
            END IF

            IF ( ITEM .NE. 0 ) THEN
C
C              Just look up the name and set the frame code.
C
               FRNAME = NAME  (ITEM)
               FRCODE = VALUES(1)
               FOUND  = .TRUE.
 
            ELSE

C
C              See if this frame is in the kernel pool frame ID-based
C              hash. First reset the hash if POOL has changed.
C
               CALL ZZPCTRCK ( PULCTR, LUPDTE )

               IF ( LUPDTE ) THEN
                  CALL ZZHSCINI ( MAXKFR, KNMLST, KNMPOL )
                  CALL ZZHSIINI ( MAXKFR, KIDLST, KIDPOL )
               END IF

C
C              Check if this ID is in the hash.
C
               CALL ZZHSICHK ( KIDLST, KIDPOL, KIDIDS, VALUES(1), ITEM )
         
               IF ( ITEM .NE. 0 ) THEN

                  FRNAME = KNAME ( ITEM )
                  FRCODE = VALUES(1)
                  FOUND  = .TRUE.

               ELSE

C
C                 It is not in the hash. See if it's in the kernel pool
C                 somewhere.
C
                  PNAME      = 'FRAME_#_NAME'
                  CALL REPMI (  PNAME, '#', VALUES(1),   PNAME )
                  CALL GCPOOL ( PNAME, 1, ROOM, N, LINE, GOTIT )
 
                  IF ( N .EQ. 1 .AND. GOTIT ) THEN
 
                     CALL LJUST ( LINE(1), FRNAME )
                     FRCODE = VALUES(1)
                     FOUND  = .TRUE.
 
C
C                    Note that since we did not collect all needed
C                    information about this frame, we will not try to
C                    add it to the hash. This addition is done only by
C                    FRINFO.
C
                  ELSE
 
                     FRCODE = VALUES(1)
                     FRNAME = ' '
                     FOUND  = .FALSE.
 
                  END IF

               END IF
 
            END IF
 
 
         END IF
C
C        One way or the other we've filled in the values at this
C        point. Nothing left to do but check out and return.
C
         CALL CHKOUT ( 'CNMFRM' )
         RETURN
 
      END IF
C
C     The only way to reach this point is if the user did not
C     specify via the kernel pool a frame to use for this center.
C
C
      FRNAME = 'IAU_#'
      CALL REPMC ( FRNAME, '#', CNAME, FRNAME )
      CALL UCASE ( FRNAME,             FRNAME )
 
C
C     Determine the location of the requested item in the array
C     of centers.
C
      CALL ZZHSCCHK ( BNMLST, BNMPOL, BNMNMS, FRNAME, ITEM )
      IF ( ITEM .NE. 0 ) THEN
         ITEM = BNMIDX( ITEM )
      END IF
 
C
C     If the name is in our hash, we can just look up its ID code and
C     name in the parallel array.
C
      IF ( ITEM .GT. 0 ) THEN
 
         FRCODE =  IDCODE(ITEM)
         FOUND  = .TRUE.
 
      ELSE
C
C        There's nothing we can do now. We don't know what frame
C        might be associated with this object.
C
         FRCODE =  0
         FOUND  = .FALSE.
 
      END IF
 
      CALL CHKOUT ( 'CNMFRM' )
      RETURN

 
C$Procedure CCIFRM ( frame Class and Class Id to FRaMe id and name )
 
      ENTRY CCIFRM ( CLASS, CLSSID, FRCODE, FRNAME, CENT, FOUND )
 
C$ Abstract
C
C     Return the frame name, frame ID, and center associated with
C     a given frame class and class ID.
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
C     FRAMES
C
C$ Keywords
C
C     FRAMES
C
C$ Declarations
C
C     INTEGER               CLASS
C     INTEGER               CLSSID
C     INTEGER               FRCODE
C     CHARACTER*(*)         FRNAME
C     INTEGER               CENT
C     LOGICAL               FOUND
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     CLASS      I   Class of frame.
C     CLSSID     I   Class ID of frame.
C     FRCODE     O   ID code of the frame identified by CLASS, CLSSID.
C     FRNAME     O   Name of the frame identified by CLASS, CLSSID.
C     CENT       O   Center of the frame identified by CLASS, CLSSID.
C     FOUND      O   TRUE if the requested information is available.
C
C$ Detailed_Input
C
C     CLASS       is the class or type of the frame. This identifies
C                 which subsystem will be used to perform frame
C                 transformations.
C
C     CLSSID      is the ID code used for the frame within its class.
C                 This may be different from the frame ID code.
C
C$ Detailed_Output
C
C     FRCODE      is the frame ID code for the reference frame 
C                 identified by CLASS and CLSSID.
C
C     FRNAME      is the name of the frame identified by CLASS and
C                 CLSSID. FRNAME should be declared as CHARACTER*(26)
C                 to ensure that it can contain the full name of the
C                 frame. If FRNAME does not have enough room to hold
C                 the full name of the frame, the name will be
C                 truncated on the right.
C
C     CENT        is the body ID code for the center of the reference
C                 frame identified by CLASS and CLSSID.
C
C     FOUND       is .TRUE. if FRCODE, FRNAME, and CENT are available.
C                 Otherwise, FOUND is returned with the value .FALSE.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) This routine assumes that the first frame found with matching
C        class and class ID is the correct one. SPICE's frame system
C        does not diagnose the situation where there are multiple,
C        distinct frames with matching classes and class ID codes, but
C        this situation could occur if such conflicting frame
C        specifications are loaded via one or more frame kernels. The
C        user is responsible for avoiding such frame specification
C        conflicts.
C
C     2) If FRNAME does not have room to contain the frame name, the
C        name will be truncated on the right. ( Declaring FRNAME to be
C        CHARACTER*(26) will ensure that the name will not be
C        truncated.
C
C     3) If a frame class assignment is found that associates a 
C        string (as opposed to numeric) value with a frame class
C        keyword, the error SPICE(INVALIDFRAMEDEF) will be signaled.
C
C     4) If a frame class assignment is found that matches the input
C        class, but a corresponding class ID assignment is not
C        found in the kernel pool, the error SPICE(INVALIDFRAMEDEF) 
C        will be signaled.
C
C     5) If a frame specification is found in the kernel pool with
C        matching frame class and class ID, but either the frame name
C        or frame ID code are not found, the error
C        SPICE(INVALIDFRAMEDEF) will be signaled.
C
C     6) If a frame specification is found in the kernel pool with
C        matching frame class and class ID, but the frame center
C        is not found, the error will be diagnosed by routines
C        in the call tree of this routine.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine allows the user to determine the frame associated
C     with a given frame class and class ID code. The built-in frame
C     list is searched first for a matching frame; if no match is
C     found, then the kernel POOL is searched.
C
C     Since the neither the frame class nor the class ID are primary
C     keys, searching for matching frames is a linear (and therefore
C     typically slow) process.
C
C$ Examples
C
C     Suppose that you want to find the name of a frame associated 
C     with a PCK class ID, such as that found in a binary PCK.
C     One could use the following code fragment:
C
C        CALL CCIFRM ( 2, CLSSID, FRCODE, FRNAME, CENT, FOUND )
C
C        IF ( .NOT. FOUND ) THEN
C
C           WRITE (*,*) 'The PCK frame for class ID ', CLSSID
C           WRITE (*,*) 'could not be identified.'
C           STOP
C
C        END IF
C
C        WRITE (*,*) 'The PCK frame having class ID ', CLSSID, ' is '
C        WRITE (*,*) FRNAME
C
C$ Restrictions
C
C     See item (1) in the Exceptions section above.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman    (JPL)
C     B.V. Semenov    (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.1.0, 08-AUG-2012 (BVS) 
C
C        The routine was updated to be more efficient by using hashes
C        instead kernel POOL look-ups for kernel POOL frames and by
C        using hases instead of ordered array searches for built-in
C        frames.
C
C        Bux fix: CCIFRM logic was corrected to examine the built-in
C        frames before looking at the kernel POOL frames.
C
C-    SPICELIB Version 1.0.0, 05-NOV-2007 (NJB)
C
C-&
 
C$ Index_Entries
C
C     Find info associated with a frame class and class id
C     Map frame class and class ID to frame info
C     Map frame class and class ID to frame name, id, and center
C
C-&


C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      END IF
 
      CALL CHKIN ( 'CCIFRM' )

C
C     Perform any needed first pass initializations.
C
      IF ( FIRST ) THEN
 
C
C        Initialize POOL state counter to the user value. 
C 
         CALL ZZCTRUIN ( PULCTR )

C
C        Initialize kernel POOL frame hashes.
C
         CALL ZZHSIINI ( MAXKFR, KIDLST, KIDPOL )
         CALL ZZHSCINI ( MAXKFR, KNMLST, KNMPOL )

C
C        Initialize built-in frame tables and hashes.
C
         CALL ZZFDAT   ( NCOUNT, MAXBFR, NAME,   IDCODE, 
     .                   CENTER, TYPE,   TYPEID, CENTRD,  
     .                   BNMLST, BNMPOL, BNMNMS, BNMIDX,
     .                   BIDLST, BIDPOL, BIDIDS, BIDIDX )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'CCIFRM' )
            RETURN
         END IF

         FIRST = .FALSE.

      END IF

C
C     No frame found so far.
C
      FOUND = .FALSE.

C
C     First try to look up from the built-in list the frame associated
C     with the input class and class ID. Unfortunately, this is a
C     linear search.
C
      DO I = 1, NCOUNT

         IF (       ( TYPE(I)   .EQ. CLASS  ) 
     .        .AND. ( TYPEID(I) .EQ. CLSSID ) ) THEN
C
C           We have a match. Assign the output arguments and return.
C
            FRNAME = NAME(I)
            FRCODE = IDCODE(I)
            CENT   = CENTER(I)
            FOUND  = .TRUE.

            CALL CHKOUT ( 'CCIFRM' )
            RETURN

         END IF

      END DO

C
C     Unfortunately we did not find a frame associated with the input
C     class and class ID in the built-in list. We need to look for this
C     frame in the kernel POOL. Since neither of these input values
C     appears in a kernel variable name, we may have to look at all of
C     the frame specifications in the kernel pool. Start out by looking
C     the frame class assignments from any loaded frame specifications.
C
      LOOKUP = 'FRAME_*_CLASS'

      START = 1
      CALL GNPOOL ( LOOKUP, START, KVBSZ, N, KVBUFF, FND )
 
      DO WHILE (  FND .AND. ( N .GT. 0 )  )
C
C        For each kernel variable name found in the buffer, look up the
C        associated class. If the class matches the input class, look
C        up the class ID as well. Set the output arguments and return
C        if we get a complete match.
C
         DO I = 1, N

            CALL GIPOOL ( KVBUFF(I), 1, 1, N, KVCLSS, FND )

            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'CCIFRM' )
               RETURN
            END IF

            IF ( .NOT. FND ) THEN

               CALL SETMSG ( 'Invalid frame specification found in '
     .         //            'kernel pool: frame class keyword '
     .         //            'is # but integer class was not '
     .         //            'associated with this keyword.'       )
               CALL ERRCH  ( '#',  KVBUFF(I)                       )
               CALL SIGERR ( 'SPICE(INVALIDFRAMEDEF)'              )
               CALL CHKOUT ( 'CCIFRM'                              )
               RETURN

            END IF

            IF ( KVCLSS .EQ. CLASS ) THEN
C
C              Get the class ID for the current frame.
C
               LOOK2 = KVBUFF(I)

               CALL SUFFIX ( '_ID', 0, LOOK2 )

               CALL GIPOOL ( LOOK2, 1, 1, N, KVCLID, FND )

               IF ( FAILED() ) THEN
                  CALL CHKOUT ( 'CCIFRM' )
                  RETURN
               END IF

               IF ( .NOT. FND ) THEN


                  CALL SETMSG ( 'Invalid frame specification found in '
     .            //            'kernel pool: frame class keyword '
     .            //            'is # but associated integer '
     .            //            'class ID assignment was not found.' )
                  CALL ERRCH  ( '#',  KVBUFF(I)                      ) 
                  CALL SIGERR ( 'SPICE(INVALIDFRAMEDEF)'             )
                  CALL CHKOUT ( 'CCIFRM'                             )
                  RETURN

               END IF

C
C              Check the class ID for the current kernel variable
C              against the input value.
C
               IF ( KVCLID .EQ. CLSSID ) THEN
C
C                 We have a match. We need to return the frame
C                 ID, frame name, and center. As long as we're
C                 looking at a valid frame specification, this is
C                 no problem.
C
C                 Look up the frame name first. Create the frame
C                 name keyword.
C
                  CALL REPMC ( KVBUFF(I), '_CLASS', '_NAME', LOOK2 )

                  CALL GCPOOL ( LOOK2, 1, 1, N, FRNAME, FND )

                  IF ( .NOT. FND ) THEN

                     CALL SETMSG ( 'Invalid frame specification found '
     .               //            'in kernel pool: frame class keyword'
     .               //            ' is # but associated frame name '
     .               //            'assignment was not found.'         )
                     CALL ERRCH  ( '#',  KVBUFF(I)                     )
                     CALL SIGERR ( 'SPICE(INVALIDFRAMEDEF)'            )
                     CALL CHKOUT ( 'CCIFRM'                            )
                     RETURN

                  END IF

C
C                 We could extract the frame ID code from KVBUFF(I), but
C                 instead we'll make sure that the ID is defined in the 
C                 kernel pool.
C
                  LOOK2 = FRNAME
                  CALL PREFIX ( 'FRAME_', 0, LOOK2 )

                  CALL GIPOOL ( LOOK2, 1, 1, N, FRCODE, FND )

                  IF ( FAILED() ) THEN
                     CALL CHKOUT ( 'CCIFRM' )
                     RETURN
                  END IF

                  IF ( .NOT. FND ) THEN

                     CALL SETMSG ( 'Invalid frame specification found '
     .               //            'in kernel pool: frame name is '
     .               //            'is # but associated frame ID '
     .               //            'assignment was not found.'         )
                     CALL ERRCH  ( '#',  FRNAME                        )
                     CALL SIGERR ( 'SPICE(INVALIDFRAMEDEF)'            )
                     CALL CHKOUT ( 'CCIFRM'                            )
                     RETURN

                  END IF

C
C                 Look up the frame center. Whether the frame center
C                 has been specified by name or ID code, the ID code
C                 will be returned by ZZDYNBID.
C
                  CALL ZZDYNBID ( FRNAME, FRCODE, 'CENTER', CENT ) 

C
C                 As long as we looked up the center successfully,
C                 we're done.
C
                  IF ( .NOT. FAILED() ) THEN
                     
                     FOUND = .TRUE.

                  END IF

C
C                 Exit here, whether or not we looked up the frame's
C                 center succesfully.
C
                  CALL CHKOUT ( 'CCIFRM' )
                  RETURN

               END IF

            END IF
C
C           Getting to this point means we didn't have a match; 
C           examine the next buffer entry.
C
         END DO

C
C        Get the next buffer full of frame class keywords.
C        
         START = START + N
         CALL GNPOOL ( LOOKUP, START, KVBSZ, N, KVBUFF, FND )

      END DO

C
C     We drop down to this point only if no matching frame was found.
C     The FOUND flag has already been set to .FALSE.
C 
      CALL CHKOUT ( 'CCIFRM' )
      RETURN
      END
