C$Procedure BLTFRM ( Built-in frame IDs )
 
      SUBROUTINE BLTFRM ( FRMCLS, IDSET )
 
C$ Abstract
C
C     Return a set containing the frame IDs of all built-in frames of a
C     specified class.
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
C     CELLS
C     FRAMES
C     NAIF_IDS
C     SETS
C
C$ Keywords
C
C     FRAME
C     SET
C     UTILITY
C
C$ Declarations

      IMPLICIT NONE

      INCLUDE 'frmtyp.inc'
      INCLUDE 'ninert.inc'
      INCLUDE 'nninrt.inc'

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      INTEGER               FRMCLS
      INTEGER               IDSET  ( LBCELL : * )

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     FRMCLS     I   Frame class.
C     IDSET      O   Set of ID codes of frames of the specified class.
C
C$ Detailed_Input
C
C     FRMCLS         is an integer code specifying the frame class or
C                    classes for which built-in frame ID codes are
C                    requested. FRMCLS may designate a single class or
C                    "all classes."
C
C                    The include file frmtyp.inc declares parameters
C                    identifying frame classes. The supported values
C                    and corresponding meanings of FRMCLS are
C
C                       Parameter      Value    Meaning
C                       =========      =====    =================
C                       ALL              -1     All frame classes
C                       INERTL            1     Built-in inertial
C                       PCK               2     PCK-based frame
C                       CK                3     CK-based frame
C                       TK                4     Fixed offset ("text
C                                               kernel") frame
C                       DYN               5     Dynamic frame
C
C$ Detailed_Output
C
C     IDSET          is a SPICE set containing the ID codes of all
C                    built-in reference frames of the specified class
C                    or classes.
C
C$ Parameters
C
C     See the INCLUDE file frmtyp.inc.
C
C$ Exceptions
C
C     1) If the input frame class argument is not defined in 
C        frmtyp.inc, the error SPICE(BADFRAMECLASS) is signaled.
C
C     2) If the size of IDSET is too small to hold the requested
C        frame ID set, the error SPICE(SETTOOSMALL) is signaled.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine has a counterpart 
C
C        KPLFRM
C
C     which fetches the frame IDs of all frames specified in the kernel
C     pool.
C     
C$ Examples
C
C     1) Display the IDs and names of all SPICE built-in frames.
C        Group the outputs by frame class. Also fetch and display
C        the entire set of IDs and names using the parameter ALL.
C
C
C        Program source code:
C
C
C              PROGRAM EX1
C
C              IMPLICIT NONE
C
C              INCLUDE 'ninert.inc'
C              INCLUDE 'nninrt.inc'
C              INCLUDE 'frmtyp.inc'
C        C
C        C     SPICELIB functions
C        C
C              INTEGER               CARDI
C        C
C        C     Local parameters
C        C
C              INTEGER               NFRAME
C              PARAMETER           ( NFRAME = NINERT + NNINRT )
C
C              INTEGER               LBCELL
C              PARAMETER           ( LBCELL = -5 )
C
C              INTEGER               LNSIZE
C              PARAMETER           ( LNSIZE = 80 )
C
C              INTEGER               FRNMLN
C              PARAMETER           ( FRNMLN = 32 )
C
C        C
C        C     Local variables
C        C
C              CHARACTER*(FRNMLN)    FRNAME
C              CHARACTER*(LNSIZE)    OUTLIN
C
C              INTEGER               I
C              INTEGER               IDSET ( LBCELL : NFRAME )
C              INTEGER               J
C
C        C
C        C     Initialize the frame set.
C        C
C              CALL SSIZEI ( NFRAME, IDSET )
C
C        C
C        C     Fetch and display the frames of each class.
C        C
C              DO I = 1, 6
C
C                 IF ( I .LT. 6 ) THEN
C        C
C        C           Fetch the frames of class I.
C        C
C                    CALL BLTFRM ( I, IDSET )
C
C                    OUTLIN = 'Number of frames of class #: #'
C                    CALL REPMI ( OUTLIN, '#', I,            OUTLIN )
C                    CALL REPMI ( OUTLIN, '#', CARDI(IDSET), OUTLIN )
C
C                 ELSE
C        C
C        C           Fetch IDs of all built-in frames.
C        C
C                    CALL BLTFRM ( ALL, IDSET )
C
C                    OUTLIN = 'Number of built-in frames: #'
C                    CALL REPMI ( OUTLIN, '#', CARDI(IDSET), OUTLIN )
C
C                 END IF
C
C                 CALL TOSTDO ( ' '    )
C                 CALL TOSTDO ( OUTLIN )
C                 CALL TOSTDO ( '   Frame IDs and names' )
C
C                 DO J = 1, CARDI(IDSET)
C                    CALL FRMNAM ( IDSET(J), FRNAME )
C                    WRITE (*,*) IDSET(J), '  ', FRNAME
C                 END DO
C
C              END DO
C
C              END
C
C
C     The output from the program, when the program was linked
C     against the N0064 SPICE Toolkit, is shown below. Note that
C     the set of built-in frames, particularly the non-inertial
C     ones, will grow over time, so the output shown here may
C     be out of sync with that produced by a current SPICE Toolkit.
C
C     The output shown here has been abbreviated.
C
C
C        Number of frames of class 1: 21
C           Frame IDs and names
C                   1   J2000
C                   2   B1950
C                   3   FK4
C                   4   DE-118
C                   5   DE-96
C                   6   DE-102
C                   7   DE-108
C                   8   DE-111
C                   9   DE-114
C                  10   DE-122
C                  11   DE-125
C                  12   DE-130
C                  13   GALACTIC
C                  14   DE-200
C                  15   DE-202
C                  16   MARSIAU
C                  17   ECLIPJ2000
C                  18   ECLIPB1950
C                  19   DE-140
C                  20   DE-142
C                  21   DE-143
C
C        Number of frames of class 2: 99
C           Frame IDs and names
C               10001   IAU_MERCURY_BARYCENTER
C               10002   IAU_VENUS_BARYCENTER
C
C                   ...
C
C               10100   IAU_ITOKAWA
C               13000   ITRF93
C
C        Number of frames of class 3: 0
C           Frame IDs and names
C
C        Number of frames of class 4: 1
C           Frame IDs and names
C               10081   EARTH_FIXED
C
C        Number of frames of class 5: 0
C           Frame IDs and names
C
C        Number of built-in frames: 121
C           Frame IDs and names
C                   1   J2000
C                   2   B1950
C
C                   ...
C
C               10100   IAU_ITOKAWA
C               13000   ITRF93
C
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
C
C$ Version
C
C-    SPICELIB Version 1.1.0, 09-AUG-2013 (BVS)
C
C        Updated for changed ZZFDAT calling sequence.
C
C-    SPICELIB Version 1.0.0, 21-MAY-2012 (NJB)
C
C-&
 
C$ Index_Entries
C
C     fetch names of built-in reference frames
C
C-&
 
C
C     SPICELIB functions
C
      INTEGER               SIZEI

      LOGICAL               FAILED
      LOGICAL               RETURN

C
C     Local parameters
C
      INTEGER               NFRAME
      PARAMETER           ( NFRAME = NINERT + NNINRT )
      
      INTEGER               FRNMLN
      PARAMETER           ( FRNMLN = 32 )

C
C     Local variables
C
      CHARACTER*(FRNMLN)    FRNAME ( NFRAME )

      INTEGER               CTRORD ( NFRAME )
      INTEGER               CENTER ( NFRAME )
      INTEGER               CORDER ( NFRAME )
      INTEGER               FCLASS ( NFRAME )
      INTEGER               FCLSID ( NFRAME )
      INTEGER               FCODE  ( NFRAME )
      INTEGER               I
      INTEGER               J
      INTEGER               TO

      LOGICAL               PASS1

C
C     Built-in frame hashes returned by ZZFDAT.
C
      INTEGER               LBPOOL
      PARAMETER           ( LBPOOL = -5 )

      INTEGER               MAXBFR
      PARAMETER           ( MAXBFR = NFRAME+1 )
      
      INTEGER               BNMLST  (          MAXBFR )
      INTEGER               BNMPOL  ( LBPOOL : MAXBFR )
      CHARACTER*(FRNMLN)    BNMNMS  (          MAXBFR )
      INTEGER               BNMIDX  (          MAXBFR )

      INTEGER               BIDLST  (          MAXBFR )
      INTEGER               BIDPOL  ( LBPOOL : MAXBFR )
      INTEGER               BIDIDS  (          MAXBFR )
      INTEGER               BIDIDX  (          MAXBFR )

C
C     Saved variables
C
C
C     Save all variables in order to avoid problems
C     in code translated by f2c.
C
      SAVE

C
C     Initial values
C
      DATA                  PASS1 / .TRUE. /


      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'BLTFRM' )
C
C     The output set starts out empty.
C
      CALL SCARDI ( 0, IDSET )

C
C     On the first pass, fetch all data for the
C     built-in frames.
C
      IF ( PASS1 ) THEN

         CALL ZZFDAT ( NFRAME, MAXBFR, FRNAME, FCODE,  
     .                 CENTER, FCLASS, FCLSID, CTRORD,
     .                 BNMLST, BNMPOL, BNMNMS, BNMIDX,
     .                 BIDLST, BIDPOL, BIDIDS, BIDIDX )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'BLTFRM' )
            RETURN
         END IF

         PASS1 = .FALSE.

      END IF

C
C     Check the input frame class. 
C
C     This block of code must be kept in sync with frmtyp.inc.
C
      IF  (      ( FRMCLS .GT. DYN )
     .      .OR. ( FRMCLS .EQ. 0   )
     .      .OR. ( FRMCLS .LT. ALL )  ) THEN

         CALL SETMSG ( 'Frame class specifier FRMCLS was #; this '
     .   //            'value is not supported.'                  )
         CALL ERRINT ( '#',  FRMCLS                               )
         CALL SIGERR ( 'SPICE(BADFRAMECLASS)'                     )
         CALL CHKOUT ( 'BLTFRM'                                   )
         RETURN

      END IF

C
C     Make sure the set is large enough to hold all of
C     the IDs of the built-in frames.
C
      IF ( SIZEI(IDSET) .LT. NFRAME ) THEN

         CALL SETMSG ( 'Frame ID set argument IDSET has size #; '
     .   //            'required size is at least #.'           )
         CALL ERRINT ( '#', SIZEI(IDSET)                        )
         CALL ERRINT ( '#', NFRAME                              )
         CALL SIGERR ( 'SPICE(SETTOOSMALL)'                     )
         CALL CHKOUT ( 'BLTFRM'                                 )
         RETURN

      END IF

C
C     Transfer ID codes of all frames of the specified class
C     to the output set. First, generate an order vector for
C     the ID codes.
C
      CALL ORDERI ( FCODE, NFRAME, CORDER )

      TO = 0

      DO I = 1, NFRAME
C
C        Get the index J in the parallel data arrays of
C        the Ith frame, ordered by ID code. 
C
         J = CORDER(I)

         IF (      ( FCLASS(J) .EQ. FRMCLS ) 
     .        .OR. ( FRMCLS    .EQ. ALL    )  ) THEN
C
C           The frame at index J belongs to the
C           requested class. Append the frame's ID
C           code to the set.
C
            TO        = TO + 1
            IDSET(TO) = FCODE(J)

         END IF

      END DO

C
C     Set the cardinality of the output set. 
C
C     Note that we populated the set using an order vector, so sorting
C     the elements is not necessary. We rely on ZZFDAT to not give us
C     duplicate frame specifications.
C
      CALL SCARDI ( TO, IDSET )

      CALL CHKOUT ( 'BLTFRM' )
      RETURN
      END

