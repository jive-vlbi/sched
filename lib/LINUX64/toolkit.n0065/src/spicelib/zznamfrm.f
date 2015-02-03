C$Procedure ZZNAMFRM ( Frame name to ID translation, with bypass )

      SUBROUTINE ZZNAMFRM ( USRCTR, SAVNAM, SAVCDE, FRNAME, FRCODE )

C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Translate a string containing a frame name to its ID code, but
C     bypass calling NAMFRM and return saved value provided by the
C     caller if the name is the same as the saved name and the POOL
C     state did not change.
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
C     NAIF_IDS
C
C$ Keywords
C
C     PRIVATE
C     FRAME
C
C$ Declarations

      IMPLICIT NONE

      INCLUDE              'zzctr.inc'

      INTEGER               USRCTR    ( CTRSIZ )
      CHARACTER*(*)         SAVNAM
      INTEGER               SAVCDE
      CHARACTER*(*)         FRNAME
      INTEGER               FRCODE

C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     USRCTR    I/O  POOL state counter saved by the caller.
C     SAVNAM    I/O  Frame name saved by the caller.
C     SAVCDE    I/O  Frame ID code saved by the caller.
C     FRNAME     I   Frame name.
C     FRCODE     O   Frame ID code.
C     CTRSIZ     P   Counter array size.
C
C$ Detailed_Input
C
C     USRCTR      is the value of the POOL state counter tracked by
C                 (saved in) the caller (user) routine specifically for
C                 this frame name/ID pair of variables.
C
C     SAVNAM      is the frame name saved in the caller routine
C                 specifically for this frame name/ID pair of
C                 variables. For detailed description of allowed values
C                 see description of the FRNAME argument in NAMFRM.
C
C     SAVCDE      is the frame ID code saved in the caller routine
C                 specifically for this frame name/ID pair of
C                 variables. For detailed description of allowed values
C                 see description of the FRCODE argument in NAMFRM.
C
C     FRNAME      is the input frame name. For detailed description of
C                 allowed values see description of the FRNAME argument
C                 in NAMFRM.
C
C$ Detailed_Output
C
C     USRCTR      is the current POOL state counter.
C
C     SAVNAM      is the frame name saved in the caller routine
C                 specifically for this frame name/ID pair of
C                 variables. On the output SAVNAM always equals FRNAME.
C
C     SAVCDE      is the frame ID code saved in the caller routine
C                 specifically for this frame name/ID pair of
C                 variables. If the frame name cannot be mapped to an
C                 ID, FRCODE is returned as 0. On the output SAVCDE
C                 always equals FRCODE.
C
C     FRCODE      is the output frame ID code. For detailed description
C                 of possible values see description of the FRCODE
C                 argument in NAMFRM. If the frame name cannot be
C                 mapped to an ID, FRCODE is returned as 0. On the
C                 output FRCODE always equals SAVCDE.
C
C$ Parameters
C
C     CTRSIZ      is the dimension of the counter array used by
C                 various SPICE subsystems to uniquely identify
C                 changes in their states. This parameter is 
C                 defined in the private include file 'zzctr.inc'.
C
C$ Exceptions
C
C     1) Errors may be signaled by routines in the call tree of this
C        routine.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine translates a string containing a frame name to an
C     integer code but bypasses calling NAMFRM to do the translation
C     and simply returns the saved ID value provided by the caller if
C     the input name is the same as the saved name provided by the
C     caller, the saved ID provided by the caller is not 0, and the
C     POOL state counter tracked by the caller is the same as the
C     current POOL state counter.
C
C     The POOL state counter and name/ID pair of saved variables
C     tracked by the caller must be specific for each frame of
C     interest. I.e. if the caller routine needs to call this routine
C     to do translations for two distinct frames, the caller routine
C     mush use must have its own set of saved POOL state counter and
C     name/ID variables for each of the two frames.
C
C$ Examples
C
C     This example shows how a routine that needs to do frame name-ID
C     conversion for two distinct frames (FRA and FRB) can do it using
C     ZZNAMFRM.
C        
C           SUBROUTINE <name> ( FRA, FRB, ... )
C           ...
C           INCLUDE               'zzctr.inc'
C           ...
C     C
C     C     Saved frame name length.
C     C
C           INTEGER               FRNMLN
C           PARAMETER           ( FRNMLN = 32 )
C           ....
C     C     
C     C
C     C     Saved name/ID item declarations.
C     C
C           INTEGER               SVCTR1 ( CTRSIZ )
C           CHARACTER*(FRNMLN)    SVFRA
C           INTEGER               SVFRAI
C           INTEGER               SVCTR2 ( CTRSIZ )
C           CHARACTER*(FRNMLN)    SVFRB
C           INTEGER               SVFRBI
C           LOGICAL               FIRST
C     C
C     C     Saved name/ID items.
C     C
C           SAVE                  SVCTR1
C           SAVE                  SVFRA
C           SAVE                  SVFRAI
C           SAVE                  SVCTR2
C           SAVE                  SVFRB
C           SAVE                  SVFRBI
C           SAVE                  FIRST
C     C
C     C     Initial values.
C     C
C           DATA                  FIRST   / .TRUE. /
C           ...
C     C
C     C     Initialization.
C     C
C           IF ( FIRST ) THEN
C     C
C     C        Initialize POOL counters.
C     C
C              CALL ZZCTRUIN( SVCTR1 )
C              CALL ZZCTRUIN( SVCTR2 )
C              FIRST = .FALSE.
C           END IF
C     C
C     C     Starting from translation of FRA name to ID.
C     C
C           CALL ZZNAMFRM ( SVCTR1, SVFRA, SVFRAI, FRA, FRAID )
C      
C           IF ( FRAID .EQ. 0 ) THEN
C              CALL SETMSG ( '...' )
C              CALL SIGERR ( 'SPICE(FRAMEAIDCODENOTFOUND)' )
C              CALL CHKOUT ( '<name>' )
C              RETURN
C           END IF
C     C
C     C     Now do the same for FRB.
C     C
C           CALL ZZNAMFRM ( SVCTR2, SVFRB, SVFRBI, FRB, FRBID )
C      
C           IF ( FRBID .EQ. 0 ) THEN
C              CALL SETMSG ( '...' )
C              CALL SIGERR ( 'SPICE(FRAMEBIDCODENOTFOUND)' )
C              CALL CHKOUT ( '<name>' )
C              RETURN
C           END IF
C           ... 
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
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 23-SEP-2013 (BVS)
C
C-&

C
C     SPICE functions.
C     
      LOGICAL               RETURN

C
C     Local variables.
C
      LOGICAL               UPDATE

C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      END IF

C
C     Check/update POOL state counter.
C     
      CALL ZZPCTRCK ( USRCTR, UPDATE )

C
C     Check update flag, saved ID, and saved name against the input.
C
      IF ( .NOT. UPDATE       .AND. 
     .     SAVCDE .NE. 0      .AND.
     .     SAVNAM .EQ. FRNAME       ) THEN

C
C        No change in the POOL state, the saved name was successfully
C        resolved earlier, and input and saved names are the same.
C        Return saved ID.
C
         FRCODE = SAVCDE

      ELSE

C
C        Check in because NAMFRM may fail.
C
         CALL CHKIN  ( 'ZZNAMFRM' )

C
C        POOL state changed, or the saved name was never successfully
C        resolved earlier, or input and saved names are different. Call
C        NAMFRM to look up ID and reset saved values.
C
         CALL NAMFRM( FRNAME, FRCODE )

         SAVNAM = FRNAME
         SAVCDE = FRCODE

C
C        Check out.
C
         CALL CHKOUT ( 'ZZNAMFRM' )

      END IF

      RETURN

      END
