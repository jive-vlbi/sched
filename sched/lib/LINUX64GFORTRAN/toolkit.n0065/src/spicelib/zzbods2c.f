C$Procedure ZZBODS2C ( Body name to ID translation, with bypass )

      SUBROUTINE ZZBODS2C ( USRCTR, SAVNAM, SAVCDE, SAVFND, 
     .                              NAME,   CODE,   FOUND    )

C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Translate a string containing a body name or ID code to an
C     integer code, but bypass calling BODS2C and return saved values
C     provided by the caller if the name is the same as the saved name
C     and the ZZBODTRN state did not change.
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
C     BODY
C
C$ Declarations

      IMPLICIT NONE

      INCLUDE              'zzctr.inc'

      INTEGER               USRCTR    ( CTRSIZ )
      CHARACTER*(*)         SAVNAM
      INTEGER               SAVCDE
      LOGICAL               SAVFND
      CHARACTER*(*)         NAME
      INTEGER               CODE
      LOGICAL               FOUND

C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     USRCTR    I/O  ZZBODTRN state counter saved by the caller.
C     SAVNAM    I/O  Body name saved by the caller.
C     SAVCDE    I/O  Body ID code saved by the caller.
C     SAVFND    I/O  Translation success flag saved in the caller.
C     NAME       I   Body name.
C     CODE       O   Body ID code.
C     FOUND      O   Translation success flag.
C     CTRSIZ     P   Counter array size.
C
C$ Detailed_Input
C
C     USRCTR      is the value of the ZZBODTRN state counter tracked by
C                 (saved in) the caller (user) routine specifically for
C                 this body name/ID/found flag triplet of variables.
C
C     SAVNAM      is the body name saved in the caller routine
C                 specifically for this body name/ID/found flag triplet
C                 of variables. For detailed description of allowed
C                 values see description of the NAME argument in
C                 BODS2C.
C
C     SAVCDE      is the body ID code saved in the caller routine
C                 specifically for this body name/ID/found flag triplet
C                 of variables. For detailed description of allowed
C                 values see description of the CODE argument in
C                 BODS2C.
C
C     SAVFND      is the body name to ID translation success flag saved
C                 in the caller routine specifically for this body
C                 name/ID/found flag triplet of variables. SAVFND
C                 should .TRUE. if NAME had a translation or represents
C                 an integer.
C
C     NAME        is the input body name. For detailed description of
C                 allowed values see description of the NAME argument
C                 in BODS2C.
C
C$ Detailed_Output
C
C     USRCTR      is the current ZZBODTRN state counter.
C
C     SAVNAM      is the body name saved in the caller routine
C                 specifically for this body name/ID/found flag triplet
C                 of variables. On the output SAVNAM always equals
C                 NAME.
C
C     SAVCDE      is the body ID code saved in the caller routine
C                 specifically for this body name/ID/found flag triplet
C                 of variables. On the output SAVCDE always equals
C                 CODE.
C
C     SAVFND      is the body name to ID translation success flag saved
C                 in the caller routine specifically for this body
C                 name/ID/found flag triplet of variables. On the
C                 output SAVFND always equals FOUND.
C
C     CODE        is the output body ID code. For detailed description
C                 of possible values see description of the CODE
C                 argument in BODS2C. On the output CODE always equals
C                 SAVCDE.
C
C     FOUND       is the body name to ID translation success flag.
C                 FOUND is .TRUE. if NAME has a translation or
C                 represents an integer. Otherwise, FOUND is .FALSE.
C                 On the output FOUND always equals SAVFND.
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
C     This routine translates a string containing a body name or ID code
C     to an integer code but bypasses calling BODS2C to do the
C     translation and simply returns the saved ID value and translation
C     success flag provided by the caller if the input name is the same
C     as the saved name provided by the caller, the saved translation
C     success flag provided by the caller is .TRUE. and the ZZBODTRN
C     state counter tracked by the caller is the same as the current
C     ZZBODTRN state counter.
C
C     The ZZBODTRN state counter and name/ID/found flag triplet of
C     saved variables tracked by the caller must be specific for each
C     body of interest. I.e. if the caller routine needs to call this
C     routine to do translations for a target body and for an observer
C     body, the caller routine mush use must have its own set of saved
C     ZZBODTRN state counter and name/ID/found flag variables for each
C     of the two bodies .
C
C$ Examples
C
C     This example shows how a routine that needs to do body name-ID
C     conversion for two distinct bodies can do it using ZZBODS2C.
C        
C           SUBROUTINE <name> ( TARG, OBS, ... )
C           ...
C           INCLUDE               'zzctr.inc'
C           ...
C     C
C     C     Saved body name length.
C     C
C           INTEGER               MAXL
C           PARAMETER           ( MAXL  = 36 )
C           ....
C     C     
C     C
C     C     Saved name/ID item declarations.
C     C
C           INTEGER               SVCTR1 ( CTRSIZ )
C           CHARACTER*(MAXL)      SVTARG
C           INTEGER               SVTGID
C           LOGICAL               SVFND1
C           INTEGER               SVCTR2 ( CTRSIZ )
C           CHARACTER*(MAXL)      SVOBSN
C           INTEGER               SVOBSI
C           LOGICAL               SVFND2
C           LOGICAL               FIRST
C     C
C     C     Saved name/ID items.
C     C
C           SAVE                  SVCTR1
C           SAVE                  SVTARG
C           SAVE                  SVTGID
C           SAVE                  SVFND1
C           SAVE                  SVCTR2
C           SAVE                  SVOBSN
C           SAVE                  SVOBSI
C           SAVE                  SVFND2
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
C     C        Initialize ZZBODTRN counters.
C     C
C              CALL ZZCTRUIN( SVCTR1 )
C              CALL ZZCTRUIN( SVCTR2 )
C              FIRST = .FALSE.
C           END IF
C     C
C     C     Starting from translation of target name to ID.
C     C
C           CALL ZZBODS2C ( SVCTR1, SVTARG, SVTGID, SVFND1,
C          .                TARG, TARGID, FOUND )
C      
C           IF ( .NOT. FOUND ) THEN
C              CALL SETMSG ( '...' )
C              CALL SIGERR ( 'SPICE(TARGIDCODENOTFOUND)' )
C              CALL CHKOUT ( '<name>' )
C              RETURN
C           END IF
C     C
C     C     Now do the same for observer
C     C
C           CALL ZZBODS2C ( SVCTR2, SVOBSN, SVOBSI, SVFND2,
C          .                OBS, OBSID, FOUND )
C      
C           IF ( .NOT. FOUND ) THEN
C              CALL SETMSG ( '...' )
C              CALL SIGERR ( 'SPICE(OBSIDCODENOTFOUND)' )
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
C-    SPICELIB Version 1.0.0, 21-SEP-2013 (BVS)
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
C     Check/update ZZBODTRN state counter.
C     
      CALL ZZBCTRCK ( USRCTR, UPDATE )

C
C     Check update flag, saved found flag, and saved name against the
C     input.
C
      IF ( .NOT. UPDATE     .AND. 
     .     SAVFND           .AND.
     .     SAVNAM .EQ. NAME      ) THEN

C
C        No change in body-name mapping state, the saved name was
C        successfully resolved earlier, and input and saved names are
C        the same. Return saved ID and FOUND.
C
         CODE   = SAVCDE
         FOUND  = SAVFND

      ELSE

C
C        Check in because BODS2C may fail.
C
         CALL CHKIN  ( 'ZZBODS2C' )

C
C        Body-name mapping state changed, or the saved name was never
C        successfully resolved earlier, or input and saved names are
C        different. Call BODS2C to look up ID and FOUND and reset saved
C        values.
C
         CALL BODS2C( NAME, CODE, FOUND )

         SAVNAM = NAME
         SAVCDE = CODE
         SAVFND = FOUND

C
C        Check out.
C
         CALL CHKOUT ( 'ZZBODS2C' )

      END IF

      RETURN

      END
