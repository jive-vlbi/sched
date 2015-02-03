C$Procedure      M2XIST ( META/2 --- does a named template word exist )
 
      LOGICAL FUNCTION M2XIST ( NAME )
      IMPLICIT NONE
 
C$ Abstract
C
C     Determine whether or not a named template word has been matched
C     and had the corresponding matching word boundaries stored in
C     the META/2 parse tables.
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
C     META/2 --- a language specification language.
C
C$ Keywords
C
C     META/2
C     PARSING
C
C$ Declarations
 
      CHARACTER*(*)         NAME
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     NAME       I   The name of a META/2 template word.
C
C$ Detailed_Input
C
C     NAME       is the name of a META/2 template word that may have
C                been matched by a call to M2GMCH.  The case of NAME
C                is significant.  'BOB', 'Bob', and 'bob' will be
C                regarded as different names.
C
C$ Detailed_Output
C
C     M2XIST     is returned .TRUE. if the named template word has
C                been stored in the META/2 parse table.  Otherwise
C                it is returned .FALSE.
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
C     None.
C
C$ Particulars
C
C     Once a string has matched some META/2 template, it is often
C     necessary to determine which template has been matched or which
C     sub-template has been matched before a program can respond
C     appropriately.  In some cases, the mere existance of a match
C     is sufficient to determine the action a routine should take.
C
C     This routine exists so that you can easily find out whether a
C     match for a particular named template word occurred.
C
C$ Examples
C
C     Suppose that a string command was expected to match one of the
C     following two templates.
C
C        'FIND SEPARATION[separation]
C              (2:2){ OF    @int[body1] @int[body2]
C                   | FROM  @int[observer]          }'
C
C        'FIND DISTANCE[distance]
C              (2:2){ BETWEEN @int[body1]  @int[body2]
C                   | FROM    @int[observer]  }'
C
C     The action a routine will take will depend upon which template
C     was actually matched.  But since we know that we have a match
C     of one of these templates,  the work of extracting the bodies
C     and observer can be common to both types of strings.
C
C
C           CALL M2GETI ( 'body1',    STRING, FOUND, BODY1 )
C           CALL M2GETI ( 'body2',    STRING, FOUND, BODY2 )
C           CALL M2GETI ( 'observer', STRING, FOUND, OBS   )
C     C
C     C     Look up the apparent states of the bodies relative
C     C     to the specified observer.
C     C
C           CALL SPKEZ ( BODY1, ET, 'J2000', 'LT+S', OBS, STATE1, LT )
C           CALL SPKEZ ( BODY2, ET, 'J2000', 'LT+S', OBS, STATE2, LT )
C
C     C
C     C     Now compute the ANSWER based upon whether separation or
C     C     distance was specified.
C     C
C           IF ( M2XIST('separation') ) THEN
C
C              ANSWER = VSEP  ( STATE1, STATE2 )
C
C           ELSE IF ( M2XIST('distance') ) THEN
C
C              ANSWER = VDIST ( STATE1, STATE2 )
C
C           END IF
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
C     W.L. Taber     (JPL)
C
C$ Version
C
C-     META/2 Configured Version 2.0.0, 9-MAY-1994 (WLT)
C
C         This is the configured version of the Command Loop
C         software as of May 9, 1994
C
C
C-     META/2 Configured Version 1.0.0, 3-MAY-1994 (WLT)
C
C         This is the configured version of META/2
C         software as of May 3, 1994
C
C
C-    Beta Version 1.0.0, 27-NOV-1991 (WLT)
C
C-&
C
C$ Index_Entry
C
C     Check for the presence of a named match in the META/2 tables.
C
C-&
 
 
C
C     Local variables
C
      INTEGER               SIZE
      SAVE                  SIZE
 
 
 
 
 
C
C     Find out how many endpoints were matched.  The NAME is there
C     if SIZE is greater than 0.
C
      CALL M2VSIZ ( NAME, SIZE )
 
      M2XIST = SIZE .GT. 0
 
      RETURN
      END
