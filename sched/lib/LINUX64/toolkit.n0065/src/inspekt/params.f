C$Procedure      PARAMS ( Manage SUBTeX parameters )
 
      SUBROUTINE PARAMS ( ACTION, NAME, VALUE )
 
C$ Abstract
C
C     Set or get the values of adjustable SUBTeX parameters.
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
C     SUBTeX
C
C$ Keywords
C
C     SUBTeX
C
C$ Declarations
 
      CHARACTER*(*)         ACTION
      CHARACTER*(*)         NAME
      INTEGER               VALUE
 
 
C$ Detailed_Input
C
C     ACTION      determines whether the value of the parameter
C                 is to be set ('SET') or returned ('GET').
C
C     NAME        is the name of the parameter to be assigned or
C                 queried. Recognized parameters and their default
C                 values are listed below.
C
C                    Name                 Default value
C                    -----------------    -------------
C                    PAGEWIDTH                       80
C                    LEFTSKIP                         8
C                    RIGHTSKIP                        0
C                    LITERALINDENT                    3
C                    ITEMINDENT                       6
C                    ITEMSKIP                         3
C                    VARNAMESIZE                     10
C                    VARNAMESKIP                      3
C                    VARTYPESIZE                      5
C                    VARTYPESKIP                      3
C                    PARAMNAMESIZE                   10
C                    PARAMNAMESKIP                    3
C                    LISTINDEX                        1
C
C     VALUE       is the new value to be assigned to the specified
C                 parameter whenever ACTION is 'SET'.
C
C$ Detailed_Output
C
C     VALUE       is the current value of the specified parameter
C                 whenever ACTION is 'GET'.
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     ACTION     I   'SET' or 'GET'.
C     NAME       I   Parameter name.
C     VALUE     I/O  Parameter value.
C
C$ Files
C
C     None.
C
C$ Exceptions
C
C     1) If ACTION is not recognized, the error 'SUBTeX(BADPARAMACTION)'
C        is signalled.
C
C     2) If NAME is not recognized, the error 'SUBTeX(BADPARAMNAME)'
C        is signalled.
C
C$ Particulars
C
C     The values of the adjustable parameters are typically set by
C     the calling program (FORTeX, for example) according to its
C     special needs. The current values are retrieved by SUBTeX as
C     required.
C
C     The value of LISTINDEX is reset to one by the control sequence
C     @newlist.
C
C$ Examples
C
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C$Include SUBTeX.REFS
C
C$ Author_and_Institution
C
C     I.M. Underwood (JPL)
C
C$ Version
C
C     Beta Version 1.0.0, 11-JUN-1988 (IMU)
C
C-&
 
 
C
C     SPICELIB functions
C
      LOGICAL               RETURN
 
C
C     Local variables
C
 
      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )
 
      INTEGER               MAXPAR
      PARAMETER           ( MAXPAR = 13 )
 
      CHARACTER*16          NAMES         ( LBCELL:MAXPAR )
      INTEGER               PTRS          ( LBCELL:MAXPAR )
      INTEGER               VALUES        ( LBCELL:MAXPAR )
 
      LOGICAL               INIT
 
      CHARACTER*4           WHAT
      CHARACTER*17          WHO
 
      LOGICAL               FOUND
 
C
C     Saved variables
C
      SAVE                  NAMES
      SAVE                  PTRS
      SAVE                  VALUES
      SAVE                  INIT
 
C
C     Initial values
C
      DATA                  INIT          / .FALSE. /
 
 
C
C     Standard SPICE error handling
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'PARAMS' )
      END IF
 
C
C     Initialize the table if necessary.
C
      IF ( .NOT. INIT ) THEN
 
         CALL SSIZEC ( MAXPAR, NAMES  )
         CALL SSIZEI ( MAXPAR, PTRS   )
         CALL SSIZEI ( MAXPAR, VALUES )
 
         CALL SYSETI ( 'PAGEWIDTH    ', 80, NAMES, PTRS, VALUES )
         CALL SYSETI ( 'LEFTSKIP     ',  8, NAMES, PTRS, VALUES )
         CALL SYSETI ( 'RIGHTSKIP    ',  0, NAMES, PTRS, VALUES )
         CALL SYSETI ( 'LITERALINDENT',  3, NAMES, PTRS, VALUES )
         CALL SYSETI ( 'ITEMINDENT   ',  6, NAMES, PTRS, VALUES )
         CALL SYSETI ( 'ITEMSKIP     ',  3, NAMES, PTRS, VALUES )
         CALL SYSETI ( 'VARNAMESIZE  ', 10, NAMES, PTRS, VALUES )
         CALL SYSETI ( 'VARNAMESKIP  ',  3, NAMES, PTRS, VALUES )
         CALL SYSETI ( 'VARTYPESIZE  ',  5, NAMES, PTRS, VALUES )
         CALL SYSETI ( 'VARTYPESKIP  ',  3, NAMES, PTRS, VALUES )
         CALL SYSETI ( 'PARAMNAMESIZE', 10, NAMES, PTRS, VALUES )
         CALL SYSETI ( 'PARAMNAMESKIP',  3, NAMES, PTRS, VALUES )
         CALL SYSETI ( 'LISTINDEX    ',  1, NAMES, PTRS, VALUES )
 
         INIT = .TRUE.
 
      END IF
 
C
C     Shake or bake?
C
      CALL UCASE ( ACTION, WHAT )
      CALL UCASE ( NAME,   WHO  )
 
      IF ( WHAT .EQ. 'SET' ) THEN
         CALL SYSETI ( WHO, VALUE, NAMES, PTRS, VALUES )
 
      ELSE IF ( WHAT .EQ. 'GET' ) THEN
         CALL SYNTHI ( WHO, 1, NAMES, PTRS, VALUES, VALUE, FOUND )
 
         IF ( .NOT. FOUND ) THEN
            CALL SETMSG ( 'Trying to retrieve #' )
            CALL ERRCH  ( '#', WHO )
 
            CALL SIGERR ( 'SUBTeX(BADPARAMNAME)' )
         END IF
 
      ELSE
         CALL SETMSG ( 'Trying to #' )
         CALL ERRCH  ( '#', WHAT )
 
         CALL SIGERR ( 'SUBTeX(BADPARAMACTION)' )
      END IF
 
      CALL CHKOUT ( 'PARAMS' )
      RETURN
      END
 
