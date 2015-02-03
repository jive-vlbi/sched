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
      SUBROUTINE RPTSYM ( ID, COMP, STRING, WDTH, NAME, DEF, VALUE )
 
      IMPLICIT NONE
 
      INTEGER               ID
      INTEGER               COMP
      CHARACTER*(*)         STRING
      INTEGER               WDTH
      CHARACTER*(*)         NAME
      CHARACTER*(*)         DEF
      CHARACTER*(*)         VALUE
 
C
C     This routine is a utility for setting and retrieving symbol
C     names, definitions and expanded values.  It is intended that
C     this be used by a higher level routine that fetches symbol
C     definitions one at a time, puts the definition here and
C     passes the routine RETSYM to a formatting routine.
C
C     The ENTRY point SETSYM allows you to set the symbol and its
C     values.
C
C     The ENTRY point RETSYM returns the last set values.  To
C     request a portion of a symbol you supply the following
C     values for ID and COMP
C
C        1,1 for the symbol name
C        2,1 for the symbol definition
C        2,2 or 3,1 for the symbol expanded value.
C
C     If used with the routine TABRPT you can then easily display
C     symbols as:
C
C        name    definition    fully_expanded_value
C
C     or
C
C        name    definition
C                fully_expanded_value.
C
 
      INTEGER               RTRIM
 
      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE = 32 )
 
      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 1000 )
 
 
 
      CHARACTER*(WDSIZE)    SYMNAM
      CHARACTER*(LNSIZE)    SYMDEF
      CHARACTER*(LNSIZE)    SYMVAL
      SAVE
 
      RETURN
 
      ENTRY SETSYM ( NAME, DEF, VALUE )
 
         SYMNAM = NAME
         SYMDEF = DEF
         SYMVAL = VALUE
 
         RETURN
 
 
      ENTRY RETSYM ( ID, COMP, STRING, WDTH )
 
         IF      ( ID .EQ. 1 ) THEN
 
            IF ( COMP .NE. 1 ) THEN
               STRING = ' '
            ELSE
               STRING = SYMNAM
            END IF
 
         ELSE IF ( ID .EQ. 2 ) THEN
 
            IF ( COMP .EQ. 1 ) THEN
               STRING = SYMDEF
            ELSE IF ( COMP .EQ. 2 ) THEN
               STRING = SYMVAL
            ELSE
               STRING = ' '
            END IF
 
         ELSE IF ( ID .EQ. 3 ) THEN
 
            IF ( COMP .EQ. 1 ) THEN
               STRING = SYMVAL
            ELSE
               STRING = ' '
            END IF
 
         END IF
 
         WDTH = RTRIM ( STRING )
 
         RETURN
 
 
 
 
      END
