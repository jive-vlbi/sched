C$Procedure      SIZE ( Process a SUBTeX size change request )
 
      SUBROUTINE SIZE ( SOURCE )
 
C$ Abstract
C
C     Process a @setvarsize or @setparamsize control sequence.
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
 
      CHARACTER*(*)         SOURCE
 
C$ Detailed_Input
C
C     SOURCE      is a source line containing a @setvarsize or
C                 @setparamsize control sequence.
C
C$ Detailed_Output
C
C     No lines of output are produced by this routine. The values of
C     VARNAMESIZE, VARTYPESIZE, and PARAMNAMESIZE may be changed.
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     SOURCE     I   Source line.
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
      INTEGER               CPOS
      INTEGER               NBLEN
      INTEGER               NCPOS
      LOGICAL               RETURN
 
C
C     Local variables
C
      INTEGER               BEGINC
      INTEGER               ENDC
      INTEGER               BEGINN
      INTEGER               ENDN
      INTEGER               BEGINT
      INTEGER               ENDT
 
C
C     Standard SPICE error handling
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SIZE' )
      END IF
 
C
C     We should see something like
C
C        @setvarsize{xxx}{yyy}
C
C     or
C
C        @setparamsize{zzz}
C
C     In either case, the control sequence is delimited by '{',
C     and the variable or parameter name is delimited by '}'.
C
      BEGINC = NCPOS ( SOURCE, ' ', 1      )
      ENDC   = CPOS  ( SOURCE, '{', BEGINC ) - 1
 
      BEGINN = CPOS  ( SOURCE, '{', ENDC   ) + 1
      ENDN   = CPOS  ( SOURCE, '}', BEGINN ) - 1
 
C
C     Set the name size. Go back for the type template if this
C     is a variable.
C
      IF ( SOURCE(BEGINC:ENDC) .EQ. '@setvarsize' ) THEN
         CALL PARAMS ( 'SET',
     .                 'VARNAMESIZE',
     .                 NBLEN ( SOURCE(BEGINN:ENDN) ) )
 
         BEGINT = CPOS  ( SOURCE, '{', ENDN   ) + 1
         ENDT   = CPOS  ( SOURCE, '}', BEGINT ) - 1
 
         CALL PARAMS ( 'SET',
     .                 'VARTYPESIZE',
     .                 NBLEN ( SOURCE(BEGINT:ENDT) ) )
 
      ELSE IF ( SOURCE(BEGINC:ENDC) .EQ. '@setparamsize' ) THEN
         CALL PARAMS ( 'SET',
     .                 'PARAMNAMESIZE',
     .                 NBLEN ( SOURCE(BEGINN:ENDN) ) )
      END IF
 
      CALL CHKOUT ( 'SIZE' )
      RETURN
      END
 
