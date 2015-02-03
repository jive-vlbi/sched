C$Procedure      EXCESS ( Report an excess of elements in a cell )
 
      SUBROUTINE EXCESS ( NUMBER, STRUCT )
 
C$ Abstract
C
C     Set the long error message so as to indicate the number of excess
C     elements encountered by a routine operating on cells or on data
C     structures based on cells.
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
C
C$ Keywords
C
C     CELLS,  ERROR
C
C$ Declarations
 
      INTEGER          NUMBER
      CHARACTER*(*)    STRUCT
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     NUMBER     I   Number of excess elements.
C     STRUCT     I   Name of the data structure.
C
C$ Detailed_Input
C
C
C     NUMBER      is the number of excess elements encountered.
C                 This may be zero or negative, which indicates
C                 no excess.
C
C     STRUCT      is the name of the data structure being manipulated.
C                 Typically, this is one of the strings: 'cell', 'set',
C                 or 'symbol table'. However, it may be any character
C                 string. STRUCT should NOT end in a period.
C                 The period at the end of the message is supplied
C                 automatically.
C
C$ Detailed_Output
C
C     None.
C
C$ Parameters
C
C      None.
C
C$ Particulars
C
C     This routine is part of the SPICELIB error handling mechanism.
C
C     EXCESS sets the long error message.  The message has the form:
C
C                 An excess of <NUMBER> element(s) could
C                 not be accomodated in the output <STRUCT>.
C
C     Leading and trailing blanks in STRUCT are removed. If there is
C     no excess (NUMBER is zero or negative), then is blank.
C
C$ Examples
C
C     The response of EXCESS to a variety of inputs is illustrated
C     below.
C
C           NUMBER = 1
C           STRUCT = 'set'
C           ERROR  = 'An excess of 1 element could not
C                     be accomodated in the output set.'
C
C           NUMBER = 5
C           STRUCT = 'stack'
C           ERROR  =  An excess of 5 elements could not
C                     be accomodated in the output stack.'
C
C           NUMBER = 0
C           STRUCT =
C           ERROR  = ' '
C
C           NUMBER = -6
C           STRUCT =
C           ERROR  = ' '
C
C     In particular, note that EXCESS does not set the long error
C     message when the number of excess elements is not positive. Also,
C     the singular 'element' is used for an excess of one, while
C     the plural 'elements' is used for all other positive excesses.
C
C$ Restrictions
C
C     None.
C
C$ Exceptions
C
C     This subprogram does not detect any errors.
C
C$ Files
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
C     C.A. Curzon     (JPL)
C     W.L. Taber      (JPL)
C     I.M. Underwood  (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (CAC) (WLT) (IMU)
C
C-&
 
C$ Index_Entries
C
C     report an excess of elements in a cell
C
C-&
 
 
C$ Revisions
C
C-    Beta Version 2.0.0, 11-JAN-1989 (NJB)
C
C        Sets the long error message directly.  No longer returns
C        an error message.  Message no longer contains name of
C        routine which detected the error.
C
C-&
 
 
 
 
C
C     SPICELIB functions
C
      LOGICAL               RETURN
 
 
C
C     Local variables
C
      INTEGER               LONGLN
      PARAMETER           ( LONGLN = 320 )
      CHARACTER*(LONGLN)    ERROR
C
C     Set up the error processing.
C
      IF ( RETURN () ) RETURN
      CALL CHKIN ( 'EXCESS' )
C
C     If there is no excess, don't report one.
C
      IF ( NUMBER .GT. 0 ) THEN
 
C
C        Begin with the number. We will build the rest of the
C        message around it.
C
         CALL INTSTR ( NUMBER, ERROR )
 
C
C        A short blurb goes in front of the number.
C
         CALL PREFIX ( 'An excess of', 1, ERROR )
 
C
C        Singular or plural?
C
         IF ( NUMBER .EQ. 1 ) THEN
            CALL SUFFIX ( 'element', 1, ERROR )
         ELSE
            CALL SUFFIX ( 'elements', 1, ERROR )
         END IF
 
C
C        Another short blurb.
C
         CALL SUFFIX ( 'could not be accommodated in the output',
     .                 1,
     .                 ERROR                                      )
 
C
C        And the name of the structure.
C
         CALL SUFFIX ( STRUCT, 1, ERROR   )
 
C
C        And a period at the end, to complete the sentence.
C
         CALL SUFFIX ( '.', 0, ERROR )
 
C
C        Set the long error message:
C
         CALL SETMSG ( ERROR )
 
      ELSE
 
          ERROR = ' '
 
      END IF
 
      CALL CHKOUT ( 'EXCESS' )
 
      RETURN
      END
