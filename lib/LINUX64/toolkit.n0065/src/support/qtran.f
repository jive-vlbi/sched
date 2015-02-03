C$Procedure     QTRAN
 
      SUBROUTINE QTRAN ( INPUT, OUTPUT, TRAN )
 
C$ Abstract
C
C     Prompt the user to supply values for the first query in a string.
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
C$ Keywords
C
C     PARSE, QUERY
C
C$ Declarations
 
      IMPLICIT NONE
      CHARACTER*(*)         INPUT
      CHARACTER*(*)         OUTPUT
      LOGICAL               TRAN
 
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     INPUT      I   Input string, possibly containing queries.
C     OUTPUT     O   Equivalent string, with first query replaced.
C     TRAN       O   True when a query was replaced.
C
C$ Detailed_Input
C
C     INPUT      is the input string. This may contain any number
C                of queries, for which the user will be expected to
C                supply values. A query is any string of up to 32
C                consecutive non-blank characters ending with '?'.
C
C$ Detailed_Output
C
C     OUTPUT     is the equivalent of INPUT after the first of the
C                queries in INPUT has been supplied with a value.
C
C                OUTPUT may overwrite INPUT.
C
C      TRAN      is true whenever a query was found and replaced, and is
C                false otherwise.
C
C$ Input_Files
C
C     None.
C
C$ Output_Files
C
C     None.
C
C$ Input_Output_Common
C
C     None.
C
C$ Exceptions
C
C     It is possible that query resolution will result in an overflow
C     of the output string.  This situation is dianosed by a routine
C     called by QTRAN.
C
C$ Detailed_Description
C
C     Look for a query in INPUT. (It will end with '?'.) Ask the user
C     to supply a value for the query. Insert the value into OUTPUT in
C     place of the query itself.
C
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     None.
C
C$ Author_and_Institution
C
C     I. M. Underwood (JPL)
C
C$ Version_and_Date
C
C     Version 1.1, 14-SEP-1995
C
C        Assignment to otherwise unused variable SUPRES deleted.
C
C     Version 1,   17-SEP-1986
C
C-&
 
 
 
C
C     OPTLIB functions
C
      INTEGER               LASTNB
 
C
C     Local variables
C
      CHARACTER*(1)         EQUOTE
      CHARACTER*(1)         DELIM
      CHARACTER*33          QUERY
      INTEGER               LOC
      INTEGER               QLEN
      CHARACTER*128         REPLY
      INTEGER               RLEN
 
      CHARACTER*55          PROMPT
      INTEGER               I
 
C
C     Look up the special marker used for suppressing query
C     evaluation.
C
      CALL GETEQ ( EQUOTE )
      CALL GETDEL( DELIM  )
 
 
C
C     Look at each word. If a word ends with '?', it's a query.
C     (QUERY is a character longer than a valid query. So any
C     valid query will have at least one blank at the end.)
C
      TRAN   = .FALSE.
      I      =  1
 
 
 
      CALL NTHUQW ( INPUT, I, EQUOTE, QUERY, LOC )
 
      DO WHILE ( (.NOT. TRAN)  .AND.  QUERY .NE. ' ' )
 
C
C        First we have to look for the translation supression
C        character.
C
 
 
 
         TRAN = (       ( INDEX (QUERY,'? ') .GT.  0       )
     .            .AND. (        QUERY       .NE. '?'     ) )
 
         IF ( .NOT. TRAN ) THEN
            I = I + 1
            CALL NTHUQW ( INPUT, I, EQUOTE, QUERY, LOC )
         END IF
 
 
      END DO
 
 
      OUTPUT = INPUT
 
C
C     If we found a query, get the user's response, and insert it
C     in place of the query.
C
      IF ( TRAN ) THEN
 
         QLEN   = LASTNB ( QUERY )
         PROMPT = 'Enter value for ' // QUERY(1:QLEN-1) // ' > '
 
         CALL RDSTMN  ( PROMPT, DELIM, REPLY   )
         RLEN   = MAX ( 1,  LASTNB ( REPLY ) )
 
         CALL REPSUB  ( OUTPUT, LOC, LOC+QLEN-1, REPLY(1:RLEN), OUTPUT )
 
       END IF
 
      RETURN
      END
