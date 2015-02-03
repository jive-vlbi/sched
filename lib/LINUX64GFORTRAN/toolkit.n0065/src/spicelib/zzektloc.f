C$Procedure      ZZEKTLOC ( EK, locate token in tokenized EK query )
 
      SUBROUTINE ZZEKTLOC (  TOKID,  KWCODE,  NTOKEN,  TOKENS,  VALUES,
     .                       LOC,    FOUND                            )
 
C$ Abstract
C
C     Locate the first occurrence of a specified token in a tokenized
C     EK query.  The input may actually be any subset of token codes
C     and corresponding keyword codes from a tokenized query.
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
C     EK
C
C$ Keywords
C
C     EK
C     PRIVATE
C
C$ Declarations
 
      INCLUDE 'ektokn.inc'
      INCLUDE 'ekkeyw.inc'
 
      INTEGER               TOKID
      INTEGER               KWCODE
      INTEGER               NTOKEN
      INTEGER               TOKENS ( * )
      INTEGER               VALUES ( * )
      INTEGER               LOC
      LOGICAL               FOUND
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     TOKID      I   Token ID.
C     KWCODE     I   Keyword code.
C     NTOKEN     I   Number of tokens in query.
C     TOKENS     I   Token codes.
C     VALUES     I   Pointers to numeric and string token values.
C     LOC        O   Location of first occurence of token.
C     FOUND      O   Flag indicating whether token was found.
C
C$ Detailed_Input
C
C     TOKID          is a token code identifying the type of token
C                    sought.
C
C     KWCODE         is a code that specifies the desired keyword,
C                    if the desired token is a keyword.  KWCODE is
C                    ignored if the desired token is not a keyword.
C
C     NTOKEN         is the number of tokens in the input query.
C
C     TOKENS         is an array of token codes.  This array normally
C                    represents a tokenized EK query or a sublist of
C                    such a query.
C
C     VALUES         is a list of values associated with the codes
C                    contained in TOKENS.  When the Ith element of
C                    TOKENS indicates that the Ith token is a keyword,
C                    the Ith element of VALUES contains the code
C                    specifying which keyword is meant.
C
C$ Detailed_Output
C
C     LOC            is the index in the input token list at which
C                    the desired token was first encountered.  LOC
C                    is meaningful only if FOUND is .TRUE.
C
C     FOUND          is a logical flag indicating whether the desired
C                    token was found.
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
C     This is a utility that simplifies parsing of tokenized EK
C     queries.
C
C$ Examples
C
C     See ZZEKPARS.
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
C     N.J. Bachman   (JPL)
C
C$ Version
C
C-    Beta Version 1.0.0, 11-OCT-1995 (NJB)
C
C-&
 
C
C     Error free.
C
      FOUND  = .FALSE.
      LOC    =  1
 
      DO WHILE ( LOC .LE. NTOKEN )
 
 
         IF ( TOKENS(LOC) .EQ. TOKID ) THEN
 
            IF ( TOKID .EQ. TKKEY ) THEN
C
C              To get a match, the keyword codes must match.
C
               IF ( VALUES(LOC) .EQ. KWCODE ) THEN
                  FOUND  =  .TRUE.
                  RETURN
               END IF
 
            ELSE
C
C              For non-keyword tokens, we're done at this point.
C
               FOUND  =  .TRUE.
               RETURN
 
            END IF
 
         END IF
 
 
         LOC = LOC + 1
 
      END DO
 
 
      RETURN
      END
