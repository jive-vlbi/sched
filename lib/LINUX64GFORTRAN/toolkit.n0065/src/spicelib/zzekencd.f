C$Procedure      ZZEKENCD ( EK, encode query )
 
      SUBROUTINE ZZEKENCD (  QUERY,
     .                       EQRYI,  EQRYC,   EQRYD,
     .                       ERROR,  ERRMSG,  ERRPTR  )
 
C$ Abstract
C
C     Convert an EK query to encoded form.
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
 
 
      INCLUDE 'ekopcd.inc'
      INCLUDE 'ekqlimit.inc'
      INCLUDE 'ekquery.inc'
      INCLUDE 'ektype.inc'
 
      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )
 
      CHARACTER*(*)         QUERY
      INTEGER               EQRYI  ( LBCELL : * )
      CHARACTER*(*)         EQRYC
      DOUBLE PRECISION      EQRYD  ( * )
      LOGICAL               ERROR
      CHARACTER*(*)         ERRMSG
      INTEGER               ERRPTR
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     QUERY      I   Query specifying events to be found.
C     EQRYI      O   Integer component of encoded query.
C     EQRYC      O   Character component of encoded query.
C     EQRYD      O   Numeric component of encoded query.
C     ERROR      O   Flag indicating whether query parsed correctly.
C     ERRMSG     O   Parse error description.
C     ERRPTR     O   Error pointer.
C
C$ Detailed_Input
C
C     QUERY          is an EK query, starting after the FROM keyword.
C
C$ Detailed_Output
C
C     EQRYI          is the integer portion of an encoded EK query.
C                    Semantic checking will have been performed.
C
C     EQRYC          is the character portion of an encoded EK query.
C
C     EQRYD          is the numeric portion of an encoded EK query.
C
C     ERROR          is a logical flag indicating whether the query
C                    was syntactically correct and, as far as this
C                    routine could determine, semantically correct.
C
C     ERRMSG         is a character string that describes ZZEKENCD's
C                    diagnosis of a parse error, should one occur.
C                    Otherwise, ERRMSG will be returned blank.
C
C     ERRPTR         is the index, within the input query, of the
C                    first character at which an error was detected
C                    ERRPTR is valid only if ERROR is returned .TRUE.
C
C$ Parameters
C
C     See the include files.
C
C$ Exceptions
C
C     If a parse error occurs, either the outputs ERROR, ERRMSG, and
C     ERRPTR will be set by routines called by this routine, or an
C     error will be signalled by routines called by this routine.
C     Under normal circumstances, no errors will be signalled.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine assumes that encoded EK query architecture version
C     1 is to be used with the query to be initialized; this routine
C     will not work with any other architecture version.
C
C$ Examples
C
C     See EKPSEL.
C
C$ Restrictions
C
C     1)  Uses EK encoded query architecture version 1.
C
C     2)  A leapseconds kernel must be loaded before this routine may
C         be called, if UTC time values are used in input queries.
C
C     3)  An appropriate SCLK kernel must be loaded before this routine
C         may be called, if SCLK values are used in input queries.
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
C-    Beta Version 4.0.0, 10-OCT-1995 (NJB)
C
C-&
 
 
C
C     SPICELIB functions
C
      LOGICAL               RETURN
 
C
C     Local parameters
C
C
C     Storage limits:
C
      INTEGER               MAXCOL
      PARAMETER           ( MAXCOL = 100 )
 
C
C     Local variables
C
      CHARACTER*(MAXCLN)    CHRBUF
 
      DOUBLE PRECISION      NUMVLS ( MAXQNM )
 
      INTEGER               CHBEGS ( MAXTOK )
      INTEGER               CHENDS ( MAXTOK )
      INTEGER               LXBEGS ( MAXTOK )
      INTEGER               LXENDS ( MAXTOK )
      INTEGER               NTOKEN
      INTEGER               TOKENS ( MAXTOK )
      INTEGER               VALUES ( MAXTOK )
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'ZZEKENCD' )
      END IF
 
C
C     Initialize the encoded query each time, for safety.
C
      CALL ZZEKQINI ( EQIMIN, MAXQNM, EQRYI, EQRYC, EQRYD )
 
C
C     Find the tokens in the input query.
C
      CALL ZZEKSCAN ( QUERY,  MAXTOK, MAXQNM, NTOKEN, TOKENS,
     .                LXBEGS, LXENDS, VALUES, NUMVLS, CHRBUF,
     .                CHBEGS, CHENDS, ERROR,  ERRMSG         )
 
      IF ( ERROR ) THEN
         ERRPTR  = 1
         CALL CHKOUT ( 'ZZEKENCD' )
         RETURN
      END IF
 
 
C
C     Now parse the query.
C
      CALL ZZEKPARS (  QUERY,   NTOKEN,  LXBEGS,  LXENDS,
     .                 TOKENS,  VALUES,  NUMVLS,  CHRBUF,
     .                 CHBEGS,  CHENDS,  EQRYI,   EQRYC,
     .                 EQRYD,   ERROR,   ERRMSG          )
 
      IF ( ERROR ) THEN
         ERRPTR = 1
         CALL CHKOUT ( 'ZZEKENCD' )
         RETURN
      END IF
 
 
C
C     Resolve names.
C
      CALL ZZEKNRES ( QUERY, EQRYI, EQRYC,
     .                       ERROR, ERRMSG, ERRPTR )
 
      IF ( ERROR ) THEN
         CALL CHKOUT ( 'ZZEKENCD' )
         RETURN
      END IF
 
 
C
C     Resolve time values, if necessary.
C
      CALL ZZEKTRES ( QUERY, EQRYI,  EQRYC,  EQRYD,
     .                       ERROR,  ERRMSG, ERRPTR  )
 
      IF ( ERROR ) THEN
         CALL CHKOUT ( 'ZZEKENCD' )
         RETURN
      END IF
 
 
C
C     Perform semantic checks.
C
      CALL ZZEKSEMC ( QUERY, EQRYI, EQRYC,
     .                       ERROR, ERRMSG, ERRPTR )
 
      IF ( ERROR ) THEN
         CALL CHKOUT ( 'ZZEKENCD' )
         RETURN
      END IF
 
 
      CALL CHKOUT ( 'ZZEKENCD' )
      RETURN
      END
