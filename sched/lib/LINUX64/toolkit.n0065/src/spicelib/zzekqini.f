C$Procedure   ZZEKQINI ( Private: EK, intialize encoded query )
 
      SUBROUTINE ZZEKQINI ( ISIZE, DSIZE, EQRYI, EQRYC, EQRYD )
 
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Initialize encoded EK query.
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
 
      INCLUDE 'ekquery.inc'
      INCLUDE 'ekqlimit.inc'
      INCLUDE 'ekbool.inc'
 
      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )
 
      INTEGER               ISIZE
      INTEGER               DSIZE
      INTEGER               EQRYI ( LBCELL : * )
      CHARACTER*(*)         EQRYC
      DOUBLE PRECISION      EQRYD ( * )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     ISIZE      I   Size of integer component of encoded query.
C     DSIZE      I   Size of d.p. component of encoded query.
C     EQRYI     I-O  Integer component of query.
C     EQRYC     I-O  Character component of query.
C     EQRYD     I-O  D.p. component of query.
C
C$ Detailed_Input
C
C     ISIZE          is the size of the cell comprising the integer
C                    component of the encoded query.
C
C     DSIZE          is the size of the array comprising the double
C                    precision component of the encoded query.
C
C     EQRYI          is an integer array that is to serve as the
C                    integer portion of an encoded EK query.  EQRYI
C                    will be initialized as an integer cell having
C                    size ISIZE.
C
C     EQRYC          is a character string that is to serve as
C                    the character portion of an encoded EK query.
C                    EQRYC will be set to blank.
C
C     EQRYD          is a double precision array that is to serve as
C                    the numeric portion of an encoded EK query.
C
C$ Detailed_Output
C
C     EQRYI,
C     EQRYD,
C     EQRYC          are the components of an initialized EK query.
C                    The query's architecture code will be set to 1.
C                    All counts will be set to zero.  The array
C                    EQRYD will be zeroed out.  The character string
C                    EQRYC will be blank.
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) ISIZE must be large enough to accommodate a query with the
C        maximum number of tables, constraints, and tokens, and
C        indicated by the parameters in ekqlimit.inc.  If ISIZE is
C        too small, the error SPICE(CELLTOOSMALL) will be signalled.
C
C     2) DSIZE must be large enough to accommodate the largest number
C        of numeric tokens that can occur in a query.  If DSIZE is
C        too small, the error SPICE(CELLTOOSMALL) will be signalled.
C
C     3) EQRYC must be long enough to accommodate all of the character
C        data that can occur in a query.  If EQRYC is too short, the
C        error  SPICE(STRINGTOOSHORT) will be signalled.
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
C     See ZZEKPARS.
C
C$ Restrictions
C
C     1) This routine is private to the EK library.  No routines
C        external to the EK library should call this routine.
C
C     2) Uses EK encoded query architecture version 1.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman       (JPL)
C
C$ Version
C
C-    Beta Version 1.0.0, 17-OCT-1995 (NJB)
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
C     Minimum upper bound for the integer cell of an encoded query:
C
      INTEGER               MNISIZ
      PARAMETER           ( MNISIZ =     EQVBAS
     .                                +  MAXTAB * EQVDSZ * 2
     .                                +  MAXCON * EQCDSZ
     .                                +  MAXCON
     .                                +  MAXORD * EQODSZ
     .                                +  MAXSEL * EQSDSZ )
 
C
C     Local variables
C
      INTEGER               I
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'ZZEKQINI' )
      END IF
 
 
C
C     Check sizes:
C
      IF ( ISIZE .LT. MNISIZ ) THEN
 
         CALL SETMSG ( 'Size of integer component of encoded query ' //
     .                 'is #; at least # elements are required.'      )
         CALL ERRINT ( '#',   ISIZE                                   )
         CALL ERRINT ( '#',   MNISIZ                                  )
         CALL SIGERR ( 'SPICE(CELLTOOSMALL)'                          )
         CALL CHKOUT ('ZZEKQINI'                                      )
         RETURN
 
      END IF
 
 
      IF ( DSIZE .LT. MAXQNM ) THEN
 
         CALL SETMSG ( 'Size of d.p. component of encoded query ' //
     .                 'is #; at least # elements are required.'      )
         CALL ERRINT ( '#',   DSIZE                                   )
         CALL ERRINT ( '#',   MAXQNM                                  )
         CALL SIGERR ( 'SPICE(CELLTOOSMALL)'                          )
         CALL CHKOUT ('ZZEKQINI'                                      )
         RETURN
 
      END IF
 
 
      IF ( LEN(EQRYC) .LT. MAXCLN ) THEN
 
         CALL SETMSG ( 'Size of character component of encoded query '//
     .                 'is #; a length of at least # characters is '  //
     .                 'required.'                                    )
         CALL ERRINT ( '#',   LEN(EQRYC)                              )
         CALL ERRINT ( '#',   MAXCLN                                  )
         CALL SIGERR ( 'SPICE(STRINGTOOSHORT)'                        )
         CALL CHKOUT ('ZZEKQINI'                                      )
         RETURN
 
      END IF
 
C
C     Initialize the integer cell, the d.p. array, and the string.
C
      CALL SSIZEI ( ISIZE, EQRYI )
      CALL CLEARD ( DSIZE, EQRYD )
      EQRYC = ' '
 
C
C     Append enough elements to the integer cell to contain the
C     fixed-size portion of the encoded query:
C
      DO I = 1, EQVBAS
         CALL APPNDI ( 0, EQRYI )
      END DO
 
C
C     Clear out the fixed-size portion of the integer cell.
C
      CALL CLEARI ( EQVBAS, EQRYI(1) )
 
C
C     Fill in the architecture version.
C
      EQRYI( EQARCH )  =  1
 
C
C     Set the parse completion and name and time resolution flags to
C     indicate `not done':
C
      EQRYI(EQPARS)    =  IFALSE
      EQRYI(EQNRES)    =  IFALSE
      EQRYI(EQTRES)    =  IFALSE
 
C
C     Set the buffer sizes:
C
      EQRYI(EQCSIZ)    =  LEN(EQRYC)
      EQRYI(EQNSIZ)    =  DSIZE
 
C
C     Set the free pointers:
C
      EQRYI(EQNPTR)    =  1
      EQRYI(EQCPTR)    =  1
 
C
C     Indicate that initialization has been done:
C
      EQRYI(EQINIT)    =  ITRUE
 
      CALL CHKOUT ( 'ZZEKQINI' )
      RETURN
      END
