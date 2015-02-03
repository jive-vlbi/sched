C$Procedure   ZZEKREQI ( Private: EK, read from encoded query, integer )
 
      SUBROUTINE ZZEKREQI ( EQRYI, NAME, VALUE )
 
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Read scalar integer value from encoded EK query.
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
 
      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )
 
      INTEGER               EQRYI ( LBCELL : * )
      CHARACTER*(*)         NAME
      INTEGER               VALUE
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     EQRYI      I   Integer component of query.
C     NAME       I   Name of scalar item to read.
C     VALUE      O   Value of item.
C
C$ Detailed_Input
C
C     EQRYI          is the integer portion of an encoded EK query.
C
C     NAME           is the name of the item whose value is to be read.
C                    This item is some element of the integer portion
C                    of an encoded query.
C
C$ Detailed_Output
C
C     VALUE          is the integer value designated by NAME.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the input name is not recognized, the error
C         SPICE(INVALIDNAME) is signalled.  The encoded query is not
C         modified.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine is the inverse of ZZEKWEQI.
C
C$ Examples
C
C     See EKSRCH.
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
      INTEGER               ISRCHC
 
C
C     Local parameters
C
      INTEGER               NAMLEN
      PARAMETER           ( NAMLEN = 32 )
 
      INTEGER               MAXNAM
      PARAMETER           ( MAXNAM = 15 )
C
C     Local variables
C
      CHARACTER*(NAMLEN)    NAMLST ( MAXNAM )
      CHARACTER*(NAMLEN)    TMPNAM
 
      INTEGER               I
      INTEGER               NAMIDX ( MAXNAM )
 
C
C     Saved variables
C
      SAVE
 
C
C     Initial values
C
      DATA  (   NAMLST(I),          NAMIDX(I),   I = 1, MAXNAM  )  /
     .           'ARCHITECTURE',      EQARCH,
     .           'INITIALIZED',       EQINIT,
     .           'PARSED',            EQPARS,
     .           'NAMES_RESOLVED',    EQNRES,
     .           'TIMES_RESOLVED',    EQTRES,
     .           'SEM_CHECKED',       EQSCHK,
     .           'NUM_TABLES',        EQNTAB,
     .           'NUM_CONJUNCTIONS',  EQNCNJ,
     .           'NUM_CONSTRAINTS',   EQNCNS,
     .           'NUM_SELECT_COLS',   EQNSEL,
     .           'NUM_ORDERBY_COLS',  EQNORD,
     .           'NUM_BUF_SIZE',      EQNSIZ,
     .           'FREE_NUM',          EQNPTR,
     .           'CHR_BUF_SIZE',      EQCSIZ,
     .           'FREE_CHR',          EQCPTR                       /
 
 
C
C     Use discovery check-in.
C
C
C     Find the location of the named item.
C
      CALL LJUST ( NAME,   TMPNAM )
      CALL UCASE ( TMPNAM, TMPNAM )
 
      I  =  ISRCHC ( TMPNAM, MAXNAM, NAMLST )
 
      IF ( I .EQ. 0 ) THEN
 
         CALL CHKIN  ( 'ZZEKREQI'           )
         CALL SETMSG ( 'Item # not found.'  )
         CALL ERRCH  ( '#',  NAME           )
         CALL SIGERR ( 'SPICE(INVALIDNAME)' )
         CALL CHKOUT ( 'ZZEKREQI'           )
         RETURN
 
      END IF
 
C
C     Do the deed.
C
      VALUE  =  EQRYI( NAMIDX(I) )
 
      RETURN
      END
