C$Procedure   ZZEKWEQI ( Private: EK, write to encoded query, integer )
 
      SUBROUTINE ZZEKWEQI ( NAME, VALUE, EQRYI )
 
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Write scalar integer value to encoded EK query.
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
 
      CHARACTER*(*)         NAME
      INTEGER               VALUE
      INTEGER               EQRYI ( LBCELL : * )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     NAME       I   Name of scalar item to write.
C     VALUE      I   Value to write.
C     EQRYI     I-O  Integer component of query.
C
C$ Detailed_Input
C
C     NAME           is the name of the item whose value is to be set.
C                    This item is some element of the integer portion
C                    of an encoded query.  The currently supported set
C                    of names is:
C
C                       ARCHITECTURE
C                       INITIALIZED
C                       PARSED
C                       NAMES_RESOLVED
C                       TIMES_RESOLVED
C                       SEM_CHECKED
C                       NUM_TABLES
C                       NUM_CONJUNCTIONS
C                       NUM_CONSTRAINTS
C                       NUM_SELECT_COLS
C                       NUM_ORDERBY_COLS
C                       NUM_BUF_SIZE
C                       FREE_NUM
C                       CHR_BUF_SIZE
C                       FREE_CHR
C
C     VALUE          is an integer value to assign to the quantity
C                    designated by NAME.
C
C     EQRYI          is the integer portion of an encoded EK query.
C
C$ Detailed_Output
C
C     EQRYI          is the integer portion of an encoded EK query,
C                    updated to reflect the requested assignment.
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
C     This routine is intended to hide from calling routines the
C     specifics of the EK encoded query structure.  See the include
C     file ekquery.inc if details of this structure are desired.
C
C$ Examples
C
C     See ZZEKNRES.
C
C$ Restrictions
C
C     1) Uses EK encoded query architecture version 2.
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
C-    Beta Version 1.0.0, 01-AUG-1995 (NJB)
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
 
         CALL CHKIN  ( 'ZZEKWEQI'           )
         CALL SETMSG ( 'Item # not found.'  )
         CALL ERRCH  ( '#',  NAME           )
         CALL SIGERR ( 'SPICE(INVALIDNAME)' )
         CALL CHKOUT ( 'ZZEKWEQI'           )
         RETURN
 
      END IF
 
C
C     Do the deed.
C
      EQRYI( NAMIDX(I) ) = VALUE
 
      RETURN
      END
