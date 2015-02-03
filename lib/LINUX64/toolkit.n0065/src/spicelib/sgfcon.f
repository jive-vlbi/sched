C$Procedure      SGFCON ( Generic Segments: Fetch constants )
 
      SUBROUTINE SGFCON ( HANDLE, DESCR, FIRST, LAST, VALUES )
 
C$ Abstract
C
C     Given the descriptor for a generic segment in a DAF file
C     associated with HANDLE, fetch from the constants partition
C     of the segment the double precision numbers from FIRST to
C     LAST.
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
C     DAF Required Reading.
C
C$ Keywords
C
C     GENERIC SEGMENTS
C
C$ Declarations
 
      IMPLICIT NONE
 
      INTEGER               HANDLE
      DOUBLE PRECISION      DESCR  ( * )
      INTEGER               FIRST
      INTEGER               LAST
      DOUBLE PRECISION      VALUES ( * )
 
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     HANDLE     I   Handle of a DAF open for reading.
C     DESCR      I   Descriptor for a generic segment in the DAF.
C     FIRST      I   The index of the first constant value to fetch.
C     LAST       I   The index of the last constant value to fetch.
C     VALUES     O   The constant values that were requested.
C
C$ Detailed_Input
C
C     HANDLE     is the handle of a DAF opened for reading that
C                contains the segment described by DESCR.
C
C     DESCR      is the descriptor of the segment with the desired
C                constant values. This must be the descriptor for a
C                generic segment in the DAF associated with HANDLE.
C
C     FIRST      is the index of the first value to fetch from the
C                constants section of the generic segment associated
C                with HANDLE and DESCR.
C
C     LAST       is the index of the last value to fetch from the
C                constants section of the generic segment associated
C                with HANDLE and DESCR.
C
C$ Detailed_Output
C
C     VALUES      is the array of constant values obtained from the
C                 constants section of the generic segment associated
C                 with HANDLE and DESCR.
C
C$ Parameters
C
C     This subroutine makes use of parameters defined in the file
C     'sgparam.inc'.
C
C$ Files
C
C     See the description of HANDLE above.
C
C$ Exceptions
C
C     1) The error SPICE(REQUESTOUTOFBOUNDS) will be signalled
C        if FIRST is less than 1 or LAST is greater than the
C        number of constants.
C
C     2) The error SPICE(REQUESTOUTOFORDER) will be signalled
C        if LAST is less than FIRST.
C
C$ Particulars
C
C     This routine allows easy access to values from the constants
C     partition of a generic segment in a DAF file. Please see the DAF
C     Required Reading or the include file 'sgparam.inc' for a more
C     detailed description of a generic segment.
C
C$ Examples
C
C     Suppose that you have located a DAF generic segment. The
C     fragment of code below shows how to fetch all of the
C     constants from that segment.
C
C        Declarations:
C
C        DOUBLE PRECISION      CONSTS(<enough room to hold constants>)
C
C        INTEGER               MYNCON
C
C        Get the number of items in the constants section.
C
C        CALL SGMETA ( HANDLE, DESCR, NCON, MYNCON )
C
C        Fetch the constants from the segment.
C
C        CALL SGFCON ( HANDLE, DESCR, 1, MYNCON, CONSTS )
C
C$ Restrictions
C
C     The segment described by DESCR must be a generic segment,
C     otherwise the results of this routine are not predictable.
C
C$ Author_and_Institution
C
C     K.R. Gehringer  (JPL)
C     W.L. Taber      (JPL)
C
C$ Literature_References
C
C     None.
C
C$ Version
C
C-    SPICELIB Version 1.1.0, 07-SEP-2001 (EDW)
C
C        Replaced DAFRDA call with DAFGDA.
C
C-    SPICELIB Version 1.0.0, 11-APR-1995 (KRG) (WLT)
C
C-&
 
 
C$ Index_Entries
C
C     fetch constants from a generic segment
C
C-&
 
C
C     Spicelib Functions
C
      LOGICAL               FAILED
      LOGICAL               RETURN
 
C
C     Local Parameters
C
C     Include the mnemonic values for the generic segment declarations.
C
      INCLUDE 'sgparam.inc'
 
C
C     Local Variables
C
      INTEGER               B
      INTEGER               BASE
      INTEGER               E
      INTEGER               MYNCON
C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      END IF
 
      CALL CHKIN ( 'SGFCON' )
 
C
C     Get the value for the base of the constants and the number of
C     constants in the generic segment.
C
      CALL SGMETA ( HANDLE, DESCR, CONBAS, BASE   )
      CALL SGMETA ( HANDLE, DESCR, NCON,   MYNCON )
 
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'SGFCON' )
         RETURN
      END IF
 
C
C     Perform checks on the inputs for reasonableness.
C
      IF ( ( FIRST .LT. 1 ) .OR. ( LAST .GT. MYNCON ) ) THEN
 
         CALL SETMSG ( 'The range of constants requested'
     .   //            ' extends beyond the available constant'
     .   //            ' data.  Constants are available for'
     .   //            ' indices 1 to #.  You have requested'
     .   //            ' data from # to #. '                     )
         CALL ERRINT ( '#', MYNCON                               )
         CALL ERRINT ( '#', FIRST                                )
         CALL ERRINT ( '#', LAST                                 )
         CALL SIGERR ( 'SPICE(REQUESTOUTOFBOUNDS)'               )
         CALL CHKOUT ( 'SGFCON'                                  )
         RETURN
 
      END IF
 
      IF ( LAST .LT. FIRST ) THEN
 
         CALL SETMSG ( 'The last constant item requested, #, is'
     .   //            ' before the first constant item'
     .   //            ' requested, #.'                            )
         CALL ERRINT ( '#', LAST                                   )
         CALL ERRINT ( '#', FIRST                                  )
         CALL SIGERR ( 'SPICE(REQUESTOUTOFORDER)'                  )
         CALL CHKOUT ( 'SGFCON'                                    )
         RETURN
 
      END IF
 
C
C     Compute the addresses of the data within the file and then fetch
C     the data.
C
      B = BASE + FIRST
      E = BASE + LAST
 
      CALL DAFGDA ( HANDLE, B, E, VALUES )
 
      CALL CHKOUT ( 'SGFCON' )
      RETURN
 
      END
