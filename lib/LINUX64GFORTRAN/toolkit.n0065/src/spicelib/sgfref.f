C$Procedure      SGFREF ( Generic Segments: Fetch references )
 
      SUBROUTINE SGFREF ( HANDLE, DESCR, FIRST, LAST, VALUES )
      IMPLICIT NONE
 
C$ Abstract
C
C     Given the descriptor for a generic segment in a DAF file
C     associated with HANDLE, fetch from the references partition
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
C     FIRST      I   The index of the first reference value to fetch.
C     LAST       I   The index of the last reference value to fetch.
C     VALUES     O   The reference values that were requested.
C
C$ Detailed_Input
C
C     HANDLE     is the handle of a DAF file opened for reading
C                that contains the segment described by DESCR.
C
C     DESCR      is the descriptor of the segment with the desired
C                constant values. This must be the descriptor for a
C                segment in the DAF associated with HANDLE.
C
C     FIRST      is the index of the first value to fetch from the
C                reference section of the DAF generic segment associated
C                with HANDLE and DESCR.
C
C     LAST       is the index of the last value to fetch from the
C                constants section of the DAF generic segment associated
C                with HANDLE and DESCR.
C
C$ Detailed_Output
C
C     VALUES      is the array of reference values obtained from the
C                 reference section of the DAF generic segment
C                 associated with HANDLE and DESCR.
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
C        number of reference values.
C
C     2) The error SPICE(REQUESTOUTOFORDER) will be signalled
C        if LAST is less than FIRST.
C
C     3) The error SPICE(UNKNOWNREFDIR) will be signalled if the
C        reference directory structure is unrecognized.  The most
C        likely cause of this error is that an upgrade to your
C        version of the SPICE toolkit is needed.
C
C$ Particulars
C
C     This routine allows you to easily fetch values from the reference
C     section of a generic segment.
C
C$ Examples
C
C     Suppose that you have located a DAF generic segment. The code
C     fragment below shows how to fetch the I'th reference value from
C     that segment.
C
C        Declarations:
C
C        DOUBLE PRECISION      REFVAL
C
C        Fetch the Ith reference value from the segment.
C
C        CALL SGFREF ( HANDLE, DESCR, I, I, REFVAL )
C
C
C$ Restrictions
C
C     The segment described by DESCR MUST be a generic segment,
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
C-    SPICELIB Version 1.2.0, 07-SEP-2001 (EDW)
C
C        Replaced DAFRDA call with DAFGDA.
C
C-    SPICELIB Version 1.0.0, 12-APR-1995 (KRG) (WLT)
C
C-&
 
 
C$ Index_Entries
C
C     fetch reference values from a generic segment
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
      DOUBLE PRECISION      BUFFER ( 2 )
 
      INTEGER               B
      INTEGER               BASE
      INTEGER               E
      INTEGER               I
      INTEGER               MYNREF
      INTEGER               MYREFT
 
      IF ( RETURN() ) THEN
         RETURN
      END IF
 
      CALL CHKIN ( 'SGFREF' )
C
C     Perform the needed initialization
C
      CALL SGMETA ( HANDLE, DESCR, REFBAS, BASE   )
      CALL SGMETA ( HANDLE, DESCR, RDRTYP, MYREFT )
      CALL SGMETA ( HANDLE, DESCR, NREF,   MYNREF )
 
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'SGFREF' )
         RETURN
      END IF
 
C
C     Perform checks on the inputs for reasonableness.
C
      IF ( ( FIRST .LT. 1 ) .OR. ( LAST .GT. MYNREF) ) THEN
 
         CALL SETMSG ( 'The range of reference items requested'
     .   //            ' extends beyond the available range of'
     .   //            ' reference items.  The reference data is'
     .   //            ' available'
     .   //            ' for indexes 1 to #.  You''ve requested'
     .   //            ' data from # to #.'                        )
         CALL ERRINT ( '#', MYNREF                                 )
         CALL ERRINT ( '#', FIRST                                  )
         CALL ERRINT ( '#', LAST                                   )
         CALL SIGERR ( 'SPICE(REQUESTOUTOFBOUNDS)'                 )
         CALL CHKOUT ( 'SGFREF'                                    )
         RETURN
 
      END IF
 
      IF ( LAST .LT. FIRST ) THEN
 
         CALL SETMSG ( 'The last reference item requested, #, is'
     .   //            ' before the first reference item'
     .   //            ' requested, #.'                            )
         CALL ERRINT ( '#', LAST                                   )
         CALL ERRINT ( '#', FIRST                                  )
         CALL SIGERR ( 'SPICE(REQUESTOUTOFORDER)'                  )
         CALL CHKOUT ( 'SGFREF'                                    )
         RETURN
 
      END IF
C
C     Ok.  We are ready to go. If the reference type is recognized
C     fetch the requested data.
C
      IF ( MYREFT .EQ. IMPLE ) THEN
C
C        The reference values are implied in this case. Read the
C        reference base value and step. If we fail, check out and
C        return; we don't want to try and comput anything with bogus
C        data.
C
         B = BASE + 1
         E = BASE + 2
 
         CALL DAFGDA ( HANDLE, B, E, BUFFER )
 
         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'SGFREF' )
            RETURN
         END IF
C
C        Now simply compute the reference values using the implicit
C        model for them.
C
         DO I = FIRST, LAST
 
            VALUES(I) = BUFFER(1) + DINT ( DBLE(I-1) * BUFFER(2) )
 
         END DO
 
      ELSE IF ( MYREFT .EQ. IMPCLS ) THEN
C
C        The reference values are implied in this case. Read the
C        reference base value and step. If we fail, check out and
C        return; we don't want to try and comput anything with bogus
C        data.
C
         B = BASE + 1
         E = BASE + 2
 
         CALL DAFGDA ( HANDLE, B, E, BUFFER )
 
         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'SGFREF' )
            RETURN
         END IF
C
C        Now simply compute the reference values using the implicit
C        model for them.
C
         DO I = FIRST, LAST
 
            VALUES(I) = BUFFER(1) + DINT ( DBLE(I-1) * BUFFER(2) )
 
         END DO
 
      ELSE IF (      MYREFT .EQ. EXPLE
     .          .OR. MYREFT .EQ. EXPLT
     .          .OR. MYREFT .EQ. EXPCLS ) THEN
C
C        In this case the reference values are actually stored
C        in the file.  This is even easier than the last case.
C        We simply fetch them with a call to DAF. We do not check for a
C        failure here, since all we do after the attempt to read is
C        checkout and return anyway.
C
         B = BASE + FIRST
         E = BASE + LAST
 
         CALL DAFGDA ( HANDLE, B, E, VALUES )
 
      ELSE
 
         CALL SETMSG ( 'The generic DAF segment you attempted'
     .   //            ' to read has an unsupported reference'
     .   //            ' directory structure. The integer code'
     .   //            ' given for this structure is #. The'
     .   //            ' likely cause of this anomoly is that'
     .   //            ' your version of SPICELIB needs to be'
     .   //            ' updated. Contact your system'
     .   //            ' administrator or NAIF for a toolkit'
     .   //            ' update. '                                 )
         CALL ERRINT ( '#', MYREFT                                 )
         CALL SIGERR ( 'SPICE(UNKNOWNREFDIR)'                      )
         CALL CHKOUT ( 'SGFREF'                                    )
         RETURN
 
      END IF
 
      CALL CHKOUT ( 'SGFREF' )
      RETURN
 
      END
