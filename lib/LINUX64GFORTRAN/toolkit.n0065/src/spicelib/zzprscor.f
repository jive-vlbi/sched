C$Procedure ZZPRSCOR ( Parse aberration correction )

      SUBROUTINE ZZPRSCOR ( ABCORR, ATTBLK )
      IMPLICIT NONE 
 
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Parse an aberration correction string; return attributes. 
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
C     None.
C
C$ Keywords
C
C     ABERRATION
C     PARSING
C     PRIVATE
C     UTILITY
C
C$ Declarations

      CHARACTER*(*)         ABCORR
      LOGICAL               ATTBLK ( * )

      INCLUDE 'zzabcorr.inc'
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  -------------------------------------------------
C     ABCORR     I   Aberration correction string.
C     ATTBLK     O   Aberration correction attribute block.
C
C$ Detailed_Input
C
C     ABCORR         is a string representing a aberration
C                    correction.  The supported values are:
C
C                       'CN'
C                       'CN+S'
C                       'LT'
C                       'LT+S'
C                       'NONE'
C                       'RL'
C                       'RL+S'
C                       'S'
C                       'XCN'
C                       'XCN+S'
C                       'XLT'
C                       'XLT+S'
C                       'XRL'
C                       'XRL+S'
C                       'XS'
C
C                    Note that some values not supported by the
C                    SPICELIB SPK subsystem are supported by
C                    this routine:
C
C                       - The letter 'R' indicates relativistic
C                         corrections.
C
C                       - Stellar aberration-only corrections are
C                         indicated by the strings
C
C                            'S'
C                            'XS'
C
C                    Case and embedded blanks are not significant in
C                    ABCORR.
C
C$ Detailed_Output
C
C     ATTBLK         is a block of logical flags indicating the
C                    attributes of the aberration correction 
C                    specified by ABCORR.  The attributes are:
C
C                       - Is the correction "geometric"?
C
C                       - Is light time correction indicated?
C
C                       - Is stellar aberration correction indicated?
C
C                       - Is the light time correction of the 
C                         "converged Newtonian" variety?
C
C                       - Is the correction for the transmission 
C                         case?
C
C                       - Is the correction relativistic?
C
C                    The structure of ATTBLK is defined in the
C                    include file 
C
C                       zzabcorr.inc
C
C                    The size of ATTBLK and the offsets of the
C                    component flags are defined there.
C                    
C$ Parameters
C
C     See INCLUDE file zzabcorr.inc.
C
C$ Exceptions
C
C     1) If the input aberration correction choice is not recognized,
C        the error SPICE(INVALIDOPTION) is signaled.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     Many SPICELIB routines have logic branches based on the
C     attributes of aberration corrections.  Much duplicated 
C     parsing code can be avoided by using this routine.
C
C     In particular, the routine ZZCOREPC uses this routine
C     to combine an epoch and light time value to compute
C     a light-time-adjusted epoch.
C
C$ Examples
C
C     See ZZCOREPC.
C
C$ Restrictions
C
C     1) This is a SPICE private routine; the routine is subject
C        to change without notice.  User applications should not
C        call this routine.
C
C     2) This routine recognizes some aberration corrections not
C        handled by most SPICELIB routines.  Callers should do
C        their own checking to ensure the parsed correction is
C        acceptable.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman    (JPL)
C     B.V. Semenov    (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.1.0, 21-SEP-2013 (BVS) 
C
C        Efficiency updates: moved CHKIN inside the exception block;
C        replaced CMPRSS/UCASE with LJUCRS.
C
C-    SPICELIB Version 1.0.0, 13-DEC-2004 (NJB) 
C
C-&

C
C     SPICELIB functions
C
      INTEGER               BSRCHC
      LOGICAL               RETURN

C
C     Local variables
C
      CHARACTER*(CORLEN)    CORLST ( NABCOR )
      CHARACTER*(CORLEN)    TMPCOR

      INTEGER               I
      INTEGER               LOC
      INTEGER               ORDVEC ( NABCOR )

      LOGICAL               CONV   ( NABCOR )
      LOGICAL               FIRST
      LOGICAL               GEO    ( NABCOR )
      LOGICAL               LT     ( NABCOR )
      LOGICAL               REL    ( NABCOR )
      LOGICAL               STL    ( NABCOR )
      LOGICAL               XMIT   ( NABCOR )

C
C     Saved variables
C
      SAVE                  CONV
      SAVE                  CORLST
      SAVE                  FIRST
      SAVE                  GEO
      SAVE                  LT
      SAVE                  REL
      SAVE                  STL
      SAVE                  XMIT

C
C     Initial values
C    
C
C     It is recommended that, for maintainability, the correction
C     strings be kept in increasing order in this list.  However,
C     this routine does not rely on the strings being ordered
C     in this data statement:  the strings and associated values
C     are ordered at run time.
C
      DATA  
     .( 
     .   CORLST(I), GEO(I), LT(I),   STL(I),  CONV(I), XMIT(I), REL(I),
     .   I= 1, NABCOR                                                  
     .) /
     . 'CN',      .FALSE., .TRUE.,  .FALSE., .TRUE.,  .FALSE., .FALSE.,
     . 'CN+S',    .FALSE., .TRUE.,  .TRUE.,  .TRUE.,  .FALSE., .FALSE.,
     . 'LT',      .FALSE., .TRUE.,  .FALSE., .FALSE., .FALSE., .FALSE.,
     . 'LT+S',    .FALSE., .TRUE.,  .TRUE.,  .FALSE., .FALSE., .FALSE., 
     . 'NONE',    .TRUE.,  .FALSE., .FALSE., .FALSE., .FALSE., .FALSE.,
     . 'RL',      .FALSE., .TRUE.,  .FALSE., .FALSE., .FALSE., .TRUE.,
     . 'RL+S',    .FALSE., .TRUE.,  .TRUE.,  .FALSE., .FALSE., .TRUE., 
     . 'S',       .FALSE., .FALSE., .TRUE.,  .FALSE., .FALSE., .FALSE., 
     . 'XCN',     .FALSE., .TRUE.,  .FALSE., .TRUE.,  .TRUE.,  .FALSE.,
     . 'XCN+S',   .FALSE., .TRUE.,  .TRUE.,  .TRUE.,  .TRUE.,  .FALSE.,
     . 'XLT',     .FALSE., .TRUE.,  .FALSE., .FALSE., .TRUE.,  .FALSE.,
     . 'XLT+S',   .FALSE., .TRUE.,  .TRUE.,  .FALSE., .TRUE.,  .FALSE.,
     . 'XRL',     .FALSE., .TRUE.,  .FALSE., .FALSE., .TRUE.,  .TRUE.,
     . 'XRL+S',   .FALSE., .TRUE.,  .TRUE.,  .FALSE., .TRUE.,  .TRUE.,
     . 'XS',      .FALSE., .FALSE., .TRUE.,  .FALSE., .TRUE.,  .FALSE. 
     .  /
     

      DATA                  FIRST  / .TRUE. /



      IF ( RETURN() ) THEN
         RETURN
      END IF

      IF ( FIRST ) THEN
C
C        The first time this routine is called, we sort the 
C        aberration correction strings and the associated flag
C        lists.  This ensures we have an ordered list suitable
C        for a binary search.
C
C        Find the sorted order of the aberration correction strings.
C
         CALL ORDERC ( CORLST, NABCOR, ORDVEC )

C
C        Put the aberration correction strings and the associated
C        arrays into increasing order.
C
         CALL REORDC ( ORDVEC, NABCOR, CORLST )

         CALL REORDL ( ORDVEC, NABCOR, GEO    )
         CALL REORDL ( ORDVEC, NABCOR, LT     )
         CALL REORDL ( ORDVEC, NABCOR, STL    )
         CALL REORDL ( ORDVEC, NABCOR, CONV   )
         CALL REORDL ( ORDVEC, NABCOR, XMIT   )
         CALL REORDL ( ORDVEC, NABCOR, REL    )

         FIRST = .FALSE.

      END IF

C
C     Obtain a blank-free, upper-case copy of the aberration
C     correction string.
C      
      CALL LJUCRS ( 0, ABCORR, TMPCOR )

C
C     Search the list for the aberration correction string.
C
      LOC = BSRCHC ( TMPCOR, NABCOR, CORLST )

      IF ( LOC .EQ. 0 ) THEN

         CALL CHKIN  ( 'ZZPRSCOR' )      
         CALL SETMSG ( 'Aberration correction specification # is not '//
     .                 'recognized.'                                  )
         CALL ERRCH  ( '#',  ABCORR                                   )
         CALL SIGERR ( 'SPICE(INVALIDOPTION)'                         )
         CALL CHKOUT ( 'ZZPRSCOR'                                     )
         RETURN

      END IF

C
C     Set the output flags.
C
      ATTBLK( GEOIDX )  =  GEO   ( LOC )
      ATTBLK( LTIDX  )  =  LT    ( LOC )
      ATTBLK( STLIDX )  =  STL   ( LOC )
      ATTBLK( CNVIDX )  =  CONV  ( LOC )
      ATTBLK( XMTIDX )  =  XMIT  ( LOC )
      ATTBLK( RELIDX )  =  REL   ( LOC )

      RETURN
      END



