C$Procedure      SCLU01 ( SCLK look up, type 1 )
 
      SUBROUTINE SCLU01 (  NAME,  SC,  MAXNV,  N,  IVAL,  DVAL )
 
C$ Abstract
C
C     Look up type 1 SCLK kernel data.
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
C     KERNEL
C     SCLK
C
C$ Keywords
C
C     UTILITY
C
C$ Declarations
 
      IMPLICIT              NONE

      INCLUDE               'sclk.inc'
      
      CHARACTER*(*)         NAME
      INTEGER               SC
      INTEGER               MAXNV
      INTEGER               N
      INTEGER               IVAL ( * )
      DOUBLE PRECISION      DVAL ( * )
   
C$ Brief_I/O
C
C     Variable  I/O  Entry points
C     --------  ---  --------------------------------------------------
C     NAME       I   SCLD01, SCLI01
C     SC         I   SCLD01, SCLI01
C     MAXNV      I   SCLD01, SCLI01
C     N          O   SCLD01, SCLI01
C     IVAL       O   SCLI01
C     DVAL       O   SCLD01
C     MXCOEF     P   SCLD01, SCLI01
C     MXPART     P   SCLD01, SCLI01
C     MXNFLD     P   SCLD01, SCLI01
C     NDELIM     P   SCLI01
C     MXTSYS     P   SCLI01
C
C$ Detailed_Input
C
C     See entry points SCLI01, SCLD01.
C
C$ Detailed_Output
C
C     See entry points SCLI01, SCLD01.
C
C$ Parameters
C
C     See the INCLUDE file sclk.inc for descriptions and values
C     of the global parameters used by this routine and
C     its entry points.
C
C$ Exceptions
C
C     1)  IF SCLU01 is called directly, the error SPICE(BOGUSENTRY) is
C         signaled.
C
C     See entry points SCLI01, SCLD01 for descriptions of exceptions
C     specific to those routines.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine is a utility whose purpose is to localize error
C     checking for type 1 SCLK kernel pool lookups in a single place.
C
C     SLCU01 exists solely as an umbrella routine in which the
C     variables for its entry points are declared.  SCLU01 should never
C     be called directly.
C
C$ Examples
C
C     See entry points SCLI01, SCLD01.
C
C$ Restrictions
C
C     1)  SCLU01 handles lookups of type 1 SCLK data only.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman   (JPL)
C     E.D. Wright    (JPL)
C
C$ Version
C
C-    SPICELIB Version 2.3.0, 05-FEB-2008 (NJB)
C
C        Values of parameters 
C
C           MXCOEF, MXPART, MXNFLD, NDELIM, MXTSYS
C
C        are now provided by the INCLUDE file sclk.inc.
C
C-    SPICELIB Version 2.2.0, 20-NOV-2006 (NJB) (EDW)
C
C        Entry points SCLI01 and SCLD01 were update to use kernel pool
C        fetch routines GIPOOL and GDPOOL respectively.  Formerly these
C        entry points called the deprecated routine RTPOOL.
C
C        All headers have been updated to remove warnings about memory
C        corruption that could occur due to use of RTPOOL.
C
C        Header references to LDPOOL were replaced with references to
C        FURNSH.
C
C-    SPICELIB Version 2.1.0, 19-OCT-1992 (NJB)
C
C        Entry points SCLI01 and SCLD01 were updated to fix a bug:
C        if a kernel pool lookup fails, the number of elements returned
C        N is now set to zero.
C
C-    SPICELIB Version 2.0.0, 17-APR-1992 (NJB) (WLT)
C
C        Entry point SCLI01 was updated to handle a time
C        system specification for the `parallel' time system
C        in the SCLK kernel.  Comment section for permuted index
C        source lines was added following the header.
C
C-    SPICELIB Version 1.0.0, 06-SEP-1990 (NJB)
C
C-&
 
C$ Index_Entries
C
C     lookup type_1 spacecraft_clock
C
C-&
 
C$ Revisions
C
C-    SPICELIB Version 2.1.0, 19-OCT-1992 (NJB)
C
C        Entry points SCLI01 and SCLD01 were updated to fix a bug:
C        if a kernel pool lookup fails, the number of elements returned
C        N is now set to zero.  Formerly, these routines returned
C        whatever value was returned by RTPOOL.  RTPOOL, however,
C        does not set N to zero when the data item requested from it
C        is not found.
C
C-    SPICELIB Version 2.0.0, 17-APR-1992 (NJB) (WLT)
C
C        Entry point SCLI01 was updated to handle a time
C        system specification for the `parallel' time system
C        in the SCLK kernel.  The update consists of these
C        changes:
C
C           -- The parameter MXTSYS is now defined.
C
C           -- The local saved variable NAMLST has been expanded
C              to include the name SCLK01_TIME_SYSTEM
C
C           -- The local saved variable LB has been expanded to
C              include the lower bound for the number of returned
C              values when SCLK01_TIME_SYSTEM_nn is looked up in
C              the kernel pool.
C
C           -- SCLI01 checks the value returned by RTPOOL when
C              SCLK01_TIME_SYSTEM_nn is looked up to verify that
C              it is within the range [1, MXTSYS].
C
C        Also, a comment section for permuted index source lines was
C        added following the header.
C
C-&
 
 
C
C     SPICELIB functions
C
      INTEGER               ISRCHC
 
      LOGICAL               RETURN
 
C
C     Local parameters
C
      INTEGER               MSGLEN
      PARAMETER           ( MSGLEN =        320 )
 
      INTEGER               NAMLEN
      PARAMETER           ( NAMLEN =         80 )
 
      INTEGER               NKITEM
      PARAMETER           ( NKITEM =          9 )
 
      INTEGER               MXNCFF
      PARAMETER           ( MXNCFF = 3 * MXCOEF )
 
 
C
C     DELIDX is the index of the delimiter code name in NAMLST.  If
C     the declaration of NAMLST or assignment of values to NAMLST
C     changes, this parameter value may have to change.
C
      INTEGER               DELIDX
      PARAMETER           ( DELIDX =  7 )
 
C
C     NFLIDX is the index of the SCLK field count in NAMLST.
C
      INTEGER               NFLIDX
      PARAMETER           ( NFLIDX =  4 )
 
C
C     MODIDX is the index of the SCLK moduli in NAMLST.
C
      INTEGER               MODIDX
      PARAMETER           ( MODIDX =  6 )
 
C
C     SYSIDX is the index of the time system in NAMLST.
C
      INTEGER               SYSIDX
      PARAMETER           ( SYSIDX =  9 )
 
 
C
C     Local variables
C
      CHARACTER*(MSGLEN)    BVLMSG
      CHARACTER*(MSGLEN)    ERRMSG
      CHARACTER*(MSGLEN)    NFDMSG
      CHARACTER*(MSGLEN)    NUMMSG
      CHARACTER*(NAMLEN)    NAMLST ( NKITEM )
      CHARACTER*(NAMLEN)    TMPNAM
      CHARACTER*(1)         TYPE
 
      INTEGER               I
      INTEGER               LB     ( NKITEM )
 
      LOGICAL               FOUND
 
 
C
C     Saved variables
C
      SAVE                  NAMLST
      SAVE                  NUMMSG
      SAVE                  NFDMSG
      SAVE                  LB
 
C
C     Initial values
C
 
C
C     Names of type 1 SCLK items and lower bounds on the number of
C     associated values.
C
      DATA (   NAMLST(I),             LB(I),    I = 1, NKITEM )        /
     .
     .      'SCLK01_COEFFICIENTS',      3,
     .      'SCLK_PARTITION_START',     1,
     .      'SCLK_PARTITION_END',       1,
     .      'SCLK01_N_FIELDS',          1,
     .      'SCLK01_OFFSETS',           1,
     .      'SCLK01_MODULI',            1,
     .      'SCLK01_OUTPUT_DELIM',      1,
     .      'SCLK01_KERNEL_ID',         1,
     .      'SCLK01_TIME_SYSTEM',       0                              /
 
 
      DATA NFDMSG / '# not found. Did you load the SCLK kernel?'       /
 
      DATA NUMMSG / 'Invalid number of values found for #:  #.'        /
 
      DATA BVLMSG / 'Invalid value found for #:  #.'                   /
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SCLU01' )
      END IF
 
      CALL SIGERR ( 'SPICE(BOGUSENTRY)' )
 
      CALL CHKOUT ( 'SCLU01' )
      RETURN
 
 
 
 
C$Procedure      SCLI01 ( SCLK lookup of integer data, type 1 )
 
      ENTRY SCLI01 ( NAME, SC, MAXNV, N, IVAL )
 
C$ Abstract
C
C     Look up integer type 1 SCLK data from the kernel pool.
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
C     KERNEL
C     SCLK
C
C$ Keywords
C
C     UTILITY
C
C$ Declarations
C
C     CHARACTER*(*)         NAME
C     INTEGER               SC
C     INTEGER               MAXNV
C     INTEGER               N
C     INTEGER               IVAL   ( * )
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     NAME,
C     SC         I   Name of kernel data item, NAIF spacecraft ID code.
C     MAXNV      I   Maximum number of integer values to return.
C     N          O   Number of values actually returned.
C     IVAL       O   Returned integer values.
C     MXNFLD     P   Maximum number of fields in an SCLK string.
C     NDELIM     P   Maximum number of delimiter codes.
C     MXTSYS     P   Maximum number of supported parallel time systems.
C
C$ Detailed_Input
C
C     NAME,
C     SC             are, respectively, a name and a NAIF integer code
C                    of a spacecraft that together define the name of a
C                    requested kernel data item.  NAME is the full name
C                    as it appears in the SCLK kernel, except that it
C                    lacks the final underscore and spacecraft integer
C                    code (actually, the negative of the spacecraft
C                    code).  This routine combines NAME and SC to
C                    make up the appropriate kernel variable name.
C
C                    For example, to look up data associated with the
C                    name
C
C                       SCLK01_N_FIELDS_77
C
C                    you would supply NAME as
C
C                       SCLK01_N_FIELDS
C
C                    and SC as -77.
C
C
C     MAXNV          is the maximum number of values to return.  MAXNV
C                    is used to prevent SCLI01 from writing past the end
C                    of the supplied array IVAL.
C
C$ Detailed_Output
C
C     N              is the number of values actually returned.
C
C     IVAL           is an array containing the requested integer
C                    kernel data item.
C
C$ Parameters
C
C     MXNFLD         is an upper bound on the number of fields in a
C                    SCLK string.
C
C     NDELIM         is the number of delimiter codes.
C
C     MXTSYS         is the maximum number of supported parallel time
C                    systems that SCLK values may be mapped to or from.
C
C$ Exceptions
C
C
C     1)  If item specified by NAME and SC is not found in the kernel
C         pool, and if the presence of the item is required, the error
C         SPICE(KERNELVARNOTFOUND) is signaled.  The output arguments
C         are not modified.
C
C         If the specified item is not required, the output argument N
C         will take the value 0, and the output argument IVAL is not
C         modified.
C
C     2)  This routine can check certain data for validity.  If any of
C         these items have invalid values, the error
C         SPICE(VALUEOUTOFRANGE) is signaled.  The output arguments are
C         not modified.  The values in question are:
C
C            -  The number of fields of a SCLK string
C            -  The number of delimiter codes
C            -  The output delimiter code
C            -  The time system code
C
C     3) If the dimension of the requested item exceeds MAXNV, the 
C        error SPICE(ARRAYTOOSMALL) is signaled.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     The purpose of this routine is to localize error checking for
C     lookups of type 1 SCLK kernel pool data.  This routine handles
C     lookups of integer data.
C
C$ Examples
C
C     1)  To get the number of SCLK fields for the Galileo spacecraft
C         clock, you can use the code fragment below:
C
C            C
C            C     Load the SCLK kernel in question.  We use a
C            C     made-up name for the kernel file; you would use
C            C     the actual name of your kernel file instead if you
C            C     were to carry out this procedure.
C            C
C                  CALL FURNSH ( 'SAMPLE_GLL_SCLK.KER' )
C
C                  SC   = -77
C                  NAME = 'SCLK01_N_FIELDS'
C
C                  CALL SCLI01 ( NAME, SC, MXNFLD, N, NFIELD )
C
C
C         After this subroutine call, NFIELD has the value 4.
C
C
C$ Restrictions
C
C     1)  SCLI01 assumes that a SCLK kernel appropriate to the
C         spacecraft identified by SC has been loaded.
C
C     2)  SCLI01 handles lookups of type 1 SCLK data only.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman   (JPL)
C     E.D. Wright    (JPL)
C
C$ Version
C
C-    SPICELIB Version 2.2.0, 20-NOV-2006 (NJB) (EDW)
C
C        Routine was updated to use GIPOOL instead of RTPOOL.  Header
C        has been updated to remove warnings about memory corruption and
C        to document exception handling for output buffer overflow 
C        errors.
C
C        Header references to LDPOOL were replaced with references to
C        FURNSH.
C
C-    SPICELIB Version 2.1.0, 19-OCT-1992 (NJB)
C
C        This entry point was updated to fix a bug:  if a kernel pool
C        lookup fails, the number of elements returned N is now set to
C        zero.
C
C-    SPICELIB Version 2.0.0, 17-APR-1992 (NJB) (WLT)
C
C        SCLI01 was updated to handle a time system specification for
C        the `parallel' time system in the SCLK kernel.  Some
C        corrections and other minor enhancements were made to the
C        header.  Comment section for permuted index source lines was
C        added following the header.
C
C-    SPICELIB Version 1.0.0, 06-SEP-1990 (NJB)
C
C-&
 
C$ Index_Entries
C
C     lookup of type_1 spacecraft_clock integer data
C     lookup type_1 spacecraft_clock integer data
C
C-&
 
C$ Revisions
C
C-    SPICELIB Version 2.1.0, 19-OCT-1992 (NJB)
C
C        This entry point was updated to fix a bug:  if a kernel pool
C        lookup fails, the number of elements returned N is now set to
C        zero.  Formerly, this routine returned whatever value was
C        returned by RTPOOL.  RTPOOL, however, does not set N to zero
C        when the data item requested from it is not found.
C
C-    SPICELIB Version 2.0.0, 17-APR-1992 (NJB) (WLT)
C
C        Entry point SCLI01 was updated to handle a time
C        system specification for the `parallel' time system
C        in the SCLK kernel.  The update consists of these
C        changes:
C
C           -- The parameter MXTSYS is now defined.
C
C           -- The local saved variable NAMLST has been expanded
C              to include the name SCLK01_TIME_SYSTEM
C
C           -- The local saved variable LB has been expanded to
C              include the lower bound for the number of returned
C              values when SCLK01_TIME_SYSTEM_nn is looked up in
C              the kernel pool.
C
C           -- SCLI01 checks the value returned by RTPOOL when
C              SCLK01_TIME_SYSTEM_nn is looked up to verify that
C              it is within the range [1, MXTSYS].
C
C        Also, a comment section for permuted index source lines was
C        added following the header.
C
C        The $Exceptions header section was updated accordingly.
C
C        Some corrections and other minor enhancements were made to the
C        header.
C
C-&
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SCLI01' )
      END IF
 
C
C     Form the name of the kernel pool data item, and do the lookup.
C     Note that eventually we should use a kernel pool lookup entry
C     that allows us to specify the maximum number of entries that
C     can be returned.
C
      TMPNAM = NAME
 
      CALL SUFFIX ( '_#',    0,         TMPNAM )
      CALL REPMI  ( TMPNAM, '#', -SC,   TMPNAM )
 
C
C     Make sure we have enough room for the item in our output
C     array.  Look up the dimension of the item.
C     
      CALL DTPOOL ( TMPNAM,  FOUND,  N,  TYPE )

      IF ( N .GT. MAXNV ) THEN

         CALL SETMSG ( 'Item # has size # but output array ' 
     .   //            'has size #.'                         )
         CALL ERRCH  ( '#',  TMPNAM                          )
         CALL ERRINT ( '#',  N                               )
         CALL ERRINT ( '#',  MAXNV                           )
         CALL SIGERR ( 'SPICE(ARRAYTOOSMALL)'                )
         CALL CHKOUT ( 'SCLI01'                              )
         RETURN

      END IF

      CALL GIPOOL ( TMPNAM, 1, MAXNV, N, IVAL, FOUND )
 
C
C     Make sure we found what we were looking for, if the item
C     is required.
C
      IF ( .NOT. FOUND ) THEN
C
C        Currently, the only item that is NOT required is the time
C        system specification.  In any case, no values will be returned.
C
         N = 0
 
         IF (  NAME .EQ. NAMLST(SYSIDX)  ) THEN
 
            CALL CHKOUT ( 'SCLI01'                   )
            RETURN
 
         ELSE
 
            CALL SETMSG (  NFDMSG                    )
            CALL ERRCH  ( '#', TMPNAM                )
            CALL SIGERR ( 'SPICE(KERNELVARNOTFOUND)' )
            CALL CHKOUT ( 'SCLI01'                   )
            RETURN
 
         END IF
 
      END IF
 
C
C     Now we must check that the number of returned values is in the
C     appropriate range.  We test for the following conditions:
C
C        - The number of SCLK fields is at least 1 and is not
C          more than MAXNV.
C
C        - The number of delimiter codes is at least 1 and is not
C          more than MAXNV.
C
C        - The output delimiter code is at least 1 and is not
C          greater than the number of delimiters.
C
C        - The time system code is at least 1 and is not greater
C          than MXTSYS.
C
 
C
C     See if the input name is in the list of items we know about.
C     If it is, perform the bound checks that apply.
C
      I = ISRCHC ( NAME, NKITEM, NAMLST )
 
      IF ( I .NE. 0 ) THEN
 
         IF (  N .LT. LB(I) ) THEN
 
            CALL REPMC  (  NUMMSG, '#', TMPNAM, ERRMSG )
            CALL REPMI  (  ERRMSG, '#', N,      ERRMSG )
            CALL SETMSG (  ERRMSG                      )
            CALL SIGERR ( 'SPICE(VALUEOUTOFRANGE)'     )
            CALL CHKOUT ( 'SCLI01'                     )
            RETURN
 
         END IF
 
      END IF
 
C
C     Check the value of the delimiter code itself.
C
      IF ( NAME .EQ. NAMLST(DELIDX) ) THEN
 
         IF (  ( IVAL(1) .LT. 1 ) .OR. ( IVAL(1) .GT. NDELIM )  ) THEN
 
            CALL REPMC  (  BVLMSG, '#', TMPNAM,  ERRMSG )
            CALL REPMI  (  ERRMSG, '#', IVAL(1), ERRMSG )
            CALL SETMSG (  ERRMSG                       )
            CALL SIGERR ( 'SPICE(VALUEOUTOFRANGE)'      )
            CALL CHKOUT ( 'SCLI01'                      )
            RETURN
 
         END IF
 
      END IF
 
C
C     Check the value of the field count, too.
C
      IF ( NAME .EQ. NAMLST(NFLIDX) ) THEN
 
         IF (  ( IVAL(1) .LT. 1 ) .OR. ( IVAL(1) .GT. MXNFLD )  ) THEN
 
            CALL REPMC  (  BVLMSG, '#', TMPNAM,  ERRMSG )
            CALL REPMI  (  ERRMSG, '#', IVAL(1), ERRMSG )
            CALL SETMSG (  ERRMSG                       )
            CALL SIGERR ( 'SPICE(VALUEOUTOFRANGE)'      )
            CALL CHKOUT ( 'SCLI01'                      )
            RETURN
 
         END IF
 
      END IF
 
C
C     Check the value of the time system code.
C
      IF ( NAME .EQ. NAMLST(SYSIDX) ) THEN
 
         IF (  ( IVAL(1) .LT. 1 ) .OR. ( IVAL(1) .GT. MXTSYS )  ) THEN
 
            CALL REPMC  (  BVLMSG, '#', TMPNAM,  ERRMSG )
            CALL REPMI  (  ERRMSG, '#', IVAL(1), ERRMSG )
            CALL SETMSG (  ERRMSG                       )
            CALL SIGERR ( 'SPICE(VALUEOUTOFRANGE)'      )
            CALL CHKOUT ( 'SCLI01'                      )
            RETURN
 
         END IF
 
      END IF
 
 
      CALL CHKOUT ( 'SCLI01' )
      RETURN
 
 
 
 
C$Procedure      SCLD01 ( SCLK lookup of double precision data, type 1 )
 
      ENTRY SCLD01 ( NAME, SC, MAXNV, N, DVAL )
 
C$ Abstract
C
C     Look up double precision type 1 SCLK data from the kernel pool.
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
C     KERNEL
C     SCLK
C
C$ Keywords
C
C     UTILITY
C
C$ Declarations
C
C     CHARACTER*(*)         NAME
C     INTEGER               SC
C     INTEGER               MAXNV
C     INTEGER               N
C     DOUBLE PRECISION      DVAL   ( * )
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     NAME,
C     SC         I   Name of kernel data item, NAIF spacecraft ID code.
C     MAXNV      I   Maximum number of d.p. values to return.
C     N          O   Number of values actually returned.
C     DVAL       O   Requested kernel data item.
C     MXCOEF     P   Maximum number of coefficient sets in SCLK kernel.
C
C$ Detailed_Input
C
C     NAME,
C     SC             are, respectively, a name and a NAIF integer code
C                    of a spacecraft that together define the name of a
C                    requested kernel data item.  NAME is the full name
C                    as it appears in the SCLK kernel, except that it
C                    lacks the final underscore and spacecraft integer
C                    code (actually, the negative of the spacecraft
C                    code).  This routine combines NAME and SC to
C                    make up the appropriate kernel variable name.
C
C                    For example, to look up data associated with the
C                    name
C
C                       SCLK01_COEFFICIENTS_77
C
C                    you would supply NAME as
C
C                       SCLK01_COEFFICIENTS
C
C                    and SC as -77.
C
C
C     MAXNV          is the maximum number of values to return.  MAXNV
C                    is used to prevent SCLD01 from writing past the end
C                    of the supplied array DVAL.
C
C$ Detailed_Output
C
C     N              is the number of values actually returned.
C
C     DVAL           is an array containing the requested double
C                    precision kernel data item.
C
C$ Parameters
C
C     MXCOEF         is the maximum number of coefficient sets in the
C                    array COEFFS that defines the mapping between
C                    encoded type 1 SCLK and a parallel time system.
C                    This array has dimension 3 x MXCOEF.  The value of
C                    MXCOEF may be increased as required.
C
C$ Exceptions
C
C     1)  If item specified by NAME and SC is not found in the kernel
C         pool, the error SPICE(KERNELVARNOTFOUND) is signaled.  The
C         output arguments are not modified.
C
C     2)  This routine can check certain data for validity.  If any of
C         these items have invalid values, the error
C         SPICE(VALUEOUTOFRANGE) is signaled.  The output arguments are
C         not modified.  The values in question are:
C
C            - The number of coefficients.
C            - The number of partition start values.
C            - The number of partition end values.
C            - The number of moduli.
C            - The values of the moduli (lower bounds)
C            - The number of offsets.
C            - The number of kernel identifiers.
C
C     3)  If the partition times or SCLK coefficients themselves
C         are invalid, this routine does nothing about it.  It is
C         simply not possible to detect all of the possible errors
C         that these data may be subject to.
C
C     4) If the dimension of the requested item exceeds MAXNV, the 
C        error SPICE(ARRAYTOOSMALL) is signaled.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     The purpose of this routine is to localize error checking for
C     lookups of type 1 SCLK kernel pool data.  This routine handles
C     lookups of double precision data.
C
C$ Examples
C
C     1)  Check a NAIF SCLK kernel for accuracy by converting the
C         encoded SCLK coefficients to strings with partition numbers
C         and converting the parallel times to UTC strings.  Print out
C         the results in tabular form.  In this example, the spacecraft
C         is Mars Observer, which has NAIF ID code -94.  We could
C         make the program work for Galileo by using the NAIF ID code
C         -77 instead of -94.
C
C            C
C            C     Load the SCLK kernel in question, and also load
C            C     a leapseconds kernel.  We use made-up names for the
C            C     kernel file; you would use the actual names of your
C            C     kernel files instead if you were to carry out this
C            C     procedure.
C            C
C                  CALL FURNSH ( 'SAMPLE_MO_SCLK.KER' )
C                  CALL FURNSH ( 'LEAPSECONDS.KER'    )
C
C                  CONAME =  SCLK01_COEFFICIENTS
C                  SC     =  -94
C
C            C
C            C     Grab the coefficients.
C            C
C                  CALL SCLD01 ( CONAME, SC, 3*MXCOEF, NCOEFF, COEFFS )
C
C            C
C            C     The SCLK coefficients are in the first row of the
C            C     coefficients array; the parallel times are in the
C            C     second.  Since the parallel time system used for MO
C            C     is terrestrial dynamical time (TDT), we will convert
C            C     the parallel time values to ET (TDB) first and then
C            C     convert the resulting times to UTC.
C            C
C            C     In a more robust algorithm, we'd look up the parallel
C            C     time system code used in the SCLK kernel rather than
C            C     assume that it is a particular system.  We omit this
C            C     check for simplicity.
C            C
C            C     We decode the SCLK coefficients using SCDECD.  Write
C            C     out the results to a file we'll call COMPARE.DAT.
C            C
C                  OUTFIL = 'COMPARE.DAT'
C
C                  CALL WRLINE ( OUTFIL, '    SCLK               UTC' )
C                  CALL WRLINE ( OUTFIL, ' '                          )
C
C                  DO I = 1, NCOEFF / 3
C
C                     CALL SCDECD ( -94,  COEFF(1,I),  CLKSTR )
C            C
C            C        Convert the parallel time coefficients, which are
C            C        given in TDT, to ET.  UNITIM returns this value.
C            C
C                     CALL ET2UTC ( UNITIM ( COEFF(2,I), 'TDT', 'TDB' ),
C                 .                 'D',
C                 .                  3,
C                 .                  UTC    )
C
C                     LINE = ' SCLK        UTC '
C
C                     CALL REPMC  ( LINE, 'SCLK', CLKSTR, LINE )
C                     CALL REPMC  ( LINE, 'UTC',  UTC,    LINE )
C
C                     CALL WRLINE ( OUTFIL, LINE )
C
C                  END DO
C
C
C$ Restrictions
C
C     1)  SCLD01 assumes that a SCLK kernel appropriate to the
C         spacecraft identified by SC has been loaded.
C
C     2)  SCLD01 handles lookups of type 1 SCLK data only.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman   (JPL)
C     E.D. Wright    (JPL)
C
C$ Version
C
C-    SPICELIB Version 2.2.0, 20-NOV-2006 (NJB) (EDW)
C
C        Routine was updated to use GDPOOL instead of RTPOOL. Header
C        has been updated to remove warnings about memory corruption and
C        to document exception handling for output buffer overflow 
C        errors.
C
C        Header references to LDPOOL were replaced with references to
C        FURNSH.
C
C-    SPICELIB Version 2.1.0, 19-OCT-1992 (NJB)
C
C        This entry point was updated to fix a bug:  if a kernel pool
C        lookup fails, the number of elements returned N is now set to
C        zero.
C
C-    SPICELIB Version 2.0.0, 17-APR-1992 (NJB) (WLT)
C
C        One constant was changed in the code for clarity; no functional
C        change results from this.  Some corrections and other minor
C        enhancements were made to the header.  Comment section for
C        permuted index source lines was added following the header.
C 
C-    SPICELIB Version 1.0.0, 06-SEP-1990 (NJB)
C
C-&
 
C$ Index_Entries
C
C     lookup of type_1 spacecraft_clock d.p. data
C     lookup type_1 spacecraft_clock d.p. data
C
C-&
 
C$ Revisions
C
C-    SPICELIB Version 2.1.0, 19-OCT-1992 (NJB)
C
C        This entry point was updated to fix a bug:  if a kernel pool
C        lookup fails, the number of elements returned N is now set to
C        zero.  Formerly, this routine returned whatever value was
C        returned by RTPOOL.  RTPOOL, however, does not set N to zero
C        when the data item requested from it is not found.
C
C-    SPICELIB Version 2.0.0, 17-APR-1992 (NJB) (WLT)
C
C        The constant 1 was changed to 1.D0 in the test for the
C        validity of the moduli for a spacecraft clock.  The change
C        was made simply for clarity.
C
C        Some corrections and other minor enhancements were made to the
C        header.  Comment section for permuted index source lines was
C        added following the header.
C
C-&
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SCLD01' )
      END IF
 
C
C     Form the name of the kernel pool datum, and do the lookup.
C
      TMPNAM = NAME
 
      CALL SUFFIX ( '_#',    0,        TMPNAM )
      CALL REPMI  ( TMPNAM, '#', -SC,  TMPNAM )
 
C
C     Make sure we have enough room for the item in our output
C     array.  Look up the dimension of the item.
C     
      CALL DTPOOL ( TMPNAM,  FOUND,  N,  TYPE )

      IF ( N .GT. MAXNV ) THEN

         CALL SETMSG ( 'Item # has size # but output array ' 
     .   //            'has size #.'                         )
         CALL ERRCH  ( '#',  TMPNAM                          )
         CALL ERRINT ( '#',  N                               )
         CALL ERRINT ( '#',  MAXNV                           )
         CALL SIGERR ( 'SPICE(ARRAYTOOSMALL)'                )
         CALL CHKOUT ( 'SCLD01'                              )
         RETURN

      END IF

      CALL GDPOOL ( TMPNAM, 1, MAXNV, N, DVAL, FOUND )
 
C
C     Make sure we found what we were looking for.
C
      IF ( .NOT. FOUND ) THEN
C
C        No values are returned in this case.
C
         N = 0
 
         CALL SETMSG (  NFDMSG                    )
         CALL ERRCH  ( '#', TMPNAM                )
         CALL SIGERR ( 'SPICE(KERNELVARNOTFOUND)' )
         CALL CHKOUT ( 'SCLD01'                   )
         RETURN
 
      END IF
 
C
C     Now we must check that the number of returned values is in the
C     appropriate range.  We test for the following conditions:
C
C        - The number of coefficients is at least 3.
C
C        - The number of partition start values is at least 1.
C
C        - The number of partition end values is at least 1.
C
C        - The number of moduli is at least 1.
C
C        - The number of offsets is at least 1.
C
C
 
C
C     See if the input name is in the list of items we know about.
C     If it is, perform the bounds checks that apply.
C
      I = ISRCHC ( NAME, NKITEM, NAMLST )
 
      IF ( I .NE. 0 ) THEN
 
         IF ( N .LT. LB(I) ) THEN
 
            CALL REPMC  (  NUMMSG, '#', TMPNAM, ERRMSG )
            CALL REPMI  (  ERRMSG, '#', N,      ERRMSG )
            CALL SETMSG (  ERRMSG                      )
            CALL SIGERR ( 'SPICE(VALUEOUTOFRANGE)'     )
            CALL CHKOUT ( 'SCLD01'                     )
            RETURN
 
         END IF
 
      END IF
 
 
C
C     Check the values of the moduli themselves.
C
      IF ( NAME .EQ. NAMLST(MODIDX) ) THEN
 
         DO I = 1, N
 
            IF ( DVAL(1) .LT. 1.D0 ) THEN
               CALL REPMC  (  BVLMSG, '#', TMPNAM,      ERRMSG )
               CALL REPMD  (  ERRMSG, '#', DVAL(1), 14, ERRMSG )
               CALL SETMSG (  ERRMSG                           )
               CALL SIGERR ( 'SPICE(VALUEOUTOFRANGE)'          )
               CALL CHKOUT ( 'SCLD01'                          )
               RETURN
            END IF
 
         END DO
 
      END IF
 
      CALL CHKOUT ( 'SCLD01' )
      RETURN
      END
