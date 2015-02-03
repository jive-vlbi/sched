C$Procedure      ZZEKSTYP ( EK, determine segment type )
 
      INTEGER FUNCTION ZZEKSTYP ( NCOLS, CDSCRS )
 
C$ Abstract
C
C     Determine the type of segment required to support a specified
C     set of columns.
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
C
C$ Declarations
 
      INCLUDE 'ekcoldsc.inc'
      INCLUDE 'ekglimit.inc'
 
      INTEGER               NCOLS
      INTEGER               CDSCRS ( CDSCSZ, * )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     NCOLS      I   Number of columns in the segment.
C     CDSCRS     I   Descriptors of columns.
C
C     The function returns the type of segment that is compatible with
C     the input column descriptors.
C
C$ Detailed_Input
C
C
C     NCOLS          is the number of columns in a new segment.
C
C     CDSCRS         is an array of column descriptors:  the Ith
C                    descriptor applies to the Ith column in the
C                    segment.
C
C$ Detailed_Output
C
C     The function returns the type of segment that is compatible with
C     the input column descriptors.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If NCOLS is non-positive or greater than the maximum allowed
C         number MXCLSG, the error SPICE(INVALIDCOUNT) is signalled.
C
C     2)  If the input column descriptors do not contain compatible
C         attributes, the error SPICE(BADATTRIBUTES) will be signalled.
C
C$ Files
C
C     See the EK Required Reading for a discussion of the EK file
C     format.
C
C$ Particulars
C
C     This routine determines the appropriate segment type to contain
C     a specified set of columns.  Currently, there are two segment
C     types.  The first type accommodates column classes 1 through 6;
C     the second type accommodates column classes 7 through 9.  The
C     latter set of column classes are `fixed_count' classes:  a column
C     in one of these classes may not have entries added or deleted
C     after the column is created.  Fixed and variable count columns
C     may not coexist in the same segment.
C
C$ Examples
C
C     See EKBSEG.
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
C-    Beta Version 1.0.0, 06-NOV-1995 (NJB)
C
C-&
 
C
C     SPICELIB functions
C
      LOGICAL               RETURN
 
C
C     Local variables
C
      INTEGER               I
 
      LOGICAL               FIXED
      LOGICAL               VAR
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         ZZEKSTYP = 0
         RETURN
      ELSE
         CALL CHKIN ( 'ZZEKSTYP' )
      END IF
 
C
C     FIXED and VAR indicate whether we've seen any fixed or variable
C     column classes so far.
C
      FIXED  =  .FALSE.
      VAR    =  .FALSE.
 
      DO I = 1, NCOLS
 
         IF (       ( CDSCRS(CLSIDX,I) .GE. 1 )
     .        .AND. ( CDSCRS(CLSIDX,I) .LE. 6 )  ) THEN
 
            VAR   = .TRUE.
 
         ELSE IF (       ( CDSCRS(CLSIDX,I) .GE. 7 )
     .             .AND. ( CDSCRS(CLSIDX,I) .LE. 9 )  ) THEN
 
            FIXED = .TRUE.
 
         END IF
 
      END DO
 
 
      IF (  VAR  .AND. ( .NOT. FIXED )  ) THEN
 
         ZZEKSTYP = 1
 
      ELSE IF (  FIXED  .AND. ( .NOT. VAR )  ) THEN
 
         ZZEKSTYP = 2
 
      ELSE
 
         ZZEKSTYP = 0
 
         CALL SETMSG ( 'Column set contains a mixture of variable ' //
     .                 'and fixed-count columns.  Segments must '   //
     .                 'contain all variable or all fixed count '   //
     .                 'columns.'                                    )
         CALL SIGERR ( 'SPICE(BADATTRIBUTES)'                        )
         CALL CHKOUT ( 'ZZEKSTYP'                                    )
         RETURN
 
      END IF
 
 
      CALL CHKOUT ( 'ZZEKSTYP' )
      RETURN
      END
