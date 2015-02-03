C$Procedure DAFPS ( DAF, pack summary )

      SUBROUTINE DAFPS ( ND, NI, DC, IC, SUM )

C$ Abstract
C
C     Pack (assemble) an array summary from its double precision and
C     integer components.
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
C     DAF
C
C$ Keywords
C
C     CONVERSION
C     FILES
C
C$ Declarations

      INTEGER               ND
      INTEGER               NI
      DOUBLE PRECISION      DC       ( * )
      INTEGER               IC       ( * )
      DOUBLE PRECISION      SUM      ( * )

C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     ND         I   Number of double precision components.
C     NI         I   Number of integer components.
C     DC         I   Double precision components.
C     IC         I   Integer components.
C     SUM        O   Array summary.
C
C$ Detailed_Input
C
C     ND          is the number of double precision components in
C                 the summary to be packed.
C
C     NI          is the number of integer components in the summary.
C
C     DC          are the double precision components of the summary.
C
C     IC          are the integer components of the summary.
C
C$ Detailed_Output
C
C     SUM         is an array summary containing the components in DC
C                 and IC. This identifies the contents and location of
C                 a single array within a DAF.
C
C$ Parameters
C
C      None.
C
C$ Exceptions
C
C     Error free.
C
C     1) If ND is zero or negative, no DP components are stored.
C
C     2) If NI is zero or negative, no integer components are stored.
C
C     3) If the total size of the summary is greater than 125 double
C        precision words, some components may not be stored.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     The components of array summaries are packed into double
C     precision arrays for reasons outlined in [1]. Two routines,
C     DAFPS (pack summary) and DAFUS (unpack summary) are provided
C     for packing and unpacking summaries.
C
C     The total size of the summary is
C
C             (NI - 1)
C        ND + -------- + 1
C                 2
C
C     double precision words (where ND, NI are nonnegative).
C
C$ Examples
C
C     Maybe later.
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
C     I.M. Underwood  (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.3, 10-OCT-2012 (EDW)
C
C        Removed the obsolete Reference citation to "NAIF
C        Document 167.0."
C
C        Corrected ordering of header section.
C
C-    SPICELIB Version 1.0.2, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN)
C
C        Literature references added to the header.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
C
C-&

C$ Index_Entries
C
C     pack daf summary
C
C-&


C
C     Local variables
C
      DOUBLE PRECISION      DEQUIV   ( 125 )
      INTEGER               IEQUIV   ( 250 )

      INTEGER               N
      INTEGER               M

C
C     Equivalences
C
      EQUIVALENCE         ( DEQUIV, IEQUIV )

C
C     Here's the deal: the DP components always precede the integer
C     components, avoiding alignment problems. The DP components can
C     be stored directly.
C
      N = MIN ( 125, MAX ( 0, ND ) )

      CALL MOVED ( DC, N, SUM )

C
C     The integer components must detour through an equivalence.
C
      M = MIN ( 250 - 2 * N, MAX ( 0, NI ) )

      CALL MOVEI ( IC,          M,              IEQUIV   )
      CALL MOVED ( DEQUIV,     (M - 1) / 2 + 1, SUM(N+1) )

      RETURN



C$Procedure DAFUS ( DAF, unpack summary )

      ENTRY DAFUS ( SUM, ND, NI, DC, IC )

C$ Abstract
C
C     Unpack an array summary into its double precision and integer
C     components.
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
C     DAF
C
C$ Keywords
C
C     CONVERSION
C     FILES
C
C$ Declarations
C
C     DOUBLE PRECISION      SUM      ( * )
C     INTEGER               ND
C     INTEGER               NI
C     DOUBLE PRECISION      DC       ( * )
C     INTEGER               IC       ( * )
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     SUM        I   Array summary.
C     ND         I   Number of double precision components.
C     NI         I   Number of integer components.
C     DC         O   Double precision components.
C     IC         O   Integer components.
C
C$ Detailed_Input
C
C     SUM         is an array summary. This identifies the contents and
C                 location of a single array within a DAF.
C
C     ND          is the number of double precision components in
C                 the summary.
C
C     NI          is the number of integer components in the summary.
C
C$ Detailed_Output
C
C     DC          are the double precision components of the summary.
C
C     IC          are the integer components of the summary.
C
C$ Parameters
C
C      None.
C
C$ Exceptions
C
C     Error free.
C
C     1) If ND is zero or negative, no double precision components
C        are returned.
C
C     2) If NI is zero or negative, no integer components are returned.
C
C     3) If the total size of the summary is greater than 125 double
C        precision words, some components may not be returned.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     The components of array summaries are packed into double
C     precision arrays for reasons outlined in [1]. Two routines,
C     DAFPS (pack summary) and DAFUS (unpack summary) are provided
C     for packing and unpacking summaries.
C
C     The total size of the summary is
C
C             (NI - 1)
C        ND + -------- + 1
C                 2
C
C     double precision words (where ND, NI are nonnegative).
C
C$ Examples
C
C     Use a simple routine to output the double precision and integer
C     values stored in an SPK's segments descriptors. This function
C     opens a DAF for read, performs a forwards search for the DAF
C     arrays, prints segments description for each array found, then
C     closes the DAF.
C
C           PROGRAM DAF_T
C
C           INTEGER             HANDLE
C
C     C
C     C     Define the summary parameters appropriate
C     C     for an SPK file.
C     C
C           INTEGER             ND
C           PARAMETER         ( ND = 2 )
C
C           INTEGER             NI
C           PARAMETER         ( NI = 6 )
C
C           INTEGER             IC( NI )
C
C           DOUBLE PRECISION    DC( ND )
C
C           CHARACTER*(32)      KERNEL
C
C           LOGICAL             FOUND
C
C
C     C
C     C     Open a DAF for read. Return a HANDLE referring to the file.
C     C
C           KERNEL = 'de421.bsp'
C           CALL DAFOPR ( KERNEL, HANDLE )
C
C     C
C     C     Begin a forward search on the file.
C     C
C           CALL DAFBFS ( HANDLE )
C
C     C
C     C     Search until a DAF array is found.
C     C
C           CALL DAFFNA ( FOUND )
C
C     C
C     C     Loop while the search finds subsequent DAF arrays.
C     C
C           DO WHILE ( FOUND )
C
C              CALL DAFGS ( SUM )
C              CALL DAFUS ( SUM, ND, NI, DC, IC )
C
C              WRITE(*,*)                'Doubles: ', DC(1:ND)
C              WRITE(*, FMT='(A,6I9)' ) 'Integers: ', IC(1:NI)
C
C     C
C     C        Check for another segment.
C     C
C              CALL DAFFNA ( FOUND )
C
C           END DO
C
C     C
C     C     Safely close the DAF.
C     C
C           CALL DAFCLS ( HANDLE )
C
C           END
C
C     The program outputs:
C
C      Doubles:   -3169195200.0000000        1696852800.0000000
C     Integers:         1        0        1        2      641   310404
C      Doubles:   -3169195200.0000000        1696852800.0000000
C     Integers:         2        0        1        2   310405   423048
C      Doubles:   -3169195200.0000000        1696852800.0000000
C     Integers:         3        0        1        2   423049   567372
C      Doubles:   -3169195200.0000000        1696852800.0000000
C     Integers:         4        0        1        2   567373   628976
C      Doubles:   -3169195200.0000000        1696852800.0000000
C     Integers:         5        0        1        2   628977   674740
C      Doubles:   -3169195200.0000000        1696852800.0000000
C     Integers:         6        0        1        2   674741   715224
C      Doubles:   -3169195200.0000000        1696852800.0000000
C     Integers:         7        0        1        2   715225   750428
C      Doubles:   -3169195200.0000000        1696852800.0000000
C     Integers:         8        0        1        2   750429   785632
C      Doubles:   -3169195200.0000000        1696852800.0000000
C     Integers:         9        0        1        2   785633   820836
C      Doubles:   -3169195200.0000000        1696852800.0000000
C     Integers:        10        0        1        2   820837   944040
C      Doubles:   -3169195200.0000000        1696852800.0000000
C     Integers:       301        3        1        2   944041  1521324
C      Doubles:   -3169195200.0000000        1696852800.0000000
C     Integers:       399        3        1        2  1521325  2098608
C      Doubles:   -3169195200.0000000        1696852800.0000000
C     Integers:       199        1        1        2  2098609  2098620
C      Doubles:   -3169195200.0000000        1696852800.0000000
C     Integers:       299        2        1        2  2098621  2098632
C      Doubles:   -3169195200.0000000        1696852800.0000000
C     Integers:       499        4        1        2  2098633  2098644
C
C      Note, the final entries in the integer array contains the segment
C      start/end indexes. The output indicates the search proceeded
C      from the start of the file (low value index) towards the end
C      (high value index).
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
C     I.M. Underwood  (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.3, 10-OCT-2012 (EDW)
C
C        Added a functional code example to the Examples section.
C
C        Removed the obsolete Reference citation to "NAIF
C        Document 167.0."
C
C        Corrected ordering of header section.
C
C-    SPICELIB Version 1.0.2, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN)
C
C        Literature references added to the header.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
C
C-&

C$ Index_Entries
C
C     unpack daf summary
C
C-&


C
C     Just undo whatever DAFPS did.
C
      N = MIN ( 125, MAX ( 0, ND ) )

      CALL MOVED ( SUM, N, DC )

      M = MIN ( 250 - 2 * N, MAX ( 0, NI ) )

      CALL MOVED ( SUM(N+1), (M - 1) / 2 + 1, DEQUIV )
      CALL MOVEI ( IEQUIV,    M,              IC     )

      RETURN
      END

