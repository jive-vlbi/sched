C$Procedure      M2HAVE ( META/2 --- How many matches do we have )
 
      INTEGER FUNCTION M2HAVE ( NAME )
      IMPLICIT NONE
 
C$ Abstract
C
C     Find the number of matches there were for a particular named
C     META/2 template word.
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
C     META/2 --- a language specification language.
C
C$ Keywords
C
C     META/2
C     PARSING
C
C$ Declarations
 
      CHARACTER*(*)         NAME
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     NAME       I   The name of a META/2 template word.
C
C     M2HAVE is returned with the number of words that matched the
C     template word specified by NAME.
C
C$ Detailed_Input
C
C     NAME       is the name of a META/2 template word that may have
C                been matched by a call to M2GMCH.  The case of NAME
C                is significant.  'BOB', 'Bob', and 'bob' will be
C                regarded as different names.
C
C$ Detailed_Output
C
C     M2HAVE     is the number of matches that were made agains
C                the named META/2 template word specified by NAME.
C                If there were no matches, M2HAVE is returned as zero.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     None.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     Once a string has matched some META/2 template, one normally
C     needs to retrieve the information in the string.  In some cases
C     the META/2 template will allow for a variable number of
C     matches with a particular template word.  To extract the
C     information in the string, it is necessary to determine how many
C     words matched the template word in question.
C
C     This routine exists so that you can easily find out how many
C     matches there were for a particular named template word.
C
C$ Examples
C
C     Suppose that a string is known to have matched the following
C     META/2 template.
C
C        FIND UNION OF (2:)@name[sets]
C
C     To accurately carry out the task specified by this string,
C     you will need to find the "names" of the sets specified.
C
C        NSETS = M2HAVE('sets')
C
C        CALL M2GETC ( 'sets', STRING, 1, FOUND, NAME )
C
C           copy the named set into the set UNION.
C
C        DO I = 2, NSETS
C
C           CALL M2GETC ( 'sets', STRING, I, FOUND, NAME )
C
C           form the union of UNION with the set specified by NAME
C
C        END DO
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
C     W.L. Taber     (JPL)
C
C$ Version
C
C-     META/2 Configured Version 2.0.0, 9-MAY-1994 (WLT)
C
C         This is the configured version of the Command Loop
C         software as of May 9, 1994
C
C
C-     META/2 Configured Version 1.0.0, 3-MAY-1994 (WLT)
C
C         This is the configured version of META/2
C         software as of May 3, 1994
C
C
C-    Beta Version 1.0.0, 27-NOV-1991 (WLT)
C
C-&
C
C$ Index_Entry
C
C     Check for the presence of a named match in the META/2 tables.
C
C-&
 
C
C     Local variables
C
      INTEGER               SIZE
 
      SAVE
 
 
C
C     Find out how many endpoints were matched, and put the answer into
C     M2HAVE.
C
      CALL M2VSIZ ( NAME, SIZE )
 
      M2HAVE = SIZE
 
      RETURN
      END
