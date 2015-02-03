C$Procedure M2PARS ( META/2 --- Parsing utility. )
 
      SUBROUTINE M2PARS ( NAME, B, E, NTH, FOUND, SIZE )
      IMPLICIT NONE
 
C$ Abstract
C
C     M2PARS serves as an umbrella subroutine for a series of entry
C     points that serve as a storage utility for parsed words of
C     a string that matches a META/2 language statement template.
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
C     META/2 --- A language specification language.
C
C$ Keywords
C
C     META/2
C     PARSING
C
C$ Declarations
 
      CHARACTER*(*)         NAME
      INTEGER               B
      INTEGER               E
      INTEGER               NTH
      LOGICAL               FOUND
      INTEGER               SIZE
 
      INTEGER               MXNAME
      PARAMETER           ( MXNAME  = 100 )
 
      INTEGER               MXVALS
      PARAMETER           ( MXVALS  = 400 )
 
C$ Brief_I/O
C
C     Variable  I/O  Entry Points
C     --------  ---  --------------------------------------------------
C     NAME       I   M2SAVE, M2VGET
C     B         I/O  M2SAVE, M2VGET
C     E         I/O  M2SAVE, M2VGET
C     NTH        O   M2VGET
C     FOUND      O   M2VGET
C     SIZE       O   M2VSIZ
C
C     MXNAME     P   Maximum number of named variables that can be saved
C     MXVALS     P   Maximum number of variable values that can be saved
C
C$ Detailed_Input
C
C     NAME       is the variable name associated with some META/2
C                template word.
C
C     B          is the index of the beginning of a word in a string
C                that matches the template word associated with NAME.
C
C     E          is the index of the ending of a word in a string
C                that matches the template word associated with NAME.
C
C     NTH        is the number of the matching substring to to locate
C                in the table of parsed matches.
C
C$ Detailed_Output
C
C     B          is the index of the beginning of a word in a string
C                that matches the template word associated with NAME.
C
C     E          is the index of the ending of a word in a string
C                that matches the template word associated with NAME.
C
C     FOUND      is a logical flag that is returned .TRUE. if
C                a specified named template word matched a word
C                in  string.  Otherwise it returns .FALSE.
C
C     SIZE       is the size of the set of words that matched
C                a particular named META/2 template word.
C
C$ Parameters
C
C     MXNAME     is the maximum number of named template variables that
C                can be saved at any time.
C
C     MXVALS     is the maximum number of name template variable values
C                that can be saved at any time.
C
C$ Exceptions
C
C     1) If the number of named template variables or the total number
C        of values exceeds the space allotted for these items, an error
C        will be diagnosed and signalled by a routine in the call
C        tree of this routine.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine serves as an umbrella subroutine for a collection
C     of related entry points that are used to manage storage of
C     parsed words in strings that match a META/2 language template.
C
C     These entry points cooperate to allow simple parsing of statements
C     that match META/2 templates.  To understand how this cooperation
C     takes place, you need to understand how strings are matched
C     against META/2 templates.  General META/2 templates are composed
C     of collections of simple templates organized via the placement
C     of grouping  symbols.  These groups are called switches. See the
C     META/2 required reading from a more complete description of
C     switches.
C
C     Attempts to match a string with a META/2 template proceed from
C     left to right in both the string and template.  When a switch
C     is encountered, an attempt is made to match the remaining
C     portion of the string with each simple template until a match
C     attempt succeeds or all attempts fail.
C
C     The parsing portion of META/2 lies in the simple template
C     matching module.  As words in the input string are matched with
C     words in the simple template, their boundaries in the string
C     are saved in tables located in this routine.  These boundaries
C     can then be retrieved by the name attached to the META/2 template
C     word they matched.
C
C     Since a string can match any of several templates within a switch,
C     several match attempts in a switch may fail before the matching
C     simple template is encountered.  As a result, there needs to be
C     a mechanism for accumulating parsed matches until a full simple
C     template is matched.  At that point the accumulated matches need
C     to be moved to a more stable storage area.  In this way the
C     string can be parsed as it is matched.
C
C
C     The entry points and the functions they serve are listed here:
C
C     M2SAVE   this entry point is used to store the beginning and
C              ending indexes of a word in a string that matches
C              a named variable/word in a META/2 language template.
C
C     M2PCLR   this entry point is used to clear all stored information
C              in the tables containing substring indexes for matched
C              META/2 template words.
C
C     M2TCLR   this entry point is used to clear information stored
C              in the temporary accumulation tables that store
C              indexes for the beginning and ending of matched
C              META/2 template words from simple templates.
C
C     M2KEEP   is the routine that transfers the accumulated matches to
C              the finished set of parsed matches.
C
C     M2VGET   retrieves the N'th substring boundaries (of a string
C              that matches a META/2 template) that correspond to a
C              specific named word of the matching META/2 template.
C
C     M2VSIZ   retrieve the number of pair of indices marking beginnings
C              and endings of string words that matched a particular
C              named template word.
C
C     Related routines exist.  For use in logical expressions:
C
C        M2XIST(NAME) will be .TRUE. if there is a marked substring
C        that matches a META/2 template word having name NAME.
C
C     To determine the number of substrings associated with a given
C     named template word use the function:
C
C        M2HAVE ( NAME )
C
C     To extract the n'th word or first word associated with a
C     named template word
C
C        CALL M2SELC ( NAME, STRING, NTH, FOUND, WORD )
C        CALL M2GETC ( NAME, STRING,      FOUND, WORD )
C
C     To extract and parse the n'th integer or the first integer
C     associated with a named template word
C
C        CALL M2SELI ( NAME, STRING, NTH, FOUND, INT  )
C        CALL M2GETI ( NAME, STRING,      FOUND, INT  )
C
C     To extract and parse the n'th number or first number  associated
C     with a named template word
C
C        CALL M2SELD ( NAME, STRING, NTH, FOUND, DP   )
C        CALL M2GETD ( NAME, STRING,      FOUND, DP   )
C
C$ Examples
C
C     The average user will never need to call any of the entry points
C     to this routine.  However, it may be desirable to design a routine
C     that makes use of the entry points to this routine.  Example
C     routines are outlined in each of the individual entry points.
C
C$ Restrictions
C
C     See individual entry points.
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
C-    Beta Version 1.0.0, 22-NOV-1991 (WLT)
C
C-&
 
 
C
C     Spicelib Functions.
C
      INTEGER               CARDC
      INTEGER               SYDIMI
 
C
C     Private Parameters
C
      INTEGER               LBCELL
      PARAMETER           ( LBCELL  =  -5 )
 
C
C     Local Variables.
C
      CHARACTER*(32)        NAMES  ( LBCELL : MXNAME )
      INTEGER               PTRS   ( LBCELL : MXNAME )
      INTEGER               VALS   ( LBCELL : MXVALS )
 
      CHARACTER*(32)        ANAMES ( LBCELL : MXNAME )
      INTEGER               APTRS  ( LBCELL : MXNAME )
      INTEGER               AVALS  ( LBCELL : MXVALS )
 
 
      INTEGER               BEGEND (          2      )
      INTEGER               I
      INTEGER               J
      INTEGER               K
 
      CHARACTER*(32)        MYNAME
 
      INTEGER               TEMP   (          MXVALS )
      INTEGER               TOTAL
 
 
      LOGICAL               FIRST
      LOGICAL               GOTIT
 
      SAVE
 
      DATA                  FIRST   / .TRUE. /
 
      RETURN
 
 
C$Procedure M2SAVE ( META/2 --- save substring boundaries )
 
      ENTRY M2SAVE ( NAME, B, E )
 
C$ Abstract
C
C     Store the substring boundaries of a word that matches a META/2
C     template word.
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
C     META/2 --- A language specification language.
C
C$ Keywords
C
C     PARSING
C     UTILITITY
C
C$ Declarations
C
C     CHARACTER*(*)         NAME
C     INTEGER               B
C     INTEGER               E
C
C
C     INTEGER               MXNAME
C     PARAMETER           ( MXNAME  = 100 )
C
C     INTEGER               MXVALS
C     PARAMETER           ( MXVALS  = 400 )
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     NAME       I   The name of a template word that was matched.
C     B          I   The beginning of the matching substring.
C     E          I   The ending of the matching substring.
C
C     MXNAME     P   Maximum number of named variables that can be saved
C     MXVALS     P   Maximum number of variable values that can be saved
C
C$ Detailed_Input
C
C     NAME       is the name associated with a particular META/2
C                template word that has been matched against some
C                word in a string.
C
C     B          is the beginning index of a word in a string that
C                matched the META/2 template word associated with
C                NAME.
C
C     E          is the ending index of a word in a string that
C                matched the META/2 template word associated with
C                NAME.
C
C$ Detailed_Output
C
C     None.
C
C$ Parameters
C
C     MXNAME     is the maximum number of named template variables that
C                can be saved at any time.
C
C     MXVALS     is the maximum number of named template variable values
C                that can be saved at any time.
C
C$ Exceptions
C
C     1) If the table for storing string endpoints is unable to store
C        the input endpoints, the error will be diagnosed and signalled
C        by a routine in this routine's call-tree.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine provides the META/2 matching routines a place to
C     deposit the boundaries of words that match named META/2 template
C     words.  It is not intendend for direct use by general users.
C
C$ Examples
C
C     See the routine M2WMCH for an example of the use of this routine.
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
C-    Beta Version 1.0.0, 25-NOV-1991 (WLT)
C
C-&
C$ Index_Entry
C
C     Save the boundaries of words matching META/2 template words
C
C-&
      IF ( FIRST ) THEN
 
         FIRST = .FALSE.
C
C        Initialize the keepers table.
C
         CALL SSIZEC ( MXNAME, NAMES  )
         CALL SSIZEI ( MXNAME, PTRS   )
         CALL SSIZEI ( MXVALS, VALS   )
 
C
C        Initialize the accumulation table
C
         CALL SSIZEC ( MXNAME, ANAMES )
         CALL SSIZEI ( MXNAME, APTRS  )
         CALL SSIZEI ( MXVALS, AVALS  )
 
      END IF
 
C
C     Enque the new string boundaries in the accumulation table.
C
      CALL SYENQI ( NAME, B, ANAMES, APTRS, AVALS )
      CALL SYENQI ( NAME, E, ANAMES, APTRS, AVALS )
 
      RETURN
 
 
C$Procedure      M2PCLR ( META/2 --- Parse table clear )
 
      ENTRY M2PCLR
 
C$ Abstract
C
C     Clear both the accumulation and parse tables.
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
C     PARSING
C     UTILITY
C
C$ Declarations
C
C     None.
C
C$ Brief_I/O
C
C     None.
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C     None.
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
C     This routine clears all tables used by M2PARS that store beginning
C     and ends of words that match names META/2 template words.
C     It should never be called directly by users.
C
C$ Examples
C
C     None.  See the routine M2GMCH for the only instance of use of this
C     routine.
C
C$ Restrictions
C
C     User's should not call this routine directly.  It is intended
C     only for use as utility for META/2 software.
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
C-    Beta Version 1.0.0, 25-NOV-1991 (WLT)
C
C-&
C
C$ Index_Entry
C
C     Clear the parse META/2 parse tables
C
C-&
 
 
      FIRST = .FALSE.
C
C     Initialize the keepers table.
C
      CALL SSIZEC ( MXNAME, NAMES  )
      CALL SSIZEI ( MXNAME, PTRS   )
      CALL SSIZEI ( MXVALS, VALS   )
 
C
C     Initialize the accumulation table
C
      CALL SSIZEC ( MXNAME, ANAMES )
      CALL SSIZEI ( MXNAME, APTRS  )
      CALL SSIZEI ( MXVALS, AVALS  )
 
      RETURN
 
 
 
 
C$Procedure      M2TCLR ( META/2 --- Temporary parse table clear )
 
      ENTRY M2TCLR
 
C$ Abstract
C
C     Clear both the accumulation (temporary) parse table.
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
C     PARSING
C     UTILITY
C
C$ Declarations
C
C     None.
C
C$ Brief_I/O
C
C     None.
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C     None.
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
C     This routine clears the temporary (accumulation) table used
C     by M2PARS that stores beginning and ends of words that match
C     names META/2 template words.  It should never be called directly
C     by users.
C
C$ Examples
C
C     None.  See the routine M2GMCH for the only instance of use of this
C     routine.
C
C$ Restrictions
C
C     User's should not call this routine directly.  It is intended
C     only for use as utility for META/2 software.
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
C-    Beta Version 1.0.0, 25-NOV-1991 (WLT)
C
C-&
C
C$ Index_Entry
C
C     Clear the parse META/2 temporary parse tables
C
C-&
 
C
C     Just in case, we initialize the keepers table if it hasn't been
C     initialized already.
C
      IF ( FIRST ) THEN
 
         FIRST = .FALSE.
 
C
C        Initialize the keepers table.
C
         CALL SSIZEC ( MXNAME, NAMES  )
         CALL SSIZEI ( MXNAME, PTRS   )
         CALL SSIZEI ( MXVALS, VALS   )
 
      END IF
C
C     Initialize the accumulation table
C
      CALL SSIZEC ( MXNAME, ANAMES )
      CALL SSIZEI ( MXNAME, APTRS  )
      CALL SSIZEI ( MXVALS, AVALS  )
 
      RETURN
 
 
 
C$Procedure M2KEEP ( META/2 --- Keep temporary parse table values )
 
      ENTRY M2KEEP
 
C$ Abstract
C
C     Copy names/value associations from the temporary (accumulation)
C     parse table to the long-term parse table.
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
C     PARSING
C     UTILITY
C
C$ Declarations
C
C     None.
C
C$ Brief_I/O
C
C     None.
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C     None.
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
C     This routine copies values from the temporary (accumulation)
C     parse tables into the long-term parse tables used by M2GMCH.
C     It should never be called directly by users.
C
C$ Examples
C
C     None.  See the routine M2GMCH for the only instance of use of this
C     routine.
C
C$ Restrictions
C
C     User's should not call this routine directly.  It is intended
C     only for use as utility for META/2 software.
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
C-    Beta Version 1.0.0, 25-NOV-1991 (WLT)
C
C-&
C
C$ Index_Entry
C
C     Keep values in the META/2 temporary parse tables
C
C-&
 
C
C     For each entry in the accumulation table...
C
      DO I = 1, CARDC(ANAMES)
 
C
C        Find out its name,
C
 
         CALL SYFETI ( I,   ANAMES, APTRS,  AVALS,  MYNAME, GOTIT )
 
         IF ( GOTIT ) THEN
 
C
C           ...extract the values.
C
            CALL SYGETI ( MYNAME, ANAMES,
     .                            APTRS,
     .                            AVALS, TOTAL, TEMP, GOTIT )
 
C
C           and put them in the keepers table.
C
            CALL SYPUTI ( MYNAME, TEMP, TOTAL, NAMES, PTRS, VALS )
 
         END IF
 
      END DO
 
      RETURN
 
 
 
C$Procedure M2VGET ( META/2 --- Get variable )
 
      ENTRY M2VGET ( NAME, NTH, FOUND, B, E )
 
C$ Abstract
C
C     Retrieve the boundaries of the Nth substring word that matches a
C     named META/2 template word.
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
C     META/2 --- a language specification language
C
C$ Keywords
C
C     PARSING
C     RETRIEVAL
C
C$ Declarations
C
C     CHARACTER*(*)         NAME
C     INTEGER               NTH
C     LOGICAL               FOUND
C     INTEGER               B
C     INTEGER               E
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     NAME       I   is the name of some variable in the parse table.
C     NTH        I   is the number of the substring boundary to get.
C     FOUND      O   is returned .TRUE. if requested data can be found.
C     B          O   is the beginning index of the matched word.
C     E          O   is the ending index of the matched word.
C
C$ Detailed_Input
C
C     NAME       is the name attached to some META/2 template word
C                that may have successfully matched a word in a
C                string.
C
C     NTH        is the number (in sequence) of the word substring to
C                locate that matched the names META/2 template word.
C
C$ Detailed_Output
C
C     FOUND      is .TRUE. if the requested information was available
C                in the parse table.  Otherwise it is returned .FALSE.
C
C     B          is the beginning of the word in the string
C                corresponding to the requested information.
C
C     E          is the ending of the word in the string corresponding
C                to the requested information.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If the requested variable is not present in the table or
C        the requested substring bounds are not available (for example
C        you ask for the 4th word boundaries and there are only
C        3 word boundaries) then FOUND will be returned as .FALSE.
C        and the values of B and E will be returned unchanged.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     Once a string has been matched against a META/2 template, it
C     usually must be parsed to determine the information content of
C     the string.  By designing the META/2 language so that the needed
C     information corresponds to named words of the META/2 template,
C     this routine can be used to aid in the parsing of the matched
C     string.
C
C     It is intended that this routine not be called very often
C     by programmers who make use of the META/2 interface.  More
C     convenient high level routines exist that perform the most
C     frequently needed parsing functions.  Nevertheless, it may
C     sometimes be more convenient to make direct use of this routine.
C
C     META/2 templates allow for "variable length" words such as
C     (3:5)@int.  A template that matches a template with such a
C     META/2 word will have several words that match the (3:5)@int
C     template word.  If the template word is named as in
C
C          (3:5)@int[COEFFICIENTS]
C
C     you can ask for the first, second, third, etc word of the
C     string that matched this particular word.  The call below
C     will locate the word index boundaries for you.
C
C         CALL M2VGET ( 'COEFFICIENTS', NTH, FOUND, B, E )
C
C     You will then need to process as needed the string  STRING(B:E) to
C     determine the actual information present in the matching string.
C
C$ Examples
C
C     Suppose you wished to collect all of the string words that matched
C     the named META/2 template word MYWORDS.  The code below would
C     do the job.  (This assumes that you have declared the array
C     WORDS to be sufficiently large to hold all of the matching words.)
C
C     C
C     C   Start with the first word...
C     C
C         I = 1
C         CALL M2VGET ( 'MYWORDS', I, FOUND, B, E )
C
C         DO WHILE ( FOUND )
C
C            WORDS(I)  =   STRING(B:E)
C
C      C
C      C     ... and continue collecting words until no more are found.
C      C
C            I         =   I + 1
C            CALL M2VGET ( 'MYWORDS', I, FOUND, B, E )
C
C         END DO
C
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
C-    Beta Version 1.0.0, 25-NOV-1991 (WLT)
C
C-&
C
C$ Index_Entry
C
C     Get word boundaries for words matching META/2 templates
C
C-&
 
 
C
C     Look up any parsed values.
C
      FOUND = .FALSE.
 
      J = 2*NTH - 1
      K = 2*NTH
 
      CALL SYSELI ( NAME,  J, K, NAMES,
     .                           PTRS,
     .                           VALS,   BEGEND, FOUND )
 
      IF ( FOUND ) THEN
 
         B = BEGEND(1)
         E = BEGEND(2)
 
      END IF
C
C     That's all folks....
C
 
      RETURN
 
 
C$Procedure M2VSIZ ( META/2 --- matched variable template size )
 
      ENTRY M2VSIZ ( NAME, SIZE )
 
C$ Abstract
C
C     Determine the size of the collection of words from a string that
C     matched a named META/2 template word.
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
C     PARSING
C
C$ Declarations
C
C     CHARACTER*(*)         NAME
C     INTEGER               SIZE
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     NAME       I   The name of some META/2 template word.
C     SIZE       O   The number of string words that matched the word.
C
C$ Detailed_Input
C
C     NAME       is the name of some named META/2 template word that a
C                string has been matched against.
C
C$ Detailed_Output
C
C     SIZE       is the size (number of members) of the collection of
C                words that matched the named META/2 template word
C                specified by NAME.  If NAME does not appear in the
C                parse table, SIZE will be returned as zero.
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
C     This entry point returns the number of words in a string that
C     matched a named META/2 template word.  A function, M2HAVE,
C     also exists that returns this number and may be more convenient
C     in some cases.
C
C$ Examples
C
C     Suppose you wished to collect all of the words that matched
C     a META/2 template word with name 'MYWORD'.  You might use
C     this entry point to help determine loop boundaries.
C
C           CALL M2VSIZ ( 'MYWORD', SIZE )
C
C           DO I = 1, SIZE
C              CALL M2VGET ( 'MYWORD', I, FOUND, B, E, )
C              WORDS(I)    =  STRING(B:E)
C           END DO
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
C-    Beta Version 1.0.0, 26-NOV-1991 (WLT)
C
C-&
C
C$ Index_Entry
C
C     Find the number of words matching a META/2 template word
C
C-&
 
 
C
C     Just look up the number of word boundaries and divide by two.
C
      TOTAL = SYDIMI ( NAME, NAMES, PTRS, VALS )
      SIZE  = TOTAL / 2
      RETURN
 
      END
