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
C
C     Include Section:  EK Query Limit Parameters
C
C        ekqlimit.inc  Version 3    16-NOV-1995 (NJB)
C
C           Parameter MAXCON increased to 1000.
C
C        ekqlimit.inc  Version 2    01-AUG-1995 (NJB)
C
C           Updated to support SELECT clause.
C
C
C        ekqlimit.inc  Version 1    07-FEB-1995 (NJB)
C
C
C     These limits apply to character string queries input to the
C     EK scanner.  This limits are part of the EK system's user
C     interface:  the values should be advertised in the EK required
C     reading document.
C
C
C     Maximum length of an input query:  MAXQRY.  This value is
C     currently set to twenty-five 80-character lines.
C
      INTEGER               MAXQRY
      PARAMETER           ( MAXQRY = 2000 )
 
C
C     Maximum number of columns that may be listed in the
C     `order-by clause' of a query:  MAXSEL.  MAXSEL = 50.
C
      INTEGER               MAXSEL
      PARAMETER           ( MAXSEL = 50 )
 
C
C     Maximum number of tables that may be listed in the `FROM
C     clause' of a query: MAXTAB.
C
      INTEGER               MAXTAB
      PARAMETER           ( MAXTAB = 10 )
 
C
C     Maximum number of relational expressions that may be listed
C     in the `constraint clause' of a query: MAXCON.
C
C     This limit applies to a query when it is represented in
C     `normalized form': that is, the constraints have been
C     expressed as a disjunction of conjunctions of relational
C     expressions. The number of relational expressions in a query
C     that has been expanded in this fashion may be greater than
C     the number of relations in the query as orginally written.
C     For example, the expression
C
C             ( ( A LT 1 ) OR ( B GT 2 ) )
C        AND
C             ( ( C NE 3 ) OR ( D EQ 4 ) )
C
C     which contains 4 relational expressions, expands to the
C     equivalent normalized constraint
C
C             (  ( A LT 1 ) AND ( C NE 3 )  )
C        OR
C             (  ( A LT 1 ) AND ( D EQ 4 )  )
C        OR
C             (  ( B GT 2 ) AND ( C NE 3 )  )
C        OR
C             (  ( B GT 2 ) AND ( D EQ 4 )  )
C
C     which contains eight relational expressions.
C
C
      INTEGER               MAXCON
      PARAMETER           ( MAXCON = 1000 )
 
C
C     MXJOIN is the maximum number of tables that can be joined.
C
      INTEGER               MXJOIN
      PARAMETER           ( MXJOIN = 10 )
 
C
C     MXJCON is the maximum number of join constraints allowed.
C
      INTEGER               MXJCON
      PARAMETER           ( MXJCON = 100 )
 
C
C     Maximum number of order-by columns that may be used in the
C     `order-by clause' of a query: MAXORD. MAXORD = 10.
C
      INTEGER               MAXORD
      PARAMETER           ( MAXORD = 10 )
 
C
C     Maximum number of tokens in a query: 500. Tokens are reserved
C     words, column names, parentheses, and values. Literal strings
C     and time values count as single tokens.
C
      INTEGER               MAXTOK
      PARAMETER           ( MAXTOK = 500 )
 
C
C     Maximum number of numeric tokens in a query:
C
      INTEGER               MAXQNM
      PARAMETER           ( MAXQNM = 100 )
 
C
C     Maximum total length of character tokens in a query:
C
      INTEGER               MAXCLN
      PARAMETER           ( MAXCLN = MAXQRY )
 
C
C     Maximum length of literal string values allowed in queries:
C     MAXSTR.
C
      INTEGER               MAXSTR
      PARAMETER           ( MAXSTR = 1024 )
 
C
C     End Include Section:  EK Query Limit Parameters
C
