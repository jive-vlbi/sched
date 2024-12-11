C*PGSLS -- set line style
C%void cpgsls(int ls);
C+
      SUBROUTINE PGSLS (LS)
      INTEGER  LS
C
C Set the line style attribute for subsequent plotting. This
C attribute affects line primitives only; it does not affect graph
C markers, text, or area fill.
C
C Ten different line styles are available; the first five are:
C 1 (full line), 2 (dashed), 3 (dot-dash-dot-dash), 4 (dotted),
C 5 (dash-dot-dot-dot). The default is 1 (normal full line).
C
C Argument:
C  LS     (input)  : the line-style code for subsequent plotting
C                    (in range 1-10).
C--
C  8-Aug-1985 - new routine, equivalent to GRSLS [TJP].
C  3-Jun-1984 - add GMFILE device [TJP].
C  9-Dec-1984 - update comments for 10 styles [TJP].
C-----------------------------------------------------------------------
      LOGICAL PGNOTO
C
      IF (PGNOTO('PGSLS')) RETURN
      CALL GRSLS(LS)
      END
