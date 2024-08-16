       IDENTIFICATION DIVISION.
       PROGRAM-ID. DivideExample.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  A       PIC 9(2) VALUE 10.
       01  B       PIC 9(2) VALUE 20.
       01  C       PIC 9(2).
       01  D       PIC 9(2).
       01  X       PIC 9(2).
       01  Y       PIC 9(2).
       01  REM     PIC 9(2).

       PROCEDURE DIVISION.
           DIVIDE A INTO B REMAINDER REM.
           DIVIDE A INTO 10 GIVING C.
           DIVIDE 3 INTO B GIVING C REMAINDER REM.
           DIVIDE B BY A GIVING D.
           DIVIDE A INTO B GIVING X Y.
           DIVIDE 3 INTO B GIVING C ROUNDED.
           STOP RUN.