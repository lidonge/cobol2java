    IDENTIFICATION DIVISION.
    PROGRAM-ID. HELLO.
    DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-NUM1 PIC 9(9) VALUE 10 .
       01
        WS-NUM2 PIC 9(9) VALUE 10.
       01 ACCOUNT.
           05 WS-NUM3 PIC 9(9) VALUE 10.
           05 WS-NUM4 PIC 9(9) VALUE 10.
           05 WS-NUMA PIC 9(9) VALUE 10.
           05 WS-NUMB PIC 9(9) VALUE 10.
           05 WS-NUMC PIC 9(9) VALUE 10.
           05 WS-NUMD PIC 9(9) VALUE 10.
           05 WS-NUME PIC 9(9) VALUE 10.
       01 WS-ARRAYS.
           05 WS-NUMBER-ARRAY.
               10 WS-NUMBER-ITEM OCCURS 10 TIMES PIC 9(3) VALUE 0.
    PROCEDURE DIVISION.
       ADD 123.12 TO WS-NUM3 WS-NUM4.
       ADD WS-NUM1 WS-NUM2 TO WS-NUM3 WS-NUM4.
       ADD WS-NUMBER-ITEM(5) WS-NUM2 TO WS-NUM3 WS-NUM4.
       ADD WS-NUMA WS-NUMB WS-NUMC TO WS-NUMD GIVING WS-NUME.
       DISPLAY "WS-NUM1     : " WS-NUM1
       DISPLAY "WS-NUM2     : " WS-NUM2
       DISPLAY "WS-NUM3     : " WS-NUM3
       DISPLAY "WS-NUM4     : " WS-NUM4
       DISPLAY "WS-NUMA     : " WS-NUMA
       DISPLAY "WS-NUMB     : " WS-NUMB
       DISPLAY "WS-NUMC     : " WS-NUMC
       DISPLAY "WS-NUMD     : " WS-NUMD
       DISPLAY "WS-NUME     : " WS-NUME
    STOP RUN.