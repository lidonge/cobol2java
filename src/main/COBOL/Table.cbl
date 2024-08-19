    IDENTIFICATION DIVISION.
    PROGRAM-ID. HELLO.
    DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-TABLE.
          05 WS-A PIC A(10) VALUE 'TUTORIALS' OCCURS 5 TIMES.
       01 WS-TABLE1.
          05 WS-A1 OCCURS 10 TIMES.
             10 WS-B1 PIC A(10).
             10 WS-C1 OCCURS 5 TIMES.
                15 WS-D1 PIC X(6).
    PROCEDURE DIVISION.
       DISPLAY "ONE-D TABLE : "WS-A(1).
       DISPLAY "TWO-D TABLE : "WS-TABLE1.
          MOVE '12ABCDEF34GHIJKL56MNOPQR' TO WS-TABLE1.
          DISPLAY 'WS-A(1)   : ' WS-A1(1).
          DISPLAY 'WS-C(1,1) : ' WS-C1(1,1).
          DISPLAY 'WS-B(6)   : ' WS-B1(6).
          DISPLAY 'WS-C(1,2) : ' WS-C1(1,2).
          DISPLAY 'WS-A(2)   : ' WS-A1(2).
          DISPLAY 'WS-C(2,1) : ' WS-D1(2,1).
          DISPLAY 'WS-C(2,2) : ' WS-C1(2,2).
          DISPLAY 'WS-A(3)   : ' WS-A1(3).
          DISPLAY 'WS-C(3,1) : ' WS-C1(3,1).
          DISPLAY 'WS-C(3,2) : ' WS-C1(3,2).
    STOP RUN.