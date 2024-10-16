    IDENTIFICATION DIVISION.
    PROGRAM-ID. TESTTABLE.
    DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-SRCH PIC A(10) VALUE 'TUTORIALS'.
       01 WS-SRCH1 PIC A(10) VALUE 'TUTORIALS'.
       01 WS-TABLE.
          05 WS-A PIC A(10) VALUE 'TUTORIALS' OCCURS 5 TIMES INDEXED BY I J.
       01 WS-TABLE1.
          05 WS-A1 OCCURS 10 TIMES INDEXED BY I.
             10 WS-B1 PIC A(10).
             10 WS-C1 OCCURS 5 TIMES INDEXED BY J.
                15 WS-D1 PIC X(6) VALUE ALL 'A'.
    PROCEDURE DIVISION.
       IF TFT-DRW-TYP(1:1) = C-SA-DRW-BY-PSBK
       THEN
          MOVE '0801'               TO AIF-REFERENCE-TYPE
          MOVE TFT-PSBK-PRT-NO1     TO AIF-REFERENCE-NO
       END-IF.
       DISPLAY "ONE-D TABLE : "WS-A(1).
       DISPLAY "TWO-D TABLE : "WS-TABLE1.
       Finished.
*          MOVE '12ABCDEF34GHIJKL56MNOPQR' TO WS-TABLE1.
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
          DISPLAY 'WS-TABLE1(5) : ' WS-TABLE1(5).
           IF WS-A(1) = SPACES
           THEN
             MOVE 'EN005'              TO WS-A(2)
           END-IF.
           IF WS-A(1) = '999'
           THEN
             MOVE 'EN005'              TO WS-A(2)
           END-IF.
       TODO-SET.
          SET ADDRESS OF WS-SRCH     TO WS-SRCH1.
          SET I J TO 1 2.
          DISPLAY WS-C1(I,J).
          SET I J UP BY 1.
          DISPLAY WS-C1(I,J).
       TODO-SEARCH.
          SET I TO 1.
          SEARCH WS-A1
             AT END DISPLAY 'M NOT FOUND IN TABLE'
             WHEN WS-A1(I) = WS-SRCH
             DISPLAY 'LETTER M FOUND IN TABLE'
          END-SEARCH.
    STOP RUN.