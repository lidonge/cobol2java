       IDENTIFICATION DIVISION.
       PROGRAM-ID. TESTCOMPUTE.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  Price1        PIC 9(4)V99 VALUE 15.50.
       01  Price2        PIC 9(4)V99 VALUE 20.75.
       01  Quantity1     PIC 9(3)    VALUE 10.
       01  Quantity2     PIC 9(3)    VALUE 5.
       01  Total1        PIC 9(5)V99.
       01  Total2        PIC 9(5)V99.
       01  WS-Score1        PIC 9(3)    VALUE 85.
       01  Score2        PIC 9(3)    VALUE 90.
       01  Score3        PIC 9(3)    VALUE 95.
       01  Average       PIC 9(3)V9(2).
       01  TaxRate       PIC V9(4)   VALUE 0.075.
       01  RoundedTotal  PIC 9(5)V99.
       01  LargeResult   PIC 9(7).
       01  WS-A             PIC 9(4)    VALUE 1000.
       01  WS-B             PIC 9(4)    VALUE 1000.
       01  WS-Cls.
           05 WS-C             PIC 9(4)    VALUE 1000.
       01 WS-ARRAYS.
           05 WS-NUMBER-ARRAY.
               10 WS-NUMBER-ITEM OCCURS 10 TIMES PIC 9(3) VALUE 0.
       01 WS-TABLE1.
          05 WS-A1 OCCURS 10 TIMES.
             10 WS-B1 PIC A(10).
             10 WS-C1 OCCURS 5 TIMES.
                15 WS-D1 PIC X(6).
       PROCEDURE DIVISION.
           COMPUTE Average = WS-C OF WS-Cls.
           COMPUTE Average = (WS-Score1 + Score2 + Score3) / 3.
           COMPUTE Total1 =  FUNCTION MOD(Price1, 5)+WS-D1(2, 1).
           COMPUTE Total1 = LENGTH OF FUNCTION MOD(Price1 5) + 3+WS-D1(2, 1).
           COMPUTE Total1 = 3+WS-NUMBER-ITEM(4) * Quantity1 - 6.
           COMPUTE Total1 = 3+Price1 * Quantity1 - 6.
           COMPUTE Total1 WS-Score1 = 3 +(5 - Price1) ** 6 + 3**6.
           COMPUTE RoundedTotal ROUNDED = Total1 * TaxRate .
           COMPUTE LargeResult = WS-A * WS-C OF WS-Cls ON SIZE ERROR DISPLAY "Error: Size exceeded".
*           COMPUTE Total1 = Price1 * Quantity1, Total2 = Price2 * Quantity2.

           STOP RUN.