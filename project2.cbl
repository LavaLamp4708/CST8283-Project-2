      ******************************************************************
      * Author: Peter Stainforth
      * Date: 2024-07-19
      * Purpose: Project 2
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROJECT-2.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT STOCKS-IN ASSIGN TO 'STOCKS.txt'
           ORGANIZATION IS LINE SEQUENTIAL.

           SELECT PORTFOLIO-IN ASSIGN TO 'PORTFOLIO.txt'
           ORGANIZATION IS LINE SEQUENTIAL.

           SELECT REPORT-OUT ASSIGN TO 'REPORT.txt'
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD STOCKS-IN.
       01 STOCK.
           05 S-STOCK-SYMBOL PIC X(7).
           05 STOCK-NAME PIC X(25).
           05 CLOSING-PRICE PIC 9(4)V99.

       FD PORTFOLIO-IN.
       01 PORTFOLIO.
           05 P-STOCK-SYMBOL PIC X(7).
           05 NUM-OF-SHARES PIC 9(5).
           05 AVG-COST PIC 9(4)V99.

       FD REPORT-OUT.
       01 REPORT-OUT-LINE PIC X(109).

       WORKING-STORAGE SECTION.
       01 WS-EOF PIC 9 VALUE 0.

       01 WS-STOCKS.
           05 WS-STOCKS-TABLE OCCURS 20 TIMES.
               10 WS-S-STOCK-SYMBOL            PIC X(7).
               10 WS-STOCK-NAME-S              PIC X(25).
               10 WS-CLOSING-PRICE-S           PIC 9(4)V99.

       01 WS-PORTFOLIO.
           05 WS-PORTFOLIO-TABLE OCCURS 7 TIMES.
               10 WS-P-STOCK-SYMBOL            PIC X(7).
               10 WS-NUM-OF-SHARES-P           PIC 9(5).
               10 WS-AVG-COST-P                PIC 9(4)V99.

       01 SUBS.
           05 STOCKS-SUB PIC 99.
           05 PORTFOLIO-SUB PIC 99.

           05 JOIN-SUB PIC 99 VALUE 0.
           05 NUM-OF-MATCHES PIC 99.
           05 INNER-JOIN-SUB-PAIR OCCURS 20 TIMES.
               10 JOIN-STOCKS-SUB PIC 99.
               10 JOIN-PORTFOLIO-SUB PIC 99.

       01 PORTFOLIO-RECORDS-READ PIC 99.

       01 REPORT-LINES.
           05 REPORT-LINE OCCURS 20 TIMES.
             10 RPT-STOCK-NAME-S     PIC X(25).
             10 FILL0                PIC XXX             VALUE SPACES.
             10 RPT-NUM-OF-SHARES-P  PIC Z(5).
             10 FILL1                PIC XXX             VALUE SPACES.
             10 RPT-AVG-COST-P       PIC $,$$$,$$9.99.
             10 FILL2                PIC XXX             VALUE SPACES.
             10 RPT-CLOSING-PRICE-S  PIC $,$$$,$$9.99.
             10 FILL3                PIC XXX             VALUE SPACES.
             10 RPT-ADJ-COST-BASE    PIC $,$$$,$$9.99.
             10 FILL4                PIC XXX             VALUE SPACES.
             10 RPT-MARKET-VALUE     PIC $,$$$,$$9.99.
             10 FILL5                PIC XXX             VALUE SPACES.
             10 RPT-TOTAL-GAIN-LOSS  PIC $,$$$,$$9.99-.

       01 WS-CALCS.
           05 CALC-ADJ-COST-BASE PIC 9(7)V99.
           05 CALC-MARKET-VALUE PIC 9(7)V99.
           05 CALC-TOTAL-GAIN-LOSS PIC S9(7)V99.

       01 SPACER PIC X(109) VALUE ALL "=".
       01 HEADER.
           05 FIELD0 PIC X(25) VALUE
               "STOCK NAME               ".
           05 FIELD1 PIC X(8) VALUE
               " #SHARES".
           05 FIELD2 PIC X(15) VALUE
               "      UNIT COST".
           05 FIELD3 PIC X(15) VALUE
               "     AT CLOSING".
           05 FIELD4 PIC X(15) VALUE
               "      COST BASE".
           05 FIELD5 PIC X(15) VALUE
               "   MARKET VALUE".
           05 FIELD6 PIC X(16) VALUE
               "      GAIN/LOSS ".
       01 RECORDS-READ-WRITTEN-LINE.
           05 RECORDS-READ PIC X(25) VALUE "Portfolio records read: ".
           05 PORTFOLIO-RECORDS-VALUE PIC ZZ.
           05 RECORDS-WRITTEN PIC X(20) VALUE "   Records written: ".
           05 RECORDS-VALUE PIC ZZ.

       PROCEDURE DIVISION.
      * Main procedure. Performs all paragraphs.
       100-MAIN-PROCEDURE.
           PERFORM 210-OPEN-FILES.

           PERFORM 220-LOAD-WS.

           PERFORM 230-INNER-JOIN
           VARYING PORTFOLIO-SUB
           FROM 1 BY 1 UNTIL PORTFOLIO-SUB > 7
           AFTER STOCKS-SUB
           FROM 1 BY 1 UNTIL STOCKS-SUB > 20.

           PERFORM 240-FILL-REPORT-LINE
           VARYING JOIN-SUB FROM 1 BY 1
           UNTIL JOIN-SUB > NUM-OF-MATCHES
           OR JOIN-SUB > 20.

           PERFORM 250-WRITE-TO-FILE.

           PERFORM 260-CLOSE-REPORT.

           PERFORM 270-OPEN-REPORT-AS-INPUT.

           INITIALIZE WS-EOF.
           PERFORM 280-DISPLAY-REPORT UNTIL WS-EOF=1.

           PERFORM 290-CLOSE-ROUTINE.

      *Opens all necessary files.
       210-OPEN-FILES.
           OPEN INPUT STOCKS-IN.
           OPEN INPUT PORTFOLIO-IN.
           OPEN OUTPUT REPORT-OUT.

      *Loads tables in the working-storage section from all input files.
       220-LOAD-WS.
           INITIALIZE WS-EOF.

           PERFORM 310-LOAD-STOCKS
           VARYING STOCKS-SUB
           FROM 1 BY 1
           UNTIL STOCKS-SUB > 20 OR WS-EOF = 1.

           INITIALIZE WS-EOF.

           PERFORM 320-LOAD-PORTFOLIO
           VARYING PORTFOLIO-SUB
           FROM 1 BY 1
           UNTIL PORTFOLIO-SUB > 7 OR WS-EOF = 1.

      *Loads table in the working-storage section from the STOCKS.txt file.
       310-LOAD-STOCKS.
           READ STOCKS-IN
           AT END MOVE 1 TO WS-EOF
           NOT AT END
               MOVE STOCK TO WS-STOCKS-TABLE(STOCKS-SUB)
           END-READ.

      *Loads table in the working-storage section from the PORTFOLIO.txt file.
       320-LOAD-PORTFOLIO.
           READ PORTFOLIO-IN
           AT END MOVE 1 TO WS-EOF
           NOT AT END
               ADD 1 TO PORTFOLIO-RECORDS-READ
               MOVE PORTFOLIO TO WS-PORTFOLIO-TABLE(PORTFOLIO-SUB)
           END-READ.

      *Loads a table pairing the subs of both input file tables where the stock symbols match.
       230-INNER-JOIN.
           IF WS-S-STOCK-SYMBOL(STOCKS-SUB)
               = WS-P-STOCK-SYMBOL(PORTFOLIO-SUB)
               ADD 1 TO NUM-OF-MATCHES
               MOVE STOCKS-SUB TO JOIN-STOCKS-SUB(NUM-OF-MATCHES)
               MOVE PORTFOLIO-SUB TO JOIN-PORTFOLIO-SUB(NUM-OF-MATCHES)
           END-IF.

      *Fills report table with all necessary fields
       240-FILL-REPORT-LINE.
           MOVE WS-STOCK-NAME-S(JOIN-STOCKS-SUB(JOIN-SUB))
           TO RPT-STOCK-NAME-S(JOIN-SUB).

           MOVE WS-NUM-OF-SHARES-P(JOIN-PORTFOLIO-SUB(JOIN-SUB))
           TO RPT-NUM-OF-SHARES-P(JOIN-SUB).

           MOVE WS-AVG-COST-P(JOIN-PORTFOLIO-SUB(JOIN-SUB))
           TO RPT-AVG-COST-P(JOIN-SUB).

           MOVE WS-CLOSING-PRICE-S(JOIN-STOCKS-SUB(JOIN-SUB))
           TO RPT-CLOSING-PRICE-S(JOIN-SUB).

           PERFORM 330-CALCULATE-ADJ-COST-BASE.
           PERFORM 340-CALCULATE-MARKET-VALUE.
           PERFORM 350-CALCULATE-TOTAL-GAIN-LOSS.

      *Calculates adjusted cost base column for the stock report.
       330-CALCULATE-ADJ-COST-BASE.
           MULTIPLY WS-AVG-COST-P(JOIN-PORTFOLIO-SUB(JOIN-SUB))
           BY WS-NUM-OF-SHARES-P(JOIN-PORTFOLIO-SUB(JOIN-SUB))
           GIVING CALC-ADJ-COST-BASE.
           MOVE CALC-ADJ-COST-BASE TO RPT-ADJ-COST-BASE(JOIN-SUB).

      *Calculates market value column for the stock report.
       340-CALCULATE-MARKET-VALUE.
           MULTIPLY WS-NUM-OF-SHARES-P(JOIN-PORTFOLIO-SUB(JOIN-SUB))
           BY WS-CLOSING-PRICE-S(JOIN-STOCKS-SUB(JOIN-SUB))
           GIVING CALC-MARKET-VALUE.
           MOVE CALC-MARKET-VALUE TO RPT-MARKET-VALUE(JOIN-SUB).

      *Calculates total gain or loss of the stock for the column in the report.
       350-CALCULATE-TOTAL-GAIN-LOSS.
           SUBTRACT CALC-ADJ-COST-BASE FROM CALC-MARKET-VALUE
           GIVING CALC-TOTAL-GAIN-LOSS.
           MOVE CALC-TOTAL-GAIN-LOSS TO RPT-TOTAL-GAIN-LOSS(JOIN-SUB).

      *Writes REPORT.txt to file.
       250-WRITE-TO-FILE.
           PERFORM 360-WRITE-HEADER-TO-FILE.

           PERFORM 370-WRITE-TABLE-DATA-TO-FILE
           VARYING JOIN-SUB
           FROM 1 BY 1 UNTIL JOIN-SUB > 20
           OR JOIN-SUB > NUM-OF-MATCHES.

           PERFORM 380-WRITE-FOOTER-TO-FILE.

      *Writes header to the REPORT.txt file.
       360-WRITE-HEADER-TO-FILE.
           WRITE REPORT-OUT-LINE FROM SPACER.
           WRITE REPORT-OUT-LINE FROM HEADER.
           WRITE REPORT-OUT-LINE FROM SPACER.

      *Writes table rows to the REPORT.txt file.
       370-WRITE-TABLE-DATA-TO-FILE.
           WRITE REPORT-OUT-LINE FROM REPORT-LINE(JOIN-SUB).

      *Writes footer to the REPORT.txt file.
       380-WRITE-FOOTER-TO-FILE.
           WRITE REPORT-OUT-LINE FROM SPACER
           BEFORE ADVANCING 2 LINE.

           MOVE NUM-OF-MATCHES TO RECORDS-VALUE.
           MOVE PORTFOLIO-RECORDS-READ TO PORTFOLIO-RECORDS-VALUE.
           WRITE REPORT-OUT-LINE FROM RECORDS-READ-WRITTEN-LINE.

      *Closes the REPORT.txt file so it may be opened in input mode.
       260-CLOSE-REPORT.
           CLOSE REPORT-OUT.

      *Opens the REPORT.txt file in input mode.
       270-OPEN-REPORT-AS-INPUT.
           OPEN INPUT REPORT-OUT.

      *Displays all data in the report file to console.
       280-DISPLAY-REPORT.
           READ REPORT-OUT
           AT END MOVE 1 TO WS-EOF
           NOT AT END DISPLAY REPORT-OUT-LINE
           END-READ.

      *Closes all files and stops the run.
       290-CLOSE-ROUTINE.
           CLOSE STOCKS-IN.
           CLOSE PORTFOLIO-IN.
           CLOSE REPORT-OUT.
           STOP RUN.

       END PROGRAM PROJECT-2.

      *DEBUG.
      *    PERFORM VARYING STOCKS-SUB FROM 1 BY 1 UNTIL STOCKS-SUB > 20
      *        DISPLAY WS-STOCKS-TABLE(STOCKS-SUB)
      *    END-PERFORM.

      *    DISPLAY SPACES.

      *    PERFORM VARYING PORTFOLIO-SUB
      *    FROM 1 BY 1 UNTIL PORTFOLIO-SUB > 7
      *        DISPLAY WS-PORTFOLIO-TABLE(PORTFOLIO-SUB)
      *    END-PERFORM.

      *    INITIALIZE JOIN-SUB.

      *    DISPLAY SPACES.
      *    DISPLAY "PAIRS FOUND:".
      *    DISPLAY "P  S".
      *    PERFORM VARYING JOIN-SUB
      *    FROM 1 BY 1 UNTIL JOIN-SUB > 20
      *    OR JOIN-SUB > NUM-OF-MATCHES
      *        DISPLAY JOIN-PORTFOLIO-SUB(JOIN-SUB) " "
      *        JOIN-STOCKS-SUB(JOIN-SUB)
      *    END-PERFORM.

      *    DISPLAY SPACES.
      *    DISPLAY HEADER.
      *    DISPLAY SPACER.
      *    PERFORM VARYING JOIN-SUB
      *    FROM 1 BY 1 UNTIL JOIN-SUB > 20
      *    OR JOIN-SUB > NUM-OF-MATCHES
      *        DISPLAY REPORT-LINE(JOIN-SUB)
      *    END-PERFORM.

      *    DISPLAY SPACES.
