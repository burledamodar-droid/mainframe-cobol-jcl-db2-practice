       ID DIVISION.
       PROGRAM-ID. MATCHLP1.
       AUTHOR. ANIL POLSANI.
       DATE-WRITTEN. TODAY.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT TRAN-INFILE ASSIGN TO TRANSDD1
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-TF-STATUS.

           SELECT MAST-INFILE ASSIGN TO MASTDD01
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-MF-STATUS.

           SELECT BILL-OTFILE ASSIGN TO BILLDD01
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-BF-STATUS.

       DATA DIVISION.
       FILE SECTION.

       FD TRAN-INFILE
           LABEL RECORD ARE STANDARD.
       COPY TRANCPY2.

       FD MAST-INFILE
           LABEL RECORD ARE STANDARD.
       COPY MASTCPY1.

       FD BILL-OTFILE
           LABEL RECORD ARE STANDARD.
       COPY BILLCPY1.

       WORKING-STORAGE SECTION.

       01 WS-TF-STATUS       PIC X(02) VALUE SPACE.
       01 WS-MF-STATUS       PIC X(02) VALUE SPACE.
       01 WS-BF-STATUS       PIC X(02) VALUE SPACE.

       01 WS-END-OF-TFILE    PIC X(01) VALUE SPACE.
       01 WS-END-OF-MFILE    PIC X(01) VALUE SPACE.

       01 WS-IN-REC-CNT      PIC 9(02) VALUE ZERO.
       01 WS-OT-REC-CNT      PIC 9(02) VALUE ZERO.
       01 WS-ABENDPGM        PIC X(08) VALUE 'ABENDPGM'.

       01 WS-TOTL-DUE-2-PAY  PIC S9(05)V9(02) VALUE ZERO.
       01 WS-PREV-DUE-AMT    PIC S9(05)V9(02) VALUE ZERO.
       01 WS-DUE-INST        PIC S9(05)V9(02) VALUE ZERO.
       01 WS-MIN-DUE-AMT     PIC S9(05)V9(02) VALUE ZERO.

       01 WS-CURR-DATE-ALPNUM PIC X(08) VALUE SPACE.
       01 WS-CURR-DATE-NUM    PIC 9(08) VALUE ZERO.
       01 WS-DUE-DATE         PIC 9(08) VALUE ZERO.
       01 WS-DUE-DATE-ALPNUM  PIC X(08) VALUE SPACE.
       01 WS-NUM-OF-DAYS      PIC 9(08) VALUE ZERO.

       PROCEDURE DIVISION.

       000-MAIN-PARA.

           DISPLAY 'MATCHLP1 STARTED'.

           PERFORM 100-INITIAL-PARA THRU 100-EXIT
           PERFORM 200-GET-INPUT-PARA THRU 200-EXIT
           PERFORM 300-PROCESS-PARA THRU 300-EXIT
               UNTIL WS-END-OF-TFILE = 'Y'.

           CLOSE TRAN-INFILE
                 MAST-INFILE
                 BILL-OTFILE.

           DISPLAY 'TOTAL INPUT RECORD COUNT ' WS-IN-REC-CNT.
           DISPLAY 'TOTAL OUTPUT RECORD COUNT ' WS-OT-REC-CNT.

           STOP RUN.

       100-INITIAL-PARA.

           MOVE 'N' TO WS-END-OF-TFILE.
           MOVE 'N' TO WS-END-OF-MFILE.

           MOVE ZERO TO WS-IN-REC-CNT
                         WS-OT-REC-CNT.

           OPEN INPUT TRAN-INFILE
                       MAST-INFILE.

           OPEN OUTPUT BILL-OTFILE.

           IF WS-MF-STATUS NOT = '00'
              OR WS-TF-STATUS NOT = '00'
              OR WS-BF-STATUS NOT = '00'

               DISPLAY 'ERROR IN 100-PARA'
               DISPLAY 'STATUS CODE TRAN FILE IS ' WS-TF-STATUS
               DISPLAY 'STATUS CODE MAST FILE IS ' WS-MF-STATUS
               DISPLAY 'STATUS CODE BILL FILE IS ' WS-BF-STATUS

               CALL WS-ABENDPGM

           END-IF.

           INITIALIZE TRANS-LAYOUT
                      MASTER-LAYOUT
                      BILL-LAYOUT.

       100-EXIT.
           EXIT.

       200-GET-INPUT-PARA.

           PERFORM 210-GET-TRAN-FILE-PARA THRU 210-EXIT
           PERFORM 220-GET-MAST-FILE-PARA THRU 220-EXIT.

       200-EXIT.
           EXIT.

       210-GET-TRAN-FILE-PARA.

           READ TRAN-INFILE
               AT END
                   MOVE 'Y' TO WS-END-OF-TFILE
                   GO TO 210-EXIT.

           ADD +1 TO WS-IN-REC-CNT.

       210-EXIT.
           EXIT.

       220-GET-MAST-FILE-PARA.

           READ MAST-INFILE
               AT END
                   MOVE 'Y' TO WS-END-OF-MFILE
                   GO TO 220-EXIT.

       220-EXIT.
           EXIT.

       300-PROCESS-PARA.

           IF CARD-NUMBER OF TRANS-LAYOUT =
              CARD-NUMBER OF MASTER-LAYOUT

               PERFORM 310-CAL-TOT-DUE-PARA THRU 310-EXIT
               PERFORM 320-CAL-MIN-DUE-PARA THRU 320-EXIT
               PERFORM 330-GET-DUE-DATE-PARA THRU 330-EXIT
               PERFORM 340-MOVE-WRITE-PARA THRU 340-EXIT
               PERFORM 200-GET-INPUT-PARA THRU 200-EXIT

           ELSE

               IF CARD-NUMBER OF TRANS-LAYOUT >
                  CARD-NUMBER OF MASTER-LAYOUT

                   PERFORM 220-GET-MAST-FILE-PARA THRU 220-EXIT

               ELSE

                   DISPLAY 'INVALID CARD-NUMBER IN TRANS-FILE'
                   DISPLAY 'NO RECORD IN MASTER FILE'
                   DISPLAY 'CARD NUMBER IS '
                       CARD-NUMBER OF TRANS-LAYOUT

                   CALL WS-ABENDPGM

               END-IF

           END-IF.

       300-EXIT.
           EXIT.

       310-CAL-TOT-DUE-PARA.

           MOVE ZERO TO WS-PREV-DUE-AMT
                         WS-DUE-INST
                         WS-TOTL-DUE-2-PAY.

           COMPUTE WS-PREV-DUE-AMT =
                   PREVIOUS-DUE - PREVIOUS-DUE-PAID.

           IF WS-PREV-DUE-AMT > 0
               COMPUTE WS-DUE-INST =
                       WS-PREV-DUE-AMT * .15
           END-IF.

           COMPUTE WS-TOTL-DUE-2-PAY =
                   WS-PREV-DUE-AMT
                 + WS-DUE-INST
                 + DUE-AMT-TO-PAY.

       310-EXIT.
           EXIT.

       320-CAL-MIN-DUE-PARA.

           MOVE ZERO TO WS-MIN-DUE-AMT.

           COMPUTE WS-MIN-DUE-AMT =
                   WS-TOTL-DUE-2-PAY * .15.

       320-EXIT.
           EXIT.

       330-GET-DUE-DATE-PARA.

           MOVE FUNCTION CURRENT-DATE
             TO WS-CURR-DATE-ALPNUM.

           MOVE WS-CURR-DATE-ALPNUM
             TO WS-CURR-DATE-NUM.

           COMPUTE WS-NUM-OF-DAYS =
                   FUNCTION INTEGER-OF-DATE(WS-CURR-DATE-NUM).

           COMPUTE WS-NUM-OF-DAYS =
                   WS-NUM-OF-DAYS + 20.

           COMPUTE WS-DUE-DATE =
                   FUNCTION DATE-OF-INTEGER(WS-NUM-OF-DAYS).

           MOVE WS-DUE-DATE
             TO WS-DUE-DATE-ALPNUM.

       330-EXIT.
           EXIT.

       340-MOVE-WRITE-PARA.

           MOVE CARD-NUMBER OF MASTER-LAYOUT
             TO CARD-NUMBER OF BILL-LAYOUT.

           MOVE CUST-NAME OF MASTER-LAYOUT
             TO CUST-NAME OF BILL-LAYOUT.

           MOVE WS-TOTL-DUE-2-PAY
             TO TOT-DUE-2-PAY.

           MOVE WS-MIN-DUE-AMT
             TO MIN-DUE-2-PAY.

           MOVE WS-CURR-DATE-ALPNUM
             TO BILLING-DATE.

           MOVE WS-DUE-DATE-ALPNUM
             TO DUE-DATE.

           WRITE BILL-LAYOUT.

           IF WS-BF-STATUS NOT = '00'

               DISPLAY 'ERROR IN 340-PARA'
               DISPLAY 'WRITE ERROR STATUS CODE IS ' WS-BF-STATUS
               DISPLAY 'RECORD IS '
                   CARD-NUMBER OF MASTER-LAYOUT

               CALL WS-ABENDPGM

           END-IF.

           ADD +1 TO WS-OT-REC-CNT.

       340-EXIT.
           EXIT.
