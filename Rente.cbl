       IDENTIFICATION DIVISION.
       PROGRAM-ID. RENTE-BEREKENING.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
           01  CAPITAL        PIC 9(8)V99.
           01  RATE             PIC 9(2)V99.
           01  Years             PIC 9(2).
           01  INTEREST         PIC 9(8)V99.
           01  TOTAL-AMOUNT     PIC 9(8)V99.
           01  TOTAL-AMOUNT-DISPLAY PIC ZZZ,ZZZ,ZZ9.99.

       PROCEDURE DIVISION.
           DISPLAY "Voer het startkapitaal in: ".
           ACCEPT CAPITAL.
           DISPLAY "Voer de rentevoet in (bijv. 5.5): ".
           ACCEPT RATE.
           DISPLAY "Voer de looptijd in jaren in: ".
           ACCEPT Years.

           COMPUTE INTEREST = (CAPITAL * RATE * Years) / 100.
           COMPUTE TOTAL-AMOUNT = CAPITAL + INTEREST.
           move TOTAL-AMOUNT to TOTAL-AMOUNT-DISPLAY.

           DISPLAY "Totale bedrag na " Years " jaar: " TOTAL-AMOUNT-DISPLAY.

           STOP RUN.
