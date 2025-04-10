       IDENTIFICATION DIVISION.
       PROGRAM-ID. RENTE-BEREKENING.
       environment division.
       input-output section.
           file-control.
               select input-file assign to "Inputfile.csv"
                   organization is line sequential.
               select output-file assign to "Outputfile.csv"
                   organization is line sequential.


       DATA DIVISION.
       file section.
           FD input-file.
            01 Leesregel pic x(80).

           FD output-file.
           01 Output-Regel PIC X(80).
           01 Header PIC X(21) value "Capital,Rate,Interest".
           01  INTEREST-F PIC Z(8).99.

              01  EOF-Flag PIC X(1) VALUE "0".

              
       WORKING-STORAGE SECTION.
          
           01  TOTAL-AMOUNT-S PIC 9(8)V99.
           01  CAPITAL-S PIC x(10).
           01  RATE-S PIC x(4).
           01  Years-S PIC x(2).
           01  INTEREST PIC 9(8)V99.
           01  CAPITAL-N PIC 9(8)V99.
           01  RATE-N PIC 9(2)V99.
           01  Years-N PIC 9(2).

       PROCEDURE DIVISION.

           OPEN INPUT input-file
           OPEN OUTPUT output-file
           READ input-file INTO Leesregel

           PERFORM UNTIL EOF-Flag = 1
              read input-file INTO Leesregel
           AT END
               MOVE 1 to EOF-Flag
           NOT AT END
               UNSTRING Leesregel DELIMITED BY ","
                   INTO CAPITAL-S RATE-S Years-S
               END-UNSTRING
    
               MOVE function numval(CAPITAL-S) TO CAPITAL-N
               MOVE function numval(RATE-S) TO RATE-N
               MOVE function numval(Years-S) TO Years-N
               
               COMPUTE INTEREST = (CAPITAL-N * RATE-N * Years-N) / 100
               COMPUTE TOTAL-AMOUNT-S = CAPITAL-N + INTEREST

               move INTEREST to INTEREST-F

                string
                      CAPITAL-S delimited by size
                        "," delimited by size
                        RATE-S delimited by size
                        "," delimited by size
                        Years-S delimited by size
                        "," delimited by size
                        INTEREST-F delimited by size
                       into Output-Regel

                       write Output-Regel
           END-READ
           END-PERFORM.

           close input-file.
           close output-file.
           
           STOP RUN.
