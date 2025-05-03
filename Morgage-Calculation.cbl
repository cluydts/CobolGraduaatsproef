       IDENTIFICATION DIVISION.
       PROGRAM-ID. RENTE-BEREKENING.
       environment division.
       input-output section.
           file-control.
               select input-file assign to DYNAMIC-INFILE
                   organization is line sequential.
               select output-file assign to DYNAMIC-OUTFILE
                   organization is line sequential.


       DATA DIVISION.
       linkage section.
              01 LINK-INPUT-FILE pic x(30).

       file section.
           FD input-file.
            01 Leesregel pic x(20).

           FD output-file.
           01 Output-Regel PIC X(30).
           01 Header PIC X(21) value "Capital,Rate,Interest".
           01  INTEREST-F PIC Z(8).99.

              01  EOF-Flag PIC X(1) VALUE "0".

              
       WORKING-STORAGE SECTION.
          
              01 DYNAMIC-INFILE pic x(30).
              01 DYNAMIC-OUTFILE pic x(30).
              01 OUTPUT-PREFIX pic x(8) value "Output-".

           01  TOTAL-AMOUNT-S PIC 9(8)V99.
           01  CAPITAL-S PIC x(10).
           01  RATE-S PIC x(4).
           01  Years-S PIC x(2).

           01  DISPLAY-CAPITAL-S PIC Z(9).ZZ.
           01  DISPLAY-RATE-S PIC Z(2).ZZ.
           01  DISPLAY-Years-S PIC Z(2).
           01 DISPLAY-INTEREST-S PIC Z(8).ZZ.
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
             display "Leesregel: " Leesregel
               UNSTRING Leesregel DELIMITED BY ","
                   INTO CAPITAL-S RATE-S Years-S
               END-UNSTRING
                DISPLAY "Capital: " CAPITAL-S
                DISPLAY "Rate: " RATE-S
                DISPLAY "Years: " Years-S
                display " "
    
           MOVE function numval-c(function trim(CAPITAL-S)) TO CAPITAL-N
           MOVE function numval-c(function trim(RATE-S)) TO RATE-N
           MOVE function numval-c(function trim(Years-S)) TO Years-N
                   
              DISPLAY "Capital-N: " CAPITAL-N
                DISPLAY "Rate-N: " RATE-N
                DISPLAY "Years-N: " Years-N
                DISPLAY " "


               

               COMPUTE INTEREST = (CAPITAL-N * RATE-N * Years-N)

               compute INTEREST = INTEREST /100
               COMPUTE TOTAL-AMOUNT-S = CAPITAL-N + INTEREST
                   
                DISPLAY "INTEREST: " INTEREST

                   move CAPITAL-S to DISPLAY-CAPITAL-S
                   move RATE-S to DISPLAY-RATE-S
                   move Years-S to DISPLAY-Years-S

                   move INTEREST to DISPLAY-INTEREST-S 
                DISPLAY "INTEREST-S: " DISPLAY-INTEREST-S
                display " "

                string
                     function trim(DISPLAY-CAPITAL-S) delimited by size
                        "," delimited by size
                       function trim(DISPLAY-RATE-S) delimited by size
                        "," delimited by size
                        function trim(DISPLAY-Years-S) delimited by size
                        "," delimited by size
                       function trim(DISPLAY-INTEREST-S) delimited by size
                       into Output-Regel
                       display "Output-Regel: " Output-Regel
                        display "---------------------------"

                       write Output-Regel
           END-READ
           END-PERFORM.

           close input-file.
           close output-file.
           
           STOP RUN.
