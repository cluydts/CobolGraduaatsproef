       IDENTIFICATION DIVISION.
           PROGRAM-ID. SalaryCalculation.
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
       
            FILE SECTION.
            FD input-file.
            01 input-record.
              05 naam PIC X(30).
              05 type-werknemer PIC X(8).
              05 brutoloon-in PIC 9(5)V99.
    
            FD output-file.
            01 output-record.
              05 naam-out PIC X(30).
              05 brutoloon-out PIC 9(5)V99.
              05 voorheffing-out PIC 9(5)V99.
              05 netto-out PIC 9(5)V99.
              05 rsz-out PIC 9(5)V99.
           01 EOF-FLAG PIC X(1) VALUE "0".

       WORKING-STORAGE SECTION.

           01 DYNAMIC-INFILE pic x(30).
           01 DYNAMIC-OUTFILE pic x(30).
           01 OUTPUT-PREFIX pic x(8) value "Output-".
   
           01 brutoloon PIC 9(5)V99.
           01 brutoloon-Arbeider PIC 9(5)V99.
           01 RSZ PIC 9(5)V99.
           01 Voorheffing PIC 9(5)V99.
           01 NettoLoon PIC 9(5)V99.
          
    
       PROCEDURE DIVISION using LINK-INPUT-FILE.

              MOVE LINK-INPUT-FILE TO DYNAMIC-INFILE.
              string
               OUTPUT-PREFIX delimited by size
               DYNAMIC-INFILE delimited by size
               into DYNAMIC-OUTFILE
              end-string

              OPEN INPUT input-file
                OPEN OUTPUT output-file
                READ input-file INTO input-record
                
                PERFORM UNTIL EOF-FLAG = "1"
                    READ input-file INTO input-record
                        AT END
                            MOVE "1" TO EOF-FLAG
                        NOT AT END
                            MOVE brutoloon-in TO Brutoloon
                    

                    IF type-werknemer = "Bediende"
                        COMPUTE RSZ = Brutoloon * 0.1307
                    ELSE IF type-werknemer = "Arbeider"
                           compute brutoloon-Arbeider = brutoloon * 1.08
                        COMPUTE RSZ = brutoloon-Arbeider * 0.1307
                    END-IF.


                   if  brutoloon-in <= 1318.33
                    compute Voorheffing = brutoloon-in * 0.15
                   else if brutoloon-in <= 2326.66
                    compute Voorheffing = brutoloon-in * 0.25
                   else if brutoloon-in <= 4026.66
                    compute voorheffing = brutoloon-in * 0.45
                    else 
                    compute voorheffing = brutoloon-in * 0.50
                   end-if.

                   COMPUTE NettoLoon = Brutoloon - RSZ - Voorheffing.

                   MOVE Naam TO naam-out
                   MOVE Brutoloon TO brutoloon-out
                   MOVE RSZ TO rsz-out
                   MOVE Voorheffing TO voorheffing-out
                   MOVE NettoLoon TO netto-out

                   WRITE output-record
                   
                END-READ
              END-PERFORM


                CLOSE input-file
                CLOSE output-file
                DISPLAY "Salarisberekening voltooid."

           goback.
