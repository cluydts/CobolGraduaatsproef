           identification division.
           program-id. NcursesDemo.

           environment division.
           

           data division.
           working-storage section.
           01 UserInput PIC X(20).

           procedure division.
           DISPLAY "Welkom bij de UI!" AT 0101.
           DISPLAY "Geef een naam in: " AT 0201.
           ACCEPT UserInput AT 0220.
           DISPLAY "Hallo, " at 0320 UserInput AT 0308.
           STOP RUN.
