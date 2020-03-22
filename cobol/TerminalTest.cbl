      * ************************************************************** *
       Identification Division.
       Program-ID. disp1.

      * ************************************************************** *
       Environment Division.

      * ************************************************************** *
       Data Division.
       Working-Storage Section.
       01 TermFld   Pic X.
          88  TermNow   Value "T".

       Screen Section.
       01  ScrName
           Background-color 3
           Foreground-color 7
           Highlight.
           05   Column 10
                Line   3
                Value "Enter 'T' to terminate this test".
           05   Column Plus 2
                        Background-color 7
                        Foreground-color 4
                    Pic X
                    Using TermFld.

      * ************************************************************** *
       Procedure Division.
       Mainline.
           Perform until TermNow
               Display ScrName
               Accept ScrName
           End-Perform
           GoBack.
      *>  End Program disp1.
