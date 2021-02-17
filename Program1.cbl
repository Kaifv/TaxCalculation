       identification division.
       program-id. MidtermPractical1.
       author. Kaifkhan Vakil.
       date-written. 17th Feb 2020.
      *Program Description:
      *THis program will output the data from the input file in the 
      *desired output which will be showing the amount of tax a person 
      *has paid according to their province. This will use a fair amount
      *of logic structure as it deals with lots of ifs.                 
       environment division.
       input-output section.
       file-control.
      *This is the input file declarion section where we are defining 
      *all the files we would be refferring from.
            select in-file
                assign to '../../../test1.dat'
                organization is line sequential.

            select print-file
                assign to '../../../test1.out'
                organization is line sequential.

       data division.
       file section.
      *Input and output files and record definitions
       fd in-file
          record contains 33 characters
          data record is tax-record.

       01 tax-record.
         05 tax-name pic x(20).
         05 province-code pic x(2).
         05 gross-salary pic 9(6).
         05 exemption-amount pic 9(5).

       fd print-file
           record contains 80 characters
           data record is print-line.

      *This will the output records in teh output file with this 
      *variables
       01 print-line.
         05 filler pic x(2).
         05 prt-name pic x(20).
         05 filler pic x(3).
         05 prt-fed-tax pic zz,zz9.99.
         05 filler pic x(6).
         05 prt-prov-tax pic zz,zz9.
         05 filler pic x(5).
         05 prt-tot-tax pic zzz,zz9.
         05 filler pic x(2).

       working-storage section.

      *This is the summary total calculation display at the end of the 
      *page
       01 ws-total-1.
         05 filler pic x(39) value spaces.
         05 filler pic x(11) value "Total Tax: ".
         05 ws-total-tot-tax pic $$$$,$$9.

      *  This is the summary total calculation for ontario average tax
       01 ws-total-2.
         05 filler pic x(29) value spaces.
         05 filler pic x(23) value "Average Ontario Tax: ".
         05 ws-avg-ont-tax pic $$,$$9.

      *  This is the heading section which will show part of the 
      *  heading.
       01 ws-heading1.
         05 filler pic x(2) value spaces.
         05 filler pic x(4) value "Name".
         05 filler pic x(19) value spaces.
         05 filler pic x(7) value "Federal".
         05 filler pic x(8) value spaces.
         05 filler pic x(10) value "Provincial".
         05 filler pic x(2) value spaces.
         05 filler pic x(5) value "Total".
         05 filler pic x(20) value spaces.

      *  This is the second heading section which will show second line
      *  of heading.
       01 ws-heading2.
         05 filler pic x(25) value spaces.
         05 filler pic x(3) value "Tax".
         05 filler pic x(12) value spaces.
         05 filler pic x(3) value "Tax".
         05 filler pic x(9) value spaces.
         05 filler pic x(3) value "Tax".
         05 filler pic x(20) value spaces.

      *This will store all the counters we will be using for calculation
      *purposes.
       01 ws-counter.
         05 ws-ontario-counter pic 9.
         05 ws-total-ontario-tax pic 9(5).
         05 ws-total-tax-counter pic 9(8).

      *  This is the calculation section which will be storing the 
      *  variables used for the purpose of calculation.
       01 ws-calcs.
         05 ws-mediator pic 9(5).
         05 ws-federal-tax-calc pic 9(5)v99.
         05 ws-provinvial-tax-calc pic 9(5).
         05 ws-total-tax-calc pic 9(5).
         05 ws-ontarion-tax-calc pic 9(5).

      *  This is the end of file flag which will be showing values of 
      *  the file
       01 ws-flags.
         05 ws-eof pic x value "n".

      *This is the 77 storage variables storing all the string literals 
      *all the values varaible.
       77 ws-end-reached pic x value "y".
       77 ws-one pic 9 value 1.
       77 ws-two pic 9 value 2.
       77 ws-zero pic 9 value 0.
       77 ws-21-percent pic 99v9999 value 0.214.
       77 ws-7-percent pic 99v999 value 0.078.
       77 ws-14-percent pic 99v9999 value 0.1491.
       77 ws-19-percent pic 99v9999 value 0.197.
       77 ws-ontario-code pic x(2) value "ON".
       77 ws-alberta-code pic x(2) value "AB".
       77 ws-quebec-code pic x(2) value "QC".


       procedure division.
       000-Main.
      *    Opening files
           perform 10-open-files.
      *    Read input
           perform 20-read-input.
      *    Print headings
           perform 30-print-headings.
      *    Main logic of reading files
           perform 100-main-logic until ws-eof = ws-end-reached.
      *    printing summary calculation
           perform 200-summary-calculation.
      *    Closing all the files.
           perform 300-close-files.
     
           
           

           stop run.
      *Performing main printing of the values with all the calculations 
      *    parted in paragraph
       100-main-logic.
           perform 110-federal-tax-calculation.
           perform 120-provincial-tax-calculation.
           perform 130-total-tax-calculation.

           move ws-federal-tax-calc to prt-fed-tax.
           move ws-provinvial-tax-calc to prt-prov-tax.
           move ws-total-tax-calc to prt-tot-tax.
           move tax-name to prt-name.
           write print-line after advancing ws-one line.

          perform 20-read-input.

      *Open files
       10-open-files.
           open input in-file,
             output print-file.

      *Read input
       20-read-input.
           read in-file
               at end
                   move ws-end-reached to ws-eof.

      *Print headings.
       30-print-headings.
           write print-line from ws-heading1 after advancing ws-one
             lines.
           write print-line from ws-heading2 before advancing ws-one
             lines.

      *Write summary calculation.
       200-summary-calculation.
           compute ws-ontarion-tax-calc rounded = ws-total-ontario-tax /
             ws-ontario-counter.
           move ws-ontarion-tax-calc to ws-avg-ont-tax.
           move ws-total-tax-counter to ws-total-tot-tax.
           write print-line from ws-total-1 after advancing ws-two line.
           write print-line from ws-total-2 after advancing ws-one
             lines.

      *Close files.
       300-close-files.
           close print-file,
             in-file.

      *Federal tax calculation 
       110-federal-tax-calculation.
      *Perform federal tax calculation
           move ws-zero to ws-mediator.
           move ws-zero to ws-provinvial-tax-calc.
           move ws-zero to ws-federal-tax-calc.
           compute ws-mediator rounded = gross-salary -
             exemption-amount.
           compute ws-federal-tax-calc rounded = ws-21-percent *
             ws-mediator.

      *Provincial tax calculation 
       120-provincial-tax-calculation.
      *Perform prvinvial tax calculation
           if (province-code = ws-alberta-code) then
               compute ws-provinvial-tax-calc rounded = ws-7-percent *
                 ws-mediator
           end-if.
           if (province-code = ws-ontario-code) then
               compute ws-provinvial-tax-calc rounded = ws-14-percent *
                 ws-mediator
               add 1 to ws-ontario-counter
               add ws-provinvial-tax-calc to ws-total-ontario-tax
           end-if.

           if (province-code = ws-quebec-code) then
               compute ws-provinvial-tax-calc rounded = ws-19-percent *
                 ws-mediator
           end-if.

       130-total-tax-calculation.
      *    Perform total  tax calculation
           compute ws-total-tax-calc rounded = ws-federal-tax-calc +
             ws-provinvial-tax-calc.
           add ws-total-tax-calc to ws-total-tax-counter.

       end program MidtermPractical1.