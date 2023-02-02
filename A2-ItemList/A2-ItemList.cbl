       identification division.
       program-id. A2_ItemList.
       author. Rob Savoie.
       date-written. rev: V1.0 Jan 30/2023 rev: V1.3 on Feb 2/2023.
      *
       environment division.
       configuration section.
       input-output section.
      *
       file-control.
      *
      *input-file declaration
      *
           select input-file
               assign to "../../../data/A2-ItemList.dat"
               organization is line sequential.
      *        
      *output-file declaration
      *
           select output-file
               assign to "../../../data/A2-ItemList.out"
               organization is line sequential.
      *
       data division.
       file section.
      *
       fd input-file
           data record is input-line
           record contains 27 characters.
      *
       01 input-line.
           05 il-item-number           pic 9(4).
           05 il-product-class         pic x.
           05 il-desc                  pic x(13).
           05 il-qty                   pic 999.
           05 il-price-per-unit        pic 9(4)v99.
      *
       fd output-file
           data record is output-line
           record contains 108 characters.
      *
       01 output-line                  pic x(108) value spaces.
      *
       working-storage section.
      *
       01 ws-name.
           05 filler                   pic x(94).
           05 filler                   pic x(14) value "ROB SAVOIE, A2".
      *
       01 ws-heading-one.
           05 filler                   pic x     value spaces.
           05 ws-head-one              pic x(4)  value "ITEM".
           05 filler                   pic x(5)  value spaces.
           05 ws-head-two              pic x(4)  value "ITEM".
           05 filler                   pic x(6)  value spaces.
           05 ws-head-three            pic xxx   value "QTY".
           05 filler                   pic x(5)  value spaces.
           05 ws-head-four             pic x(4)  value "UNIT".
           05 filler                   pic x(7)  value spaces.
           05 ws-head-five             pic x(8)  value "EXTENDED".
           05 filler                   pic x(7)  value spaces.
           05 ws-head-six              pic x(8)  value "DISCOUNT".
           05 filler                   pic x(6)  value spaces.
           05 ws-head-seven            pic x(9)  value "NET PRICE".
           05 filler                   pic xx    value spaces.
           05 ws-head-eight            pic x(5)  value "CLASS".
           05 filler                   pic xx    value spaces.
           05 ws-head-nine             pic x(5)  value "TRANS".
           05 filler                   pic xxx   value spaces.
           05 ws-head-ten              pic x(14) value "TRANSPORTATION".
      *
       01 ws-heading-two.
           05 filler                   pic xx    value spaces.
           05 ws-head-one              pic x     value "#".
           05 filler                   pic x(4)  value spaces.
           05 ws-head-two              pic x(11) value "DESCRIPTION".
           05 filler                   pic x(10) value spaces.
           05 ws-head-four             pic x(5)  value "PRICE".
           05 filler                   pic x(7)  value spaces.
           05 ws-head-five             pic x(5)  value "PRICE".
           05 filler                   pic x(10) value spaces.
           05 ws-head-six              pic x(6)  value "AMOUNT".
           05 filler                   pic x(26) value spaces.
           05 ws-head-nine             pic x     value "%".
           05 filler                   pic x(10) value spaces.
           05 ws-head-ten              pic x(6)  value "CHARGE".
           05 filler                   pic x(4)  value spaces.
      *
       01 ws-general.
           05 filler                   pic x     value spaces.
           05 ws-item-number           pic x(4).
           05 filler                   pic x     value spaces.
           05 ws-desc                  pic x(13).
           05 filler                   pic x     value spaces.
           05 ws-qty                   pic zz9.
           05 filler                   pic xx    value spaces.
           05 ws-price-per-unit        pic z,zz9.99.
           05 filler                   pic xx  value spaces.
           05 ws-ext-price             pic z,zzz,zz9.99.
           05 filler                   pic x(5)  value spaces.
           05 ws-discount-amount       pic zzz,zz9.99.
           05 filler                   pic xxx  value spaces.
           05 ws-net-price             pic z,zzz,zz9.99.
           05 filler                   pic x(4)  value spaces.
           05 ws-product-class         pic x.
           05 filler                   pic x(4)  value spaces.
           05 ws-trans-percent         pic z9.9.
           05 ws-percent               pic x.
           05 filler                   pic x(4)  value spaces.
           05 ws-trans-charge          pic z,zzz,zz9.99.
      *
       01 ws-totals.
           05 filler                   pic x(33) value spaces.
           05 ws-ext-total             pic $$$,$$$,$$9.99.
           05 filler                   pic x(16) value spaces.
           05 ws-net-total             pic $$$,$$$,$$9.99.
           05 filler                   pic x(17) value spaces.
           05 ws-trans-total           pic $$$,$$$,$$9.99.
      *
       01 ws-discount-analysis.
           05 filler                   pic z(25) value
                                       "ITEMS WITHOUT DISCOUNT =".
           05 filler                   pic x     value spaces.
           05 ws-without-discount      pic zz9.99.
           05 filler                   pic x     value "%".
           05 filler                   pic x(75).
      *
       01 ws-flags.
           05 ws-eof-flag              pic x     value "n".
           05 ws-eof-yes               pic x     value "y".
           05 ws-eof-no                pic x     value "n".
           05 ws-eof-other             pic x     value "x".
      *
       01 ws-math-store.
           05 ws-store-ext             pic 9(10)v9999.
           05 ws-store-trans           pic 9(10)v9999.
           05 ws-store-discount        pic 9(10)v9999.
           05 ws-store-net             pic 9(10)v9999.
           05 ws-store-ext-total       pic 9(10)v9999.
           05 ws-store-net-total       pic 9(10)v9999.
           05 ws-store-trans-total     pic 9(10)v9999.
           05 ws-store-alldisc         pic 9(4).
           05 ws-store-disc-total      pic 9(4).
           05 ws-store-nodisc-total    pic 9(4).
           05 ws-store-without         pic 999v99999.
      *
       01 ws-cnsts.
           05 ws-transport-A           pic 99v9  value 12.5.
           05 ws-transport-B           pic 9v9   value 8.5.
           05 ws-transport-F           pic 9v9   value 4.5.
           05 ws-transport-default     pic 9v9   value 6.5.
           05 ws-trans-cost            pic 99    value 45.
           05 ws-discount              pic 9v99  value 0.05.
           05 ws-class-A               pic x     value "A".
           05 ws-class-B               pic x     value "B".
           05 ws-class-F               pic x     value "F".
           05 ws-percent-A             pic 9v999 value 0.125.
           05 ws-percent-B             pic 9v999 value 0.085.
           05 ws-percent-F             pic 9v999 value 0.045.
           05 ws-percent-default       pic 9v999 value 0.065.
           05 ws-percent-symbol        pic x     value "%".
      *
       procedure division.
      *
       000-main.
      *
      *read input file
      *
           open input input-file.
           open output output-file.
      *
           perform 110-read-input-file.
      *
           perform 100-write-headings.
      *
           perform 200-process-file
               until ws-eof-flag equals ws-eof-yes.
      *
           perform 350-calculate-without-discount.
      *
           perform 120-write-footers.
      *
           goback.
      *
       100-write-headings.
      *
      *display the name and heading
      *
           write output-line from ws-name
             before advancing 3 lines.
           write output-line from ws-heading-one.
           write output-line from ws-heading-two
             before advancing 3 lines.
      *
       110-read-input-file.
           read input-file
               at end
                   move ws-eof-yes to ws-eof-flag.
      *
       120-write-footers.
      *          
      *write final lines
      *
           write output-line from ws-totals
             after advancing 1 lines.
           write output-line from ws-discount-analysis
             after advancing 3 lines.
      *
       200-process-file.
      *
      *clear output buffer
      *
           move spaces to ws-general.
      *
           perform 310-calculate-ext-price.
      *
      *move detail output data
           move il-item-number     to ws-item-number.
           move il-desc            to ws-desc.
           move il-qty             to ws-qty.
           move il-price-per-unit  to ws-price-per-unit.
           move il-product-class   to ws-product-class.
           move ws-percent-symbol  to ws-percent.
      *
           perform 320-calculate-discount.
           perform 330-calculate-trans-charge.
           perform 340-calculate-net-price.
      *
           move ws-store-trans     to ws-trans-charge.
           move ws-store-discount  to ws-discount-amount.
           move ws-store-ext       to ws-ext-price.
           move ws-store-net       to ws-net-price.
      *
      *write detail output
      *
           write output-line from ws-general
               before advancing 2 lines.
      *
      *read next record from input-file
      *
           perform 110-read-input-file.
      *
       310-calculate-ext-price.
      *
      *calculate extended price
      *
           multiply il-qty
                 by il-price-per-unit
             giving ws-store-ext.
      *
       320-calculate-discount.
      *    
      *calculate discount
      *
           if (ws-store-ext is greater than 100 and ws-product-class is
                                           equal to ws-class-A) then
          multiply ws-store-ext
                by ws-discount
            giving ws-store-discount
      *
               add 1
                to ws-store-disc-total
      *
           else if (ws-store-ext is greater than 50 and ws-product-class
                                        is equal to ws-class-F) then
              multiply ws-store-ext
                    by ws-discount
                giving ws-store-discount
      *
                   add 1
                    to ws-store-disc-total
      *
               else if (ws-product-class is equal to ws-class-B and
                                      il-qty is greater than 5) then
                  multiply ws-store-ext
                        by ws-discount
                    giving ws-store-discount
      *
                       add 1
                        to ws-store-disc-total
      *
                   else
                       move 0.0 to ws-store-discount
      *
                       add 1
                        to ws-store-nodisc-total
      *
                   end-if.
       330-calculate-trans-charge.
      *
      *calculate transportation charge
      *
           if (ws-product-class is equal to ws-class-A) then
               move ws-transport-A to ws-trans-percent
               multiply ws-percent-A
                     by ws-store-ext
                 giving ws-store-trans
      *
           else if (ws-product-class is equal to ws-class-B) then
                   move ws-transport-B to ws-trans-percent
                   multiply ws-percent-B
                         by ws-store-ext
                     giving ws-store-trans
      *
               else if (ws-product-class is equal to ws-class-F) then
                       move ws-transport-F to ws-trans-percent
                       multiply ws-percent-F
                             by ws-store-ext
                         giving ws-store-trans
      *
                   else if (ws-qty is less than or equal to 100) then
                           move ws-transport-default to ws-trans-percent
                           multiply ws-percent-default
                                 by ws-store-ext
                             giving ws-store-trans
      *
                       else
                           move 0.0 to ws-trans-percent
                           move ws-trans-cost to ws-store-trans
                       end-if.
       340-calculate-net-price.
      *
      *calculate net price
      *
           subtract ws-store-discount
               from ws-store-ext
             giving ws-store-net.
      *
           add ws-store-ext
            to ws-store-ext-total.
      *
           add ws-store-net
            to ws-store-net-total.
      *
           add ws-store-trans
            to ws-store-trans-total.
      *
       350-calculate-without-discount.
      *    
      *calculate percentage of items without discount
      *
                add ws-store-nodisc-total
                 to ws-store-disc-total
             giving ws-store-alldisc.
      *
             divide ws-store-nodisc-total
                 by ws-store-alldisc
             giving ws-store-without.
      *
           multiply ws-store-without
                 by 100
             giving ws-without-discount.
      *
      *move totals to output
      *
           move ws-store-ext-total to ws-ext-total.
           move ws-store-net-total to ws-net-total.
           move ws-store-trans-total to ws-trans-total.
      *
       end program A2_ItemList.