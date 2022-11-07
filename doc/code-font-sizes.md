November 2, 2022

Code on a phone
===========================

414px wide = iPhone 8 Plus in portrait mode.

Courier 10px will print 45 characters before wrapping on this phone.
To get 70 characters, the font size fs can be estimated as:

     45 characters x 10px/character = 70 characters x ?

     ? = 450/70 = 6.4px
                  =====

It turns out that 7px works.

Estimate a break point in pixels for each unit jump in font size.

    8px = 7px * (x/414) --> x = 473
    9px = 7px * (x/414) --> x = 532
   10px = ...           --> x = 591
   11px = ...           --> x = 651
   12                       x = 710
   13                       x = 769
   14                       x = 828
   15                       x = 887


