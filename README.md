# GoldenEagles_VehicleCollisionMitigation
 Code used to produce results in “Assessing carcass relocation for offsetting golden eagle mortality at wind energy facilities”
 Eric Lonsdorf et al,The Journal of Wildlife Management, 2023 

 This repository is an R code based on a set of matlab codes written by james gerber (university of minnesota)
 Questions on either set of codes can be addressed to jgerber68@gmail.com

 Notes on this set of codes:

 User can modify GOEAVCMconstantsfileR.R, many constants in here.
 run setGOEAVCMpaths.R
 then run GOEAVCMscriptR.R

There is some hard-wiring in here the user should be aware of:
Nrealizations is hard-wired in EagleMortalityR.R    The code has 10 which is low for publication-level results (although I haven't tested this).  I used a value of 500 for the paper which is definitely big enough (I did test that)  

10 is probably OK for exploring a parameter space or comparing counties or road types because there will be 10 simulations for each permutation of county/road type /carcass number. In the example in the paper, there are 23 counties,  15 road types, and typical carcass numbers can be around 5-15, so there are more simulations than it seems like from the value of Nrealizations.

I would suggest a user keep Nrealizations at 10 or so for some quick studies, and then bump up Nrealizations to something larger and run overnight.

On line 74 of EagleMortalityR.R , I pre-initialize a vector with 1e7 calls to erfinv.  This makes sense in Matlab, where the call to erfinv is parallelized and very fast.  (in other words, call the function once and get a bunch of erfinv calculations and just use those instead of multiple calls to erfinv inside the nested loops.)

This code is slow.  I programmed the original version taking advantage of tricks in matlab that really slow things down in R.  If users aren't willing to let this run overnight they might be disappointed (remember, we had all sorts of statistics, and a brute force search over the param space.)

It would be fairly straightforward to parallelize this code - we didn’t because I don’t know what kind of computer this will be run on.


