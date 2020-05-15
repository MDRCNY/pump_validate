# Power-Under-Multiplicity

## Purpose
The read me is a guide for our colleagues at Harvard to _hopefully_ easily navigate the different folders we have.

### Folder Structure

* __Validation__: _Monte Carlo Simulation Code is here_
     * __Simulations:__ 
          * gen_blocked_i1_2.R: Data generating distributions for the specified design 
          * gen_simple_c2_2r.R: Data generating distributions for the specified design
          * EstimatePowerWithSimulation.R: Estimating power with Monte Carlo Simulations
          * adjust.WY.R : Westfall-Young adjustment function
          * (MarkdownHelpers): _IGNORE_ (Background Functions to help generate validation markdown report)
* __Methods__: Derived Methods. __mdes__ function is under __blocked_i1_2cfr.R__ Section to be __updated__ next week. Parallel function exist in R package. 
     * __blocked_i1_2cfr.R__  : derived method for 2-level blocked RCT. Refer to program map excel for details
     * __utils.R__ : helper function for __blocked_i1_2cfr.R__
     * __TwoLevelBlockedTreatmentLevel1.Rmd__ : Old code. _IGNORE_
* __Admin__: _Program_Map.xlsx is here._

_Remaining folders and their content:_ 
_Note: If you run into getting the right packages, please let me know._ 

* __DemoPUM__: RShiny Code. R Shiny is launched from here.
* __Domino__: _IGNORE_
* __References__: Old code. _IGNORE_
* __Testing__: Speed test/profile code. _IGNORE_
