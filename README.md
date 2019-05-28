# FireScar
Simulation modelling of fire scar occurrence and statistical detection. 

This set of R scripts simulates the repeated burning of a forest of trees with a given fire regime, including fire-dependent and fire-independent mortality, and changing probability of fire scar recording on trees based on age, and time since fire.  Dendrochronological sampling techniques can then be applied to the simulated fire scar record to asses statistical discrimination of changes in fire regime.

This version is not formatted as an R package, but may be in the future.  Models are run in parallel and generate simulation output files in a defined directory.

Files:

* cat_face_mortality_pfire.r - Model functions for running a fixed fire regime
* cat_face_mortality_pfire_split.r - Model functions for running a split fire regime with a change point at a given year.
* summary_functions.r

* experiment_0.r - Run latin hypercube sensitivity test of mortality functions
* experiment_1.r - Run four fixed regime simulations
* experiment_2.r - Fun a series of split regimes (wide and narrow) with varying split year
* calc_summary_stats.r - Calculates summary MFI statistics for fixed regime simulation runs

* plot_fig1.r - plots paper figure 1 - MFI estimation of fixed regimes