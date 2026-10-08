By_Seed_CSV3.csv contains all of the seedling-level data used for this project. This includes
developmental organ scores, root and shoot lengths, and root-to-shoot ratios across measurement
dates.

OrganScoreSims.csv contains the output from developmental-stage matrix model simulations. This
includes the projected numbers of seedlings in each organ-development state through time for 
each species and nitrogen treatment.

Linear_models.R runs the root and shoot linear regressions used to compute parameter estimates
describing root and shoot growth. It computes the 95% confidence intervals for each parameter
and generates Figure 2.

Seedling_Filtering_Manual_Colors.R contains code to generate Figure 6 (see Appendix A). It also
runs the stochastic simulations used to project root:shoot ratios over 10 weeks and generates
Figure 3.

OrganScoreMatrixSims.R contains code to generate Figure 4.

LeafChangeProportions.R filters the Organ Score data to generate Figure 5.
