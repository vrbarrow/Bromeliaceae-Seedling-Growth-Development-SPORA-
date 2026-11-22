By_Seed_CSV3.csv contains all of the seedling-level data used for this project. This includes
developmental organ scores, root and shoot lengths, and root-to-shoot ratios across 
measurement dates. Each row corresponds to one individual seedling. Seedlings are identified 
by Trial number in column 1, nitrogen treatment (1 = no nitrogen, 2 = low nitrogen, 3 = high 
nitrogen) in column 2, species (1 = _Aechmea recurvata_, 2 = _Billbergia brasiliensis_, 3 = 
_Puya mirabilis_, 4 = _Vriesea rafaelii_) in column 3, plate identity in column 4, and 
seedling identity in column 5. Seedling identity corresponds to each seedling's position 
within the 3 x 3 plate grid relative to a fixed orientation point on the plate. Organ scores 
collected at days 3, 7, 10, 14, 17 and 21 are reported in columns 6-11, respectively. Root 
length (mm) measurements collected at days 7, 14, and 21 are reported in columns 12, 14, and 
16, respectively. Shoot length (mm) measurements collected at days 7, 14, and 21 are reported 
in columns 13, 15, and 17, respectively. Root-to-shoot ratio calculated at days 7, 14, and 21 
are reported in columns 18, 19, and 20, respectively. Root-to-shoot ratios are only reported 
when both root length and shoot length data were available for a seedling at the 
corresponding timepoint.


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
