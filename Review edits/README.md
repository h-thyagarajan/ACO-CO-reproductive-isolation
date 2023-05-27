# Evolution of reproductive isolation in a long-term evolution experiment with Drosophila melanogaster: 30 years of divergent life history selection.

## Description of the data and file structure

This data was collected to answer whether 3 decades and 1500 generations of divergent life history selection on age at reproduction has resulted in speciation between laboratory populations of Drosophila melanogaster. We tested the existance of premating, postmating-prezygotic and postzygotic reproductive isolation between 3 replicate pairs of early breeding ACO and late breeding CO populations in 2019-2020. Results from individual male and female mate choice trials indicate that populations from both selection treatments show a marked bias towards homotypic mate choice, indicative of prezygotic reproductive isolation. Hybrid offspring were viable with intermediate size and developmental traits. We found no significant evidence suggesting the evolution of postmating-prezygotic or postzygotic reproductive isolation. Our results agree with previous findings that premating barriers evolve before postmating forms of reproductive isolation.

The presented data is derived from a series of mate choice and hybrid viability assays. In most data files populations are referred to by the shortform A (ACO) and C (CO), and replicates are noted using numbers 1-3 (A1-A3, C1-C3). For mate choice assays coloured dye (red or blue) via food intake was used to differentiate flies from different populations. For hybrid viability assays, parental (P/WT), first generation (F1) and second generation (F2) hybrid crosses (BT) were tested. 

A description of specific .csv files:

‘Group choice – compiled data.csv’: This data was collected during a group mate choice assay where 10 females and 12 males from each regime were combined in vials. The assay was repeated over 2 days, with 10 vials tested for each replicate population during each day. Column ‘Female ID’ refers to the replicate population of females. Columns ‘Proportion C mating’ and ‘Proportion A mating’ columns indicate the proportion of females mating with C and A males respectively. This was derived from dividing the ‘number mated Cs/As’ by the ‘number mating pairs’ column. ‘NA’ values refer to vials where mating did not take place. 

‘female choice – compiled data.csv’: This data was collected during a female mate choice assay where females were placed in a vial containing 2 males, 1 from each population, and allowed to mate. The assay was repeated in 4 trials. During each trial, 50 females from each replicate population were tested. Data shows the identity of females and the chosen male as well as the mating start time. Columns ‘mating latency’ and ‘mating duration’ show the time from the start of the observation period till mating and the length of mating in minutes. Columns containing ‘NA’ values refer to vials that were discarded because mating did not take place within 60 minutes of the beginning of observation. 

‘male choice – compiled data.csv’: This data was collected during a male mate choice assay where males were placed in a vial containing 2 females, 1 from each population, and allowed to mate. Replication and columns are the same as the female mate choice assay described above. Information related to the colour of flies (red or blue) is not included because females could be differentiated based on size along and so the dye method was not needed to differentiate between populations. 

‘Hybrid size data.csv’: This data was collected during a hybrid viability assay where hybrids were monitored through development to uncover postmating prezygotic or postzygotic reproductive isolation in the form of reduced hybrid viability. The dry weight of female flies from each cross were weighed in 4 groups of 5 flies. Column ‘Total.Weight’ values were divided by 5 to determine ‘Individual.Weight’ an estimate of the dry weight of an individual fly. 

‘Hybrid viability data.csv’: This data was collected during the hybrid viability assay and includes information related to the hatching success, viability and development time of offspring. 9 vials for each hybrid cross and replicate population, each containing 90 eggs, were created. Relevant columns include ‘Hatchability’ (Viable.Eggs/Total.Eggs), ‘EggtoAdultCorrected’ (Total.Adults/Viable.Eggs.corrected), and ‘Av.Development.Time’ (hours). In some cases, the number of pupae counted in the ‘Pupae.Count’ column is larger than those in the ‘Viable.Eggs’ column. To account for this the ‘Viable.Eggs.corrected’ column was made to reflect the number of pupae observed when the number of pupae was larger than the number of viable eggs. 

‘PZRF compiled data.csv’: This data was collected during a hybrid reproductive fitness assay to uncover evidence of postzygotic reproductive isolation in the form of reduced hybrid fertility. Focal red-eyed flies from hybrid or parental crosses were put in competition with brown eyed competitors for brown eyed mates. Focal flies were combined with 4 same sex competitors and 4 same sex mates in vials. Since the brown eye marker is recessive, the number of red eyed progeny was used to determine the reproductive output of focal flies. 30 vials were tested for each sex and cross identity within each replicate population. The ‘Proportion Red’ column refers to the proportion of progeny produced by the focal fly (Red/(Red+Brown)). In the ‘Sex’ column ‘f’ refers to a female focal fly, and ‘m’ refers to a male focal fly. ‘NA’ refers to vials where fertility could not be determined. 

## Sharing/Access information

Specific information related to the results and methods of the experiment, as well as figures and supplementary information can be found in the corresponding publication DOI. 

This data is also available by the authors on OSF: osf.io/y5ckb/ 

## Code/Software

We include 3 scripts corresponding to data analyses performed in R and R Studio. These scripts include ‘G-test Script.R’ which covers the analysis of mate choice data using a repeated G-test for goodness of fit. The analysis of hybrid viability and fertility data through the fitting of mixed models is covered in ‘Hybrid viability, size, reproductive fitness script.Rmd’, and ‘Latency, Duration script.Rmd’. Code is well annotated including the steps of analysis and packages used. 


