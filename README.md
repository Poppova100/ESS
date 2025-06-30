#####Author: Zuzana Poppová
#####bestakova@fzp.czu.cz

#####Scripts and data for creating graphs and images published in the paper Climatological characteristics and atmospheric circulation associated with 3D heat wave types in Middle Europe


DESCRIPTION OF FILES:

The script study_area.R displays the study area - Middle Europe. The script boxplots.R displays boxplots of differences between potential evapotranspiration (PET) and precipitation (P) for individual heat wave types on three days before, during, and three days after heat waves.  The script multiple_pie.R displays the frequencies (in %) of all-anticyclonic, all-cyclonic, purely-directional and indeterminate circulation types on three days before, during, and three days after the heat wave types. The script pie_4.R displays the frequencies (in %) of the anticyclonic type, types with southerly advection, indeterminate flow, and all other circulation types during the individual heat wave types. The script col_plot.R displays the frequencies of 11 supertypes on three days before, during, and three days after each heat wave type. The script table_1.R displays a combination of information from the tab_June_Sep_27.rds file and HW_TAB_27.rds and table_2 displays likewise the tab_June_Sep_11.rds file and HW_TAB_11.rds.

The file dta_boxplots.rds is the data of daily PET-P values ​​three days before, during, after the heat wave, and during the June-September season. 

The dat_before.rds, dat_during.rds and dat_after.rds files contain the frequencies of circulation types (Stryhal, 2025) during individual heat wave types (HWG, HWL, HWO, HWH) (Lhotka, 2025). The circulation types were divided into groups:
A (A, AN, ANE, AE, ASE, AS, ASW, AW, ANW)
C (C, CN, CNE, CE, CSE, CS, CSW, CW, CNW)
DIR (N, NE, E, SE, S, SW, W, NW)
U (indeterminate flow).

The dta_4_CTs.rds file contains the frequencies of circulation types (Stryhal, 2025) during individual heat wave types (HWG, HWL, HWO, HWH) (Lhotka, 2025). The circulation types were divided into three main groups of circulation conducive to heat waves:
anticyclonic (A)
southerly (S, CS, AS, ASE, CSE, SE, ASW, CSW, SW)
indeterminate (U).
other (W, CW, AW, E, CE, AE, ANE, CNE, NE, N, CN, AN, ANW, CNW, NW, C)

The dat2_prob_before.rds, dat2_prob_during.rds and dat2_prob_after.rds files contain the frequencies of circulation types (Stryhal, 2025) during individual heat wave types (HWG, HWL, HWO, HWH) (Lhotka, 2025). The circulation types were divided into groups:
A 
C 
U (indeterminate flow)
S (S, CS, AS)
N (N, CN, AN)
W (W, CW, AW)
E (E, CE, AE)
NE (NE, ANE, CNE)
SE (SE, ASE, CSE)
SW (SW, ASW, CSW)
NW (NW, ANW, CNW)

The tab_June_Sep_27.rds file contains the frequencies of circulation types (27) and daily mean PET-P, TG and P values during the June-September season.  The HW_TAB_27.rds file contains the frequencies of circulation types (27) and daily mean PET-P, TG, P and Cef (ratio of frequency of a given CT in heat waves to the mean June–September frequency) values individual heat wave types (HWG, HWL, HWO, HWH). The klima.rds file contains mean daily temperature in the June-September season. Likewise (analogously) the documents tab_June_Sep_11.rds, HW_TAB_11.rds and klima.rds.
