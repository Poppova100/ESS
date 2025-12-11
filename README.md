# Scripts and Data for "Climatological Characteristics and Atmospheric Circulation Associated with 3D Heat Wave Types in Middle Europe"

**Author:**  
Zuzana Poppová  
[bestakova@fzp.czu.cz](mailto:bestakova@fzp.czu.cz)

This repository contains the scripts and data used to create the graphs and images published in the paper:

**Climatological characteristics and atmospheric circulation associated with 3D heat wave types in Middle Europe**

---

## Description

This repository provides R scripts and datasets for analyzing and visualizing the climatological and atmospheric patterns associated with different types of heat waves in the Middle Europe.

---

## File Overview

### R Scripts

- **study_area.R**  
  Displays the study area (Middle Europe).

- **boxplots.R**  
  Generates boxplots showing differences between potential evapotranspiration (PET) and precipitation (P) for different heat wave types, for three days before, during, and after heat waves.

- **multiple_pie.R**  
  Visualizes the frequencies (%) of all-anticyclonic, all-cyclonic, purely-directional, and indeterminate circulation types for three days before, during, and after heat waves.

- **multiple_pie2.R**  
  Revised multiple_pie.R

- **pie_4.R**  
  Shows the frequencies (%) of anticyclonic types, types with southerly advection, indeterminate flow, and all other circulation types during individual heat wave types.

- **col_plot.R**  
  Displays the frequencies of 11 circulation supertypes for three days before, during, and after each heat wave type.

- **table_1.R**  
  Combines data from `tab_June_Sep_27.rds` and `HW_TAB_27.rds`.

- **table_2.R**  
  Combines data from `tab_June_Sep_11.rds` and `HW_TAB_11.rds`.

---

### Data Files

- **dta_boxplots.rds**  
  Contains daily PET-P values for three days before, during, and after heat waves, as well as during the June–September season.

- **dat_before.rds**, **dat_during.rds**, **dat_after.rds**  
  Frequencies of circulation types during individual heat wave types (HWG, HWL, HWO, HWH).  
  Circulation types are grouped as follows:
  - **A:** A, AN, ANE, AE, ASE, AS, ASW, AW, ANW
  - **C:** C, CN, CNE, CE, CSE, CS, CSW, CW, CNW
  - **DIR (Directional):** N, NE, E, SE, S, SW, W, NW
  - **U (Indeterminate):** U

- **dta_4_CTs.rds**  
  Circulation frequencies during heat wave types, classified as:
  - **Anticyclonic (A)**
  - **Southerly:** S, CS, AS, ASE, CSE, SE, ASW, CSW, SW
  - **Indeterminate (U)**
  - **Other:** W, CW, AW, E, CE, AE, ANE, CNE, NE, N, CN, AN, ANW, CNW, NW, C

- **dat2_prob_before.rds**, **dat2_prob_during.rds**, **dat2_prob_after.rds**  
  Frequencies of circulation types grouped as:
  - A, C, U (indeterminate)
  - S (S, CS, AS)
  - N (N, CN, AN)
  - W (W, CW, AW)
  - E (E, CE, AE)
  - NE (NE, ANE, CNE)
  - SE (SE, ASE, CSE)
  - SW (SW, ASW, CSW)
  - NW (NW, ANW, CNW)

- **tab_June_Sep_27.rds**  
  Frequencies of 27 circulation types and daily mean PET-P, TG, and P values during June–September.

- **HW_TAB_27.rds**  
  Frequencies of 27 circulation types, daily mean PET-P, TG, P, and Cef (ratio of frequency of a given CT in heat waves to the mean June–September frequency) for individual heat wave types.

- **tab_June_Sep_11.rds**, **HW_TAB_11.rds**  
  Analogous to above, but with 11 circulation types.

- **klima.rds**  
  Mean daily temperature for the June–September season.

---

For any questions or further information, please contact Zuzana Poppová at [bestakova@fzp.czu.cz](mailto:bestakova@fzp.czu.cz).
