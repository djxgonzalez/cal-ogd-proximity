# Temporal trends of racial and socioeconomic disparities in population exposures to upstream oil and gas development in California: Codebase and Dataset

### Authors

David J.X. González*, Claire M. Morton*, Lee Ann L. Hill, Drew R. Michanowicz, Robert J. Rossi, Seth B.C. Shonkoff, Joan A. Casey, and Rachel Morello-Frosch

\* These authors wrote the code used in the analyses

### Description

This repository includes the processed data and codebase we used to set up and conduct the analyses presented in González et al. (under review). The key findings, abstract, and plain language summary for this study are copied below. We hope that this code can be used for instruction or for replication of our findings. We also hope that others will adapt and build on this code for further analyses. 

The sources for raw datasets we used are listed below, and both raw and interim analytic files based on publicly-available data are available upon request. The processed dataset used for all analyses is included in this repository under `data/processed/acs_exposure_2005_2019.rds`, which may be accessed using the `readRDS()` function in base R. We've also provided the processed data as a CSV file in the same sub-directory, though this file is not referenced in the code.

Once the study has been accepted for publication, we will post the link to the open access (i.e., free to read) peer-reviewed article here. Please reach out to the corresponding author, *David J.X. González (djxgonz@berkeley.edu), with any questions or data requests.

### Data Sources

For the analysis, we obtained data from the following sources:

- [American Community Survey](https://www.census.gov/programs-surveys/acs), 5-year estimates of sociodemographic charateristics provided by the U.S. Census Bureau for block groups across California. Block groups are geographic units smaller than census tracts that roughly approximate neighborhoods and typically have 600 to 3,000 residents.
- [California Geologic Energy Management Division (CalGEM)](https://www.conservation.ca.gov/calgem/Online_Data), publicly available data on oil and gas well locations and development dates; also, monthly production volume at each well in the state.
- [Enverus](https://www.enverus.com/), additional data on oil and gas well development. This dataset is not publicly available, but academic researchers may request access to the data for research purposes.

We used additional data from other sources to generate figures (i.e., not for the analysis), including: monthly price of a barrel of oil equivalent at the Midway-Sunset Oil Field (a benchmark for California-produced oil) from the U.S. Energy Information Agency; shapefiles for California counties from the U.S. Census Bureau.

### Key Findings from the Study

1.	Black, Hispanic, and socioeconomically marginalized people persistently had disproportionately high exposure to oil and gas development
2.	The widest observed disparities were for Black people residing in neighborhoods with the most intensive oil and gas production
3.	Disparities in exposure to oil and gas development may contribute to previously reported health disparities


### Abstract 

People living near oil and gas development are exposed to multiple environmental stressors that pose health risks. Some studies suggest these risks are higher for racially and socioeconomically marginalized people, which may be partly attributable to disparities in exposures. We examined whether racially and socioeconomically marginalized people in California are disproportionately exposed to oil and gas wells and associated hazards. We longitudinally assessed exposure to wells during three time periods (2005–2009, 2010–2014, and 2015–2019) using sociodemographic data at the census block group-level. For each block group and time period, we assessed exposure to new, active, retired, and plugged wells, and cumulative production volume. We calculated risk ratios to determine whether marginalized people disproportionately resided near wells (within 1 km). Averaged across the three time periods, we estimated that 1.1 million Californians (3.0%) lived within 1 km of active wells. Nearly nine million Californians (22.9%) lived within 1 km of plugged wells. The proportion of Black residents near active wells was 42–49% higher than the proportion of Black residents across California, and the proportion of Hispanic residents near active wells was 4–13% higher than their statewide proportion. Disparities were greatest in areas with the highest oil and gas production, where the proportion of Black residents was 105–139% higher than statewide. Socioeconomically marginalized residents also had disproportionately high exposure to wells. Though oil and gas production has declined in California, marginalized communities persistently had disproportionately high exposure to wells, potentially contributing to health disparities.

### Plain Language Summary

People living near oil and gas wells are exposed to pollutants that may adversely affect their health. We investigated whether racially or socioeconomically marginalized people in California had disproportionately higher exposure to wells. We also assessed whether disparities were wider in areas with more intensive oil and gas production. We examined changes in neighborhood-level sociodemographic characteristics in three time periods: 2005¬–2009, 2010–2014, and 2015–2019. For each neighborhood and time period, we estimated the number of wells and the total volume of oil and gas produced within 1 km (0.62 miles). We estimated that approximately 1.1 million Californians (3.0%) lived within 1 km of active wells between 2005 and 2019. The proportion of Black residents living near active wells was 42–49% higher than the proportion of Black residents across California, and the proportion of Latinx residents near active wells was 4–13% higher than their statewide proportion. For Black people, the disparities were widest in Los Angeles County neighborhoods with the most intensive production. Socioeconomically marginalized people also had disproportionately high exposure. Most disparities persisted throughout the 15-year study period. Black, Hispanic, and socioeconomically marginalized people had disproportionately high exposure to wells, potentially contributing to health disparities.
