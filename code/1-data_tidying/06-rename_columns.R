##============================================================================##
## 1.05 - Renames columns from detailed table column ID to name

columnRename <- function (data) {
  data <- data %>%
  rename("Total Population" = "B01003_001",
         "Race Universe" = "B02001_001", # same as Total Population(to be deleted)
         "White Alone" = "B02001_002",
         "Black or African American Alone" = "B02001_003",
         "American Indian and Alaska Native Alone" = "B02001_004",
         "Asian Alone" = "B02001_005",
         "Native Hawaiian and Other Pacific Islander Alone" = "B02001_006",
         "Some Other Race Alone" = "B02001_007",
         "Two or More Races" = "B02001_008",
         "Two Races Including Some Other Race" = "B02001_009",
         "Two Races Excluding Some Other Race, And Three or More Races" = "B02001_010",
         
         "Hispanic or Latino Universe" = "B03003_001", # same as Total Population(to be deleted)
         "Not Hispanic or Latino" = "B03003_002",
         "Hispanic or Latino" = "B03003_003",
         
         "Race Ethnicity Universe" = "B03002_001", # same as Total Population (to be deleted)
         "Not Hispanic or Latino Total" = "B03002_002", #same as not hispanic or latino (to be deleted)
         "Not Hispanic or Latino: White Alone" = "B03002_003",
         "Not Hispanic or Latino: Black or African American Alone" = "B03002_004",
         "Not Hispanic or Latino: American Indian and Alaska Native Alone" = "B03002_005",
         "Not Hispanic or Latino: Asian Alone" = "B03002_006",
         "Not Hispanic or Latino: Native Hawaiian and Other Pacific Islander Alone" = "B03002_007",
         "Not Hispanic or Latino: Some Other Race Alone" = "B03002_008",
         "Not Hispanic or Latino: Two or More Races" = "B03002_009",
         "Not Hispanic or Latino: Two Races Including Some Other Race" = "B03002_010",
         "Not Hispanic or Latino: Two Races Excluding Some Other Race, And Three or More Races" = "B03002_011",
         "Hispanic or Latino Total" = "B03002_012", #same as hispanic or latino (to be deleted)
         "Hispanic or Latino: White Alone" = "B03002_013",
         "Hispanic or Latino: Black or African American Alone" = "B03002_014",
         "Hispanic or Latino: American Indian and Alaska Native Alone" = "B03002_015",
         "Hispanic or Latino: Asian Alone" = "B03002_016",
         "Hispanic or Latino: Native Hawaiian and Other Pacific Islander Alone" = "B03002_017",
         "Hispanic or Latino: Some Other Race Alone" = "B03002_018",
         "Hispanic or Latino: Two or More Races" = "B03002_019",
         "Hispanic or Latino: Two Races Including Some Other Race" = "B03002_020",
         "Hispanic or Latino: Two Races Excluding Some Other Race, And Three or More Races" = "B03002_021",
         
         "Education Universe" = "B15002_001", # NOT SAME as Total Population(25 years and over only)
         "Male Education Universe" = "B15002_002",
         "Male No Schooling Completed" = "B15002_003",
         "Male Nursery to 4th Grade" = "B15002_004",
         "Male 5th and 6th Grade" = "B15002_005",
         "Male 7th and 8th Grade" = "B15002_006",
         "Male 9th Grade" = "B15002_007",
         "Male 10th Grade" = "B15002_008",
         "Male 11th Grade" = "B15002_009",
         "Male 12th Grade, No Diploma" = "B15002_010",
         "Male High School Graduate" = "B15002_011",
         "Male Some College, Less Than 1 Year" = "B15002_012",
         "Male Some College, 1 or More Years, No Degree" = "B15002_013",
         "Male Associate's Degree" = "B15002_014",
         "Male Bachelor's Degree" = "B15002_015",
         "Male Master's Degree" = "B15002_016",
         "Male Professional School Degree" = "B15002_017",
         "Male Doctorate Degree" = "B15002_018",
         "Female Education Universe" = "B15002_019",
         "Female No Schooling Completed" = "B15002_020",
         "Female Nursery to 4th Grade" = "B15002_021",
         "Female 5th and 6th Grade" = "B15002_022",
         "Female 7th and 8th Grade" = "B15002_023",
         "Female 9th Grade" = "B15002_024",
         "Female 10th Grade" = "B15002_025",
         "Female 11th Grade" = "B15002_026",
         "Female 12th Grade, No Diploma" = "B15002_027",
         "Female High School Graduate" = "B15002_028",
         "Female Some College, Less Than 1 Year" = "B15002_029",
         "Female Some College, 1 or More Years, No Degree" = "B15002_030",
         "Female Associate's Degree" = "B15002_031",
         "Female Bachelor's Degree" = "B15002_032",
         "Female Master's Degree" = "B15002_033",
         "Female Professional School Degree" = "B15002_034",
         "Female Doctorate Degree" = "B15002_035",
         
         "Household Poverty Universe" = "B17017_001",
         "Income in Past 12 Months Below Poverty Level" = "B17017_002",
         "Income in Past 12 Months at or Above Poverty Level" = "B17017_031",
         
         "Household Income Universe" = "B19001_001",
         "Less than $10,000" = "B19001_002",
         "$10,000 to $14,999" = "B19001_003",
         "$15,000 to $19,999" = "B19001_004",
         "$20,000 to $24,999" = "B19001_005",
         "$25,000 to $29,999" = "B19001_006",
         "$30,000 to $34,999" = "B19001_007",
         "$35,000 to $39,999" = "B19001_008",
         "$40,000 to $44,999" = "B19001_009",
         "$45,000 to $49,999" = "B19001_010",
         "$50,000 to $59,999" = "B19001_011",
         "$60,000 to $74,999" = "B19001_012",
         "$75,000 to $99,999" = "B19001_013",
         "$100,000 to $124,999" = "B19001_014",
         "$125,000 to $149,999" = "B19001_015",
         "$150,000 to $199,999" = "B19001_016",
         "$200,000 or more" = "B19001_017",
         
         "Median Contract Rent" = "B25058_001",
         
         "Renter-Occupied Housing Units Universe" = "B25070_001",
         "Gross Rent Less than 10.0 Percent of Household Income" = "B25070_002",
         "Gross Rent 10.0 to 14.9 Percent of Household Income" = "B25070_003",
         "Gross Rent 15.0 to 19.9 Percent of Household Income" = "B25070_004",
         "Gross Rent 20.0 to 24.9 Percent of Household Income" = "B25070_005",
         "Gross Rent 25.0 to 29.9 Percent of Household Income" = "B25070_006",
         "Gross Rent 30.0 to 34.9 Percent of Household Income" = "B25070_007",
         "Gross Rent 35.0 to 39.9 Percent of Household Income" = "B25070_008",
         "Gross Rent 40.0 to 49.9 Percent of Household Income" = "B25070_009",
         "Gross Rent 50.0 Percent or More of Household Income" = "B25070_010",
         "Gross Rent Percent of Household Income Not Computed" = "B25070_011") %>%
  dplyr::select(-c(76:103, 105:132, "Race Universe", "Hispanic or Latino Universe",
                   "Race Ethnicity Universe",  "Not Hispanic or Latino Total",
                   "Hispanic or Latino Total")) %>%
  mutate(across(c(4:100), as.numeric))
data <- data %>%
  mutate("Less Than High School Diploma" =
           rowSums(data[,c(36:43, 53:60)]),
         "High School Diploma" = rowSums(data[,c(44, 61)]),
         "Some College" = rowSums(data[,c(45, 46, 62, 63)]),
         "Associate's Degree" = rowSums(data[,c(47, 64)]),
         "Bachelor's Degree" = rowSums(data[,c(48, 65)]),
         "Master's Degree" = rowSums(data[,c(49, 66)]),
         "Professional School Degree" = rowSums(data[,c(50, 67)]),
         "Doctorate Degree" = rowSums(data[,c(51, 68)])) %>%
  dplyr::select(-c(35:68)) %>%
  select(c(1:34), c(67:74), everything())# reorders columns to put new education columns in correct place
data <- data %>%
  mutate("Household Income <$25,000" = rowSums(data[,c(47, 48, 49, 50)]),
         "Household Income >$100,000" = rowSums(data[,c(59, 60, 61, 62)]),
         "Household Income Universe" = rowSums(data[,c(47:62)]))
data <- data %>%
  select(c(1:46), c(75:76), c(63:74))

  return(data)
}

##============================================================================##