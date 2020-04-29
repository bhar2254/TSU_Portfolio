# Blaine Harper
# CASE - Truman State University
# IDS.R
# Automation of Truman's Annual Portfolio Project.

# Load historical data

Faculty.Count.by.Major.Historical <- read_excel("IDS_Historical.xlsx",
                                                sheet = "FacultyCountbyMajor")
# Data cleaning

IDS <- IDS %>% # Add majors data to the IDS data frame. This should be similar for all other sections.
  merge(y = Majors, by.x = c("Last Name","First Name"), by.y = c("Last","First"))  %>% # Merge with the Majors data frame
  mutate_if(is.character, function(x) plyr::revalue(x, c("4 - Strong Competence"= 4, # Data cleaning
                                                         '3 - Competence' = 3,
                                                         '2 - Minimal Competence' = 2,
                                                         '1 - Weak Competence' = 1,
                                                         '0 - No Competence Demonstrated' = 0))) %>%
  mutate(`IDS Reviewer 1` = as.numeric(`IDS Reviewer 1`),
         `IDS Reviewer 2` = as.numeric(`IDS Reviewer 2`)) %>%
  mutate(Difference = abs(`IDS Reviewer 1` - `IDS Reviewer 2`)) %>%
  portfolio_build_major()

# IDS Tables

##### Faculty Count by Major

Faculty.Count.by.Major <- bind_rows(
  IDS %>% group_by(School) %>%
    filter(!is.na(Majr1), !is.na(`IDS Reviewer 1`), Majr1 != 'IDSM') %>%
    summarize(`Majr1` = "TOTAL",
              `N2019` = n(),
              `Mean2019` = round(mean(`IDS Reviewer 1`), 2), 
              `2019` = round(length(`IDS Reviewer 1`[`IDS Reviewer 1` > 1]) / n() * 100, 0)),
  IDS %>% group_by(School, `Majr1`) %>%
    filter(!is.na(Majr1), !is.na(`IDS Reviewer 1`)) %>%
    summarize(`N2019` = n(), 
              `Mean2019` = round(mean(`IDS Reviewer 1`), 2),
              `2019` = round(length(`IDS Reviewer 1`[`IDS Reviewer 1` > 1]) / n() * 100, 0)),
  IDS %>%
    filter(!is.na(Majr1), !is.na(`IDS Reviewer 1`)) %>%
    summarize('School' = "ALL", 
              'Majr1' = "ALL",
              `N2019` = n(), 
              `Mean2019` = round(mean(`IDS Reviewer 1`), 2),
              `2019` = round(length(`IDS Reviewer 1`[`IDS Reviewer 1` > 1]) / n() * 100, 0)))

Faculty.Count.by.Major <- Faculty.Count.by.Major %>% # Used for sorting the data %>%
  merge(y = Faculty.Count.by.Major.Historical, by = c("School", "Majr1"), all = T) %>%
  mutate(School = case_when(
    School == 'ALL' ~ ' ', School == 'IDSM' ~ ' ',
    TRUE ~ as.character(School)
  )) %>%
  arrange(School, Majr1) %>%
  rename(Major = Majr1)

# Create the 'Faculty Count by Major' table from 'IDS Majors.xlsx'
# The numbers here appear to contradict those numbers from the excel report
# This is because excel automatically removes NAs while the n remains constant
# This can be seen as the N for AGSC from this command is 41 while the excel version is 42

##### Inter-rater Reliability

diff.length <- length(IDS$Difference[!is.na(IDS$Difference)])
Inter.rater.Reliability <- rbind(IDS %>% group_by(Difference) %>%
  filter(!is.na(Difference)) %>%
  filter(Difference <= 4) %>% # This will make sure any errors are not 
  # accounted for as the diff cannot be greater than 4
  summarise(N = n(), `%` = round(((N / diff.length) * 100),0)) %>%
  arrange(-Difference),
  IDS %>% filter(!is.na(Difference)) %>%
    filter(Difference <= 4) %>%
    summarise(Difference = "Total",
              N = n(),
              '%' = 100))

##### Yearly Scores by Data

Yearly.Scores.by.Prefix <- IDS %>% group_by(`IDS-COUR Discipline`) %>%
  filter(!is.na(`IDS Reviewer 1`), !is.na(`IDS-COUR Discipline`)) %>%
  summarise(N = n(),
            Mean = round(mean(`IDS Reviewer 1`),2),
            '2+' = round(length(`IDS Reviewer 1`[`IDS Reviewer 1` > 1]) / n() * 100, 0)) %>%
  arrange(-N)

Yearly.Scores.by.Prefix.Big <- Yearly.Scores.by.Prefix %>%
  filter(N >= 5)
Yearly.Scores.by.Prefix.Small <- Yearly.Scores.by.Prefix %>%
  filter(N < 5)

##### Mean Scores by Major
# Relatively similar to Faculty.Count.by.Major, but much simpler.

Mean.Scores.by.Major <-  bind_rows(
  IDS %>% group_by(School) %>%
    filter(!is.na(Majr1), !is.na(`IDS Reviewer 1`), !is.na(`IDS Reviewer 2`), Majr1 != 'IDSM') %>%
    summarise(Majr1 = "TOTAL",
              N = n(),
              `Score 1` = round(mean(`IDS Reviewer 1`),2),
              `Score 2` = round(mean(`IDS Reviewer 2`),2),
              `Abs Diff` = round(abs(mean(`IDS Reviewer 1`) - mean(`IDS Reviewer 2`)),2)),
  IDS %>% group_by(School,Majr1) %>%
  filter(!is.na(Majr1), !is.na(`IDS Reviewer 1`), !is.na(`IDS Reviewer 2`)) %>%
  summarise(N = n(),
            `Score 1` = round(mean(`IDS Reviewer 1`),2),
            `Score 2` = round(mean(`IDS Reviewer 2`),2),
            `Abs Diff` = round(abs(mean(`IDS Reviewer 1`) - mean(`IDS Reviewer 2`)),2)),
  IDS %>%
    filter(!is.na(Majr1), !is.na(`IDS Reviewer 1`), !is.na(`IDS Reviewer 2`)) %>%
    summarize('School' = "ALL", 
              'Majr1' = "ALL",
              N = n(),
              `Score 1` = round(mean(`IDS Reviewer 1`),2),
              `Score 2` = round(mean(`IDS Reviewer 2`),2),
              `Abs Diff` = round(abs(mean(`IDS Reviewer 1`) - mean(`IDS Reviewer 2`)),2))
)

Mean.Scores.by.Major <- Mean.Scores.by.Major %>%
  mutate(School = case_when(
    School == "ALL" | School == "IDSM" ~ " ",
    TRUE ~ as.character(School)
  )) %>%
  arrange(School, Majr1)

##### Make a list of kables for final representation

IDS_Kables <- list(Faculty.Count.by.Major = kable(Faculty.Count.by.Major[,c(1,2,3,6,7,8,9,4,10,11,12,13,5)], align = "c", # Rearrange the rows
                                col.names = c("", "Major","N 2019",rep(c(seq(2015:2019)),2))) %>%
                     kable_styling(full_width = F, bootstrap_options = c("condensed", "bordered")) %>%
                     column_spec(1, extra_css = "white-space:nowrap; font-weight: bold; -webkit-transform: rotate(-90.0deg);") %>%
                     collapse_rows(columns = 1, valign = "middle") %>%
                     add_header_above(c(" " = 3, "Mean" = 5, "2+" = 5)), 
                   Inter.rater.Reliability = kable(Inter.rater.Reliability, align = "c", label = "Test") %>%
                     kable_styling(full_width = F, bootstrap_options = c("condensed", "bordered")) %>%
                     column_spec(1, bold = T), 
                   Yearly.Scores.by.Prefix = kable_template(Yearly.Scores.by.Prefix.Big, type = 'basic', 
                                         col.names = c("Course Prefix","N","Mean","2+ (%)")), 
                   Mean.Scores.by.Major = kable_template(Mean.Scores.by.Major, 
                                         'rotated', 
                                         col.names = c("School","Major","N","Score 1","Score 2","Abs Diff"))) 

rm(Faculty.Count.by.Major,
               Inter.rater.Reliability,
               Yearly.Scores.by.Prefix,
               Mean.Scores.by.Major,
               Yearly.Scores.by.Prefix.Small,
               Yearly.Scores.by.Prefix.Big,
               diff.length,
               Faculty.Count.by.Major.Historical)
