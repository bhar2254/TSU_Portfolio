# Blaine Harper
# CASE - Truman State University
# CTW.R
# Automation of Truman's Annual Portfolio Project.

# CTW Tables

CTW <- CTW %>% # Add majors data to the CTW data frame. This should be similar for all other sections.
  merge(y = Majors, by.x = c("Last Name","First Name"), by.y = c("Last","First"))  %>% # Merge with the Majors data frame
  mutate_if(is.character, function(x) plyr::revalue(x, c("4 - Mastering" = 4, # Data cleaning
                                                         "4 - Mastery" = 4,
                                                         '3 - Developing' = 3,
                                                         '2 - Growing' = 2,
                                                         '1 - Emerging' = 1,
                                                         "4- Mastering"= 4, # Data cleaning
                                                         "4- Mastery" = 4,
                                                         '3- Developing' = 3,
                                                         '2- Growing' = 2,
                                                         '1- Emerging' = 1))) %>%
  mutate(`CTW13-ISS Reviewer 1` = as.numeric(`CTW13-ISS Reviewer 1`), # There is probably a cleaner way to do this.
         `CTW13-CONT Reviewer 1` = as.numeric(`CTW13-CONT Reviewer 1`),
         `CTW13-EVD Reviewer 1` = as.numeric(`CTW13-EVD Reviewer 1`),
         `CTW13-CONCL Reviewer 1` = as.numeric(`CTW13-CONCL Reviewer 1`),
         `CTW13-COMM Reviewer 1` = as.numeric(`CTW13-COMM Reviewer 1`),
         `CTW13-ISS Reviewer 2` = as.numeric(`CTW13-ISS Reviewer 2`),
         `CTW13-CONT Reviewer 2` = as.numeric(`CTW13-CONT Reviewer 2`),
         `CTW13-EVD Reviewer 2` = as.numeric(`CTW13-EVD Reviewer 2`),
         `CTW13-CONCL Reviewer 2` = as.numeric(`CTW13-CONCL Reviewer 2`),
         `CTW13-COMM Reviewer 2` = as.numeric(`CTW13-COMM Reviewer 2`)) %>%
  mutate(`CTW13-SUM-4` = `CTW13-ISS Reviewer 1` + `CTW13-CONT Reviewer 1` + `CTW13-EVD Reviewer 1` + `CTW13-CONCL Reviewer 1`) %>%
  mutate(`CTW13-SUM-4-2` = `CTW13-ISS Reviewer 2` + `CTW13-CONT Reviewer 2` + `CTW13-EVD Reviewer 2` + `CTW13-CONCL Reviewer 2`) %>%
  mutate("Abs Diff" = as.character(abs(`CTW13-SUM-4` - `CTW13-SUM-4-2`))) %>%
  portfolio_build_major()

##### First.Major

First.Major <- bind_rows(
  CTW %>% group_by(School, Majr1) %>%
    summarise(N = n()),
  CTW %>% group_by(School) %>%
    filter(Majr1 != 'IDSM') %>%
    summarise(Majr1 = "TOTAL",
              N = n()),
  CTW %>%
    summarise(School = "ALL",
              Majr1 = "ALL",
              N = n())
) %>% ungroup() %>% # Do some clean up before sending to kable
  mutate(School = case_when(
    School == 'ALL' | School == 'IDSM' ~ ' ',
    TRUE ~ as.character(School)
  )) %>%
  merge(read_excel("CTW_Historical.xlsx"),.) %>%
  arrange(School, Majr1) %>%
  rename(Major = Majr1) %>%
  rename(`2019` = N)

##### Final.Major

Final.Major <- bind_rows( # This is essentially a copy of the IDS Faculty.Count.by.Major
  CTW %>% group_by(School, Majr1) %>%
    filter(!is.na(`CTW13-ISS Reviewer 1`), !is.na(`CTW13-CONT Reviewer 1`), 
         !is.na(`CTW13-EVD Reviewer 1`), !is.na(`CTW13-CONCL Reviewer 1`), !is.na(`CTW13-COMM Reviewer 1`)) %>%
    summarise(N = n(),
            Issue = round(mean(`CTW13-ISS Reviewer 1`), 2),
            Context = round(mean(`CTW13-CONT Reviewer 1`), 2),
            Evidence = round(mean(`CTW13-EVD Reviewer 1`), 2),
            Concl = round(mean(`CTW13-CONCL Reviewer 1`), 2),
            `Sum 4` = round(mean(`CTW13-SUM-4`),2),
            `10+ (%)` = round(length(`CTW13-SUM-4`[`CTW13-SUM-4` > 10]) / n() * 100, 0),
            Comm = round(mean(`CTW13-COMM Reviewer 1`), 2)),
  CTW %>% group_by(School) %>%
    filter(Majr1 != 'IDSM', !is.na(`CTW13-ISS Reviewer 1`), !is.na(`CTW13-CONT Reviewer 1`), 
           !is.na(`CTW13-EVD Reviewer 1`), !is.na(`CTW13-CONCL Reviewer 1`), !is.na(`CTW13-COMM Reviewer 1`)) %>%
    summarise(Majr1 = "TOTAL",
              N = n(),
              Issue = round(mean(`CTW13-ISS Reviewer 1`), 2),
              Context = round(mean(`CTW13-CONT Reviewer 1`), 2),
              Evidence = round(mean(`CTW13-EVD Reviewer 1`), 2),
              Concl = round(mean(`CTW13-CONCL Reviewer 1`), 2),
              `Sum 4` = round(mean(`CTW13-SUM-4`),2),
              `10+ (%)` = round(length(`CTW13-SUM-4`[`CTW13-SUM-4` > 10]) / n() * 100, 0),
              Comm = round(mean(`CTW13-COMM Reviewer 1`), 2)),
  CTW %>% 
    filter(!is.na(`CTW13-ISS Reviewer 1`), !is.na(`CTW13-CONT Reviewer 1`), 
           !is.na(`CTW13-EVD Reviewer 1`), !is.na(`CTW13-CONCL Reviewer 1`), !is.na(`CTW13-COMM Reviewer 1`)) %>%
    summarise(School = "ALL",
              Majr1 = "ALL",
              N = n(),
              Issue = round(mean(`CTW13-ISS Reviewer 1`), 2),
              Context = round(mean(`CTW13-CONT Reviewer 1`), 2),
              Evidence = round(mean(`CTW13-EVD Reviewer 1`), 2),
              Concl = round(mean(`CTW13-CONCL Reviewer 1`), 2),
              `Sum 4` = round(mean(`CTW13-SUM-4`),2),
              `10+ (%)` = round(length(`CTW13-SUM-4`[`CTW13-SUM-4` > 10]) / n() * 100, 0),
              Comm = round(mean(`CTW13-COMM Reviewer 1`), 2))
)

Final.Major <- Final.Major %>% ungroup() %>% # Do some clean up before sending to kable
  mutate(School = case_when(
    School == 'ALL' | School == 'IDSM' ~ ' ',
    TRUE ~ as.character(School)
  )) %>%
  arrange(School, Majr1) %>%
  rename(Major = Majr1)

##### Final.Prefix

Final.Prefix <- bind_rows( # This is essentially a copy of the IDS Faculty.Count.by.Major
  CTW %>% group_by(`CTW13-COUR Discipline`) %>%
    filter(n() > 5) %>%
    filter(!is.na(`CTW13-COUR Discipline`), !is.na(`CTW13-ISS Reviewer 1`), !is.na(`CTW13-CONT Reviewer 1`),
           !is.na(`CTW13-EVD Reviewer 1`), !is.na(`CTW13-CONCL Reviewer 1`), !is.na(`CTW13-COMM Reviewer 1`)) %>%
    summarise(N = n(),
              Issue = round(mean(`CTW13-ISS Reviewer 1`), 2),
              Context = round(mean(`CTW13-CONT Reviewer 1`), 2),
              Evidence = round(mean(`CTW13-EVD Reviewer 1`), 2),
              Concl = round(mean(`CTW13-CONCL Reviewer 1`), 2),
              `Sum 4` = round(mean(`CTW13-SUM-4`),2),
              `10+ (%)` = round(length(`CTW13-SUM-4`[`CTW13-SUM-4` > 10]) / n() * 100, 0),
              Comm = round(mean(`CTW13-COMM Reviewer 1`), 2)) %>%
    ungroup() %>%
    bind_rows(
      CTW %>%
        filter(!is.na(`CTW13-COUR Discipline`), !is.na(`CTW13-ISS Reviewer 1`), !is.na(`CTW13-CONT Reviewer 1`), 
               !is.na(`CTW13-EVD Reviewer 1`), !is.na(`CTW13-CONCL Reviewer 1`), !is.na(`CTW13-COMM Reviewer 1`)) %>%
        summarise(`CTW13-COUR Discipline` = "ALL",
                  N = n(),
                  Issue = round(mean(`CTW13-ISS Reviewer 1`), 2),
                  Context = round(mean(`CTW13-CONT Reviewer 1`), 2),
                  Evidence = round(mean(`CTW13-EVD Reviewer 1`), 2),
                  Concl = round(mean(`CTW13-CONCL Reviewer 1`), 2),
                  `Sum 4` = round(mean(`CTW13-SUM-4`),2),
                  `10+ (%)` = round(length(`CTW13-SUM-4`[`CTW13-SUM-4` > 10]) / n() * 100, 0),
                  Comm = round(mean(`CTW13-COMM Reviewer 1`), 2))
    )
)

Final.Prefix <- Final.Prefix %>% ungroup() %>% # Do some clean up before sending to kable
  rename(Prefix = `CTW13-COUR Discipline`)

# Inter-rater Reliability

diff.length <- length(CTW$`Abs Diff`[!is.na(CTW$`Abs Diff`)])
Inter.rater.Reliability <- CTW %>% group_by(`Abs Diff`) %>%
  filter(!is.na(`Abs Diff`)) %>%
  summarise(N = n(),
            `%` = round(((N / diff.length) * 100),0)) %>%
  arrange(desc(`Abs Diff`)) %>%
  ungroup() %>%
  bind_rows(
    CTW %>% filter(!is.na(`Abs Diff`)) %>%
      summarise(`Abs Diff` = "TOTAL",
                N = diff.length,
                `%` = 100
                )
    )

##### Faculty Score

CTW <- CTW %>%
  filter(!is.na(`CTW13-ISS Reviewer 1`), !is.na(`CTW13-CONCL Reviewer 1`))

Faculty.Score <- bind_rows(
  CTW %>% group_by(School) %>%
    filter(!is.na(Majr1), Majr1 != 'IDSM') %>%
    summarize(`Majr1` = "TOTAL",
              ISS1 = sum(ifelse(`CTW13-ISS Reviewer 1` == 1,1,0)),
              ISS2 = sum(ifelse(`CTW13-ISS Reviewer 1` == 2,1,0)),
              ISS3 = sum(ifelse(`CTW13-ISS Reviewer 1` == 3,1,0)),
              ISS4 = sum(ifelse(`CTW13-ISS Reviewer 1` == 4,1,0)),
              ISSAvg = round(mean(`CTW13-ISS Reviewer 1`),2),
              CONT1 = sum(ifelse(`CTW13-CONT Reviewer 1` == 1,1,0)),
              CONT2 = sum(ifelse(`CTW13-CONT Reviewer 1` == 2,1,0)),
              CONT3 = sum(ifelse(`CTW13-CONT Reviewer 1` == 3,1,0)),
              CONT4 = sum(ifelse(`CTW13-CONT Reviewer 1` == 4,1,0)),
              CONTAvg = round(mean(`CTW13-CONT Reviewer 1`),2),
              EVD1 = sum(ifelse(`CTW13-EVD Reviewer 1` == 1,1,0)),
              EVD2 = sum(ifelse(`CTW13-EVD Reviewer 1` == 2,1,0)),
              EVD3 = sum(ifelse(`CTW13-EVD Reviewer 1` == 3,1,0)),
              EVD4 = sum(ifelse(`CTW13-EVD Reviewer 1` == 4,1,0)),
              EVDAvg = round(mean(`CTW13-EVD Reviewer 1`),2),
              CONCL1 = sum(ifelse(`CTW13-CONCL Reviewer 1` == 1,1,0)),
              CONCL2 = sum(ifelse(`CTW13-CONCL Reviewer 1` == 2,1,0)),
              CONCL3 = sum(ifelse(`CTW13-CONCL Reviewer 1` == 3,1,0)),
              CONCL4 = sum(ifelse(`CTW13-CONCL Reviewer 1` == 4,1,0)),
              CONCLAvg = round(mean(`CTW13-CONCL Reviewer 1`),2),
              COMM1 = sum(ifelse(`CTW13-COMM Reviewer 1` == 1,1,0)),
              COMM2 = sum(ifelse(`CTW13-COMM Reviewer 1` == 2,1,0)),
              COMM3 = sum(ifelse(`CTW13-COMM Reviewer 1` == 3,1,0)),
              COMM4 = sum(ifelse(`CTW13-COMM Reviewer 1` == 4,1,0)),
              COMMAvg = round(mean(`CTW13-COMM Reviewer 1`),2)),
  CTW %>% group_by(School, `Majr1`) %>%
    filter(!is.na(Majr1)) %>%
    summarize(ISS1 = sum(ifelse(`CTW13-ISS Reviewer 1` == 1,1,0)),
              ISS2 = sum(ifelse(`CTW13-ISS Reviewer 1` == 2,1,0)),
              ISS3 = sum(ifelse(`CTW13-ISS Reviewer 1` == 3,1,0)),
              ISS4 = sum(ifelse(`CTW13-ISS Reviewer 1` == 4,1,0)),
              ISSAvg = round(mean(`CTW13-ISS Reviewer 1`),2),
              CONT1 = sum(ifelse(`CTW13-CONT Reviewer 1` == 1,1,0)),
              CONT2 = sum(ifelse(`CTW13-CONT Reviewer 1` == 2,1,0)),
              CONT3 = sum(ifelse(`CTW13-CONT Reviewer 1` == 3,1,0)),
              CONT4 = sum(ifelse(`CTW13-CONT Reviewer 1` == 4,1,0)),
              CONTAvg = round(mean(`CTW13-CONT Reviewer 1`),2),
              EVD1 = sum(ifelse(`CTW13-EVD Reviewer 1` == 1,1,0)),
              EVD2 = sum(ifelse(`CTW13-EVD Reviewer 1` == 2,1,0)),
              EVD3 = sum(ifelse(`CTW13-EVD Reviewer 1` == 3,1,0)),
              EVD4 = sum(ifelse(`CTW13-EVD Reviewer 1` == 4,1,0)),
              EVDAvg = round(mean(`CTW13-EVD Reviewer 1`),2),
              CONCL1 = sum(ifelse(`CTW13-CONCL Reviewer 1` == 1,1,0)),
              CONCL2 = sum(ifelse(`CTW13-CONCL Reviewer 1` == 2,1,0)),
              CONCL3 = sum(ifelse(`CTW13-CONCL Reviewer 1` == 3,1,0)),
              CONCL4 = sum(ifelse(`CTW13-CONCL Reviewer 1` == 4,1,0)),
              CONCLAvg = round(mean(`CTW13-CONCL Reviewer 1`),2),
              COMM1 = sum(ifelse(`CTW13-COMM Reviewer 1` == 1,1,0)),
              COMM2 = sum(ifelse(`CTW13-COMM Reviewer 1` == 2,1,0)),
              COMM3 = sum(ifelse(`CTW13-COMM Reviewer 1` == 3,1,0)),
              COMM4 = sum(ifelse(`CTW13-COMM Reviewer 1` == 4,1,0)),
              COMMAvg = round(mean(`CTW13-COMM Reviewer 1`),2)),
  CTW %>%
    filter(!is.na(Majr1)) %>%
    summarize('School' = "ALL", 
              'Majr1' = "ALL",
              ISS1 = sum(ifelse(`CTW13-ISS Reviewer 1` == 1,1,0)),
              ISS2 = sum(ifelse(`CTW13-ISS Reviewer 1` == 2,1,0)),
              ISS3 = sum(ifelse(`CTW13-ISS Reviewer 1` == 3,1,0)),
              ISS4 = sum(ifelse(`CTW13-ISS Reviewer 1` == 4,1,0)),
              ISSAvg = round(mean(`CTW13-ISS Reviewer 1`),2),
              CONT1 = sum(ifelse(`CTW13-CONT Reviewer 1` == 1,1,0)),
              CONT2 = sum(ifelse(`CTW13-CONT Reviewer 1` == 2,1,0)),
              CONT3 = sum(ifelse(`CTW13-CONT Reviewer 1` == 3,1,0)),
              CONT4 = sum(ifelse(`CTW13-CONT Reviewer 1` == 4,1,0)),
              CONTAvg = round(mean(`CTW13-CONT Reviewer 1`),2),
              EVD1 = sum(ifelse(`CTW13-EVD Reviewer 1` == 1,1,0)),
              EVD2 = sum(ifelse(`CTW13-EVD Reviewer 1` == 2,1,0)),
              EVD3 = sum(ifelse(`CTW13-EVD Reviewer 1` == 3,1,0)),
              EVD4 = sum(ifelse(`CTW13-EVD Reviewer 1` == 4,1,0)),
              EVDAvg = round(mean(`CTW13-EVD Reviewer 1`),2),
              CONCL1 = sum(ifelse(`CTW13-CONCL Reviewer 1` == 1,1,0)),
              CONCL2 = sum(ifelse(`CTW13-CONCL Reviewer 1` == 2,1,0)),
              CONCL3 = sum(ifelse(`CTW13-CONCL Reviewer 1` == 3,1,0)),
              CONCL4 = sum(ifelse(`CTW13-CONCL Reviewer 1` == 4,1,0)),
              CONCLAvg = round(mean(`CTW13-CONCL Reviewer 1`),2),
              COMM1 = sum(ifelse(`CTW13-COMM Reviewer 1` == 1,1,0)),
              COMM2 = sum(ifelse(`CTW13-COMM Reviewer 1` == 2,1,0)),
              COMM3 = sum(ifelse(`CTW13-COMM Reviewer 1` == 3,1,0)),
              COMM4 = sum(ifelse(`CTW13-COMM Reviewer 1` == 4,1,0)),
              COMMAvg = round(mean(`CTW13-COMM Reviewer 1`),2))
)

Faculty.Score <- Faculty.Score %>% # Used for sorting the data %>%
  mutate(School = case_when(
    School == 'ALL' ~ ' ', School == 'IDSM' ~ ' ',
    TRUE ~ as.character(School)
  )) %>%
  arrange(School, Majr1) %>%
  rename(Major = Majr1)

# Course.Prefix.Score 

Course.Prefix.Score <- CTW %>% group_by(`CTW13-COUR Discipline`) %>%
    filter(!is.na(`CTW13-COUR Discipline`)) %>%
    summarize(`N 2019` = n(),
              ISS1 = sum(ifelse(`CTW13-ISS Reviewer 1` == 1,1,0)),
              ISS2 = sum(ifelse(`CTW13-ISS Reviewer 1` == 2,1,0)),
              ISS3 = sum(ifelse(`CTW13-ISS Reviewer 1` == 3,1,0)),
              ISS4 = sum(ifelse(`CTW13-ISS Reviewer 1` == 4,1,0)),
              ISSAvg = round(mean(`CTW13-ISS Reviewer 1`),2),
              CONT1 = sum(ifelse(`CTW13-CONT Reviewer 1` == 1,1,0)),
              CONT2 = sum(ifelse(`CTW13-CONT Reviewer 1` == 2,1,0)),
              CONT3 = sum(ifelse(`CTW13-CONT Reviewer 1` == 3,1,0)),
              CONT4 = sum(ifelse(`CTW13-CONT Reviewer 1` == 4,1,0)),
              CONTAvg = round(mean(`CTW13-CONT Reviewer 1`),2),
              EVD1 = sum(ifelse(`CTW13-EVD Reviewer 1` == 1,1,0)),
              EVD2 = sum(ifelse(`CTW13-EVD Reviewer 1` == 2,1,0)),
              EVD3 = sum(ifelse(`CTW13-EVD Reviewer 1` == 3,1,0)),
              EVD4 = sum(ifelse(`CTW13-EVD Reviewer 1` == 4,1,0)),
              EVDAvg = round(mean(`CTW13-EVD Reviewer 1`),2),
              CONCL1 = sum(ifelse(`CTW13-CONCL Reviewer 1` == 1,1,0)),
              CONCL2 = sum(ifelse(`CTW13-CONCL Reviewer 1` == 2,1,0)),
              CONCL3 = sum(ifelse(`CTW13-CONCL Reviewer 1` == 3,1,0)),
              CONCL4 = sum(ifelse(`CTW13-CONCL Reviewer 1` == 4,1,0)),
              CONCLAvg = round(mean(`CTW13-CONCL Reviewer 1`),2),
              COMM1 = sum(ifelse(`CTW13-COMM Reviewer 1` == 1,1,0)),
              COMM2 = sum(ifelse(`CTW13-COMM Reviewer 1` == 2,1,0)),
              COMM3 = sum(ifelse(`CTW13-COMM Reviewer 1` == 3,1,0)),
              COMM4 = sum(ifelse(`CTW13-COMM Reviewer 1` == 4,1,0)),
              COMMAvg = round(mean(`CTW13-COMM Reviewer 1`),2))

Course.Prefix.Score <- Course.Prefix.Score %>% # Used for sorting the data %>%
  filter(`N 2019` >= 5) %>%
  arrange(-`N 2019`)

CTW_Kables <- list(First.Major = kable_template(First.Major, type = "rotated"),
                   Final.Major = kable_template(Final.Major, type = 'rotated'),
                   Final.Prefix = kable_template(Final.Prefix, type = 'basic'),
                   Inter.rater.Reliability = kable_template(Inter.rater.Reliability, type = 'basic'),
                   Faculty.Score = kable(Faculty.Score, align = "c", # Rearrange the rows
                                         col.names = c("", "Major",rep(c(seq(1:4),"Avg"),5))) %>%
                     kable_styling(full_width = F, bootstrap_options = c("condensed", "bordered")) %>%
                     column_spec(1, extra_css = "white-space:nowrap; font-weight: bold; -webkit-transform: rotate(-90.0deg);") %>%
                     collapse_rows(columns = 1, valign = "middle") %>%
                     add_header_above(c(" " = 2, "Issue" = 5, "Content" = 5, "Evidence" = 5, "Conclsion" = 5, "Communication" = 5)),
                   Course.Prefix.Score = kable(Course.Prefix.Score, align = "c", # Rearrange the rows
                                               col.names = c("","N 2019",rep(c(seq(1:4),"Avg"),5))) %>%
                     kable_styling(full_width = F, bootstrap_options = c("condensed", "bordered")) %>%
                     column_spec(1, extra_css = "white-space:nowrap; font-weight: bold;") %>%
                     collapse_rows(columns = 1, valign = "middle") %>%
                     add_header_above(c(" " = 2, "Issue" = 5, "Content" = 5, "Evidence" = 5, "Conclsion" = 5, "Communication" = 5))) # Create a final list of tables

rm(Final.Major,Final.Prefix,Inter.rater.Reliability,Faculty.Score,Course.Prefix.Score,First.Major)
