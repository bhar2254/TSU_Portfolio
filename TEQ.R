# Blaine Harper
# CASE - Truman State University
# TEQ.R
# Automation of Truman's Annual Portfolio Project.

# TEQ Tables

TEQ <- portfolio_data("TEQ") %>% # copied from everywhere else
  merge(y = Majors, by.x = c("Last Name","First Name"), by.y = c("Last","First"), all=T)  %>% # Merge with the Majors data frame
  portfolio_build_major() %>%
  mutate(`TRQ-COURSE` = case_when(
    is.na(`TRQ-HOW-1...25`) & is.na(`TRQ-HOW-2...26`) & is.na(`TRQ-HOW-1...27`) | is.na(`TRQ-HOW-2...28`) | is.na(`TRQ-HOW-3`) ~ "No",
    TRUE ~ "Yes"
  )) %>%
  mutate(LeastBig4 = case_when(
    `TRQ-STUAB` == "Yes" ~ 1,
    `TRQ-SL` == "Yes" ~ 1,
    `TRQ-RSCH` == "Yes" ~ 1,
    `TRQ-INT` == "Yes" ~ 1,
    TRUE ~ 0
  )) %>%
  mutate(Big4 = ifelse(`TRQ-STUAB` == "Yes",1,0) + 
                      ifelse(`TRQ-SL` == "Yes",1,0) + 
                      ifelse(`TRQ-RSCH` == "Yes",1,0) + 
                      ifelse(`TRQ-INT` == "Yes",1,0)
  ) %>%
  mutate(LeastAny = case_when(
    `TRQ-STUAB` == "Yes" ~ 1,
    `TRQ-SL` == "Yes" ~ 1,
    `TRQ-RSCH` == "Yes" ~ 1,
    `TRQ-INT` == "Yes" ~ 1,
    `TRQ-LEAD` == "Yes" ~ 1,
    `TRQ-EDEX` == "Yes" ~ 1,
    `TRQ-WRIT` == "Yes" ~ 1,
    `TRQ-OTH` == "Yes" ~ 1,
    `TRQ-COURSE` == "Yes" ~ 1,
    TRUE ~ 0
  )) %>%
  portfolio_build_gender()

##### 2019 Overall Truman Edu. Transformative

total.n <- TEQ %>% 
  summarise(`TRQ-ED` = "Total",
            N = n(),
            Percent = 100)

Overall.Ed.Transformative <- rbind(TEQ %>% group_by(`TRQ-ED`) %>%
  summarise(N = n(),
            Percent = round(N / total.n$N * 100)),
  total.n
  )

##### Various Activities

Various.Activities <- rbind(TEQ %>%
  summarize(`Study Abroad` = sum(ifelse(`TRQ-STUAB` == "Yes",1,0)),
            Service = sum(ifelse(`TRQ-SL` == "Yes",1,0)),
            Research = sum(ifelse(`TRQ-RSCH` == "Yes",1,0)),
            Internship = sum(ifelse(`TRQ-INT` == "Yes",1,0)),
            Leadership = sum(ifelse(`TRQ-LEAD` == "Yes",1,0)),
            `Student-led` = sum(ifelse(`TRQ-SL` == "Yes",1,0)),
            Writing = sum(ifelse(`TRQ-WRIT` == "Yes",1,0)),
            Other = sum(ifelse(`TRQ-OTH` == "Yes",1,0)),
            Course = sum(ifelse(`TRQ-COURSE` == "Yes",1,0)),
            Total = total.n$N
  ),TEQ %>%
    summarize(`Study Abroad` = round(sum(ifelse(`TRQ-STUAB` == "Yes",1,0)) / n() * 100, 2),
              Service = round(sum(ifelse(`TRQ-SL` == "Yes",1,0)) / n() * 100, 2),
              Research = round(sum(ifelse(`TRQ-RSCH` == "Yes",1,0)) / n() * 100, 2),
              Internship = round(sum(ifelse(`TRQ-INT` == "Yes",1,0)) / n() * 100, 2),
              Leadership = round(sum(ifelse(`TRQ-LEAD` == "Yes",1,0)) / n() * 100, 2),
              `Student-led` = round(sum(ifelse(`TRQ-SL` == "Yes",1,0)) / n() * 100, 2),
              Writing = round(sum(ifelse(`TRQ-WRIT` == "Yes",1,0)) / n() * 100, 2),
              Other = round(sum(ifelse(`TRQ-OTH` == "Yes",1,0)) / n() * 100, 2),
              Course = round(sum(ifelse(`TRQ-COURSE` == "Yes",1,0)) / n() * 100, 2),
              Total = 100
    )) %>%
  t()

# row.names(Various.Activities) <- c("N","Percent")

##### Reporting Activity

Reporting.Activity <- TEQ %>%
  summarize(`Study Abroad` = round(sum(ifelse(`TRQ-STUAB` == "Yes",1,0)) / n(), 2),
            `Service Learning` = round(sum(ifelse(`TRQ-SL` == "Yes",1,0)) / n(), 2),
            Research = round(sum(ifelse(`TRQ-RSCH` == "Yes",1,0)) / n(), 2),
            Internship = round(sum(ifelse(`TRQ-INT` == "Yes",1,0)) / n(), 2),
            Leadership = round(sum(ifelse(`TRQ-LEAD` == "Yes",1,0)) / n(), 2),
            `Student-led` = round(sum(ifelse(`TRQ-SL` == "Yes",1,0)) / n(), 2),
            Writing = round(sum(ifelse(`TRQ-WRIT` == "Yes",1,0)) / n(), 2),
            Other = round(sum(ifelse(`TRQ-OTH` == "Yes",1,0)) / n(), 2),
            Course = round(sum(ifelse(`TRQ-COURSE` == "Yes",1,0)) / n(), 2),
            `Any (Big 4)` = round(sum(`LeastBig4`) / n(), 2),
            `Any` = round(sum(`LeastAny`) / n(), 2),
            Total = 100
  ) %>%
  t() %>%
  as.data.frame(.) %>%
  mutate(Experience = rownames(.)) %>%
  merge(read_excel("TEQ_Historical.xlsx", sheet = "Reporting.Activities"),., by = "Experience")
  
##### Activities by Gender

Activities.by.Gender <- rbind(
  TEQ %>%
    summarize(`Study Abroad` = sum(ifelse(`TRQ-STUAB` == "Yes" & `Gender` == "F",1,0)),
              Service = sum(ifelse(`TRQ-SL` == "Yes" & `Gender` == "F",1,0)),
              Research = sum(ifelse(`TRQ-RSCH` == "Yes" & `Gender` == "F",1,0)),
              Internship = sum(ifelse(`TRQ-INT` == "Yes" & `Gender` == "F",1,0)),
              Leadership = sum(ifelse(`TRQ-LEAD` == "Yes" & `Gender` == "F",1,0)),
              `Student-led` = sum(ifelse(`TRQ-SL` == "Yes" & `Gender` == "F",1,0)),
              Writing = sum(ifelse(`TRQ-WRIT` == "Yes" & `Gender` == "F",1,0)),
              Other = sum(ifelse(`TRQ-OTH` == "Yes" & `Gender` == "F",1,0)),
              Course = sum(ifelse(`TRQ-COURSE` == "Yes" & `Gender` == "F",1,0)),
              Total = total.n$N - sum(ifelse(Gender == "M",1,0))
    ),TEQ %>%
    summarize(`Study Abroad` = round(sum(ifelse(`TRQ-STUAB` == "Yes" & `Gender` == "F",1,0)) / n() * 100, 2),
              Service = round(sum(ifelse(`TRQ-SL` == "Yes" & `Gender` == "F",1,0)) / n() * 100, 2),
              Research = round(sum(ifelse(`TRQ-RSCH` == "Yes" & `Gender` == "F",1,0)) / n() * 100, 2),
              Internship = round(sum(ifelse(`TRQ-INT` == "Yes" & `Gender` == "F",1,0)) / n() * 100, 2),
              Leadership = round(sum(ifelse(`TRQ-LEAD` == "Yes" & `Gender` == "F",1,0)) / n() * 100, 2),
              `Student-led` = round(sum(ifelse(`TRQ-SL` == "Yes" & `Gender` == "F",1,0)) / n() * 100, 2),
              Writing = round(sum(ifelse(`TRQ-WRIT` == "Yes" & `Gender` == "F",1,0)) / n() * 100, 2),
              Other = round(sum(ifelse(`TRQ-OTH` == "Yes" & `Gender` == "F",1,0)) / n() * 100, 2),
              Course = round(sum(ifelse(`TRQ-COURSE` == "Yes" & `Gender` == "F",1,0)) / n() * 100, 2),
              Total = 100
    ),
  TEQ %>%
    summarize(`Study Abroad` = sum(ifelse(`TRQ-STUAB` == "Yes" & `Gender` == "M",1,0)),
              Service = sum(ifelse(`TRQ-SL` == "Yes" & `Gender` == "M",1,0)),
              Research = sum(ifelse(`TRQ-RSCH` == "Yes" & `Gender` == "M",1,0)),
              Internship = sum(ifelse(`TRQ-INT` == "Yes" & `Gender` == "M",1,0)),
              Leadership = sum(ifelse(`TRQ-LEAD` == "Yes" & `Gender` == "M",1,0)),
              `Student-led` = sum(ifelse(`TRQ-SL` == "Yes" & `Gender` == "M",1,0)),
              Writing = sum(ifelse(`TRQ-WRIT` == "Yes" & `Gender` == "M",1,0)),
              Other = sum(ifelse(`TRQ-OTH` == "Yes" & `Gender` == "M",1,0)),
              Course = sum(ifelse(`TRQ-COURSE` == "Yes" & `Gender` == "M",1,0)),
              Total = total.n$N - sum(ifelse(Gender == "F",1,0))
    ),
  TEQ %>%
    summarize(`Study Abroad` = round(sum(ifelse(`TRQ-STUAB` == "Yes" & `Gender` == "M",1,0)) / n() * 100, 2),
              Service = round(sum(ifelse(`TRQ-SL` == "Yes" & `Gender` == "M",1,0)) / n() * 100, 2),
              Research = round(sum(ifelse(`TRQ-RSCH` == "Yes" & `Gender` == "M",1,0)) / n() * 100, 2),
              Internship = round(sum(ifelse(`TRQ-INT` == "Yes" & `Gender` == "M",1,0)) / n() * 100, 2),
              Leadership = round(sum(ifelse(`TRQ-LEAD` == "Yes" & `Gender` == "M",1,0)) / n() * 100, 2),
              `Student-led` = round(sum(ifelse(`TRQ-SL` == "Yes" & `Gender` == "M",1,0)) / n() * 100, 2),
              Writing = round(sum(ifelse(`TRQ-WRIT` == "Yes" & `Gender` == "M",1,0)) / n() * 100, 2),
              Other = round(sum(ifelse(`TRQ-OTH` == "Yes" & `Gender` == "M",1,0)) / n() * 100, 2),
              Course = round(sum(ifelse(`TRQ-COURSE` == "Yes" & `Gender` == "M",1,0)) / n() * 100, 2),
              Total = 100
    )) %>%
  t()

##### Activities.by.Major

Activities.by.Major <- bind_rows(
  TEQ %>% group_by(School) %>%
    filter(Majr1 != 'IDSM') %>%
    summarize(`Majr1` = "TOTAL",
              N = n(),
              `Study Abroad` = sum(ifelse(`TRQ-STUAB` == "Yes",1,0)),
              `Study Abroad%` = round(sum(ifelse(`TRQ-STUAB` == "Yes",1,0) / n()) * 100),
              Service = sum(ifelse(`TRQ-SL` == "Yes",1,0)),
              Service.per = round(sum(ifelse(`TRQ-SL` == "Yes",1,0) / n()) * 100),
              Research = sum(ifelse(`TRQ-RSCH` == "Yes",1,0)),
              Research.per = round(sum(ifelse(`TRQ-RSCH` == "Yes",1,0) / n()) * 100),
              Internship = sum(ifelse(`TRQ-INT` == "Yes",1,0)),
              Internship.per = round(sum(ifelse(`TRQ-INT` == "Yes",1,0) / n()) * 100),
              Leadership = sum(ifelse(`TRQ-LEAD` == "Yes",1,0)),
              Leadership.per = round(sum(ifelse(`TRQ-LEAD` == "Yes",1,0) / n()) * 100),
              `Student-led` = sum(ifelse(`TRQ-SL` == "Yes",1,0)),
              `Student-led.per` = round(sum(ifelse(`TRQ-SL` == "Yes",1,0) / n()) * 100),
              Writing = sum(ifelse(`TRQ-WRIT` == "Yes",1,0)),
              Writing.per = round(sum(ifelse(`TRQ-WRIT` == "Yes",1,0) / n()) * 100),
              Course = sum(ifelse(`TRQ-COURSE` == "Yes",1,0)),
              Course.per = round(sum(ifelse(`TRQ-COURSE` == "Yes",1,0) / n()) * 100),
              Other = sum(ifelse(`TRQ-OTH` == "Yes",1,0)),
              Other.per = round(sum(ifelse(`TRQ-OTH` == "Yes",1,0) / n()) * 100)
    ),
  TEQ %>% group_by(School, `Majr1`) %>%
    summarize(N = n(),
              `Study Abroad` = sum(ifelse(`TRQ-STUAB` == "Yes",1,0)),
              `Study Abroad%` = round(sum(ifelse(`TRQ-STUAB` == "Yes",1,0) / n()) * 100),
              Service = sum(ifelse(`TRQ-SL` == "Yes",1,0)),
              Service.per = round(sum(ifelse(`TRQ-SL` == "Yes",1,0) / n()) * 100),
              Research = sum(ifelse(`TRQ-RSCH` == "Yes",1,0)),
              Research.per = round(sum(ifelse(`TRQ-RSCH` == "Yes",1,0) / n()) * 100),
              Internship = sum(ifelse(`TRQ-INT` == "Yes",1,0)),
              Internship.per = round(sum(ifelse(`TRQ-INT` == "Yes",1,0) / n()) * 100),
              Leadership = sum(ifelse(`TRQ-LEAD` == "Yes",1,0)),
              Leadership.per = round(sum(ifelse(`TRQ-LEAD` == "Yes",1,0) / n()) * 100),
              `Student-led` = sum(ifelse(`TRQ-SL` == "Yes",1,0)),
              `Student-led.per` = round(sum(ifelse(`TRQ-SL` == "Yes",1,0) / n()) * 100),
              Writing = sum(ifelse(`TRQ-WRIT` == "Yes",1,0)),
              Writing.per = round(sum(ifelse(`TRQ-WRIT` == "Yes",1,0) / n()) * 100),
              Course = sum(ifelse(`TRQ-COURSE` == "Yes",1,0)),
              Course.per = round(sum(ifelse(`TRQ-COURSE` == "Yes",1,0) / n()) * 100),
              Other = sum(ifelse(`TRQ-OTH` == "Yes",1,0)),
              Other.per = round(sum(ifelse(`TRQ-OTH` == "Yes",1,0) / n()) * 100)
    ),
  TEQ %>%
    summarize('School' = "ALL", 
              'Majr1' = "ALL",
              N = n(),
              `Study Abroad` = sum(ifelse(`TRQ-STUAB` == "Yes",1,0)),
              `Study Abroad%` = round(sum(ifelse(`TRQ-STUAB` == "Yes",1,0) / n()) * 100),
              Service = sum(ifelse(`TRQ-SL` == "Yes",1,0)),
              Service.per = round(sum(ifelse(`TRQ-SL` == "Yes",1,0) / n()) * 100),
              Research = sum(ifelse(`TRQ-RSCH` == "Yes",1,0)),
              Research.per = round(sum(ifelse(`TRQ-RSCH` == "Yes",1,0) / n()) * 100),
              Internship = sum(ifelse(`TRQ-INT` == "Yes",1,0)),
              Internship.per = round(sum(ifelse(`TRQ-INT` == "Yes",1,0) / n()) * 100),
              Leadership = sum(ifelse(`TRQ-LEAD` == "Yes",1,0)),
              Leadership.per = round(sum(ifelse(`TRQ-LEAD` == "Yes",1,0) / n()) * 100),
              `Student-led` = sum(ifelse(`TRQ-SL` == "Yes",1,0)),
              `Student-led.per` = round(sum(ifelse(`TRQ-SL` == "Yes",1,0) / n()) * 100),
              Writing = sum(ifelse(`TRQ-WRIT` == "Yes",1,0)),
              Writing.per = round(sum(ifelse(`TRQ-WRIT` == "Yes",1,0) / n()) * 100),
              Course = sum(ifelse(`TRQ-COURSE` == "Yes",1,0)),
              Course.per = round(sum(ifelse(`TRQ-COURSE` == "Yes",1,0) / n()) * 100),
              Other = sum(ifelse(`TRQ-OTH` == "Yes",1,0)),
              Other.per = round(sum(ifelse(`TRQ-OTH` == "Yes",1,0) / n()) * 100)
    )
) %>%
  ungroup() %>%
  mutate(School = case_when(
    School == "ALL" | School == "IDSM" ~ " ",
    TRUE ~ as.character(School)
  )) %>%
  arrange(School, Majr1) %>%
  rename('Major' = Majr1)

##### Activities.by.Major.percentOnly

Activities.by.Major.percentOnly <- bind_rows(
  TEQ %>% group_by(School) %>%
    filter(!is.na(Majr1), Majr1 != 'IDSM') %>%
    summarize(`Majr1` = "TOTAL",
              N = n(),
              `Study Abroad` = round(sum(ifelse(`TRQ-STUAB` == "Yes",1,0) / n()) * 100),
              Service = round(sum(ifelse(`TRQ-SL` == "Yes",1,0) / n()) * 100),
              Research = round(sum(ifelse(`TRQ-RSCH` == "Yes",1,0) / n()) * 100),
              Internship = round(sum(ifelse(`TRQ-INT` == "Yes",1,0) / n()) * 100),
              Leadership = round(sum(ifelse(`TRQ-LEAD` == "Yes",1,0) / n()) * 100),
              `Student-led` = round(sum(ifelse(`TRQ-SL` == "Yes",1,0) / n()) * 100),
              Writing = round(sum(ifelse(`TRQ-WRIT` == "Yes",1,0) / n()) * 100),
              Course = round(sum(ifelse(`TRQ-COURSE` == "Yes",1,0) / n()) * 100),
              Other = round(sum(ifelse(`TRQ-OTH` == "Yes",1,0) / n()) * 100)
    ),
  TEQ %>% group_by(School, `Majr1`) %>%
    filter(!is.na(Majr1)) %>%
    summarize(N = n(),
              `Study Abroad` = round(sum(ifelse(`TRQ-STUAB` == "Yes",1,0) / n()) * 100),
              Service = round(sum(ifelse(`TRQ-SL` == "Yes",1,0) / n()) * 100),
              Research = round(sum(ifelse(`TRQ-RSCH` == "Yes",1,0) / n()) * 100),
              Internship = round(sum(ifelse(`TRQ-INT` == "Yes",1,0) / n()) * 100),
              Leadership = round(sum(ifelse(`TRQ-LEAD` == "Yes",1,0) / n()) * 100),
              `Student-led` = round(sum(ifelse(`TRQ-SL` == "Yes",1,0) / n()) * 100),
              Writing = round(sum(ifelse(`TRQ-WRIT` == "Yes",1,0) / n()) * 100),
              Course = round(sum(ifelse(`TRQ-COURSE` == "Yes",1,0) / n()) * 100),
              Other = round(sum(ifelse(`TRQ-OTH` == "Yes",1,0) / n()) * 100)
    ),
  TEQ %>%
    filter(!is.na(Majr1)) %>%
    summarize('School' = "ALL", 
              'Majr1' = "ALL",
              N = n(),
              `Study Abroad` = round(sum(ifelse(`TRQ-STUAB` == "Yes",1,0) / n()) * 100),
              Service = round(sum(ifelse(`TRQ-SL` == "Yes",1,0) / n()) * 100),
              Research = round(sum(ifelse(`TRQ-RSCH` == "Yes",1,0) / n()) * 100),
              Internship = round(sum(ifelse(`TRQ-INT` == "Yes",1,0) / n()) * 100),
              Leadership = round(sum(ifelse(`TRQ-LEAD` == "Yes",1,0) / n()) * 100),
              `Student-led` = round(sum(ifelse(`TRQ-SL` == "Yes",1,0) / n()) * 100),
              Writing = round(sum(ifelse(`TRQ-WRIT` == "Yes",1,0) / n()) * 100),
              Course = round(sum(ifelse(`TRQ-COURSE` == "Yes",1,0) / n()) * 100),
              Other = round(sum(ifelse(`TRQ-OTH` == "Yes",1,0) / n()) * 100)
    )
) %>%
  ungroup() %>%
  mutate(School = case_when(
    School == "ALL" | School == "IDSM" ~ " ",
    TRUE ~ as.character(School)
  )) %>%
  arrange(School, Majr1) %>%
  rename('Major' = Majr1)

##### Majors.by.Gender

Majors.by.Gender <- bind_rows(
    TEQ %>% group_by(School) %>%
      filter(!is.na(Majr1), Majr1 != 'IDSM') %>%
      summarize(`Majr1` = "TOTAL",
                N = n(),
                Male = sum(ifelse(Gender == "M",1,0)),
                Female = sum(ifelse(Gender == "F",1,0))
      ),
    TEQ %>% group_by(School, `Majr1`) %>%
      filter(!is.na(Majr1)) %>%
      summarize(N = n(),
                Male = sum(ifelse(Gender == "M",1,0)),
                Female = sum(ifelse(Gender == "F",1,0))
      ),
    TEQ %>%
      filter(!is.na(Majr1)) %>%
      summarize('School' = "ALL", 
                'Majr1' = "ALL",
                N = n(),
                Male = sum(ifelse(Gender == "M",1,0)),
                Female = sum(ifelse(Gender == "F",1,0))
      )
  ) %>%
  ungroup() %>%
  mutate(School = case_when(
    School == "ALL" | School == "IDSM" ~ " ",
    TRUE ~ as.character(School)
  )) %>%
  arrange(School, Majr1) %>%
  rename('Major' = Majr1)

##### Big4Any Major

Big4Any.Major <- bind_rows(
    TEQ %>% group_by(School) %>%
      filter(!is.na(Majr1), Majr1 != 'IDSM') %>%
      summarize(`Majr1` = "TOTAL",
                N = n(),
                Big4Any = sum(LeastBig4),
                Big4Any.per = round(sum(LeastBig4) / n() * 100),
                Any = sum(LeastAny),
                Any.per = round(sum(LeastAny) / n() * 100)
      ),
    TEQ %>% group_by(School, `Majr1`) %>%
      filter(!is.na(Majr1)) %>%
      summarize(N = n(),
                Big4Any = sum(LeastBig4),
                Big4Any.per = round(sum(LeastBig4) / n() * 100),
                Any = sum(LeastAny),
                Any.per = round(sum(LeastAny) / n() * 100)
      ),
    TEQ %>%
      filter(!is.na(Majr1)) %>%
      summarize('School' = "ALL", 
                'Majr1' = "ALL",
                N = n(),
                Big4Any = sum(LeastBig4),
                Big4Any.per = round(sum(LeastBig4) / n() * 100),
                Any = sum(LeastAny),
                Any.per = round(sum(LeastAny) / n() * 100)
      )
  ) %>%
  ungroup() %>%
  mutate(School = case_when(
    School == "ALL" | School == "IDSM" ~ " ",
    TRUE ~ as.character(School)
  )) %>%
  arrange(School, Majr1) %>%
  rename('Major' = Majr1)
  
##### Big4.Counts.by.Major

Overall <- bind_rows(
  TEQ %>% group_by(School) %>%
    filter(!is.na(Majr1), Majr1 != 'IDSM') %>%
    summarize(`Majr1` = "TOTAL",
              N = n(),
              `1` = sum(ifelse(`TRQ-ED` == "Not Particularly Transformative",1,0)),
              `2` = sum(ifelse(`TRQ-ED` == "Somewhat Transformative",1,0)),
              `3` = sum(ifelse(`TRQ-ED` == "Transformative",1,0)),
              `4` = sum(ifelse(`TRQ-ED` == "Very Transfomative" | `TRQ-ED` == "Very Transformative",1,0)), ##### MISPELLED IN THE DATA #####
              `5` = sum(ifelse(`TRQ-ED` == "Totally Transformative",1,0)),
              Avg = round((1*`1` + 2*`2` + 3*`3` + 4*`4` + 5*`5`) / n(),2),
              `%4or5` = round((`4` + `5`) / n() * 100)
    ),
  TEQ %>% group_by(School, `Majr1`) %>%
    filter(!is.na(Majr1)) %>%
    summarize(N = n(),
              `1` = sum(ifelse(`TRQ-ED` == "Not Particularly Transformative",1,0)),
              `2` = sum(ifelse(`TRQ-ED` == "Somewhat Transformative",1,0)),
              `3` = sum(ifelse(`TRQ-ED` == "Transformative",1,0)),
              `4` = sum(ifelse(`TRQ-ED` == "Very Transfomative" | `TRQ-ED` == "Very Transformative",1,0)), ##### MISPELLED IN THE DATA #####
              `5` = sum(ifelse(`TRQ-ED` == "Totally Transformative",1,0)),
              Avg = round((1*`1` + 2*`2` + 3*`3` + 4*`4` + 5*`5`) / n(),2),
              `%4or5` = round((`4` + `5`) / n() * 100)
    ),
  TEQ %>%
    filter(!is.na(Majr1)) %>%
    summarize('School' = "ALL", 
              'Majr1' = "ALL",
              N = n(),
              `1` = sum(ifelse(`TRQ-ED` == "Not Particularly Transformative",1,0)),
              `2` = sum(ifelse(`TRQ-ED` == "Somewhat Transformative",1,0)),
              `3` = sum(ifelse(`TRQ-ED` == "Transformative",1,0)),
              `4` = sum(ifelse(`TRQ-ED` == "Very Transfomative" | `TRQ-ED` == "Very Transformative",1,0)), ##### MISPELLED IN THE DATA #####
              `5` = sum(ifelse(`TRQ-ED` == "Totally Transformative",1,0)),
              Avg = round((1*`1` + 2*`2` + 3*`3` + 4*`4` + 5*`5`) / n(),2),
              `%4or5` = round((`4` + `5`) / n() * 100)
    )
) %>%
  portfolio_clean_major()

##### Overall Historical

Overall.Historical <- Overall %>%
  filter(School != " ") %>%
  group_by(School) %>%
  summarise(Avg = round(mean(Avg),1),
            `%4or5` = round(mean(`%4or5`) / 100 ,2)) %>%
  rbind(Overall %>%
          filter(School == " ") %>%
          mutate(School = Major) %>%
          group_by(School) %>%
          summarise(Avg = round(mean(Avg),1),
                    `%4or5` = round(mean(`%4or5`) / 100 ,2))) %>%
  merge(read_excel("TEQ_Historical.xlsx", sheet = "Overall"),.)

##### Activity.Major.Over.Time

Activity.Major.Over.Time <- 
  bind_rows(
    TEQ %>% filter(!is.na(Majr1), Majr1 != 'IDSM') %>%
      summarize(School = "ALL",
                Year = 2019,
                N = n(),
                `Study Abroad` = round(sum(ifelse(`TRQ-STUAB` == "Yes",1,0) / n()),2),
                Service = round(sum(ifelse(`TRQ-SL` == "Yes",1,0) / n()),2),
                Research = round(sum(ifelse(`TRQ-RSCH` == "Yes",1,0) / n()),2),
                Internship = round(sum(ifelse(`TRQ-INT` == "Yes",1,0) / n()),2),
                Leadership = round(sum(ifelse(`TRQ-LEAD` == "Yes",1,0) / n()),2),
                `Student-led` = round(sum(ifelse(`TRQ-SL` == "Yes",1,0) / n()),2),
                Writing = round(sum(ifelse(`TRQ-WRIT` == "Yes",1,0) / n()),2),
                Other = round(sum(ifelse(`TRQ-OTH` == "Yes",1,0) / n()),2),
                AnyBig4 = round(sum(`LeastBig4`) / n(),2),
                Any = round(sum(`LeastAny`) / n(),2))
    ,
    TEQ %>% group_by(School) %>%
      filter(!is.na(Majr1)) %>%
      summarize(Year = 2019,
                N = n(),
                `Study Abroad` = round(sum(ifelse(`TRQ-STUAB` == "Yes",1,0) / n()),2),
                Service = round(sum(ifelse(`TRQ-SL` == "Yes",1,0) / n()),2),
                Research = round(sum(ifelse(`TRQ-RSCH` == "Yes",1,0) / n()),2),
                Internship = round(sum(ifelse(`TRQ-INT` == "Yes",1,0) / n()),2),
                Leadership = round(sum(ifelse(`TRQ-LEAD` == "Yes",1,0) / n()),2),
                `Student-led` = round(sum(ifelse(`TRQ-SL` == "Yes",1,0) / n()),2),
                Writing = round(sum(ifelse(`TRQ-WRIT` == "Yes",1,0) / n()),2),
                Other = round(sum(ifelse(`TRQ-OTH` == "Yes",1,0) / n()),2),
                AnyBig4 = round(sum(`LeastBig4`) / n(),2),
                Any = round(sum(`LeastAny`) / n(),2)),
    read_excel("TEQ_Historical.xlsx", sheet = "Activity.Major.Over.Time")
  ) %>% ungroup() %>%
  arrange(School, Year)

Activity.Major.Over.Time[4:13] = 100 * Activity.Major.Over.Time[4:13]


##### Kables

TEQ_Kables <- list(
  Overall.Ed.Transformative = kable_template(Overall.Ed.Transformative, type = "basic", col.names = c("Response","N","%")),
  Various.Activities = kable_template(Various.Activities, type = "basic", col.names = c("N Participated","%")) %>%
    add_header_above(c("Various Activity Counts" = 3)),
  Reporting.Activity = kable_template(Reporting.Activity, type = "basic", col.names = c("Experience",seq(2014,2019))) %>%
    add_header_above(c(" " = 1, "% Reporting Activity" = 6)),
  Activities.by.Gender = kable_template(Activities.by.Gender, type = "basic", col.names = c("F", "%", "M", "%")) %>%
    add_header_above(c("Activities by Gender" = 5)),
  Majors.by.Gender = kable_template(Majors.by.Gender, type = "basic"),
  Activities.by.Major = kable_template(Activities.by.Major, type = "basic"),
  Activities.by.Major.percentOnly = kable_template(Activities.by.Major.percentOnly, type = "basic"),
  Big4Any.Major = kable_template(Big4Any.Major, type = "basic", col.names = c("","Major","N","Count","%","Count","%")) %>%
    add_header_above(c(" " = 3, "Big 4" = 2, "Any" = 2)),
  Overall = kable_template(Overall, type = "basic"),
  Overall.Historical = kable_template(Overall.Historical, type = "basic", col.names = c("School",rep(c("Avg","%4or5"),5))) %>%
    add_header_above(c("Academic Year" = 1, "2015" = 2, "2016" = 2, "2017" = 2, "2018" = 2, "2019" = 2)),
  Activity.Major.Over.Time = kable_template(Activity.Major.Over.Time, type = "basic")
)

rm(Overall.Ed.Transformative,Various.Activities,Reporting.Activity,Activities.by.Gender,
   Majors.by.Gender,Activities.by.Major,Activities.by.Major.percentOnly,
   Big4Any.Major,Overall,Overall.Historical,total.n, Activity.Major.Over.Time)
