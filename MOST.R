# Blaine Harper
# CASE - Truman State University
# MOST.R
# Automation of Truman's Annual Portfolio Project.

# MOST Tables

# Cleaning

MOST <- MOST %>% # copied from everywhere else
  merge(y = Majors, by.x = c("Last Name","First Name"), by.y = c("Last","First"))  %>% # Merge with the Majors data frame
  portfolio_build_major()

##### Reasons

Reasons_Historical <- read_excel("MOST_Historical.xlsx",
                                 sheet = "Reasons")

Reasons <- MOST %>%
  summarise(`Personal Growth` = round(sum(ifelse(`MOST-WHY P. growth` == "Y",1,0) / n()),2),
            `Enjoyable` = round(sum(ifelse(`MOST-WHY enjoyable` == "Y",1,0) / n()),2),
            `Challenging` = round(sum(ifelse(`MOST-WHY Challenging` == "Y",1,0) / n()),2),
            `Professional` = round(sum(ifelse(`MOST-WHY Professional` == "Y",1,0) / n()),2),
            `Personal Goals` = round(sum(ifelse(`MOST-WHY P. goals` == "Y",1,0) / n()),2),
            `Personal Best` = round(sum(ifelse(`MOST-WHY P. best` == "Y",1,0) / n()),2),
            `Lots of Time` = round(sum(ifelse(`MOST-WHY Lots of Time` == "Y",1,0) / n()),2),
            `Collaborative` = round(sum(ifelse(`MOST-WHY Collaborative` == "Y",1,0) / n()),2),
            `Problem Solving` = round(sum(ifelse(`MOST-WHY Problem Solved` == "Y",1,0)) / n(),2)) %>%
  `rownames<-`(c("2019")) %>%
  as.matrix(.) %>%
  t(.) %>%
  as.data.frame(.) %>%
  mutate(var = rownames(.)) %>%
  merge(Reasons_Historical, .) %>%
  rename(` ` = var)

##### Scores by First Major

Scores.by.first.major <- bind_rows(
    MOST %>% group_by(School) %>%
      filter(!is.na(Majr1), Majr1 != 'IDSM') %>%
      summarize(`Majr1` = "TOTAL",
                N = n(),
                `Personal Best` = round(sum(ifelse(`MOST-WHY P. best` == "Y",1,0))),
                `Personal Best %` = round(`Personal Best` / n() * 100,0),
                `Personal Goals` = round(sum(ifelse(`MOST-WHY P. goals` == "Y",1,0))),
                `Personal Goals %` = round(`Personal Goals` / n() * 100,0),
                `Personal Growth` = round(sum(ifelse(`MOST-WHY P. growth` == "Y",1,0))),
                `Personal Growth %` = round(`Personal Growth` / n() * 100,0),
                `Challenging` = round(sum(ifelse(`MOST-WHY Challenging` == "Y",1,0))),
                `Challenging %` = round(`Challenging` / n() * 100,0),
                `Professional` = round(sum(ifelse(`MOST-WHY Professional` == "Y",1,0))),
                `Professional %` = round(`Professional` / n() * 100,0),
                `Collaborative` = round(sum(ifelse(`MOST-WHY Collaborative` == "Y",1,0))),
                `Collaborative %` = round(`Collaborative` / n() * 100,0),
                `Enjoyable` = round(sum(ifelse(`MOST-WHY enjoyable` == "Y",1,0))),
                `Enjoyable %` = round(`Enjoyable` / n() * 100,0),
                `Problem Solving` = round(sum(ifelse(`MOST-WHY Problem Solved` == "Y",1,0))),
                `Problem Solving %` = round(`Problem Solving` / n() * 100,0),
                `Lots of Time` = round(sum(ifelse(`MOST-WHY Lots of Time` == "Y",1,0))),
                `Lots of Time %` = round(`Lots of Time` / n() * 100,0),
                `No Indication` = round(sum(ifelse(`MOST-WHY No Indication` == "Y",1,0))),
                `No Indication %` = round(`No Indication` / n() * 100,0)
                ),
    MOST %>% group_by(School, `Majr1`) %>%
      filter(!is.na(Majr1)) %>%
      summarize(N = n(),
                `Personal Best` = round(sum(ifelse(`MOST-WHY P. best` == "Y",1,0))),
                `Personal Best %` = round(`Personal Best` / n() * 100,0),
                `Personal Goals` = round(sum(ifelse(`MOST-WHY P. goals` == "Y",1,0))),
                `Personal Goals %` = round(`Personal Goals` / n() * 100,0),
                `Personal Growth` = round(sum(ifelse(`MOST-WHY P. growth` == "Y",1,0))),
                `Personal Growth %` = round(`Personal Growth` / n() * 100,0),
                `Challenging` = round(sum(ifelse(`MOST-WHY Challenging` == "Y",1,0))),
                `Challenging %` = round(`Challenging` / n() * 100,0),
                `Professional` = round(sum(ifelse(`MOST-WHY Professional` == "Y",1,0))),
                `Professional %` = round(`Professional` / n() * 100,0),
                `Collaborative` = round(sum(ifelse(`MOST-WHY Collaborative` == "Y",1,0))),
                `Collaborative %` = round(`Collaborative` / n() * 100,0),
                `Enjoyable` = round(sum(ifelse(`MOST-WHY enjoyable` == "Y",1,0))),
                `Enjoyable %` = round(`Enjoyable` / n() * 100,0),
                `Problem Solving` = round(sum(ifelse(`MOST-WHY Problem Solved` == "Y",1,0))),
                `Problem Solving %` = round(`Problem Solving` / n() * 100,0),
                `Lots of Time` = round(sum(ifelse(`MOST-WHY Lots of Time` == "Y",1,0))),
                `Lots of Time %` = round(`Lots of Time` / n() * 100,0),
                `No Indication` = round(sum(ifelse(`MOST-WHY No Indication` == "Y",1,0))),
                `No Indication %` = round(`No Indication` / n() * 100,0)
                ),
    MOST %>%
      filter(!is.na(Majr1)) %>%
      summarize('School' = "ALL", 
                'Majr1' = "ALL",
                N = n(),
                `Personal Best` = round(sum(ifelse(`MOST-WHY P. best` == "Y",1,0))),
                `Personal Best %` = round(`Personal Best` / n() * 100,0),
                `Personal Goals` = round(sum(ifelse(`MOST-WHY P. goals` == "Y",1,0))),
                `Personal Goals %` = round(`Personal Goals` / n() * 100,0),
                `Personal Growth` = round(sum(ifelse(`MOST-WHY P. growth` == "Y",1,0))),
                `Personal Growth %` = round(`Personal Growth` / n() * 100,0),
                `Challenging` = round(sum(ifelse(`MOST-WHY Challenging` == "Y",1,0))),
                `Challenging %` = round(`Challenging` / n() * 100,0),
                `Professional` = round(sum(ifelse(`MOST-WHY Professional` == "Y",1,0))),
                `Professional %` = round(`Professional` / n() * 100,0),
                `Collaborative` = round(sum(ifelse(`MOST-WHY Collaborative` == "Y",1,0))),
                `Collaborative %` = round(`Collaborative` / n() * 100,0),
                `Enjoyable` = round(sum(ifelse(`MOST-WHY enjoyable` == "Y",1,0))),
                `Enjoyable %` = round(`Enjoyable` / n() * 100,0),
                `Problem Solving` = round(sum(ifelse(`MOST-WHY Problem Solved` == "Y",1,0))),
                `Problem Solving %` = round(`Problem Solving` / n() * 100,0),
                `Lots of Time` = round(sum(ifelse(`MOST-WHY Lots of Time` == "Y",1,0))),
                `Lots of Time %` = round(`Lots of Time` / n() * 100,0),
                `No Indication` = round(sum(ifelse(`MOST-WHY No Indication` == "Y",1,0))),
                `No Indication %` = round(`No Indication` / n() * 100,0)
    )
) %>%
  ungroup() %>%
  mutate(School = case_when(
    School == "ALL" | School == "IDSM" ~ " ",
    TRUE ~ as.character(School)
  )) %>%
  arrange(School, Majr1) %>%
  rename('Major' = Majr1)

##### Satisfying Context

# THIS NEEDS TO BE FINISHED, BUT I NEED TO MOVE ON...

Satisfying.Context.Historical <- read_excel("MOST_Historical.xlsx",
                                            sheet = "Satisfying.Context")

Satisfying.Context <- MOST %>% 
  rename("MOST_Satisfying" = `Most Personally Satisfying Context`) %>% 
  filter(!is.na(MOST_Satisfying)) %>%
  mutate(MOST_Satisfying = case_when(
    startsWith(MOST_Satisfying,"otheracademic") ~ "Other Academic",
    startsWith(MOST_Satisfying,"otherorg") ~ "Other Organization",
    startsWith(MOST_Satisfying,"othercreative") ~ "Other Creative",
    startsWith(MOST_Satisfying,"otherath") ~ "Other Athletics",
    startsWith(MOST_Satisfying,"other") ~ "Other Misc",
    startsWith(MOST_Satisfying,"major") ~ "Major",
    startsWith(MOST_Satisfying,"minor") ~ "Minor",
    startsWith(MOST_Satisfying,"campusjob") ~ "Campus Job",
    startsWith(MOST_Satisfying,"campusmedia") ~ "Campus Media",
    startsWith(MOST_Satisfying,"capstone") ~ "Capstone",
    startsWith(MOST_Satisfying,"clubintramural") ~ "Club Sports/Intramurals",
    startsWith(MOST_Satisfying,"rotc") ~ "ROTC",
    startsWith(MOST_Satisfying,"governanceorg") ~ "Governance Organization",
    startsWith(MOST_Satisfying,"serviceorg") ~ "Service Organization",
    startsWith(MOST_Satisfying,"religiousorg") ~ "Religious Organization",
    startsWith(MOST_Satisfying,"fraternitysorority") ~ "Social Fraternity/Sorority",
    startsWith(MOST_Satisfying,"tutorteachmentor") ~ "Tutor/Teach/Mentor",
    startsWith(MOST_Satisfying,"varsityath") ~ "Varsity Athletics",
    startsWith(MOST_Satisfying,"recital") ~ "Public Performance/Recital",
    startsWith(MOST_Satisfying,"honorsoc") ~ "Honor Society",
    startsWith(MOST_Satisfying,"internship") ~ "Internship",
    startsWith(MOST_Satisfying,"lsp") ~ "LSP",
    startsWith(MOST_Satisfying,"elective") ~ "Elective",
    startsWith(MOST_Satisfying,"offcampusjob") ~ "Off Campus Job",
    startsWith(MOST_Satisfying,"professionalmajor") ~ "Proffesional/Major",
    startsWith(MOST_Satisfying,"volunteer") ~ "Volunteer",
    startsWith(MOST_Satisfying,"studyabroad") ~ "Study Abroad",
    startsWith(MOST_Satisfying,"reslife") ~ "Residence Life",
    startsWith(MOST_Satisfying,"relationships") ~ "Relationships/Friendships",
    startsWith(MOST_Satisfying,"servicelearning") ~ "Service Learning",
    startsWith(MOST_Satisfying,"research") ~ "Research",
    startsWith(MOST_Satisfying,"collegeapp") ~ "Other Academic",
    TRUE ~ as.character(MOST_Satisfying)
  )) %>%
  group_by(MOST_Satisfying) %>%
  summarize(N = n()) %>%
  rename("Most Satisfying Contexts" = MOST_Satisfying) %>%
  rename("2019 (N)" = N)

Satisfying.Context.Historical %>% 
  merge(Satisfying.Context, all = TRUE)

##### Tables

MOST_Kables <- list(Reasons = kable_template(Reasons),
                    Scores.by.first.major = kable_template(Scores.by.first.major, type = "rotated",col.names = c(" ","Major","N 2019",rep(c("Y","%"),10))) %>%
                      add_header_above(c(" " = 3, "Personal Best" = 2, "Personal Goals" = 2, "Personal Growth" = 2, "Challenging" = 2, "Professional" = 2, "Collaborative" = 2, "Enjoyable" = 2, "Problem Solving" = 2, "Lots of Time" = 2, "No Indication" = 2))
                    )

rm(Reasons,Scores.by.first.major, Reasons_Historical, Satisfying.Context.Historical, Satisfying.Context)
