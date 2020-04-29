# Blaine Harper
# CASE - Truman State University
# SELF.R
# Automation of Truman's Annual Portfolio Project.

# SELF Tables

# Cleaning

SELF <- SELF %>% # copied from everywhere else
  merge(y = Majors, by.x = c("Last Name","First Name"), by.y = c("Last","First"))  %>% # Merge with the Majors data frame
  portfolio_build_major()

##### Rationales

Rationales <- SELF %>% 
  summarise("Deep Introspection" = sum(ifelse(`SELF-WHY Introspection` == "Y",1,0) / n()) * 100,
            "New Perspective on Self" = sum(ifelse(`SELF-WHY Perspective` == "Y",1,0) / n()) * 100,
            "Personal Growth" = sum(ifelse(`SELF-WHY P. growth` == "Y",1,0) / n()) * 100,
            "Responsibility" = sum(ifelse(`SELF-WHY Responsibility` == "Y",1,0) / n()) * 100,
            "Moral/Ethical Dilemma" = sum(ifelse(`SELF-WHY Dilemma` == "Y",1,0) / n()) * 100,
            "Personal Best" = sum(ifelse(`SELF-WHY P. best` == "Y",1,0) / n()) * 100,
            "Especially Challenging" = sum(ifelse(`SELF-WHY Challenging` == "Y",1,0) / n()) * 100,
            "Intellectual Risk" = sum(ifelse(`SELF-WHY Risk` == "Y",1,0) / n()) * 100,
            "Vocational Development" = sum(ifelse(`SELF-WHY Vocation` == "Y",1,0) / n()) * 100,
            "Worked as Professional" = sum(ifelse(`SELF-WHY Professional` == "Y",1,0) / n()) * 100,
            "Service to Others" = sum(ifelse(`SELF-WHY Service` == "Y",1,0) / n()) * 100,
            "Collaboration w/ Peers" = sum(ifelse(`SELF-WHY Collaboration Peers` == "Y",1,0) / n()) * 100,
            "Collaboration w/ Professional" = sum(ifelse(`SELF-WHY Collaboration Professional` == "Y",1,0) / n()) * 100,
            "Mentoring Internship" = sum(ifelse(`SELF-WHY Mentoring` == "Y",1,0) / n()) * 100
            ) %>%
  t() %>%
  round(0) %>% 
  data.frame()

Rationales <- data.frame(Reasons = row.names(Rationales), Current = Rationales$.)

Rationales <- Rationales %>%
  mutate(Categories = case_when(
    Reasons == "Deep Introspection" | Reasons == "New Perspective on Self" | Reasons == "Personal Growth" | Reasons == "Responsibility" | Reasons == "Moral/Ethical Dilemma" ~ "Risk/Challenge/Growth",
    Reasons == "Personal Best" | Reasons == "Especially Challenging" | Reasons == "Intellectual Risk" | Reasons == "Vocational Development" | Reasons == "Worked as Professional" ~ "Academic/Scholarship",
    Reasons == "Service to Others" | Reasons == "Collaboration w/ Peers" | Reasons == "Collaboration w/ Professional" | Reasons == "Mentoring Internship" ~ "Relationships"
  )) %>%
  merge(read_excel("SELF_Historical.xlsx",
                   sheet = "Rationales"),
        by = "Reasons") %>%
  arrange(Categories.x)

##### SRGC Rationales 

SRGC_Rationales <- bind_rows(
  SELF %>% group_by(School) %>%
    filter(!is.na(Majr1), Majr1 != 'IDSM') %>%
    summarize(`Majr1` = "TOTAL",
              N = n(),
              Introspection = sum(ifelse(`SELF-WHY Introspection` == "Y",1,0)),
              Introspection_per = round(Introspection / n() * 100),
              Perspective = sum(ifelse(`SELF-WHY Perspective` == "Y",1,0)),
              Perspective_per = round(Perspective / n() * 100),
              `Personal Growth` = sum(ifelse(`SELF-WHY P. growth` == "Y",1,0)),
              `Personal Growth_per` = round(`Personal Growth` / n() * 100),
              Responsibility = sum(ifelse(`SELF-WHY Responsibility` == "Y",1,0)),
              Responsibility_per = round(Responsibility / n() * 100),
              Dilemma = sum(ifelse(`SELF-WHY Dilemma` == "Y",1,0)),
              Dilemma_per = round(Dilemma / n() * 100)
    ),
  SELF %>% group_by(School, `Majr1`) %>%
    filter(!is.na(Majr1)) %>%
    summarize(N = n(),
              Introspection = sum(ifelse(`SELF-WHY Introspection` == "Y",1,0)),
              Introspection_per = round(Introspection / n() * 100),
              Perspective = sum(ifelse(`SELF-WHY Perspective` == "Y",1,0)),
              Perspective_per = round(Perspective / n() * 100),
              `Personal Growth` = sum(ifelse(`SELF-WHY P. growth` == "Y",1,0)),
              `Personal Growth_per` = round(`Personal Growth` / n() * 100),
              Responsibility = sum(ifelse(`SELF-WHY Responsibility` == "Y",1,0)),
              Responsibility_per = round(Responsibility / n() * 100),
              Dilemma = sum(ifelse(`SELF-WHY Dilemma` == "Y",1,0)),
              Dilemma_per = round(Dilemma / n() * 100)
    ),
  SELF %>%
    filter(!is.na(Majr1)) %>%
    summarize('School' = "ALL", 
              'Majr1' = "ALL",
              N = n(),
              Introspection = sum(ifelse(`SELF-WHY Introspection` == "Y",1,0)),
              Introspection_per = round(Introspection / n() * 100),
              Perspective = sum(ifelse(`SELF-WHY Perspective` == "Y",1,0)),
              Perspective_per = round(Perspective / n() * 100),
              `Personal Growth` = sum(ifelse(`SELF-WHY P. growth` == "Y",1,0)),
              `Personal Growth_per` = round(`Personal Growth` / n() * 100),
              Responsibility = sum(ifelse(`SELF-WHY Responsibility` == "Y",1,0)),
              Responsibility_per = round(Responsibility / n() * 100),
              Dilemma = sum(ifelse(`SELF-WHY Dilemma` == "Y",1,0)),
              Dilemma_per = round(Dilemma / n() * 100))
) %>%
  ungroup() %>%
  mutate(School = case_when(
    School == "ALL" | School == "IDSM" ~ " ",
    TRUE ~ as.character(School)
  )) %>%
  arrange(School, Majr1) %>%
  rename('Major' = Majr1)

##### AS Rationales 

AS_Rationales <- bind_rows(
  SELF %>% group_by(School) %>%
    filter(!is.na(Majr1), Majr1 != 'IDSM') %>%
    summarize(`Majr1` = "TOTAL",
              N = n(),
              PBest = sum(ifelse(`SELF-WHY P. best` == "Y",1,0)),
              PBest_per = round(PBest / n() * 100),
              Challenging = sum(ifelse(`SELF-WHY Challenging` == "Y",1,0)),
              Challenging_per = round(Challenging / n() * 100),
              Intellectual = sum(ifelse(`SELF-WHY Risk` == "Y",1,0)),
              Intellectual_per = round(Intellectual / n() * 100),
              Vocation = sum(ifelse(`SELF-WHY Vocation` == "Y",1,0)),
              Vocation_per = round(Vocation / n() * 100),
              Professional = sum(ifelse(`SELF-WHY Professional` == "Y",1,0)),
              Professional_per = round(Professional / n() * 100)
    ),
  SELF %>% group_by(School, `Majr1`) %>%
    filter(!is.na(Majr1)) %>%
    summarize(N = n(),
              PBest = sum(ifelse(`SELF-WHY P. best` == "Y",1,0)),
              PBest_per = round(PBest / n() * 100),
              Challenging = sum(ifelse(`SELF-WHY Challenging` == "Y",1,0)),
              Challenging_per = round(Challenging / n() * 100),
              Intellectual = sum(ifelse(`SELF-WHY Risk` == "Y",1,0)),
              Intellectual_per = round(Intellectual / n() * 100),
              Vocation = sum(ifelse(`SELF-WHY Vocation` == "Y",1,0)),
              Vocation_per = round(Vocation / n() * 100),
              Professional = sum(ifelse(`SELF-WHY Professional` == "Y",1,0)),
              Professional_per = round(Professional / n() * 100)
    ),
  SELF %>%
    filter(!is.na(Majr1)) %>%
    summarize('School' = "ALL", 
              'Majr1' = "ALL",
              N = n(),
              PBest = sum(ifelse(`SELF-WHY P. best` == "Y",1,0)),
              PBest_per = round(PBest / n() * 100),
              Challenging = sum(ifelse(`SELF-WHY Challenging` == "Y",1,0)),
              Challenging_per = round(Challenging / n() * 100),
              Intellectual = sum(ifelse(`SELF-WHY Risk` == "Y",1,0)),
              Intellectual_per = round(Intellectual / n() * 100),
              Vocation = sum(ifelse(`SELF-WHY Vocation` == "Y",1,0)),
              Vocation_per = round(Vocation / n() * 100),
              Professional = sum(ifelse(`SELF-WHY Professional` == "Y",1,0)),
              Professional_per = round(Professional / n() * 100))
) %>%
  ungroup() %>%
  mutate(School = case_when(
    School == "ALL" | School == "IDSM" ~ " ",
    TRUE ~ as.character(School)
  )) %>%
  arrange(School, Majr1) %>%
  rename('Major' = Majr1)

##### Rel Rationales 

Rel_Rationales <- bind_rows(
  SELF %>% group_by(School) %>%
    filter(!is.na(Majr1), Majr1 != 'IDSM') %>%
    summarize(`Majr1` = "TOTAL",
              N = n(),
              Service = sum(ifelse(`SELF-WHY Service` == "Y",1,0)),
              Service_per = round(Service / n() * 100),
              Collab_peer = sum(ifelse(`SELF-WHY Collaboration Peers` == "Y",1,0)),
              Collab_peer_per = round(Collab_peer / n() * 100),
              Collab_pro = sum(ifelse(`SELF-WHY Collaboration Professional` == "Y",1,0)),
              Collab_pro_per = round(Collab_pro / n() * 100),
              Mentoring = sum(ifelse(`SELF-WHY Vocation` == "Y",1,0)),
              Mentoring_per = round(Mentoring / n() * 100)
    ),
  SELF %>% group_by(School, `Majr1`) %>%
    filter(!is.na(Majr1)) %>%
    summarize(N = n(),
              Service = sum(ifelse(`SELF-WHY Service` == "Y",1,0)),
              Service_per = round(Service / n() * 100),
              Collab_peer = sum(ifelse(`SELF-WHY Collaboration Peers` == "Y",1,0)),
              Collab_peer_per = round(Collab_peer / n() * 100),
              Collab_pro = sum(ifelse(`SELF-WHY Collaboration Professional` == "Y",1,0)),
              Collab_pro_per = round(Collab_pro / n() * 100),
              Mentoring = sum(ifelse(`SELF-WHY Vocation` == "Y",1,0)),
              Mentoring_per = round(Mentoring / n() * 100)
    ),
  SELF %>%
    filter(!is.na(Majr1)) %>%
    summarize('School' = "ALL", 
              'Majr1' = "ALL",
              N = n(),
              Service = sum(ifelse(`SELF-WHY Service` == "Y",1,0)),
              Service_per = round(Service / n() * 100),
              Collab_peer = sum(ifelse(`SELF-WHY Collaboration Peers` == "Y",1,0)),
              Collab_peer_per = round(Collab_peer / n() * 100),
              Collab_pro = sum(ifelse(`SELF-WHY Collaboration Professional` == "Y",1,0)),
              Collab_pro_per = round(Collab_pro / n() * 100),
              Mentoring = sum(ifelse(`SELF-WHY Vocation` == "Y",1,0)),
              Mentoring_per = round(Mentoring / n() * 100))
) %>%
  ungroup() %>%
  mutate(School = case_when(
    School == "ALL" | School == "IDSM" ~ " ",
    TRUE ~ as.character(School)
  )) %>%
  arrange(School, Majr1) %>%
  rename('Major' = Majr1)

SELF_Kables <- list(Rationales = kable_template(Rationales[c(3,1,5,6,2)], type = "basic", col.names = c("Categories","Reasons",2017:2019)) %>%
                      add_header_above(c(" " = 2, "%" = 3)),
                    SRGC_Rationales = kable_template(SRGC_Rationales, type = "rotated", col.names = c("","Major","N",rep(c("Yes","Percent"),5))) %>%
                      add_header_above(c(" " = 3, "Introspection" = 2, "Perspective" = 2, "Personal Growth" = 2, "Responsibility" = 2, "Dilemma" = 2)) %>%
                      add_header_above(c(" " = 3, "Risk / Growth / Challenge" = 10)),
                    AS_Rationales = kable_template(AS_Rationales, type = "rotated", col.names = c("","Major","N",rep(c("Yes","Percent"),5))) %>%
                      add_header_above(c(" " = 3, "Pesronal Best" = 2, "Challenging" = 2, "Risk" = 2, "Vocation" = 2, "Professional" = 2)) %>%
                      add_header_above(c(" " = 3, "Academic / Scholarship" = 10)),
                    Rel_Rationales = kable_template(Rel_Rationales, type = "rotated", col.names = c("","Major","N",rep(c("Yes","Percent"),4))) %>%
                      add_header_above(c(" " = 3, "Service" = 2, "Collab w/ Peers" = 2, "Collab w/ Professionals" = 2, "Mentoring" = 2)) %>%
                      add_header_above(c(" " = 3, "Relationships" = 8))
                    )

SELF_Kables$Rationales
SELF_Kables$SRGC_Rationales
SELF_Kables$AS_Rationales
SELF_Kables$Rel_Rationales

rm(Rationales, SRGC_Rationales, AS_Rationales,Rel_Rationales)
