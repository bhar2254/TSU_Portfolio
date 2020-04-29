# Blaine Harper
# CASE - Truman State University
# LTT.R
# Automation of Truman's Annual Portfolio Project.

# LTT Tables

# Cleaning

LTT <- LTT %>%
  merge(y = Majors, by.x = c("Last Name","First Name"), by.y = c("Last","First"))  %>% # Merge with the Majors data frame
  mutate(`COV-TIM Reviewer 1` = as.numeric(`COV-TIM Reviewer 1`),
         `COV-TIM-S` = as.numeric(`COV-TIM-S`)) %>%
  portfolio_build_major()

##### Hours Spent

Hours.Spent <- bind_rows(
  LTT %>% group_by(School) %>%
    filter(!is.na(`COV-TIM Reviewer 1`), !is.na(`COV-TIM-S`), !is.na(Majr1), Majr1 != 'IDSM') %>%
    summarize(`Majr1` = "TOTAL",
              N = n(),
              SAvg = round(mean(`COV-TIM-S`),2),
              SNoOutAvg = round(mean(`COV-TIM-S`[`COV-TIM-S` < 100]),2),
              FacAvg = round(mean(`COV-TIM Reviewer 1`),2),
              STot = round(sum(`COV-TIM-S`),0),
              SNoOutTot = round(sum(`COV-TIM-S`[`COV-TIM-S` < 100]),0),
              FacTot = round(sum(`COV-TIM Reviewer 1`),0)
              ),
  LTT %>% group_by(School, `Majr1`) %>%
    filter(!is.na(`COV-TIM Reviewer 1`), !is.na(`COV-TIM-S`), !is.na(Majr1)) %>%
    summarize(
      N = n(),
      SAvg = round(mean(`COV-TIM-S`),2),
      SNoOutAvg = round(mean(`COV-TIM-S`[`COV-TIM-S` < 100]),2),
      FacAvg = round(mean(`COV-TIM Reviewer 1`),2),
      STot = round(sum(`COV-TIM-S`),0),
      SNoOutTot = round(sum(`COV-TIM-S`[`COV-TIM-S` < 100]),0),
      FacTot = round(sum(`COV-TIM Reviewer 1`),0)
    ),
  LTT %>%
    filter(!is.na(`COV-TIM Reviewer 1`), !is.na(`COV-TIM-S`), !is.na(Majr1)) %>%
    summarize('School' = "ALL", 
              'Majr1' = "ALL",
              N = n(),
              SAvg = round(mean(`COV-TIM-S`),2),
              SNoOutAvg = round(mean(`COV-TIM-S`[`COV-TIM-S` < 100]),2),
              FacAvg = round(mean(`COV-TIM Reviewer 1`),2),
              STot = round(sum(`COV-TIM-S`),0),
              SNoOutTot = round(sum(`COV-TIM-S`[`COV-TIM-S` < 100]),0),
              FacTot = round(sum(`COV-TIM Reviewer 1`),0)
    )
) %>%
  mutate(School = case_when(
    School == "ALL" ~ " ",
    School == "IDSM" ~ " ",
    TRUE ~ as.character(School)
  )) %>%
  arrange(School, Majr1)

##### Attitude Portfolio

Attitude.Portfolio <- bind_rows(
  LTT %>% group_by(School) %>%
    filter(!is.na(Majr1), !is.na(`COV-ATT-PO Reviewer 1`), !is.na(`COV-ATT-AS Reviewer 1`), Majr1 != 'IDSM') %>%
    summarize(`Majr1` = "TOTAL",
              N = n(),
              PONeg = sum(ifelse(`COV-ATT-PO Reviewer 1` == "Negative",1,0)),
              POMix = sum(ifelse(`COV-ATT-PO Reviewer 1` == "Mixed",1,0)),
              POPos = sum(ifelse(`COV-ATT-PO Reviewer 1` == "Positive",1,0)),
              PONoInd = sum(ifelse(`COV-ATT-PO Reviewer 1` == "No Indication",1,0)),
              `POW%` = round(((POPos + .5 * POMix) / (n() - PONoInd)) * 100),
              ASNeg = sum(ifelse(`COV-ATT-AS Reviewer 1` == "Negative",1,0)),
              ASMix = sum(ifelse(`COV-ATT-AS Reviewer 1` == "Mixed",1,0)),
              ASPos = sum(ifelse(`COV-ATT-AS Reviewer 1` == "Positive",1,0)),
              ASNoInd = sum(ifelse(`COV-ATT-AS Reviewer 1` == "No Indication",1,0)),
              `ASW%` = round(((ASPos + .5 * ASMix) / (n() - ASNoInd)) * 100)
    ),
  LTT %>% group_by(School, `Majr1`) %>%
    filter(!is.na(Majr1), !is.na(`COV-ATT-AS Reviewer 1`), !is.na(`COV-ATT-PO Reviewer 1`)) %>%
    summarize(
      N = n(),
      PONeg = sum(ifelse(`COV-ATT-PO Reviewer 1` == "Negative",1,0)),
      POMix = sum(ifelse(`COV-ATT-PO Reviewer 1` == "Mixed",1,0)),
      POPos = sum(ifelse(`COV-ATT-PO Reviewer 1` == "Positive",1,0)),
      PONoInd = sum(ifelse(`COV-ATT-PO Reviewer 1` == "No Indication",1,0)),
      `POW%` = round(((POPos + .5 * POMix) / (n() - PONoInd)) * 100),
      ASNeg = sum(ifelse(`COV-ATT-AS Reviewer 1` == "Negative",1,0)),
      ASMix = sum(ifelse(`COV-ATT-AS Reviewer 1` == "Mixed",1,0)),
      ASPos = sum(ifelse(`COV-ATT-AS Reviewer 1` == "Positive",1,0)),
      ASNoInd = sum(ifelse(`COV-ATT-AS Reviewer 1` == "No Indication",1,0)),
      `ASW%` = round(((ASPos + .5 * ASMix) / (n() - ASNoInd)) * 100)
    ),
  LTT %>%
    filter(!is.na(Majr1), !is.na(`COV-ATT-AS Reviewer 1`), !is.na(`COV-ATT-PO Reviewer 1`)) %>%
    summarize('School' = "ALL", 
              'Majr1' = "ALL",
              N = n(),
              PONeg = sum(ifelse(`COV-ATT-PO Reviewer 1` == "Negative",1,0)),
              POMix = sum(ifelse(`COV-ATT-PO Reviewer 1` == "Mixed",1,0)),
              POPos = sum(ifelse(`COV-ATT-PO Reviewer 1` == "Positive",1,0)),
              PONoInd = sum(ifelse(`COV-ATT-PO Reviewer 1` == "No Indication",1,0)),
              `POW%` = round(((POPos + .5 * POMix) / (n() - PONoInd)) * 100),
              ASNeg = sum(ifelse(`COV-ATT-AS Reviewer 1` == "Negative",1,0)),
              ASMix = sum(ifelse(`COV-ATT-AS Reviewer 1` == "Mixed",1,0)),
              ASPos = sum(ifelse(`COV-ATT-AS Reviewer 1` == "Positive",1,0)),
              ASNoInd = sum(ifelse(`COV-ATT-AS Reviewer 1` == "No Indication",1,0)),
              `ASW%` = round(((ASPos + .5 * ASMix) / (n() - ASNoInd)) * 100)
    )
)%>%
  mutate(School = case_when(
    School == "ALL" ~ " ",
    School == "IDSM" ~ " ",
    TRUE ~ as.character(School)
  )) %>%
  arrange(School, Majr1)

##### Attitude Education

Attitude.Education <- bind_rows(
  LTT %>% group_by(School) %>%
    filter(!is.na(Majr1), !is.na(`COV-ATT-ED Reviewer 1`), !is.na(`COV-ATT-MA Reviewer 1`), Majr1 != 'IDSM') %>%
    summarize(`Majr1` = "TOTAL",
              N = n(),
              EDNeg = sum(ifelse(`COV-ATT-ED Reviewer 1` == "Negative",1,0)),
              EDMix = sum(ifelse(`COV-ATT-ED Reviewer 1` == "Mixed",1,0)),
              EDPos = sum(ifelse(`COV-ATT-ED Reviewer 1` == "Positive",1,0)),
              EDNoInd = sum(ifelse(`COV-ATT-ED Reviewer 1` == "No Indication",1,0)),
              `EDW%` = round(((EDPos + .5 * EDMix) / (n() - EDNoInd)) * 100),
              MANeg = sum(ifelse(`COV-ATT-MA Reviewer 1` == "Negative",1,0)),
              MAMix = sum(ifelse(`COV-ATT-MA Reviewer 1` == "Mixed",1,0)),
              MAPos = sum(ifelse(`COV-ATT-MA Reviewer 1` == "Positive",1,0)),
              MANoInd = sum(ifelse(`COV-ATT-MA Reviewer 1` == "No Indication",1,0)),
              `MAW%` = round(((MAPos + .5 * MAMix) / (n() - MANoInd)) * 100)
    ),
  LTT %>% group_by(School, `Majr1`) %>%
    filter(!is.na(Majr1), !is.na(`COV-ATT-MA Reviewer 1`), !is.na(`COV-ATT-ED Reviewer 1`)) %>%
    summarize(
      N = n(),
      EDNeg = sum(ifelse(`COV-ATT-ED Reviewer 1` == "Negative",1,0)),
      EDMix = sum(ifelse(`COV-ATT-ED Reviewer 1` == "Mixed",1,0)),
      EDPos = sum(ifelse(`COV-ATT-ED Reviewer 1` == "Positive",1,0)),
      EDNoInd = sum(ifelse(`COV-ATT-ED Reviewer 1` == "No Indication",1,0)),
      `EDW%` = round(((EDPos + .5 * EDMix) / (n() - EDNoInd)) * 100),
      MANeg = sum(ifelse(`COV-ATT-MA Reviewer 1` == "Negative",1,0)),
      MAMix = sum(ifelse(`COV-ATT-MA Reviewer 1` == "Mixed",1,0)),
      MAPos = sum(ifelse(`COV-ATT-MA Reviewer 1` == "Positive",1,0)),
      MANoInd = sum(ifelse(`COV-ATT-MA Reviewer 1` == "No Indication",1,0)),
      `MAW%` = round(((MAPos + .5 * MAMix) / (n() - MANoInd)) * 100)
    ),
  LTT %>%
    filter(!is.na(Majr1), !is.na(`COV-ATT-MA Reviewer 1`), !is.na(`COV-ATT-ED Reviewer 1`)) %>%
    summarize('School' = "ALL", 
              'Majr1' = "ALL",
              N = n(),
              EDNeg = sum(ifelse(`COV-ATT-ED Reviewer 1` == "Negative",1,0)),
              EDMix = sum(ifelse(`COV-ATT-ED Reviewer 1` == "Mixed",1,0)),
              EDPos = sum(ifelse(`COV-ATT-ED Reviewer 1` == "Positive",1,0)),
              EDNoInd = sum(ifelse(`COV-ATT-ED Reviewer 1` == "No Indication",1,0)),
              `EDW%` = round(((EDPos + .5 * EDMix) / (n() - EDNoInd)) * 100),
              MANeg = sum(ifelse(`COV-ATT-MA Reviewer 1` == "Negative",1,0)),
              MAMix = sum(ifelse(`COV-ATT-MA Reviewer 1` == "Mixed",1,0)),
              MAPos = sum(ifelse(`COV-ATT-MA Reviewer 1` == "Positive",1,0)),
              MANoInd = sum(ifelse(`COV-ATT-MA Reviewer 1` == "No Indication",1,0)),
              `MAW%` = round(((MAPos + .5 * MAMix) / (n() - MANoInd)) * 100)
    )
)%>%
  mutate(School = case_when(
    School == "ALL" ~ " ",
    School == "IDSM" ~ " ",
    TRUE ~ as.character(School)
  )) %>%
  arrange(School, Majr1)

##### Self-Reflection

Self.Reflection <- bind_rows(
  LTT %>% group_by(School) %>%
    filter(!is.na(Majr1), !is.na(`COV-REFLE Reviewer 1`), Majr1 != 'IDSM') %>%
    summarize(`Majr1` = "TOTAL",
              N = n(),
              No = sum(ifelse(`COV-REFLE Reviewer 1` == "No",1,0)),
              Yes = sum(ifelse(`COV-REFLE Reviewer 1` == "Yes",1,0)),
              Findings = sum(ifelse(`COV-REFLE Reviewer 1` == "Yes, with findings",1,0)),
              `%Reflect` = round((Yes + Findings) / (Yes + Findings + No) * 100)
    ),
  LTT %>% group_by(School, `Majr1`) %>%
    filter(!is.na(Majr1), !is.na(`COV-REFLE Reviewer 1`)) %>%
    summarize(
      N = n(),
      No = sum(ifelse(`COV-REFLE Reviewer 1` == "No",1,0)),
      Yes = sum(ifelse(`COV-REFLE Reviewer 1` == "Yes",1,0)),
      Findings = sum(ifelse(`COV-REFLE Reviewer 1` == "Yes, with findings",1,0)),
      `%Reflect` = round((Yes + Findings) / (Yes + Findings + No) * 100)
    ),
  LTT %>%
    filter(!is.na(Majr1), !is.na(`COV-REFLE Reviewer 1`)) %>%
    summarize('School' = "ALL", 
              'Majr1' = "ALL",
              N = n(),
              No = sum(ifelse(`COV-REFLE Reviewer 1` == "No",1,0)),
              Yes = sum(ifelse(`COV-REFLE Reviewer 1` == "Yes",1,0)),
              Findings = sum(ifelse(`COV-REFLE Reviewer 1` == "Yes, with findings",1,0)),
              `%Reflect` = round((Yes + Findings) / (Yes + Findings + No) * 100)
    )
)%>%
  mutate(School = case_when(
    School == "ALL" ~ " ",
    School == "IDSM" ~ " ",
    TRUE ~ as.character(School)
  )) %>%
  arrange(School, Majr1)

##### Create the tables

LTT_Kables <- list(Hours.Spent = kable_template(Hours.Spent, "rotated", 
                                                col.names = c(" ", "Major", "N 2019", rep(c("Student", "Student, No Outliers", "Faculty"),2))) %>%
                     add_header_above(c(" " = 3, "Average" = 3, "Total" = 3)),
                  Attitude.Portfolio = kable_template(Attitude.Portfolio, type = "rotated", 
                                                      col.names = c(" ", "Major","N",
                                                                    rep(c("Negative","Mixed","Positive","No Indication","W% Pos"),2))) %>% 
                    add_header_above(c(" " = 3, "Attitude Towards Portfolio" = 5, "Attitude Towards Assessment" = 5)),
                  
                  Attitude.Education = kable_template(Attitude.Education, type = "rotated", 
                                                      col.names = c(" ", "Major","N",
                                                                    rep(c("Negative","Mixed","Positive","No Indication","W% Pos"),2))) %>% 
                    add_header_above(c(" " = 3, "Attitude Towards Education" = 5, "Attitude Towards Major Education" = 5)),
                  Self.Reflection = kable_template(Self.Reflection, type = "rotated", col.names = c("","Major","N","No","Yes","Findings","% Reflect"))
                  )

rm(Hours.Spent,Attitude.Education,Attitude.Portfolio,Self.Reflection)
