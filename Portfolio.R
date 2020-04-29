# Blaine Harper
# CASE - Truman State University
# Portfolio.R
# Automation of Truman's Annual Portfolio Project.

# Total Line count (including comments and spaces): 1719

# Initialize
rm(list = ls())
options(warn=1)
## Uncomment below line to install packages
# source("Package_Install.R")

library(tidyverse)
library(expss)
library(readxl)
library(knitr)
library(kableExtra)

eval.year <- 2019

# This will either need to be updated to allow the user to select the files individually
# Or to require the user to hard-code the filepath every year.

# Set a path to portfolio directory. User must have access.
# Uncomment the choose.dir() to allow the user to select the folder. Good for automating the script.
# portfolio_path <- choose.dir(default = "U:/Portfolio Project/Technical Files/Not needed for Reading Session/Data Files 2019/", caption = "Select folder")
portfolio_path <- "U:/Portfolio Project/Technical Files/Not needed for Reading Session/Data Files 2019/Raw Downloads 2019 + AEM edits/"

# Load data from the portfolio_path
portfolio_data <- function(data.set = "CTW")
{
  read_excel(paste0(portfolio_path, data.set, "export2019.xlsx"))
}

# Load the 6 data sets from the data export
CTW <- portfolio_data("CTW")
IDS <- portfolio_data("IDS")
LTT <- portfolio_data("LTT")
MOST <- portfolio_data("MOST")
SELF <- portfolio_data("SELF")
TEQ <- portfolio_data("TEQ")

# Load enrollment data and make major data frame that has only name and major 1 and 2
# CurrEnroll must be updated every year with the proper student's information
CurrEnroll <- read_excel("U:/Portfolio Project/Technical Files/Not needed for Reading Session/Data Files 2019/AM_CurrEnroll_91418.xlsx")
Majors <- CurrEnroll %>%
  select(`Pref/First Name`, `Last Name`, Majr1, Majr2) %>%
  rename(First = `Pref/First Name`, Last = `Last Name`) %>%
  mutate(Majr1 = case_when(
    Majr1 == "ARTH" | Majr1 == "ARTV" | Majr1 == "ARTS" ~ "ART",
    TRUE ~ as.character(Majr1)
  ))

# Major Recode
# Change the majors to their proper form
Major.Recode <- read_excel("Major_Recode.xlsx")
for( i in 1:length(Major.Recode$Raw)) { 
  Majors$Majr1 <- case_when( 
    Majors$Majr1 ==  Major.Recode$Raw[i] ~ as.character(Major.Recode$`Major Recode`[i]),
    TRUE ~ as.character(Majors$Majr1)
  )
}

# Template for the each of the kables
# Add "%>% add_header_above" after calling the function for a header above
kable_template <- function(data, type = 'basic',...) # This template makes making tables easier
{
  if(type == 'basic') # Basic - a simple table
  {
    output <- kable(data, align = "c", ...) %>%
      kable_styling(full_width = F, bootstrap_options = c("condensed","bordered")) %>%
      column_spec(1, bold = T) %>%
      collapse_rows(columns = 1, valign = "middle")
  }
  if(type == 'rotated') # Rotated - rotates the first column, used for 'by major' tables
  {
    output <- kable(data, align = "c", ...) %>%
      kable_styling(full_width = F, bootstrap_options = c("condensed","bordered")) %>%
      column_spec(1, extra_css = "white-space:nowrap; font-weight: bold; -webkit-transform: rotate(-90.0deg);") %>%
      column_spec(1, bold = T) %>%
      collapse_rows(columns = 1, valign = "middle")
  }
  output
}

# Add the major column to a data set
# Syntax (dplyr): data %>% portfolio_build_major()
portfolio_build_major <- function(data) # This makes adding the Majr1 column easier in each iteration
{
  data %>% 
    mutate(School = case_when(
      Majr1 == 'ART' | Majr1 == 'CML' | Majr1 == 'CRWT' | Majr1 == 'ENG' | Majr1 == 'LING' | Majr1 == 'MUSI' | Majr1 == 'THEA' ~ "Arts and Letters",
      Majr1 == 'ACCT' | Majr1 == 'BSAD' ~ "Business",
      Majr1 == 'ATHT' | Majr1 == 'CMDS' | Majr1 == 'ES' | Majr1 == 'HLTH' | Majr1 == 'NU' ~ "Hlth. Sci. and Ed.",
      Majr1 == 'COMM' | Majr1 == 'ECON' | Majr1 == 'HIST' | Majr1 == 'JUST' | Majr1 == 'PHRE' | Majr1 == 'POL' | Majr1 == 'PSYC' | Majr1 == 'SOAN' ~ "Social and Cultural Studies",
      Majr1 == 'AGSC' | Majr1 == 'BIOL' | Majr1 == 'CHEM' | Majr1 == 'CS' | Majr1 == 'MATH' | Majr1 == 'PHYS' | Majr1 == 'STTS' ~ "Sci. and Math Studies",
      Majr1 == 'IDSM' ~ "IDSM",
      TRUE ~ ''
      )
    )
}

# This function adds the gender column to a dataset
# Syntax (dplyr): data %>% portfolio_build_gender()
# Update the CyrrEnroll object above with the relevant file
portfolio_build_gender <- function(data)
{
  data %>% 
    merge(CurrEnroll %>%
            select(`Banner ID`,Gender) %>%
            mutate(`Banner ID` = as.numeric(`Banner ID`)),.)
}

# For cleaning up the major data sets
portfolio_clean_major <- function(data)
{
  data %>%
    ungroup() %>%
    mutate(School = case_when(
      School == "ALL" | School == "IDSM" ~ " ",
      TRUE ~ as.character(School)
    )) %>%
    arrange(School, Majr1) %>%
    rename('Major' = Majr1)
}

# Source all of the individual files
source("IDS.R")
source("CTW.R")
source("LTT.R")
source("MOST.R")
source("SELF.R")
source("TEQ.R")