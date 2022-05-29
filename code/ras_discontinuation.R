#*******************************************************************************
#
# Project: The effect of canagliflozin on discontinuation of RAS inhibitors in 
#          the CREDENCE trial
# Date:    06-May-2022
# Author:  Rob Fletcher
# Purpose: Prepare RAS inhibitor discontinuation time to event data
#
#*******************************************************************************


# Notes -------------------------------------------------------------------

# This is a reproducible example of the code used to assess the effect of 
# canagliflozin on RAS discontinuation in the CREDENCE trial. It should allow 
# you to replicate the analyses in the DAPA-CKD data

# Below is a description of each variable required for the analysis

# `studyid` : study identifiers for CANVAS, CANVAS-R, and CREDENCE
# `usubjid` : participant identifiers
# `trt01p`  : treatment allocation (canagliflozin or placebo)
# `ittfl`   : intention-to-treat flag
# `randfl`  : randomised allocation flag
# `ap01sdt` : randomisation date
# `ap01edt` : double blind period end date
# `astdt`   : start date of the concomitant medication
# `aendt`   : end date of the concomitant medication
# `cq11nam` : indicator variable for whether a medication is an ACE
# `cq12nam` : indicator variable for whether a medication is an ARB
# `cq19nam` : indicator variabvle for whether a medication is a RAS inhibitor
#             (which they all are)

# This is not a straight forward analysis owing to quirks identified in the
# CREDENCE data 

# Here's my email if you want to get in touch with me to notify me of any bugs 
# or errors etc: rfletcher@georgeinstitute.org.au


# Install dependencies (if required) --------------------------------------

# Library names
libs = c("here", "survival", "tidyverse")

# Install libraries
install.packages(setdiff(libs, rownames(installed.packages())))


# Load libraries ----------------------------------------------------------

# NB you'll need to be using an RStudio project for `here::here()` to work
source(here::here("src", "initialise_workspace.R"))
library(survival)


# Source functions --------------------------------------------------------

source(here::here("src", "rnd.R"))


# Create data -------------------------------------------------------------

# Create subject simulation data (1 row per subject)
subj =
  tibble(
    studyid = c("cnv", "cnv", "cnvr", "cnvr", "crd", "crd", "cnv", "cnvr"),	
    usubjid = paste0("id100", seq(1, 8, 1)),
    trt01p = c("cana", "plac", "cana", "plac", "cana", "plac", "cana", "plac"),
    ittfl = "y", 
    randfl = "y",
    ap01sdt = c(
      "2014-03-22", "2014-01-10", "2014-04-14", "2014-06-03", "2014-03-19", 
      "2014-02-28", "2014-03-05", "2015-01-04"
    ),
    ap01edt = c(
      "2014-09-18", "2018-07-27", "2018-05-08", "2014-12-03", "2017-08-09", 
      "2018-11-15", "2015-01-01", "2016-07-25"
    )
  )

# Create RAS inhibitor use simulation data (multiple rows per subject)
ras = 
  tibble(
    usubjid = c(
      "1", "1", "1", "2", "3", "3", "3", "3", "4", "4", "4", "5", "5", "5", "5",
      "5", "6", "6", "6", "7", "7", "7", "8", "8"
    ),
    astdt = c(
      "2013-09-03", "2012-07-16", "2014-04-11", NA, "2013-06-08", "2014-12-05",
      "2016-01-25", "2016-05-26", NA, NA, "2014-09-15", "2010-10-08", 
      "2015-05-21", "2015-08-17", "2015-10-23", "2016-05-01", "2014-02-28",
      "2015-07-09", "2018-12-25", "2014-03-05", "2014-06-13", "2014-11-21", 
      NA, "2016-03-03"
    ),
    aendt = c(
      "2013-10-14", "2014-04-10", "2014-09-18", "2018-07-27", "2014-12-05",
      "2015-11-17", "2017-07-25", "2016-07-19", "2014-09-03", "2014-09-03",
      "2014-12-03", "2015-06-30", "2016-04-04", "2015-08-17", "2015-10-23",
      "2016-05-01", "2015-06-30", "2019-02-12", "2019-02-12", "2014-08-27",
      "2014-09-05", "2015-01-01", "2015-02-18", "2016-07-25"
    ),
    cq11nam = c(
      "ace", NA, NA, NA, NA, "ace", "ace", "ace", "ace", "ace", NA, NA, NA, 
      "ace", "ace", "ace", NA, NA, "ace", "ace", NA, "ace", NA, NA
    ),
    cq12nam = c(
      NA, "arb", "arb", "arb", "arb", NA, NA, NA, NA, NA, "arb", "arb", "arb", 
      NA, NA, NA, "arb", "arb", NA, NA, "arb", NA, "arb", "arb"
    ),
    cq19nam = "raas inhibitor"
  ) %>%
  mutate(usubjid = paste0("id100", usubjid))

# Create screening eGFR simulation data (1 row per subject; not really required 
# for this reprex but just to show we run a stratified Cox model for the 
# CREDENCE analysis)
strat = 
  tibble(
    usubjid = paste0("id100", seq(1, 8, 1)),
    strata = c(
      "30 to <45", "45 to <60", "60 to <90", "30 to <45", "45 to <60", 
      "60 to <90", "30 to <45", "45 to <60"
    )
  ) %>%
  mutate(strata = paste0("Screening eGFR ", strata))


# Combine data ------------------------------------------------------------

ras_comb = 
  left_join(subj, ras, by = "usubjid") %>%
  mutate(
    across(ends_with("dt"), as_date),
    int = ifelse(trt01p == "Cana", 1, 0),
    drug_type = case_when(
      !is.na(cq11nam) & is.na(cq12nam) ~ "ace",
      is.na(cq11nam) & !is.na(cq12nam) ~ "arb",
      TRUE ~ NA_character_,
    )
  ) %>%
  # Remove any rows where RAS use is before the start of the trial
  filter(aendt >= ap01sdt)


# Quality control data ----------------------------------------------------

ras_end = ras_comb %>%
  # If analysis start date is missing recode it as the day of randomisation
  mutate(
    astdt = case_when(
      is.na(astdt) ~ ap01sdt,
      TRUE ~ astdt
    )
  ) %>%
  # Remove medications which start after the end of the randomised treatment
  # phase as we're treating the end this as the end of follow-up
  filter(astdt <= ap01edt) %>%
  # Recode the end date of treatment if it's after the end of the randomised
  # treatment phase to the date of the randomised treatment phase
  mutate(
    aendt = case_when(
      aendt > ap01edt ~ ap01edt,
      TRUE ~ aendt
    )
  )

rcd_diff = ras_end %>%
  arrange(usubjid, astdt, aendt) %>%
  select(usubjid, ap01sdt, ap01edt, astdt, aendt, drug_type, int) %>%
  mutate(row_id = paste0("row00", row_number()), .before = 1) %>%
  group_by(usubjid) %>%
  mutate(
    # Create indicator to exclude records that occur within (prior) records
    excl = case_when(
      astdt > lag(astdt) & astdt < lag(aendt) & aendt < lag(aendt) ~ 1,
      TRUE ~ 0
    )
  ) %>%
  ungroup() %>%
  # Exclude records that occur within (prior) records
  filter(excl == 0)

# Identify duplicate rows where medications are concurrent (i.e. same start and 
# end date as each other)
dupes = rcd_diff %>% 
  select(row_id, usubjid, drug_type, astdt, aendt) %>%
  group_by(usubjid, astdt, aendt) %>%
  filter(n() > 1) %>%
  slice_max(row_id) %>%
  ungroup()

# Identify "acute" instances or RAS inhibitor use where the start and end date
# are equivalent to one another
acute = rcd_diff %>%
  select(row_id, usubjid, drug_type, astdt, aendt) %>%
  filter(astdt == aendt)

# Remove undesirable records identified during quality control
rcd_diff_qc = 
  lst(rcd_diff, dupes, acute) %>%
  reduce(
    anti_join, 
    by = c("row_id", "usubjid", "astdt", "aendt", "drug_type")
  ) %>%
  group_by(usubjid) %>% 
  mutate(
    record = row_number(),
    first_record = ifelse(record == min(record), 1, 0),
    last_record = ifelse(record == max(record), 1, 0),
    time_diff = case_when(
      first_record != 1 ~ as.numeric(astdt - lag(aendt)),
      first_record == 1 ~ NA_real_
    )
  ) %>%
  ungroup()


# Define discontinuation/no discontinuation -------------------------------

# Define those who temporarily discontinued RAS inhibitors (we're using 28 days
# as the cut-off)
temp_d = rcd_diff_qc %>%
  group_by(usubjid) %>%
  mutate(temp_dis = ifelse(time_diff >= 28, 1, NA)) %>%
  fill(temp_dis, .direction = "updown") %>% 
  # Identify the time to event row and code the event
  mutate(
    temp_dis_event = case_when(
      lead(time_diff >= 28) ~ 1,
      TRUE ~ 0
    ),
    date_temp_dis_event = case_when(
      temp_dis_event == 1 ~ aendt,
      TRUE ~ NA_Date_
    )
  ) %>%
  ungroup()

# Define those who did not discontinue RAS inhibitors
no_d = temp_d %>%
  group_by(usubjid) %>%
  mutate(
    no_dis = case_when(
      is.na(temp_dis) & aendt >= ap01edt ~ 1,
      TRUE ~ NA_real_
    )
  ) %>%
  fill(no_dis, .direction = "updown") %>%
  ungroup()

# Define permanent discontinuation
perm_d = no_d %>%
  group_by(usubjid) %>%
  mutate(
    perm_dis = case_when(
      is.na(temp_dis) & is.na(no_dis) & max(aendt) < ap01edt ~ 1,
      TRUE ~ NA_real_
    ),
    date_perm_dis_event = case_when(
      perm_dis == 1 ~ max(aendt),
      TRUE ~ NA_Date_
    )
  ) %>%
  fill(perm_dis, .direction = "updown") %>%
  ungroup()


# Define event ------------------------------------------------------------

# Temporary discontinuation
temp_d_event = perm_d %>% 
  filter(temp_dis == 1, temp_dis_event == 1) %>%
  select(
    usubjid, ap01sdt, int, 
    event = temp_dis_event, event_date = date_temp_dis_event
  ) %>%
  group_by(usubjid) %>%
  slice_min(event_date) %>%
  ungroup()

# No discontinuation
no_d_event = perm_d %>%
  filter(no_dis == 1) %>%
  select(usubjid, ap01sdt, int, event_date = ap01edt) %>%
  mutate(event = 0) %>%
  distinct()

# Permanent discontinuation
perm_d_event = perm_d %>%
  filter(perm_dis == 1) %>%
  select(usubjid, ap01sdt, int, event_date = date_perm_dis_event) %>%
  mutate(event = 1) %>%
  distinct() 


# Run analysis ------------------------------------------------------------

hr =
  bind_rows(temp_d_event, no_d_event, perm_d_event) %>%
  arrange(usubjid) %>%
  inner_join(strat, by = "usubjid") %>%
  mutate(tte = as.numeric(event_date - ap01sdt)) %>%
  coxph(Surv(tte, event) ~ int + strata(as.factor(strata)), data = .) %>%
  broom::tidy(exponentiate = TRUE, conf.int = TRUE)
