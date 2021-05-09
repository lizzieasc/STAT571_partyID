# Load packages
pacman::p_load(skimr, tidyverse, rio, lmer)

# Load data
load('closedEnds_data/raw_data/merged-2020-n2471-03152020.Rdata')

# Recode NA codes used throughout the dataset
dta21[dta21 == 'Before question was added'] <- NA
dta21[dta21 == 'Refused'] <- NA

# Rename a few variables
names(dta21)[grepl('^FF[BW]', names(dta21))] <- 
  names(dta21)[grepl('^FF[BW]', names(dta21))] %>% 
  str_replace('_1_', '1_') %>%
  str_replace('_2_', '2_') %>%
  str_replace('_3_', '3_') %>%
  str_replace('_4_', '4_')

# NA-fill
## Identify columns to fill
patterns <- c('^PPETHM.*[1-5]$', '^PPAGECAT.*[1-5]$', '^PPINCIMP.*[1-5]$', 
              '^PPEDUCAT.*[1-5]$', '^POS1.*[1-5]$', '^POS4.*[1-5]$', '^POS6.*[1-5]$', 
              '^FFB1.*[1-5]$', '^FFB2.*[1-5]$', '^FFB3.*[1-5]$', '^FFB4.*[1-5]$',
              '^FFW1.*[1-5]$', '^FFW2.*[1-5]$', '^FFW3.*[1-5]$', '^FFW4.*[1-5]$')
baseline_df <- dta21 %>% select(MNO)

## Loop to fills
for (pattern in patterns) {
  temp_df <- dta21 %>% 
    select(order(names(dta21))) %>%
    select(MNO, matches(pattern)) %>%
    mutate(across(matches(pattern), as.character))
  
  for (i in 3:ncol(temp_df)) {
    temp_df[, 2][is.na(temp_df[, 2])] <- temp_df[, i][is.na(temp_df[, 2])] 
  }
  
  baseline_df <- temp_df %>% select(MNO, 2) %>% left_join(baseline_df, by = 'MNO')
}

# Add a few other variables
baseline_df$FFBa_3 <- dta21$FFBa_3
baseline_df$FFWa_3 <- dta21$FFWa_3

### CLEAN VARIABLES ###

# Rename columns
baseline_df <- baseline_df %>%
  rename(RACE = PPETHM_1, AGE = ppagecat_1, INCOME = PPINCIMP_1, EDU = PPEDUCAT_1,
         GAYMAR = POS1_1, ABORT = POS4_1, STEM = POS6_1)

# Recode columns / create indexes
## Gay marriage
baseline_df <- baseline_df %>%
  mutate(GAYMAR = case_when(
    grepl('full marriage rights', GAYMAR) ~ 1,
    grepl('support civil unions', GAYMAR) ~ 2,
    grepl('legal recognition', GAYMAR) ~ 3
  ))

## Stem cells
baseline_df <- baseline_df %>%
  mutate(STEM = case_when(
    grepl('no restrictions', STEM) ~ 1,
    grepl('Ease the restrictions', STEM) ~ 2,
    grepl('Keep the current restrictions', STEM) ~ 3,
    grepl('Not fund', STEM) ~ 4
  ))

## Abortion
baseline_df <- baseline_df %>%
  mutate(ABORT = case_when(
    grepl('anyone who wants', ABORT) ~ 1,
    grepl('stricter limits', ABORT) ~ 2,
    grepl('rape, incest', ABORT) ~ 3,
    grepl('not be permitted', ABORT) ~ 4
  ))

## Fear of favoritism toward Blacks
baseline_df <- baseline_df %>%
  mutate(across(starts_with('FFB'), ~ case_when(
    . == 'Strongly Disagree' ~ 1, 
    . == 'Somewhat Disagree' ~ 2, 
    . == 'Somewhat Agree' ~ 3, 
    . == 'Strongly Agree' ~ 4,
    
    . == 'Very bad' ~ 2,
    . == 'Somewhat bad' ~ 1,
    . == 'Somewhat good' ~ 0,
    . == 'Very good' ~ 0
  ))) %>%
  rowwise() %>%
  mutate(FFB = mean(c(FFB1_1, FFB2_1, FFB3_1, FFB4_1), na.rm = T) * FFBa_3) %>%
  select(-matches('^FFB[1-4]'), -FFBa_3)

## Fear of favoritism toward women
baseline_df <- baseline_df %>%
  mutate(across(starts_with('FFW'), ~ case_when(
    . == 'Strongly Disagree' ~ 1, 
    . == 'Somewhat Disagree' ~ 2, 
    . == 'Somewhat Agree' ~ 3, 
    . == 'Strongly Agree' ~ 4,
    
    . == 'Very bad' ~ 2,
    . == 'Somewhat bad' ~ 1,
    . == 'Somewhat good' ~ 0,
    . == 'Very good' ~ 0
  ))) %>%
  rowwise() %>%
  mutate(FFW = mean(c(FFW1_1, FFW2_1, FFW3_1, FFW4_1)) * FFWa_3) %>%
  select(-matches('^FFW[1-4]'), -FFWa_3)

## Age
baseline_df <- baseline_df %>%
  mutate(AGE = case_when(
    grepl('18-24', AGE) ~ 1,
    grepl('25-34', AGE) ~ 2,
    grepl('35-44', AGE) ~ 3,
    grepl('45-54', AGE) ~ 4,
    grepl('55-64', AGE) ~ 5,
    grepl('65-74', AGE) ~ 6,
    grepl('75+', AGE) ~ 7
  ))

## Income
baseline_df <- baseline_df %>%
  mutate(INCOME = case_when(
    grepl('Less than \\$5,000', INCOME) ~ 1,
    grepl('\\$5,000 to \\$7,499', INCOME) ~ 2,
    grepl('\\$7,500 to \\$9,999', INCOME) ~ 3,
    grepl('\\$10,000 to \\$12,499', INCOME) ~ 4,
    grepl('\\$12,500 to \\$14,999', INCOME) ~ 5,
    grepl('\\$15,000 to \\$19,999', INCOME) ~ 6,
    grepl('\\$20,000 to \\$24,999', INCOME) ~ 7,
    grepl('\\$25,000 to \\$29,999', INCOME) ~ 8,
    grepl('\\$30,000 to \\$34,999', INCOME) ~ 9,
    grepl('\\$35,000 to \\$39,999', INCOME) ~ 10,
    grepl('\\$40,000 to \\$49,999', INCOME) ~ 11,
    grepl('\\$50,000 to \\$59,999', INCOME) ~ 12,
    grepl('\\$60,000 to \\$74,999', INCOME) ~ 13,
    grepl('\\$75,000 to \\$84,999', INCOME) ~ 14,
    grepl('\\$85,000 to \\$99,999', INCOME) ~ 15,
    grepl('\\$100,000 to \\$124,999', INCOME) ~ 16,
    grepl('\\$125,000 to \\$149,999', INCOME) ~ 17,
    grepl('\\$150,000 to \\$174,999', INCOME) ~ 18,
    grepl('\\$175,000', INCOME) ~ 19,
    grepl('\\$200,000 to \\$249,999', INCOME) ~ 19,
    grepl('\\$250,000 or more', INCOME) ~ 19
  ))

## Education
baseline_df <- baseline_df %>%
  mutate(EDU = case_when(
    grepl('Less than high', EDU) ~ 1,
    grepl('High school', EDU) ~ 2,
    grepl('Some college', EDU) ~ 3,
    grepl('Bachelor', EDU) ~ 4
  ))

# Export clean data
save(baseline_df, file = 'closedEnds_data/clean_data/traits_data.RData')
