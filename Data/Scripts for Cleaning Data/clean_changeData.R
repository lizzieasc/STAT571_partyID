# Load packages
pacman::p_load(skimr, tidyverse, rio)

# Load data
load('closedEnds_data/raw_data//merged-2020-n2471-03152020.Rdata')
oct2020 <- import('closedEnds_data/raw_data//UPenn_FallRecontact2020_Main.sav')

# Add wave suffix to Wave 15 data
oct2020 <- oct2020 %>% 
  rename_all(paste0, "_15") %>%
  rename('MNO' = 'mno_15')

# Convert MNO columns to numeric
dta21$MNO <- dta21$MNO %>% as.character() %>% as.numeric()
oct2020$MNO <- oct2020$MNO %>% as.character() %>% as.numeric()

# Merge data
merged_df <- merge(dta21, oct2020, by = 'MNO', all = T)

# Rough deletions of duplicate / nuisance columns
merged_df <- merged_df %>% 
  select(-IDEO7, -DETH2_a_10, -DETH2_b_10, -DETH2_a_11, -DETH2_b_11,
         -DETH3_a_10, -DETH3_b_10, -DETH3_a_11, -DETH3_b_11, -DETH3_a_12, 
         -DETH3_b_12, -PPINCIMP_2_9, -RE24_12)

### DELETE / RENAME COLUMNS ###

# Miscalleneous, easy renames
names(merged_df) <- merged_df %>% 
  names() %>% 
  str_replace('PPINCIMP_', 'INC_') %>%
  str_replace('ppincimp_', 'INC_') %>%
  
  str_replace('PARTYID7', 'PID7') %>%
  str_replace('partyid7', 'PID7') %>%
  
  str_replace('IDEOLOGY_UPDATED', 'IDEO7') %>%
  str_replace('XIDEO', 'IDEO7') %>%
  str_replace('xIdeo_', 'IDEO7_') %>%
  str_replace('xideo_', 'IDEO7_') %>%

  str_replace('POS3', 'TAXES') %>%
  str_replace('POS7', 'TRADE') %>%
  str_replace('ACASCALE', 'ACA_') %>%
  str_replace('PATHWAY', 'PATHWAY_') %>%
  
  str_replace('ECO1', 'SOCECO') %>%
  str_replace('ECO3', 'PERECO') %>%
  
  str_replace('RE24_', 'SYSLEG_') %>%

  str_replace('__', '_')

merged_df <- merged_df %>% 
  rename(PID7_6 = PID6, PID7_10 = PID10, PID7_11 = PID11, PID7_14 = PID14,
         IDEO7_6 = IDEO6, IDEO7_7 = Q11_7, IDEO7_8 = Q11_8,
         
         # Add suffix to Wave 13 variables
         INC_13 = PPINCIMP, TAXES_13 = TAXES, IDEO7_13 = IDEO7,
         DETH2_1_13 = DETH2_1, DETH2_2_13 = DETH2_2, DETH3_1_13 = DETH3_1,
         DETH3_2_13 = DETH3_2, SYSLEG_1_13 = SYSLEG_1, SYSLEG_2_13 = SYSLEG_2,
         SYSLEG_3_13 = SYSLEG_3, SYSLEG_4_13 = SYSLEG_4)

# Attitudes toward Black / Hispanic people
names(merged_df)[grepl('^DETH[2|3]', names(merged_df))] <- 
  names(merged_df)[grepl('^DETH[2|3]', names(merged_df))] %>%
  str_replace('_a_', 'A_') %>%
  str_replace('_A_', 'A_') %>%
  str_replace('_b_', 'B_') %>%
  str_replace('_B_', 'B_') %>%
  str_replace('_c_', 'C_') %>%
  str_replace('_C_', 'C_') %>%
  str_replace('_d_', 'D_') %>%
  str_replace('_D_', 'D_') %>%
  str_replace('_1_', 'A_') %>%
  str_replace('_2_', 'B_') %>%
  str_replace('_3_', 'C_') %>%
  str_replace('_4_', 'D_')

names(merged_df)[grepl('^DETH2[A|B|C]_[2-8]', names(merged_df))] <- 
  names(merged_df)[grepl('^DETH2[A|B|C]_[2-8]', names(merged_df))] %>% 
  str_replace('DETH2', 'DBLACK')

names(merged_df)[grepl('^DETH3[A|B|C]_1[0-5]', names(merged_df))] <-
  names(merged_df)[grepl('^DETH3[A|B|C]_1[0-5]', names(merged_df))] %>%
  str_replace('DETH3', 'DBLACK')

names(merged_df)[grepl('^DETH[2|3]', names(merged_df))] <-
  names(merged_df)[grepl('^DETH[2|3]', names(merged_df))] %>%
  str_replace('DETH[2|3]', 'DHISP')

# Political system legitimacy
names(merged_df)[grepl('^SYSLEG', names(merged_df))] <- 
  names(merged_df)[grepl('^SYSLEG', names(merged_df))] %>%
  str_replace('_2_9', '_9') %>% 
  str_replace('_a_', 'A_') %>%
  str_replace('_b_', 'B_') %>%
  str_replace('_c_', 'C_') %>%
  str_replace('_d_', 'D_') %>%
  str_replace('_1_', 'A_') %>%
  str_replace('_2_', 'B_') %>%
  str_replace('_3_', 'C_') %>%
  str_replace('_4_', 'D_')

merged_df <- merged_df %>% select(-starts_with('RE24_'))

### RECODE COLUMNS / CREATE INDEXES ###

# Convert all factors
merged_df <- merged_df %>% mutate_if(is.factor, as.character)

# Drop NA codes from numeric columns
merged_df <- merged_df %>% mutate_if(is.numeric, ~ case_when(
  . > 0 ~ .
))

# Attitudes toward Black / Hispanic people
merged_df <- merged_df %>% 
  mutate(across(starts_with('DBLACK'), as.numeric))

merged_df <- merged_df %>% 
  mutate(across(starts_with('DHISP'), as.numeric))

# Political system legitimacy
merged_df <- merged_df %>% 
  mutate(across(starts_with('SYSLEG') & where(is.character), ~ case_when(
    . == 'Strongly disagree' ~ 1,
    . == 'Somewhat disagree' ~ 2,
    . == 'Neither agree nor disagree' ~ 3,
    . == 'Somewhat agree' ~ 4,
    . == 'Strongly agree' ~ 5
  )))

## Flip scale
merged_df$SYSLEGA_15 <- 6 - merged_df$SYSLEGA_15 
merged_df$SYSLEGD_15 <- 6 - merged_df$SYSLEGD_15 

# Economic perceptions
merged_df <-
  merged_df %>% 
  mutate(across(starts_with('SOCECO'), ~ case_when(
    . == 'Gotten a lot worse' ~ 1,
    . == 'Gotten a little worse' ~ 2,
    . == 'Stayed about the same' ~ 3,
    . == 'Gotten a little better' ~ 4,
    . == 'Gotten a lot better' ~ 5
  )))

merged_df <-
  merged_df %>% 
  mutate(across(starts_with('PERECO'), ~ case_when(
    . == 'A lot worse off' ~ 1,
    . == 'A little worse off' ~ 2,
    . == 'Just about the same' ~ 3,
    . == 'A little better off' ~ 4,
    . == 'A lot better off' ~ 5
  )))

# Income
merged_df <- merged_df %>% 
  mutate(across(starts_with('INC_') & where(is.character), ~ case_when(
    grepl('Less than', .) ~ 1,
    grepl('7,499', .) ~ 2,
    grepl('9,999', .) ~ 3,
    grepl('12,499', .) ~ 4,
    grepl('14,999', .) ~ 5,
    grepl('19,999', .) ~ 6,
    grepl('24,999', .) ~ 7,
    grepl('29,999', .) ~ 8,
    grepl('34,999', .) ~ 9,
    grepl('39,999', .) ~ 10,
    grepl('49,999', .) ~ 11,
    grepl('59,999', .) ~ 12,
    grepl('74,999', .) ~ 13,
    grepl('84,999', .) ~ 14,
    grepl('99,999', .) ~ 15,
    grepl('124,999', .) ~ 16,
    grepl('149,999', .) ~ 17,
    grepl('174,999', .) ~ 18,
    grepl('175,000', .) ~ 19,
    grepl('249,999', .) ~ 19,
    grepl('250,000', .) ~ 19
  ))) %>% mutate(across(starts_with('INC_'), ~ case_when(
    . <= 19 ~ .,
    . > 19 ~ 19
  )))

# Ideology
merged_df <- merged_df %>% 
  mutate(across(starts_with('IDEO7') & where(is.character), ~ case_when(
    . == 'Extremely liberal' ~ 1,
    . == 'Liberal' ~ 2,
    . == 'Slightly liberal' ~ 3,
    . == 'Moderate, middle of the road' ~ 4,
    . == 'Slightly conservative' ~ 5,
    . == 'Conservative' ~ 6,
    . == 'Extremely conservative' ~ 7
  )))

# PID
merged_df <- merged_df %>% 
  mutate(across(starts_with('PID7') & where(is.character), ~ case_when(
    . == 'Strong Democrat' ~ 1,
    . == 'Not Strong Democrat' ~ 2,
    . == 'Leans Democrat' ~ 3,
    . == 'Undecided/Independent/Other' ~ 4,
    . == 'Leans Republican' ~ 5,
    . == 'Not Strong Republican' ~ 6,
    . == 'Strong Republican' ~ 7
  )))
  
merged_df <- merged_df %>% mutate(PID7_12 = case_when(
  Q9_12 == 'Strong Democrat' ~ 1,
  Q9_12 == 'Not very strong Democrat' ~ 2,
  Q10_12 == 'Democratic Party' ~ 3,
  Q10_12 == 'Refused' ~ 4,
  Q10_12 == 'Republican Party' ~ 5,
  Q8_12 == 'Not very strong Republican' ~ 6,
  Q8_12 == 'Strong Republican' ~ 7
))

merged_df <- merged_df %>% mutate(PID7_13 = case_when(
  Q9 == 'Strong Democrat' ~ 1,
  Q9 == 'Not very strong Democrat' ~ 2,
  Q10 == 'Democratic Party' ~ 3,
  Q10 == 'Refused' ~ 4,
  Q10 == 'Republican Party' ~ 5,
  Q8 == 'Not very strong Republican' ~ 6,
  Q8 == 'Strong Republican' ~ 7
))

merged_df <- merged_df %>% mutate(PID7_15 = case_when(
  Q9_15 == 1 ~ 1,
  Q9_15 == 2 ~ 2,
  Q10_15 == 2 ~ 3,
  Q10_15 == -1 ~ 4,
  Q10_15 == 1 ~ 5,
  Q8_15 == 2 ~ 6,
  Q8_15 == 1 ~ 7
))

# Taxes
merged_df <- merged_df %>% 
  mutate(across(starts_with('TAXES') & where(is.character), ~ case_when(
    grepl('raised', .)  ~ 3,
    grepl('kept pretty much', .) ~ 2,
    grepl('cut', .) ~ 1
  ))) %>%
  mutate(across(starts_with('TAXES'), ~ case_when( # Flip scale
    1 <= . & . <= 3 ~ 4 - .
  )))

# Trade
merged_df <- merged_df %>% 
  mutate(across(starts_with('TRADE') & is.character, ~ case_when(
    . == 'Strongly oppose' ~ 4,
    . == 'Somewhat oppose' ~ 3,
    . == 'Somewhat favor' ~ 2,
    . == 'Strongly favor' ~ 1
  )))

# Immigration
merged_df <- merged_df %>% mutate(PATHWAY_10 = case_when(
  RE23_A_10 == "Return illegal immigrants to their native countries" ~ 1,
  RE23_A_10 == '2' ~ 2,
  RE23_A_10 == '3' ~ 3,
  RE23_A_10 == '4' ~ 4,
  RE23_A_10 == '5' ~ 5,
  RE23_A_10 == '6' ~ 6,
  RE23_A_10 == "Create a pathway to U.S. citizenship for illegal immigrants" ~ 7
))

# merged_df$RE23_A_15[merged_df$RE23_A_15 == -1] <- NA
# merged_df$PATHWAY_15 <- merged_df$RE23_A_15

merged_df <- merged_df %>% mutate(across(starts_with('PATHWAY_'), 
                                         function (x) {8 - x})) # Flip scale

# Healthcare
merged_df$RE18_1_15[merged_df$RE18_1_15 < 1 | merged_df$RE18_1_15 > 7] <- NA
merged_df$ACA_15 <- merged_df$RE18_1_15

# Create clean dataframe
clean_df <- merged_df %>% 
  select(MNO, matches('^INC_[1-9]'), starts_with('PID7_'), starts_with('IDEO7_'),
         starts_with('TAXES_'), starts_with('TRADE_'), starts_with('ACA_'),
         matches('^PATHWAY_[1-9]'), starts_with('SOCECO_'), starts_with('PERECO_'),
         starts_with('DBLACK'), starts_with('DHISP'), starts_with('SYSLEG'),
         -taxes_1, -trade_1)

# A little more cleaning. Reorder columns alphabetically
names(clean_df) <- names(clean_df) %>% toupper()
clean_df <- clean_df %>% select(sort(names(clean_df)))

# Validity checks
## Correlations
clean_df %>% select(starts_with('ACA')) %>% cor(use = 'pairwise.complete.obs')
clean_df %>% select(starts_with('IDEO7')) %>% cor(use = 'pairwise.complete.obs')
clean_df %>% select(starts_with('INC')) %>% cor(use = 'pairwise.complete.obs')
clean_df %>% select(starts_with('PATHWAY')) %>% cor(use = 'pairwise.complete.obs')
clean_df %>% select(starts_with('PERECO')) %>% cor(use = 'pairwise.complete.obs')
clean_df %>% select(starts_with('PID7')) %>% cor(use = 'pairwise.complete.obs')
clean_df %>% select(starts_with('SOCECO')) %>% cor(use = 'pairwise.complete.obs')
clean_df %>% select(starts_with('TAXES')) %>% cor(use = 'pairwise.complete.obs')
clean_df %>% select(starts_with('TRADE')) %>% cor(use = 'pairwise.complete.obs')

clean_df %>% select(starts_with('SYSLEGA')) %>% cor(use = 'pairwise.complete.obs')
clean_df %>% select(starts_with('SYSLEGB')) %>% cor(use = 'pairwise.complete.obs')
clean_df %>% select(starts_with('SYSLEGC')) %>% cor(use = 'pairwise.complete.obs')
clean_df %>% select(starts_with('SYSLEGD')) %>% cor(use = 'pairwise.complete.obs')

clean_df %>% select(starts_with('DBLACKA')) %>% cor(use = 'pairwise.complete.obs')
clean_df %>% select(starts_with('DBLACKB')) %>% cor(use = 'pairwise.complete.obs')
clean_df %>% select(starts_with('DBLACKC')) %>% cor(use = 'pairwise.complete.obs')

clean_df %>% select(starts_with('DHISPA')) %>% cor(use = 'pairwise.complete.obs')
clean_df %>% select(starts_with('DHISPB')) %>% cor(use = 'pairwise.complete.obs')
clean_df %>% select(starts_with('DHISPC')) %>% cor(use = 'pairwise.complete.obs')
clean_df %>% select(starts_with('DHISPD')) %>% cor(use = 'pairwise.complete.obs')

## Correct scale orientation
### Get average, cross-wave PID 
clean_df$PID7_AVG <- clean_df %>% select(starts_with('PID7')) %>% rowMeans(na.rm = T)

### Create separate dataframes for each party
rep_df <- clean_df %>% select(-MNO) %>% subset(PID7_AVG > 4)
dem_df <- clean_df %>% select(-MNO) %>% subset(PID7_AVG < 4)

### Get column means
rep_colMeans <- rep_df %>% summarise_all(mean, na.rm = T) 
dem_colMeans <- dem_df %>% summarise_all(mean, na.rm = T) 

### Compare
rep_colMeans - dem_colMeans %>% view()

### Delete PID7_AVG colmumn
clean_df$PID7_AVG <- NULL

# Reshape data
clean_df <- clean_df %>%
  pivot_longer(cols = -MNO,
               names_to = c('.value', 'wave'),
               names_sep = '_')

# Convert wave numbers to years since first wave of ISCAP panel
clean_df <- clean_df %>% 
  mutate(days = case_when(
    wave == 1 ~ 0,
    wave == 2 ~ 91,
    wave == 3 ~ 183,
    wave == 4 ~ 332,
    wave == 5 ~ 400,
    wave == 6 ~ 1844,
    wave == 7 ~ 1870,
    wave == 8 ~ 2572,
    wave == 9 ~ 2605,
    wave == 10 ~ 3034,
    wave == 11 ~ 3300,
    wave == 12 ~ 3345,
    wave == 13 ~ 4040,
    wave == 14 ~ 4498,
    wave == 15 ~ 4755
  )) %>%
  mutate(years = days / 365) %>%
  select(-days)

clean_df$wave <- NULL

# Create indices
## Black prejudice
clean_df %>% 
  select(starts_with('DBLACK')) %>% 
  psych::alpha()

clean_df$DBLACK <- clean_df %>% 
  select(starts_with('DBLACK')) %>% 
  rowMeans(na.rm = T)

## Hispanic prejudice
clean_df %>% 
  select(starts_with('DHISP')) %>% 
  psych::alpha()

clean_df$DHISP <- clean_df %>% 
  select(starts_with('DHISP')) %>% 
  rowMeans(na.rm = T)

## System legitimacy
### FLip scale
clean_df <- clean_df %>% 
  mutate(SYSLEGB = 6 - SYSLEGB, SYSLEGD = 6 - SYSLEGD)

### Alpha
clean_df %>% 
  select(starts_with('SYSLEG')) %>% 
  psych::alpha()

### Index
clean_df$SYSLEG <- clean_df %>% 
  select(starts_with('SYSLEG')) %>% 
  rowMeans(na.rm = T)

## Delete items
clean_df <- clean_df %>% 
  select(-DBLACKA, -DBLACKB, -DBLACKC, -DHISPA, -DHISPB, -DHISPC, -DHISPD, 
         -SYSLEGA, -SYSLEGB, -SYSLEGC, -SYSLEGD)

# Create CSV
clean_df <- clean_df %>% subset(rowSums(is.na(clean_df)) != ncol(clean_df) - 1)

save(clean_df, file = 'closedEnds_data/clean_data/change_data.RData')
