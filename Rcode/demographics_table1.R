# R Code for creating the demographics table

## ---- load_packages
library('tidyverse')
library('knitr')

## ---- load_data
datapath           <- "./data/"
demo_raw_data_file <- paste0(datapath, "demographics.csv")
df.demographics    <- read.csv(demo_raw_data_file, quote = "\'")

## ---- clean_data
df.demographics %>%
  select(-X, -X.1) %>%
  mutate(EDUCATION_yrs = ifelse(EDUCATION == '8th grade', 8,
                         ifelse(EDUCATION == '9th grade', 9, 
                         ifelse(EDUCATION == '10th grade', 10,
                         ifelse(EDUCATION == '11th grade', 11,
                         ifelse(EDUCATION == '12th grade', 12,
                         ifelse(EDUCATION == '1 yr college' | EDUCATION == '1 Year college' | EDUCATION == '1 year college', 13,
                         ifelse(EDUCATION == '2 yrs college' | EDUCATION == '2 Years college' | EDUCATION == '2 years college', 14,
                         ifelse(EDUCATION == '3 yrs college' | EDUCATION == '3 Years college' | EDUCATION == '3 years college', 15,
                         ifelse(EDUCATION == '4 yrs college' | EDUCATION == '4 Years college' | EDUCATION == '4 years college', 16,
                         ifelse(EDUCATION == 'Post-grad work', 17,
                         ifelse(EDUCATION == 'Masters', 18,
                         ifelse(EDUCATION == 'Doctoral', 20, NA)))))))))))),
         Sex = ifelse(Sex == 'F', 'Female', 
                      ifelse(Sex == 'M', 'Male', NA))) -> df.demographics 

# Force Age and Sex variables into the all capitalization variable name convention
colnames(df.demographics)[10:11] <- c("AGE", "SEX")

# Calculating Cognitive Assessment Composite Score
df.demographics %>%
  rowwise() %>%
  mutate(COMPOSITE = mean(c(SYMBOLSEARCH, DIGITSYMBOL, DIGITSPAN, 
                                            ARITHMETIC, LNSEQUENCING, VOCAB), na.rm = TRUE),
         COMPOSITE = round(COMPOSITE, digits = 2)) -> df.demographics

## ---- create_table

df.demographics %>%
  mutate(
    AgeGroup = factor(ifelse(AGE > 30, 'Old Adults', 'Younger Adults'))
  ) %>%
  select(-SUBJECT) %>%
  group_by(AgeGroup) %>%
  summarize_if(is.numeric, mean, na.rm = TRUE) %>%
  mutate_if(is.numeric, round, digits = 2) %>%
  gather(key=Measure, value=Mean, -AgeGroup) ->  temp

temp %>%
  filter(AgeGroup == "Old Adults") -> OAs
temp %>%
  filter(AgeGroup == "Younger Adults") -> YAs

table1.demographics  <- left_join(select(OAs, -AgeGroup), select(YAs, -AgeGroup), by = c("Measure"), suffix = c("_OA", "_YA"), "-AgeGroup")

names(table1.demographics) <- c("Measure", "Older Adults", "Younger Adults")

table1.demographics[9,1] <- "EDUCATION"
table1.demographics[10,1] <- "COMPOSITE"

## ---- print_table

kable(table1.demographics, format = "latex", caption = "Table 1 Participant Demographics")
