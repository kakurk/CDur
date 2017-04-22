## ----- load_data

# Initalize variables
datapath           <- './data/'
ya_raw_data_file   <- paste0(datapath, "ya_raw_data.csv")
oa_raw_data_file   <- paste0(datapath, "oa_raw_data.csv")

# Load data into dataframes
df.young        <- read.csv(ya_raw_data_file, quote = "\'")
df.old          <- read.csv(oa_raw_data_file, quote = "\'")

## ----- clean_and_concatenate

# concatenate
df.all <- bind_rows(list(df.young, df.old), .id = "AgeGroup")

# clean
df.all %>%
  select(-jit.1) %>%
  mutate(
    enctype  = ifelse(enctype == 1, '1 Second',
                      ifelse(enctype == 3, '3 Seconds', 
                             ifelse(enctype == 5, '5 Seconds', 'Lure'))),
    enctype  = factor(enctype),
    score    = ifelse(score == 1, 'Remember',
                      ifelse(score == 2, 'Know',
                             ifelse(score == 3, 'New', 'No Response'))),
    score    = factor(score),
    Response = ifelse(Response == 2, 'B Key',
                      ifelse(Response == 22, 'V Key',
                             ifelse(Response == 14, 'N Key', 'No Response'))),
    Response = factor(Response),
    type     = ifelse(type == 0, 'To Be Forgotten', 
                      ifelse(type == 1, 'To Be Remembered', 'Lure')),
    type     = factor(type),
    AgeGroup = ifelse(AgeGroup == 1, 'Young', 
                      ifelse(AgeGroup == 2, 'Old', NA)),
    AgeGroup = factor(AgeGroup)
  ) -> df.all

## ----- t_tests

# DF Effect Checkss
df.all %>%
  filter(enctype != "Lure") %>%
  mutate(enctype = factor(enctype)) %>%
  group_by(AgeGroup, enctype, type, subject) %>%
  summarize(MissRate = mean(score == "New")) %>% ungroup() -> df.missrates

df.missrates %>% filter(type == "To Be Forgotten") %>% select(-type) -> TBF.Forgotten
df.missrates %>% filter(type == "To Be Remembered") %>% select(-type) -> TBR.Forgotten

df.DFeffects <- left_join(TBF.Forgotten, TBR.Forgotten, by = c('AgeGroup','enctype', 'subject'), suffix = c("TBF", "TBR"))

df.DFeffects %>%
  transmute(
    AgeGroup = AgeGroup,
    enctype = enctype,
    subject = subject,
    DF.effect = MissRateTBF - MissRateTBR) -> df.DFeffects

# T-Tests by Condition
onesecond   <- t.test(df.DFeffects$DF.effect[df.DFeffects$enctype == '1 Second'], mu = 0)
threesecond <- t.test(df.DFeffects$DF.effect[df.DFeffects$enctype == '3 Seconds'], mu = 0)
fivesecond  <- t.test(df.DFeffects$DF.effect[df.DFeffects$enctype == '5 Seconds'], mu = 0)

# T-Tests by Age Group
young.test <- t.test(df.DFeffects$DF.effect[df.DFeffects$AgeGroup == 'Young'], mu = 0)
old.test   <- t.test(df.DFeffects$DF.effect[df.DFeffects$AgeGroup == 'Old'], mu = 0)


# T-Tests by Age Group by Condition
YA.onesecond   <- t.test(df.DFeffects$DF.effect[df.DFeffects$enctype == '1 Second' & df.DFeffects$AgeGroup == 'Young'], mu = 0)
YA.threesecond <- t.test(df.DFeffects$DF.effect[df.DFeffects$enctype == '3 Seconds' & df.DFeffects$AgeGroup == 'Young'], mu = 0)
YA.fivesecond  <- t.test(df.DFeffects$DF.effect[df.DFeffects$enctype == '5 Seconds' & df.DFeffects$AgeGroup == 'Young'], mu = 0)

OA.oneseconds  <- t.test(df.DFeffects$DF.effect[df.DFeffects$enctype == '1 Second' & df.DFeffects$AgeGroup == 'Old'], mu = 0)
OA.threesecond <- t.test(df.DFeffects$DF.effect[df.DFeffects$enctype == '3 Seconds' & df.DFeffects$AgeGroup == 'Old'], mu = 0)
OA.fivesecond  <- t.test(df.DFeffects$DF.effect[df.DFeffects$enctype == '5 Seconds' & df.DFeffects$AgeGroup == 'Old'], mu = 0)

## ---- AgebyCueDurANOVA_DF.effect

# ANOVA
m <- aov(DF.effect ~ AgeGroup + AgeGroup*enctype + Error(subject/(enctype)), data = df.DFeffects)

mean_tbl <- model.tables(m, "means")

# Summarize, break into between and repeated factors
object   <- summary(m)
between  <- object$`Error: subject`
repeated <- object$`Error: subject:enctype`

# bonferroni follow-up t-tests
follow.up.pairwise <- pairwise.t.test(df.DFeffects$DF.effect, df.DFeffects$enctype, p.adj = "bonferroni")
