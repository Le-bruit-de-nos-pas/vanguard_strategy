library(haven)  
library(data.table) 
library(tidyverse)

# Import data ---------------------------------
Pfizer_Migraine_Pat <- read_sav("Pfizer_Migraine_Pat v.2.0.sav")

Pfizer_Migraine_Pat[1:10, ]

sort(names(Pfizer_Migraine_Pat))

length(unique(Pfizer_Migraine_Pat$patNum)) # 7812
length(unique(Pfizer_Migraine_Pat$docNum)) # 747

Pfizer_Migraine_Pat[1:10, 1:10]

Pfizer_Migraine_Pat %>% group_by(qcountries) %>% count()



# ------------------

# UK Acute Lines Long Format -----------------------------------------------------

Pfizer_Migraine_Pat <- read_sav("Pfizer_Migraine_Pat v.2.0.sav")

UK_df <- Pfizer_Migraine_Pat %>% filter(qcountries==27)

UK_df %>% select(startacuteline1, endacuteline1, acutelinedur1)

Starts_acute_lines <- UK_df %>% select(patNum, contains("startacuteline") )

Starts_acute_lines <- gather(Starts_acute_lines, acuteline, start, 
                             startacuteline1:startacuteline6, factor_key=FALSE)

Starts_acute_lines <- Starts_acute_lines %>% arrange(patNum) %>% drop_na() %>% 
  mutate(acuteline=parse_number(acuteline)) %>% arrange(patNum)

Ends_acute_lines <- UK_df %>% select(patNum, contains("endacuteline") )

Ends_acute_lines <- gather(Ends_acute_lines, acuteline, end, 
                           endacuteline1:endacuteline6, factor_key=FALSE)

Ends_acute_lines <- Ends_acute_lines %>% arrange(patNum) %>% drop_na() %>% 
  mutate(acuteline=parse_number(acuteline)) %>% arrange(patNum)

Duration_acute_lines <- UK_df %>% select(patNum, contains("acutelinedur") )

Duration_acute_lines <- gather(Duration_acute_lines, acuteline, 
                               duration, acutelinedur1:acutelinedur6, factor_key=FALSE)

Duration_acute_lines <- Duration_acute_lines %>% arrange(patNum) %>% drop_na() %>% 
  mutate(acuteline=parse_number(acuteline)) %>% arrange(patNum)

Lines_durations <- Starts_acute_lines %>%  
  left_join(Ends_acute_lines) %>%  
  left_join(Duration_acute_lines) %>% 
  arrange(patNum) %>% group_by(patNum) %>% 
  mutate(end2 = ifelse( is.na(duration), lead(start) - 1, end)) 

Lines_durations <- Lines_durations %>% group_by(patNum) %>% 
  mutate(end = ifelse( is.na(end), end2, end)) %>% 
  mutate(duration = ifelse( is.na(duration), start - end, duration))

mean(Lines_durations$start) # 1470.858
mean(Lines_durations$end, na.rm = TRUE) # 1487.367
mean(Lines_durations$duration, na.rm = TRUE) # 1075.929

Lines_durations <- Lines_durations %>% select(-end2)

Lines_durations <- Lines_durations %>% 
  mutate(end = ifelse( is.na(end), 0, end )) %>%
  mutate(end = round (-end / 30.5, 0) ) %>% mutate(start = round(-start / 30.5, 0) ) %>%
  group_by(patNum) %>% mutate(end = ifelse( end == lead(start), end-1, end)) %>%
    mutate(end = ifelse( is.na(end), 0, end )) 

data.frame( Lines_durations %>% group_by(patNum) %>% count() %>% ungroup() %>% 
  filter(n>=3) %>% select(patNum) %>% left_join(Lines_durations) )

Lines_durations <- Lines_durations %>% select(-duration)

data.frame(Lines_durations)

df <- Lines_durations %>%
  mutate(month_number = map2(start, end, seq)) %>%
  unnest(month_number)

df <- df %>% select(patNum, acuteline, month_number)

df <- df %>% ungroup() %>% arrange(patNum, acuteline, month_number) %>% 
  group_by(patNum) %>% mutate(month_number2=ifelse(month_number==lead(month_number), 1, 0)) %>%
  filter(month_number2 == 0) %>% ungroup() %>% select(-month_number2)
  
wide_df <- pivot_wider(
  data = df,
  id_cols = c("patNum"),
  names_from = month_number,
  values_from = acuteline,
  values_fill = 0
)

wide_df <- wide_df[, order(as.numeric(colnames(wide_df)), decreasing = FALSE)]

wide_df[, 550:555]

sum(is.na(wide_df))

names(wide_df)

wide_df <- wide_df[, 494:555]

names(wide_df)

wide_df %>% select(patNum, `-60`:`-1`)

wide_df <- gather(wide_df, Month, Line, `-61`:`-1`) %>% 
  mutate(Month=as.numeric(Month) + 61) %>%
  spread(key=Month, value=Line)

wide_df <- UK_df %>% select(patNum)  %>% left_join(wide_df)

wide_df[is.na(wide_df)] <- 0

fwrite(wide_df, "UK_AcuteLines_Wide.txt", sep="\t")

# --------------------

# UK Preventive Lines Long Format -----------------------------------------------------

Pfizer_Migraine_Pat <- read_sav("Pfizer_Migraine_Pat v.2.0.sav")

UK_df <- Pfizer_Migraine_Pat %>% filter(qcountries==27)

UK_df %>% select(startprevline1, endprevline1, prevlinedur1)

Starts_prev_lines <- UK_df %>% select(patNum, contains("startprevline") )

Starts_prev_lines <- gather(Starts_prev_lines, prevline, start, 
                             startprevline1:startprevline6, factor_key=FALSE)

Starts_prev_lines <- Starts_prev_lines %>% arrange(patNum) %>% drop_na() %>% 
  mutate(prevline=parse_number(prevline)) %>% arrange(patNum)

Ends_prev_lines <- UK_df %>% select(patNum, contains("endprevline") )

Ends_prev_lines <- gather(Ends_prev_lines, prevline, end, 
                           endprevline1:endprevline6, factor_key=FALSE)

Ends_prev_lines <- Ends_prev_lines %>% arrange(patNum) %>% drop_na() %>% 
  mutate(prevline=parse_number(prevline)) %>% arrange(patNum)

Duration_prev_lines <- UK_df %>% select(patNum, contains("prevlinedur") )

Duration_prev_lines <- gather(Duration_prev_lines, prevline, 
                               duration, prevlinedur1:prevlinedur6, factor_key=FALSE)

Duration_prev_lines <- Duration_prev_lines %>% arrange(patNum) %>% drop_na() %>% 
  mutate(prevline=parse_number(prevline)) %>% arrange(patNum)

Lines_durations <- Starts_prev_lines %>%  
  left_join(Ends_prev_lines) %>%  
  left_join(Duration_prev_lines) %>% 
  arrange(patNum) %>% group_by(patNum) %>% 
  mutate(end2 = ifelse( is.na(duration), lead(start) - 1, end)) 

Lines_durations <- Lines_durations %>% group_by(patNum) %>% 
  mutate(end = ifelse( is.na(end), end2, end)) %>% 
  mutate(duration = ifelse( is.na(duration), start - end, duration))

mean(Lines_durations$start) # 955.892
mean(Lines_durations$end, na.rm = TRUE) # 816.9651
mean(Lines_durations$duration, na.rm = TRUE) # 626.1194

Lines_durations <- Lines_durations %>% select(-end2)

Lines_durations <- Lines_durations %>% 
  mutate(end = ifelse( is.na(end), 0, end )) %>%
  mutate(end = round (-end / 30.5, 0) ) %>% mutate(start = round(-start / 30.5, 0) ) %>%
  group_by(patNum) %>% mutate(end = ifelse( end == lead(start), end-1, end)) %>%
    mutate(end = ifelse( is.na(end), 0, end )) 

data.frame( Lines_durations %>% group_by(patNum) %>% count() %>% ungroup() %>% 
  filter(n>=3) %>% select(patNum) %>% left_join(Lines_durations) )

Lines_durations <- Lines_durations %>% select(-duration)

data.frame(Lines_durations)

df <- Lines_durations %>%
  mutate(month_number = map2(start, end, seq)) %>%
  unnest(month_number)

df <- df %>% select(patNum, prevline, month_number)

df <- df %>% ungroup() %>% arrange(patNum, prevline, month_number) %>% 
  group_by(patNum) %>% mutate(month_number2=ifelse(month_number==lead(month_number), 1, 0)) %>%
  filter(month_number2 == 0) %>% ungroup() %>% select(-month_number2)
  
df <- df %>% group_by(patNum, month_number) %>% filter(prevline==max(prevline))


wide_df <- pivot_wider(
  data = df,
  id_cols = c("patNum"),
  names_from = month_number,
  values_from = prevline,
  values_fill = 0
)


wide_df <- wide_df[, order(as.numeric(colnames(wide_df)), decreasing = FALSE)]

sum(is.na(wide_df))

names(wide_df)

wide_df <- wide_df[, 273:334]

names(wide_df)

wide_df %>% select(patNum, `-60`:`-1`)

wide_df <- gather(wide_df, Month, Line, `-61`:`-1`) %>% 
  mutate(Month=as.numeric(Month) + 61) %>%
  spread(key=Month, value=Line)

wide_df <- UK_df %>% select(patNum)  %>% left_join(wide_df)

wide_df[is.na(wide_df)] <- 0

fwrite(wide_df, "UK_PrevLines_Wide.txt", sep="\t")

# --------------------
# UK Molecule composition of Acute Lines ----------------------------------

Pfizer_Migraine_Pat <- read_sav("Pfizer_Migraine_Pat v.2.0.sav")

UK_df <- Pfizer_Migraine_Pat %>% filter(qcountries==27)

Acute_molecules <- UK_df %>% select(patNum, contains("acuteline") ) %>% 
  select(patNum, !contains("switch") ) %>%
  select(patNum, !contains("end") ) %>% 
  select(patNum, !contains("start") ) %>%
  select(patNum, !contains("headday") ) %>%
  select(patNum, !contains("dur") ) %>%
  select(patNum, !contains("lines") )

length(names(Acute_molecules)) == (1 + 6*345) 

Acute_molecules <- gather(Acute_molecules, acuteline, use, acuteline1_1:acuteline6_9501)

Acute_molecules <- Acute_molecules %>% arrange(patNum)

Acute_molecules <- Acute_molecules %>% mutate(
  line_number = str_extract(acuteline, "(?<=acuteline)\\d+"),
  molecule_number = str_extract(acuteline, "(?<=_)\\d+")
  ) %>% 
  filter(use==1) %>%
  select(-c(acuteline, use)) %>% filter(molecule_number != "9501")


unique(Acute_molecules$line_number)
unique(Acute_molecules$molecule_number)

Acute_molecules <- Acute_molecules %>% arrange(patNum, line_number, molecule_number) %>%
  group_by(patNum, line_number) %>% 
  mutate(molecule_number = paste0(molecule_number, collapse= ",")) %>% distinct()

fwrite(Acute_molecules , "Acute_Lines_MoleculeComposition.txt", sep="\t")

# --------------------
# UK Molecule composition of Preventive Lines ----------------------------------

Pfizer_Migraine_Pat <- read_sav("Pfizer_Migraine_Pat v.2.0.sav")

UK_df <- Pfizer_Migraine_Pat %>% filter(qcountries==27)

Prev_molecules <- UK_df %>% select(patNum, contains("prevline") ) %>% 
  select(patNum, !contains("switch") ) %>%
  select(patNum, !contains("end") ) %>% 
  select(patNum, !contains("start") ) %>%
  select(patNum, !contains("headday") ) %>%
  select(patNum, !contains("dur") ) %>%
  select(patNum, !contains("lines") )

data.framenames <- data.frame(names(Prev_molecules))

Prev_molecules <- gather(Prev_molecules, prevline, use, prevline1_1:prevline6_34801)

Prev_molecules <- Prev_molecules %>% arrange(patNum)

Prev_molecules <- Prev_molecules %>% mutate(
  line_number = str_extract(prevline, "(?<=prevline)\\d+"),
  molecule_number = str_extract(prevline, "(?<=_)\\d+")
  ) %>% 
  filter(use==1) %>%
  select(-c(prevline, use)) %>% filter(molecule_number != "9501" & molecule_number != "34801")


unique(Prev_molecules$line_number)
unique(Prev_molecules$molecule_number)

Prev_molecules <- Prev_molecules %>% arrange(patNum, line_number, molecule_number) %>%
  group_by(patNum, line_number) %>% 
  mutate(molecule_number = paste0(molecule_number, collapse= ",")) %>% distinct()

fwrite(Prev_molecules , "Prev_Lines_MoleculeComposition.txt", sep="\t")

# --------------------
# UK Create Wide Format with all individual molecules joined -------

UK_AcuteLines_Wide <- fread("UK_AcuteLines_Wide.txt")
UK_PrevLines_Wide <- fread("UK_PrevLines_Wide.txt")
Acute_Lines_MoleculeComposition <- fread("Acute_Lines_MoleculeComposition.txt")
Prev_Lines_MoleculeComposition <- fread("Prev_Lines_MoleculeComposition.txt")


UK_AcuteLines_Wide <- UK_AcuteLines_Wide %>% gather(month, line, `0`:`60`) 

head(UK_AcuteLines_Wide) ; head(Acute_Lines_MoleculeComposition)

UK_AcuteLines_Wide <- UK_AcuteLines_Wide %>% left_join(Acute_Lines_MoleculeComposition, by=c("patNum"="patNum", "line"="line_number"))

UK_AcuteLines_Wide[is.na(UK_AcuteLines_Wide)] <- "-"

UK_AcuteLines_Wide <- UK_AcuteLines_Wide %>% select(-line)



UK_PrevLines_Wide <- UK_PrevLines_Wide %>% gather(month, line, `0`:`60`) 

head(UK_PrevLines_Wide) ; head(Prev_Lines_MoleculeComposition)

UK_PrevLines_Wide <- UK_PrevLines_Wide %>% left_join(Prev_Lines_MoleculeComposition, by=c("patNum"="patNum", "line"="line_number"))

UK_PrevLines_Wide[is.na(UK_PrevLines_Wide)] <- "-"

UK_PrevLines_Wide <- UK_PrevLines_Wide %>% select(-line)

all_molecules_df <- UK_AcuteLines_Wide %>% bind_rows(UK_PrevLines_Wide) %>% distinct()

head(all_molecules_df)

all_molecules_df <- separate_rows(all_molecules_df, molecule_number, sep = ",", convert=T )

all_molecules_df <- all_molecules_df %>% distinct()

all_molecules_df <- all_molecules_df %>% filter(molecule_number!="-" ) 

all_molecules_df <- all_molecules_df %>% arrange(patNum, month, molecule_number) %>%
  group_by(patNum, month) %>% mutate(molecule_number = paste(molecule_number, collapse = ",")) %>%
  distinct()

unique(all_molecules_df$molecule_number)

all_molecules_df <- all_molecules_df %>% spread(key=month, value=molecule_number)

all_molecules_df[is.na(all_molecules_df)] <- "-"

UK_AcuteLines_Wide <- fread("UK_AcuteLines_Wide.txt")

all_molecules_df <- UK_AcuteLines_Wide %>% select(patNum) %>% left_join(all_molecules_df)

all_molecules_df[is.na(all_molecules_df)] <- "-"

names(all_molecules_df)

all_molecules_df <- all_molecules_df %>% select(patNum, as.character(0:60))

all_molecules_df %>% gather(month, line, `0`:`60`) %>%
  arrange(patNum, as.numeric(month)) %>% group_by(patNum) %>% filter(line=="-" & lag(line) != "-")


fwrite(all_molecules_df, "UK_AllMolecules_OverTime.txt", sep="\t")


# ------------------------
# UK Allocate EXTRA/MISSING patients ON month 60 based on "Current" columns -----------

Pfizer_Migraine_Pat <- read_sav("Pfizer_Migraine_Pat v.2.0.sav")

UK_df <- Pfizer_Migraine_Pat %>% filter(qcountries==27)

all_molecules_df <- fread("UK_AllMolecules_OverTime.txt", sep="\t")

Current_df <- UK_df %>% select(patNum, Prevent_Generic_Curr_1, Acute_Generic_Curr_1) 

Current_df <- gather(Current_df, line, drug, Prevent_Generic_Curr_1:Acute_Generic_Curr_1)

Current_df <- Current_df %>% filter(drug!=9901)

Current_df <- Current_df %>% select(patNum, drug) %>%
  distinct() %>% arrange(patNum, drug) %>% group_by(patNum) %>%
  mutate(drug = paste(drug, collapse = ",")) %>% distinct()

to_check <- all_molecules_df %>% select(patNum, `60`) %>% 
  left_join(Current_df %>% mutate(patNum = as.numeric(patNum)))

to_check[is.na(to_check)] <- "-"

to_check <- gather(to_check, col, drugs, `60`:drug)

to_check <- to_check %>% select(-col) %>% distinct() %>% filter(drugs!="-")

to_check <- separate_rows(to_check, drugs, sep = ",", convert=T )

to_check <- to_check %>% distinct() %>% arrange(patNum, drugs) %>% 
  group_by(patNum) %>% mutate(drugs= paste(drugs, collapse = ","))  %>% distinct()

all_molecules_df <- all_molecules_df %>% select(-`60`) %>% left_join(to_check)

all_molecules_df[is.na(all_molecules_df)] <- "-"

names(all_molecules_df)[62] <- "60"

fwrite(all_molecules_df, "UK_AllMolecules_OverTime_m60extended.txt", sep="\t")

all_molecules_df <- fread("UK_AllMolecules_OverTime_m60extended.txt", sep="\t")

all_molecules_df[grepl("352", `60`), ]


# -------------------------
# UK Drug to class mapping -----------

Pfizer_Migraine_Pat <- read_sav("Pfizer_Migraine_Pat v.2.0.sav")

UK_df <- Pfizer_Migraine_Pat %>% filter(qcountries==27)

Acute_1 <- UK_df %>% select(Acute_Generic_Curr_1, Acute_Class_Curr_1 ) %>% distinct() %>%
  mutate(Acute_Generic_Curr_1=as.numeric(Acute_Generic_Curr_1)) %>%
  mutate(Acute_Class_Curr_1=as.numeric(Acute_Class_Curr_1)) %>% 
  rename("Generic"="Acute_Generic_Curr_1", "Class"="Acute_Class_Curr_1") %>% 
  filter(Generic!=9901) %>% drop_na()
  
Acute_2 <- UK_df %>% select(Acute_Generic_Curr_2, Acute_Class_Curr_2 ) %>% distinct() %>%
  mutate(Acute_Generic_Curr_2=as.numeric(Acute_Generic_Curr_2)) %>%
  mutate(Acute_Class_Curr_2=as.numeric(Acute_Class_Curr_2)) %>% 
  rename("Generic"="Acute_Generic_Curr_2", "Class"="Acute_Class_Curr_2") %>% 
  filter(Generic!=9901) %>% drop_na()

Acute_3 <- UK_df %>% select(Acute_Generic_Curr_3, Acute_Class_Curr_3 ) %>% distinct() %>%
  mutate(Acute_Generic_Curr_3=as.numeric(Acute_Generic_Curr_3)) %>%
  mutate(Acute_Class_Curr_3=as.numeric(Acute_Class_Curr_3)) %>% 
  rename("Generic"="Acute_Generic_Curr_3", "Class"="Acute_Class_Curr_3") %>% 
  filter(Generic!=9901) %>% drop_na()

Acute_4 <- UK_df %>% select(Acute_Generic_Curr_4, Acute_Class_Curr_4 ) %>% distinct() %>%
  mutate(Acute_Generic_Curr_4=as.numeric(Acute_Generic_Curr_4)) %>%
  mutate(Acute_Class_Curr_4=as.numeric(Acute_Class_Curr_4)) %>% 
  rename("Generic"="Acute_Generic_Curr_4", "Class"="Acute_Class_Curr_4") %>% 
  filter(Generic!=9901) %>% drop_na()

Acute_5 <- UK_df %>% select(Acute_Generic_Curr_5, Acute_Class_Curr_5 ) %>% distinct() %>%
  mutate(Acute_Generic_Curr_5=as.numeric(Acute_Generic_Curr_5)) %>%
  mutate(Acute_Class_Curr_5=as.numeric(Acute_Class_Curr_5)) %>% 
  rename("Generic"="Acute_Generic_Curr_5", "Class"="Acute_Class_Curr_5") %>% 
  filter(Generic!=9901) %>% drop_na()

Acute_6 <- UK_df %>% select(Acute_Generic_Curr_6, Acute_Class_Curr_6 ) %>% distinct() %>%
  mutate(Acute_Generic_Curr_6=as.numeric(Acute_Generic_Curr_6)) %>%
  mutate(Acute_Class_Curr_6=as.numeric(Acute_Class_Curr_6)) %>% 
  rename("Generic"="Acute_Generic_Curr_6", "Class"="Acute_Class_Curr_6") %>% 
  filter(Generic!=9901) %>% drop_na()


Acute_7 <- UK_df %>% select(Acute_Generic_Curr_7, Acute_Class_Curr_7 ) %>% distinct() %>%
  mutate(Acute_Generic_Curr_7=as.numeric(Acute_Generic_Curr_7)) %>%
  mutate(Acute_Class_Curr_7=as.numeric(Acute_Class_Curr_7)) %>% 
  rename("Generic"="Acute_Generic_Curr_7", "Class"="Acute_Class_Curr_7") %>% 
  filter(Generic!=9901) %>% drop_na()



Acute_8 <- UK_df %>% select(Acute_Generic_Curr_8, Acute_Class_Curr_8 ) %>% distinct() %>%
  mutate(Acute_Generic_Curr_8=as.numeric(Acute_Generic_Curr_8)) %>%
  mutate(Acute_Class_Curr_8=as.numeric(Acute_Class_Curr_8)) %>% 
  rename("Generic"="Acute_Generic_Curr_8", "Class"="Acute_Class_Curr_8") %>% 
  filter(Generic!=9901) %>% drop_na()



Acute_9 <- UK_df %>% select(Acute_Generic_Curr_9, Acute_Class_Curr_9 ) %>% distinct() %>%
  mutate(Acute_Generic_Curr_9=as.numeric(Acute_Generic_Curr_9)) %>%
  mutate(Acute_Class_Curr_9=as.numeric(Acute_Class_Curr_9)) %>% 
  rename("Generic"="Acute_Generic_Curr_9", "Class"="Acute_Class_Curr_9") %>% 
  filter(Generic!=9901) %>% drop_na()


Acute_10 <- UK_df %>% select(Acute_Generic_Curr_10, Acute_Class_Curr_10 ) %>% distinct() %>%
  mutate(Acute_Generic_Curr_10=as.numeric(Acute_Generic_Curr_10)) %>%
  mutate(Acute_Class_Curr_10=as.numeric(Acute_Class_Curr_10)) %>% 
  rename("Generic"="Acute_Generic_Curr_10", "Class"="Acute_Class_Curr_10") %>% 
  filter(Generic!=9901) %>% drop_na()


Prevent_1 <- UK_df %>% select(Prevent_Generic_Curr_1, Prevent_Class_Curr_1 ) %>% distinct() %>%
  mutate(Prevent_Generic_Curr_1=as.numeric(Prevent_Generic_Curr_1)) %>%
  mutate(Prevent_Class_Curr_1=as.numeric(Prevent_Class_Curr_1)) %>% 
  rename("Generic"="Prevent_Generic_Curr_1", "Class"="Prevent_Class_Curr_1") %>% 
  filter(Generic!=9901) %>% drop_na()
  
Prevent_2 <- UK_df %>% select(Prevent_Generic_Curr_2, Prevent_Class_Curr_2 ) %>% distinct() %>%
  mutate(Prevent_Generic_Curr_2=as.numeric(Prevent_Generic_Curr_2)) %>%
  mutate(Prevent_Class_Curr_2=as.numeric(Prevent_Class_Curr_2)) %>% 
  rename("Generic"="Prevent_Generic_Curr_2", "Class"="Prevent_Class_Curr_2") %>% 
  filter(Generic!=9901) %>% drop_na()

Prevent_3 <- UK_df %>% select(Prevent_Generic_Curr_3, Prevent_Class_Curr_3 ) %>% distinct() %>%
  mutate(Prevent_Generic_Curr_3=as.numeric(Prevent_Generic_Curr_3)) %>%
  mutate(Prevent_Class_Curr_3=as.numeric(Prevent_Class_Curr_3)) %>% 
  rename("Generic"="Prevent_Generic_Curr_3", "Class"="Prevent_Class_Curr_3") %>% 
  filter(Generic!=9901) %>% drop_na()

Prevent_4 <- UK_df %>% select(Prevent_Generic_Curr_4, Prevent_Class_Curr_4 ) %>% distinct() %>%
  mutate(Prevent_Generic_Curr_4=as.numeric(Prevent_Generic_Curr_4)) %>%
  mutate(Prevent_Class_Curr_4=as.numeric(Prevent_Class_Curr_4)) %>% 
  rename("Generic"="Prevent_Generic_Curr_4", "Class"="Prevent_Class_Curr_4") %>% 
  filter(Generic!=9901) %>% drop_na()

Prevent_5 <- UK_df %>% select(Prevent_Generic_Curr_5, Prevent_Class_Curr_5 ) %>% distinct() %>%
  mutate(Prevent_Generic_Curr_5=as.numeric(Prevent_Generic_Curr_5)) %>%
  mutate(Prevent_Class_Curr_5=as.numeric(Prevent_Class_Curr_5)) %>% 
  rename("Generic"="Prevent_Generic_Curr_5", "Class"="Prevent_Class_Curr_5") %>% 
  filter(Generic!=9901) %>% drop_na()

Prevent_6 <- UK_df %>% select(Prevent_Generic_Curr_6, Prevent_Class_Curr_6 ) %>% distinct() %>%
  mutate(Prevent_Generic_Curr_6=as.numeric(Prevent_Generic_Curr_6)) %>%
  mutate(Prevent_Class_Curr_6=as.numeric(Prevent_Class_Curr_6)) %>% 
  rename("Generic"="Prevent_Generic_Curr_6", "Class"="Prevent_Class_Curr_6") %>% 
  filter(Generic!=9901) %>% drop_na()


Prevent_7 <- UK_df %>% select(Prevent_Generic_Curr_7, Prevent_Class_Curr_7 ) %>% distinct() %>%
  mutate(Prevent_Generic_Curr_7=as.numeric(Prevent_Generic_Curr_7)) %>%
  mutate(Prevent_Class_Curr_7=as.numeric(Prevent_Class_Curr_7)) %>% 
  rename("Generic"="Prevent_Generic_Curr_7", "Class"="Prevent_Class_Curr_7") %>% 
  filter(Generic!=9901) %>% drop_na()



Prevent_8 <- UK_df %>% select(Prevent_Generic_Curr_8, Prevent_Class_Curr_8 ) %>% distinct() %>%
  mutate(Prevent_Generic_Curr_8=as.numeric(Prevent_Generic_Curr_8)) %>%
  mutate(Prevent_Class_Curr_8=as.numeric(Prevent_Class_Curr_8)) %>% 
  rename("Generic"="Prevent_Generic_Curr_8", "Class"="Prevent_Class_Curr_8") %>% 
  filter(Generic!=9901) %>% drop_na()



Prevent_9 <- UK_df %>% select(Prevent_Generic_Curr_9, Prevent_Class_Curr_9 ) %>% distinct() %>%
  mutate(Prevent_Generic_Curr_9=as.numeric(Prevent_Generic_Curr_9)) %>%
  mutate(Prevent_Class_Curr_9=as.numeric(Prevent_Class_Curr_9)) %>% 
  rename("Generic"="Prevent_Generic_Curr_9", "Class"="Prevent_Class_Curr_9") %>% 
  filter(Generic!=9901) %>% drop_na()


Prevent_10 <- UK_df %>% select(Prevent_Generic_Curr_10, Prevent_Class_Curr_10 ) %>% distinct() %>%
  mutate(Prevent_Generic_Curr_10=as.numeric(Prevent_Generic_Curr_10)) %>%
  mutate(Prevent_Class_Curr_10=as.numeric(Prevent_Class_Curr_10)) %>% 
  rename("Generic"="Prevent_Generic_Curr_10", "Class"="Prevent_Class_Curr_10") %>% 
  filter(Generic!=9901) %>% drop_na()


Drug_to_class_lookup <- bind_rows(Acute_1, Acute_2, Acute_3, Acute_4, Acute_5, Acute_6, Acute_7, Acute_8, Acute_9, Acute_10,
          Prevent_1, Prevent_2, Prevent_3, Prevent_4, Prevent_5, Prevent_6) %>% distinct() %>%
  arrange(Generic, Class)

fwrite(Drug_to_class_lookup, "Drug_to_class_lookup.txt", sep="\t")

# -------------
# UK Class % Penetrance ON month 60 -----------------------------

Pfizer_Migraine_Pat <- read_sav("Pfizer_Migraine_Pat v.2.0.sav")

UK_df <- Pfizer_Migraine_Pat %>% filter(qcountries==27)

UK_df %>% select(patNum, SCR_PRFOS_3, SCR_PRFOS_4, SCR_PRFOS_5) %>%
  gather(col, value, SCR_PRFOS_3:SCR_PRFOS_5) %>% drop_na() %>%
  select(value) %>% distinct()

Drug_to_class_lookup <- fread("Drug_to_class_lookup.txt", sep="\t")
Class_names <- fread("Class_names.txt", sep="\t")
Molecule_names <- fread("Molecule_names.txt", sep="\t")

all_molecules_df <- fread("UK_AllMolecules_OverTime_m60extended.txt", sep="\t")
all_molecules_df <- all_molecules_df %>% select(patNum, `60`) 
all_molecules_df <- separate_rows(all_molecules_df, `60`, sep = ",", convert=T )
all_molecules_df <- all_molecules_df %>% filter(`60` != "-") %>% distinct()

all_molecules_df %>% mutate(`60`=as.numeric(`60`)) %>%
  left_join(Drug_to_class_lookup, by=c("60"="Generic")) %>%
  select(patNum, Class) %>% distinct() %>%
  left_join(Class_names %>% mutate(class_id=as.numeric(class_id)), by=c("Class"="class_id")) %>%
  select(patNum, class) %>% distinct() %>%
  group_by(class) %>% count() %>% mutate(n=100 * n/1032) %>% arrange(-n)


# ------------------------------
# UK Satisfaction based on current class ---------------
Pfizer_Migraine_Pat <- read_sav("Pfizer_Migraine_Pat v.2.0.sav")

UK_df <- Pfizer_Migraine_Pat %>% filter(qcountries==27)

UK_df %>% select(patNum, SCR_PRFOS_3, SCR_PRFOS_4, SCR_PRFOS_5) %>%
  gather(col, value, SCR_PRFOS_3:SCR_PRFOS_5) %>% drop_na() %>%
  select(value) %>% distinct()

Drug_to_class_lookup <- fread("Drug_to_class_lookup.txt", sep="\t")
Class_names <- fread("Class_names.txt", sep="\t")
Molecule_names <- fread("Molecule_names.txt", sep="\t")

all_molecules_df <- fread("UK_AllMolecules_OverTime_m60extended.txt", sep="\t")
all_molecules_df <- all_molecules_df %>% select(patNum, `60`) 
all_molecules_df <- separate_rows(all_molecules_df, `60`, sep = ",", convert=T )
all_molecules_df <- all_molecules_df %>% filter(`60` != "-") %>% distinct()

current_classes <- all_molecules_df %>% mutate(`60`=as.numeric(`60`)) %>%
  left_join(Drug_to_class_lookup, by=c("60"="Generic")) %>%
  select(patNum, Class) %>% distinct() %>%
  left_join(Class_names %>% mutate(class_id=as.numeric(class_id)), by=c("Class"="class_id")) %>%
  select(patNum, class) %>% distinct()



Pfizer_Migraine_Pat <- read_sav("Pfizer_Migraine_Pat v.2.0.sav")

UK_df <- Pfizer_Migraine_Pat %>% filter(qcountries==27)


# PRF_I_1a, PRF_I_3a Prev

current_classes %>% 
  inner_join(UK_df %>% select(patNum, PRF_I_1a) %>% mutate(patNum=as.numeric(patNum))) %>% drop_na() %>%
  group_by(class, PRF_I_1a) %>% count() %>% spread(key=PRF_I_1a, value=n)

# ----------
# US Acute Lines Long Format -----------------------------------------------------

Pfizer_Migraine_Pat <- read_sav("Pfizer_Migraine_Pat v.2.0.sav")

US_df <- Pfizer_Migraine_Pat %>% filter(qcountries==28)

US_df %>% select(startacuteline1, endacuteline1, acutelinedur1)

Starts_acute_lines <- US_df %>% select(patNum, contains("startacuteline") )

Starts_acute_lines <- gather(Starts_acute_lines, acuteline, start, 
                             startacuteline1:startacuteline6, factor_key=FALSE)

Starts_acute_lines <- Starts_acute_lines %>% arrange(patNum) %>% drop_na() %>% 
  mutate(acuteline=parse_number(acuteline)) %>% arrange(patNum)

Ends_acute_lines <- US_df %>% select(patNum, contains("endacuteline") )

Ends_acute_lines <- gather(Ends_acute_lines, acuteline, end, 
                           endacuteline1:endacuteline6, factor_key=FALSE)

Ends_acute_lines <- Ends_acute_lines %>% arrange(patNum) %>% drop_na() %>% 
  mutate(acuteline=parse_number(acuteline)) %>% arrange(patNum)

Duration_acute_lines <- US_df %>% select(patNum, contains("acutelinedur") )

Duration_acute_lines <- gather(Duration_acute_lines, acuteline, 
                               duration, acutelinedur1:acutelinedur6, factor_key=FALSE)

Duration_acute_lines <- Duration_acute_lines %>% arrange(patNum) %>% drop_na() %>% 
  mutate(acuteline=parse_number(acuteline)) %>% arrange(patNum)

Lines_durations <- Starts_acute_lines %>%  
  left_join(Ends_acute_lines) %>%  
  left_join(Duration_acute_lines) %>% 
  arrange(patNum) %>% group_by(patNum) %>% 
  mutate(end2 = ifelse( is.na(duration), lead(start) - 1, end)) 

Lines_durations <- Lines_durations %>% group_by(patNum) %>% 
  mutate(end = ifelse( is.na(end), end2, end)) %>% 
  mutate(duration = ifelse( is.na(duration), start - end, duration))

mean(Lines_durations$start) # 1113.643
mean(Lines_durations$end, na.rm = TRUE) # 879.0489
mean(Lines_durations$duration, na.rm = TRUE) # 904.5073

Lines_durations <- Lines_durations %>% select(-end2)

Lines_durations <- Lines_durations %>% 
  mutate(end = ifelse( is.na(end), 0, end )) %>%
  mutate(end = round (-end / 30.5, 0) ) %>% mutate(start = round(-start / 30.5, 0) ) %>%
  group_by(patNum) %>% mutate(end = ifelse( end == lead(start), end-1, end)) %>%
    mutate(end = ifelse( is.na(end), 0, end )) 

data.frame( Lines_durations %>% group_by(patNum) %>% count() %>% ungroup() %>% 
  filter(n>=3) %>% select(patNum) %>% left_join(Lines_durations) )

Lines_durations <- Lines_durations %>% select(-duration)

data.frame(Lines_durations)

df <- Lines_durations %>%
  mutate(month_number = map2(start, end, seq)) %>%
  unnest(month_number)

df <- df %>% select(patNum, acuteline, month_number)

df <- df %>% ungroup() %>% arrange(patNum, acuteline, month_number) %>% 
  group_by(patNum) %>% mutate(month_number2=ifelse(month_number==lead(month_number), 1, 0)) %>%
  filter(month_number2 == 0) %>% ungroup() %>% select(-month_number2)
  
wide_df <- pivot_wider(
  data = df,
  id_cols = c("patNum"),
  names_from = month_number,
  values_from = acuteline,
  values_fill = 0
)

wide_df <- wide_df[, order(as.numeric(colnames(wide_df)), decreasing = FALSE)]

wide_df[,340:348]

sum(is.na(wide_df))

names(wide_df)

wide_df <- wide_df[, 287:348]

names(wide_df)

wide_df %>% select(patNum, `-60`:`-1`)

wide_df <- gather(wide_df, Month, Line, `-61`:`-1`) %>% 
  mutate(Month=as.numeric(Month) + 61) %>%
  spread(key=Month, value=Line)

wide_df <- US_df %>% select(patNum)  %>% left_join(wide_df)

wide_df[is.na(wide_df)] <- 0

fwrite(wide_df, "US_AcuteLines_Wide.txt", sep="\t")

# --------------------

# US Preventive Lines Long Format -----------------------------------------------------

Pfizer_Migraine_Pat <- read_sav("Pfizer_Migraine_Pat v.2.0.sav")

US_df <- Pfizer_Migraine_Pat %>% filter(qcountries==28)

US_df %>% select(startprevline1,  endprevline1, prevlinedur1)

Starts_prev_lines <- US_df %>% select(patNum, contains("startprevline") )

Starts_prev_lines <- gather(Starts_prev_lines, prevline, start, 
                             startprevline1:startprevline6, factor_key=FALSE)

Starts_prev_lines <- Starts_prev_lines %>% arrange(patNum) %>% drop_na() %>% 
  mutate(prevline=parse_number(prevline)) %>% arrange(patNum)

Ends_prev_lines <- US_df %>% select(patNum, contains("endprevline") )

Ends_prev_lines <- gather(Ends_prev_lines, prevline, end, 
                           endprevline1:endprevline6, factor_key=FALSE)

Ends_prev_lines <- Ends_prev_lines %>% arrange(patNum) %>% drop_na() %>% 
  mutate(prevline=parse_number(prevline)) %>% arrange(patNum)

Duration_prev_lines <- US_df %>% select(patNum, contains("prevlinedur") )

Duration_prev_lines <- gather(Duration_prev_lines, prevline, 
                               duration, prevlinedur1:prevlinedur6, factor_key=FALSE)

Duration_prev_lines <- Duration_prev_lines %>% arrange(patNum) %>% drop_na() %>% 
  mutate(prevline=parse_number(prevline)) %>% arrange(patNum)

Lines_durations <- Starts_prev_lines %>%  
  left_join(Ends_prev_lines) %>%  
  left_join(Duration_prev_lines) %>% 
  arrange(patNum) %>% group_by(patNum) %>% 
  mutate(end2 = ifelse( is.na(duration), lead(start) - 1, end)) 

Lines_durations <- Lines_durations %>% group_by(patNum) %>% 
  mutate(end = ifelse( is.na(end), end2, end)) %>% 
  mutate(duration = ifelse( is.na(duration), start - end, duration))

mean(Lines_durations$start) # 849.7457
mean(Lines_durations$end, na.rm = TRUE) # 885.4945
mean(Lines_durations$duration, na.rm = TRUE) # 599.9765

Lines_durations <- Lines_durations %>% select(-end2)

Lines_durations <- Lines_durations %>% 
  mutate(end = ifelse( is.na(end), 0, end )) %>%
  mutate(end = round (-end / 30.5, 0) ) %>% mutate(start = round(-start / 30.5, 0) ) %>%
  group_by(patNum) %>% mutate(end = ifelse( end == lead(start), end-1, end)) %>%
    mutate(end = ifelse( is.na(end), 0, end )) 

data.frame( Lines_durations %>% group_by(patNum) %>% count() %>% ungroup() %>% 
  filter(n>=3) %>% select(patNum) %>% left_join(Lines_durations) )

Lines_durations <- Lines_durations %>% select(-duration)

data.frame(Lines_durations)

df <- Lines_durations %>%
  mutate(month_number = map2(start, end, seq)) %>%
  unnest(month_number)

df <- df %>% select(patNum, prevline, month_number)

df <- df %>% ungroup() %>% arrange(patNum, prevline, month_number) %>% 
  group_by(patNum) %>% mutate(month_number2=ifelse(month_number==lead(month_number), 1, 0)) %>%
  filter(month_number2 == 0) %>% ungroup() %>% select(-month_number2)
  
df <- df %>% group_by(patNum, month_number) %>% filter(prevline==max(prevline))


wide_df <- pivot_wider(
  data = df,
  id_cols = c("patNum"),
  names_from = month_number,
  values_from = prevline,
  values_fill = 0
)


wide_df <- wide_df[, order(as.numeric(colnames(wide_df)), decreasing = FALSE)]

sum(is.na(wide_df))

names(wide_df)

wide_df <- wide_df[, 176:237]

names(wide_df)

wide_df %>% select(patNum, `-60`:`-1`)

wide_df <- gather(wide_df, Month, Line, `-61`:`-1`) %>% 
  mutate(Month=as.numeric(Month) + 61) %>%
  spread(key=Month, value=Line)

wide_df <- US_df %>% select(patNum)  %>% left_join(wide_df)

wide_df[is.na(wide_df)] <- 0

fwrite(wide_df, "US_PrevLines_Wide.txt", sep="\t")

# --------------------
# US Molecule composition of Acute Lines ----------------------------------

Pfizer_Migraine_Pat <- read_sav("Pfizer_Migraine_Pat v.2.0.sav")

US_df <- Pfizer_Migraine_Pat %>% filter(qcountries==28)

Acute_molecules <- US_df %>% select(patNum, contains("acuteline") ) %>% 
  select(patNum, !contains("switch") ) %>%
  select(patNum, !contains("end") ) %>% 
  select(patNum, !contains("start") ) %>%
  select(patNum, !contains("headday") ) %>%
  select(patNum, !contains("dur") ) %>%
  select(patNum, !contains("lines") )

length(names(Acute_molecules)) == (1 + 6*345) 

Acute_molecules <- gather(Acute_molecules, acuteline, use, acuteline1_1:acuteline6_9501)

Acute_molecules <- Acute_molecules %>% arrange(patNum)

Acute_molecules <- Acute_molecules %>% mutate(
  line_number = str_extract(acuteline, "(?<=acuteline)\\d+"),
  molecule_number = str_extract(acuteline, "(?<=_)\\d+")
  ) %>% 
  filter(use==1) %>%
  select(-c(acuteline, use)) %>% filter(molecule_number != "9501")


unique(Acute_molecules$line_number)
unique(Acute_molecules$molecule_number)

Acute_molecules <- Acute_molecules %>% arrange(patNum, line_number, molecule_number) %>%
  group_by(patNum, line_number) %>% 
  mutate(molecule_number = paste0(molecule_number, collapse= ",")) %>% distinct()

fwrite(Acute_molecules , "US_Acute_Lines_MoleculeComposition.txt", sep="\t")

# --------------------
# US Molecule composition of Preventive Lines ----------------------------------

Pfizer_Migraine_Pat <- read_sav("Pfizer_Migraine_Pat v.2.0.sav")

US_df <- Pfizer_Migraine_Pat %>% filter(qcountries==28)

Prev_molecules <- US_df %>% select(patNum, contains("prevline") ) %>% 
  select(patNum, !contains("switch") ) %>%
  select(patNum, !contains("end") ) %>% 
  select(patNum, !contains("start") ) %>%
  select(patNum, !contains("headday") ) %>%
  select(patNum, !contains("dur") ) %>%
  select(patNum, !contains("lines") )

data.framenames <- data.frame(names(Prev_molecules))

Prev_molecules <- gather(Prev_molecules, prevline, use, prevline1_1:prevline6_34801)

Prev_molecules <- Prev_molecules %>% arrange(patNum)

Prev_molecules <- Prev_molecules %>% mutate(
  line_number = str_extract(prevline, "(?<=prevline)\\d+"),
  molecule_number = str_extract(prevline, "(?<=_)\\d+")
  ) %>% 
  filter(use==1) %>%
  select(-c(prevline, use)) %>% filter(molecule_number != "9501" & molecule_number != "34801")


unique(Prev_molecules$line_number)
unique(Prev_molecules$molecule_number)

Prev_molecules <- Prev_molecules %>% arrange(patNum, line_number, molecule_number) %>%
  group_by(patNum, line_number) %>% 
  mutate(molecule_number = paste0(molecule_number, collapse= ",")) %>% distinct()

fwrite(Prev_molecules , "US_Prev_Lines_MoleculeComposition.txt", sep="\t")

# --------------------
# US Create Wide Format with all individual molecules joined -------

US_AcuteLines_Wide <- fread("US_AcuteLines_Wide.txt")
US_PrevLines_Wide <- fread("US_PrevLines_Wide.txt")
Acute_Lines_MoleculeComposition <- fread("US_Acute_Lines_MoleculeComposition.txt")
Prev_Lines_MoleculeComposition <- fread("US_Prev_Lines_MoleculeComposition.txt")


US_AcuteLines_Wide <- US_AcuteLines_Wide %>% gather(month, line, `0`:`60`) 

head(US_AcuteLines_Wide) ; head(Acute_Lines_MoleculeComposition)

US_AcuteLines_Wide <- US_AcuteLines_Wide %>% left_join(Acute_Lines_MoleculeComposition, by=c("patNum"="patNum", "line"="line_number"))

US_AcuteLines_Wide[is.na(US_AcuteLines_Wide)] <- "-"

US_AcuteLines_Wide <- US_AcuteLines_Wide %>% select(-line)



US_PrevLines_Wide <- US_PrevLines_Wide %>% gather(month, line, `0`:`60`) 

head(US_PrevLines_Wide) ; head(Prev_Lines_MoleculeComposition)

US_PrevLines_Wide <- US_PrevLines_Wide %>% left_join(Prev_Lines_MoleculeComposition, by=c("patNum"="patNum", "line"="line_number"))

US_PrevLines_Wide[is.na(US_PrevLines_Wide)] <- "-"

US_PrevLines_Wide <- US_PrevLines_Wide %>% select(-line)

all_molecules_df <- US_AcuteLines_Wide %>% bind_rows(US_PrevLines_Wide) %>% distinct()

head(all_molecules_df)

all_molecules_df <- separate_rows(all_molecules_df, molecule_number, sep = ",", convert=T )

all_molecules_df <- all_molecules_df %>% distinct()

all_molecules_df <- all_molecules_df %>% filter(molecule_number!="-" ) 

all_molecules_df <- all_molecules_df %>% arrange(patNum, month, molecule_number) %>%
  group_by(patNum, month) %>% mutate(molecule_number = paste(molecule_number, collapse = ",")) %>%
  distinct()

unique(all_molecules_df$molecule_number)

all_molecules_df <- all_molecules_df %>% spread(key=month, value=molecule_number)

all_molecules_df[is.na(all_molecules_df)] <- "-"

US_AcuteLines_Wide <- fread("US_AcuteLines_Wide.txt")

all_molecules_df <- US_AcuteLines_Wide %>% select(patNum) %>% left_join(all_molecules_df)

all_molecules_df[is.na(all_molecules_df)] <- "-"

names(all_molecules_df)

all_molecules_df <- all_molecules_df %>% select(patNum, as.character(0:60))

all_molecules_df %>% gather(month, line, `0`:`60`) %>%
  arrange(patNum, as.numeric(month)) %>% group_by(patNum) %>% filter(line=="-" & lag(line) != "-")


fwrite(all_molecules_df, "US_AllMolecules_OverTime.txt", sep="\t")


# ------------------------
# US Allocate EXTRA/MISSING patients ON month 60 based on "Current" columns -----------

Pfizer_Migraine_Pat <- read_sav("Pfizer_Migraine_Pat v.2.0.sav")

US_df <- Pfizer_Migraine_Pat %>% filter(qcountries==28)

all_molecules_df <- fread("US_AllMolecules_OverTime.txt", sep="\t")

Current_df <- US_df %>% select(patNum, Prevent_Generic_Curr_1, Acute_Generic_Curr_1) 

Current_df <- gather(Current_df, line, drug, Prevent_Generic_Curr_1:Acute_Generic_Curr_1)

Current_df <- Current_df %>% filter(drug!=9901)

Current_df <- Current_df %>% select(patNum, drug) %>%
  distinct() %>% arrange(patNum, drug) %>% group_by(patNum) %>%
  mutate(drug = paste(drug, collapse = ",")) %>% distinct()

to_check <- all_molecules_df %>% select(patNum, `60`) %>% 
  left_join(Current_df %>% mutate(patNum = as.numeric(patNum)))

to_check[is.na(to_check)] <- "-"

to_check <- gather(to_check, col, drugs, `60`:drug)

to_check <- to_check %>% select(-col) %>% distinct() %>% filter(drugs!="-")

to_check <- separate_rows(to_check, drugs, sep = ",", convert=T )

to_check <- to_check %>% distinct() %>% arrange(patNum, drugs) %>% 
  group_by(patNum) %>% mutate(drugs= paste(drugs, collapse = ","))  %>% distinct()

all_molecules_df <- all_molecules_df %>% select(-`60`) %>% left_join(to_check)

all_molecules_df[is.na(all_molecules_df)] <- "-"

names(all_molecules_df)[62] <- "60"

fwrite(all_molecules_df, "US_AllMolecules_OverTime_m60extended.txt", sep="\t")

all_molecules_df <- fread("US_AllMolecules_OverTime_m60extended.txt", sep="\t")

all_molecules_df[grepl("352", `60`), ]


# -------------------------
# US Drug to class mapping -----------

Pfizer_Migraine_Pat <- read_sav("Pfizer_Migraine_Pat v.2.0.sav")

US_df <- Pfizer_Migraine_Pat %>% filter(qcountries==28)

Acute_1 <- US_df %>% select(Acute_Generic_Curr_1, Acute_Class_Curr_1 ) %>% distinct() %>%
  mutate(Acute_Generic_Curr_1=as.numeric(Acute_Generic_Curr_1)) %>%
  mutate(Acute_Class_Curr_1=as.numeric(Acute_Class_Curr_1)) %>% 
  rename("Generic"="Acute_Generic_Curr_1", "Class"="Acute_Class_Curr_1") %>% 
  filter(Generic!=9901) %>% drop_na()
  
Acute_2 <- US_df %>% select(Acute_Generic_Curr_2, Acute_Class_Curr_2 ) %>% distinct() %>%
  mutate(Acute_Generic_Curr_2=as.numeric(Acute_Generic_Curr_2)) %>%
  mutate(Acute_Class_Curr_2=as.numeric(Acute_Class_Curr_2)) %>% 
  rename("Generic"="Acute_Generic_Curr_2", "Class"="Acute_Class_Curr_2") %>% 
  filter(Generic!=9901) %>% drop_na()

Acute_3 <- US_df %>% select(Acute_Generic_Curr_3, Acute_Class_Curr_3 ) %>% distinct() %>%
  mutate(Acute_Generic_Curr_3=as.numeric(Acute_Generic_Curr_3)) %>%
  mutate(Acute_Class_Curr_3=as.numeric(Acute_Class_Curr_3)) %>% 
  rename("Generic"="Acute_Generic_Curr_3", "Class"="Acute_Class_Curr_3") %>% 
  filter(Generic!=9901) %>% drop_na()

Acute_4 <- US_df %>% select(Acute_Generic_Curr_4, Acute_Class_Curr_4 ) %>% distinct() %>%
  mutate(Acute_Generic_Curr_4=as.numeric(Acute_Generic_Curr_4)) %>%
  mutate(Acute_Class_Curr_4=as.numeric(Acute_Class_Curr_4)) %>% 
  rename("Generic"="Acute_Generic_Curr_4", "Class"="Acute_Class_Curr_4") %>% 
  filter(Generic!=9901) %>% drop_na()

Acute_5 <- US_df %>% select(Acute_Generic_Curr_5, Acute_Class_Curr_5 ) %>% distinct() %>%
  mutate(Acute_Generic_Curr_5=as.numeric(Acute_Generic_Curr_5)) %>%
  mutate(Acute_Class_Curr_5=as.numeric(Acute_Class_Curr_5)) %>% 
  rename("Generic"="Acute_Generic_Curr_5", "Class"="Acute_Class_Curr_5") %>% 
  filter(Generic!=9901) %>% drop_na()

Acute_6 <- US_df %>% select(Acute_Generic_Curr_6, Acute_Class_Curr_6 ) %>% distinct() %>%
  mutate(Acute_Generic_Curr_6=as.numeric(Acute_Generic_Curr_6)) %>%
  mutate(Acute_Class_Curr_6=as.numeric(Acute_Class_Curr_6)) %>% 
  rename("Generic"="Acute_Generic_Curr_6", "Class"="Acute_Class_Curr_6") %>% 
  filter(Generic!=9901) %>% drop_na()


Acute_7 <- US_df %>% select(Acute_Generic_Curr_7, Acute_Class_Curr_7 ) %>% distinct() %>%
  mutate(Acute_Generic_Curr_7=as.numeric(Acute_Generic_Curr_7)) %>%
  mutate(Acute_Class_Curr_7=as.numeric(Acute_Class_Curr_7)) %>% 
  rename("Generic"="Acute_Generic_Curr_7", "Class"="Acute_Class_Curr_7") %>% 
  filter(Generic!=9901) %>% drop_na()

Acute_8 <- US_df %>% select(Acute_Generic_Curr_8, Acute_Class_Curr_8 ) %>% distinct() %>%
  mutate(Acute_Generic_Curr_8=as.numeric(Acute_Generic_Curr_8)) %>%
  mutate(Acute_Class_Curr_8=as.numeric(Acute_Class_Curr_8)) %>% 
  rename("Generic"="Acute_Generic_Curr_8", "Class"="Acute_Class_Curr_8") %>% 
  filter(Generic!=9901) %>% drop_na()



Acute_9 <- US_df %>% select(Acute_Generic_Curr_9, Acute_Class_Curr_9 ) %>% distinct() %>%
  mutate(Acute_Generic_Curr_9=as.numeric(Acute_Generic_Curr_9)) %>%
  mutate(Acute_Class_Curr_9=as.numeric(Acute_Class_Curr_9)) %>% 
  rename("Generic"="Acute_Generic_Curr_9", "Class"="Acute_Class_Curr_9") %>% 
  filter(Generic!=9901) %>% drop_na()


Acute_10 <- US_df %>% select(Acute_Generic_Curr_10, Acute_Class_Curr_10 ) %>% distinct() %>%
  mutate(Acute_Generic_Curr_10=as.numeric(Acute_Generic_Curr_10)) %>%
  mutate(Acute_Class_Curr_10=as.numeric(Acute_Class_Curr_10)) %>% 
  rename("Generic"="Acute_Generic_Curr_10", "Class"="Acute_Class_Curr_10") %>% 
  filter(Generic!=9901) %>% drop_na()


Prevent_1 <- US_df %>% select(Prevent_Generic_Curr_1, Prevent_Class_Curr_1 ) %>% distinct() %>%
  mutate(Prevent_Generic_Curr_1=as.numeric(Prevent_Generic_Curr_1)) %>%
  mutate(Prevent_Class_Curr_1=as.numeric(Prevent_Class_Curr_1)) %>% 
  rename("Generic"="Prevent_Generic_Curr_1", "Class"="Prevent_Class_Curr_1") %>% 
  filter(Generic!=9901) %>% drop_na()
  
Prevent_2 <- US_df %>% select(Prevent_Generic_Curr_2, Prevent_Class_Curr_2 ) %>% distinct() %>%
  mutate(Prevent_Generic_Curr_2=as.numeric(Prevent_Generic_Curr_2)) %>%
  mutate(Prevent_Class_Curr_2=as.numeric(Prevent_Class_Curr_2)) %>% 
  rename("Generic"="Prevent_Generic_Curr_2", "Class"="Prevent_Class_Curr_2") %>% 
  filter(Generic!=9901) %>% drop_na()

Prevent_3 <- US_df %>% select(Prevent_Generic_Curr_3, Prevent_Class_Curr_3 ) %>% distinct() %>%
  mutate(Prevent_Generic_Curr_3=as.numeric(Prevent_Generic_Curr_3)) %>%
  mutate(Prevent_Class_Curr_3=as.numeric(Prevent_Class_Curr_3)) %>% 
  rename("Generic"="Prevent_Generic_Curr_3", "Class"="Prevent_Class_Curr_3") %>% 
  filter(Generic!=9901) %>% drop_na()

Prevent_4 <- US_df %>% select(Prevent_Generic_Curr_4, Prevent_Class_Curr_4 ) %>% distinct() %>%
  mutate(Prevent_Generic_Curr_4=as.numeric(Prevent_Generic_Curr_4)) %>%
  mutate(Prevent_Class_Curr_4=as.numeric(Prevent_Class_Curr_4)) %>% 
  rename("Generic"="Prevent_Generic_Curr_4", "Class"="Prevent_Class_Curr_4") %>% 
  filter(Generic!=9901) %>% drop_na()

Prevent_5 <- US_df %>% select(Prevent_Generic_Curr_5, Prevent_Class_Curr_5 ) %>% distinct() %>%
  mutate(Prevent_Generic_Curr_5=as.numeric(Prevent_Generic_Curr_5)) %>%
  mutate(Prevent_Class_Curr_5=as.numeric(Prevent_Class_Curr_5)) %>% 
  rename("Generic"="Prevent_Generic_Curr_5", "Class"="Prevent_Class_Curr_5") %>% 
  filter(Generic!=9901) %>% drop_na()

Prevent_6 <- US_df %>% select(Prevent_Generic_Curr_6, Prevent_Class_Curr_6 ) %>% distinct() %>%
  mutate(Prevent_Generic_Curr_6=as.numeric(Prevent_Generic_Curr_6)) %>%
  mutate(Prevent_Class_Curr_6=as.numeric(Prevent_Class_Curr_6)) %>% 
  rename("Generic"="Prevent_Generic_Curr_6", "Class"="Prevent_Class_Curr_6") %>% 
  filter(Generic!=9901) %>% drop_na()


Prevent_7 <- US_df %>% select(Prevent_Generic_Curr_7, Prevent_Class_Curr_7 ) %>% distinct() %>%
  mutate(Prevent_Generic_Curr_7=as.numeric(Prevent_Generic_Curr_7)) %>%
  mutate(Prevent_Class_Curr_7=as.numeric(Prevent_Class_Curr_7)) %>% 
  rename("Generic"="Prevent_Generic_Curr_7", "Class"="Prevent_Class_Curr_7") %>% 
  filter(Generic!=9901) %>% drop_na()



Prevent_8 <- US_df %>% select(Prevent_Generic_Curr_8, Prevent_Class_Curr_8 ) %>% distinct() %>%
  mutate(Prevent_Generic_Curr_8=as.numeric(Prevent_Generic_Curr_8)) %>%
  mutate(Prevent_Class_Curr_8=as.numeric(Prevent_Class_Curr_8)) %>% 
  rename("Generic"="Prevent_Generic_Curr_8", "Class"="Prevent_Class_Curr_8") %>% 
  filter(Generic!=9901) %>% drop_na()



Prevent_9 <- US_df %>% select(Prevent_Generic_Curr_9, Prevent_Class_Curr_9 ) %>% distinct() %>%
  mutate(Prevent_Generic_Curr_9=as.numeric(Prevent_Generic_Curr_9)) %>%
  mutate(Prevent_Class_Curr_9=as.numeric(Prevent_Class_Curr_9)) %>% 
  rename("Generic"="Prevent_Generic_Curr_9", "Class"="Prevent_Class_Curr_9") %>% 
  filter(Generic!=9901) %>% drop_na()


Prevent_10 <- US_df %>% select(Prevent_Generic_Curr_10, Prevent_Class_Curr_10 ) %>% distinct() %>%
  mutate(Prevent_Generic_Curr_10=as.numeric(Prevent_Generic_Curr_10)) %>%
  mutate(Prevent_Class_Curr_10=as.numeric(Prevent_Class_Curr_10)) %>% 
  rename("Generic"="Prevent_Generic_Curr_10", "Class"="Prevent_Class_Curr_10") %>% 
  filter(Generic!=9901) %>% drop_na()


Drug_to_class_lookup <- bind_rows(Acute_1, Acute_2, Acute_3, Acute_4, Acute_5, Acute_6, Acute_7, Acute_8, Acute_9, Acute_10,
          Prevent_1, Prevent_2, Prevent_3, Prevent_4, Prevent_5, Prevent_6, Prevent_7, Prevent_8, Prevent_9, Prevent_10) %>% distinct() %>%
  arrange(Generic, Class)

fwrite(Drug_to_class_lookup, "US_Drug_to_class_lookup.txt", sep="\t")


# -------------------------
# US Class % Penetrance ON month 60 -----------------------------

Pfizer_Migraine_Pat <- read_sav("Pfizer_Migraine_Pat v.2.0.sav")

US_df <- Pfizer_Migraine_Pat %>% filter(qcountries==28)

Drug_to_class_lookup <- fread("US_Drug_to_class_lookup.txt", sep="\t")
Class_names <- fread("Class_names.txt", sep="\t")
Molecule_names <- fread("Molecule_names.txt", sep="\t")

all_molecules_df <- fread("US_AllMolecules_OverTime_m60extended.txt", sep="\t")
all_molecules_df <- all_molecules_df %>% select(patNum, `60`) 
all_molecules_df <- separate_rows(all_molecules_df, `60`, sep = ",", convert=T )
all_molecules_df <- all_molecules_df %>% filter(`60` != "-") %>% distinct()

length(unique(all_molecules_df$`60`))

all_molecules_df %>% mutate(`60`=as.numeric(`60`)) %>%
  left_join(Drug_to_class_lookup, by=c("60"="Generic")) %>%
  select(patNum, Class) %>% distinct() %>%
  left_join(Class_names %>% mutate(class_id=as.numeric(class_id)), by=c("Class"="class_id")) %>%
  select(patNum, class) %>% distinct() %>%
  group_by(class) %>% count() %>% mutate(n=100 * n/1452) %>% arrange(-n)

# -----------------

# US Satisfaction based on current class ---------------
Pfizer_Migraine_Pat <- read_sav("Pfizer_Migraine_Pat v.2.0.sav")

US_df <- Pfizer_Migraine_Pat %>% filter(qcountries==28)

Drug_to_class_lookup <- fread("US_Drug_to_class_lookup.txt", sep="\t")
Class_names <- fread("Class_names.txt", sep="\t")
Molecule_names <- fread("Molecule_names.txt", sep="\t")

all_molecules_df <- fread("US_AllMolecules_OverTime_m60extended.txt", sep="\t")
all_molecules_df <- all_molecules_df %>% select(patNum, `60`) 
all_molecules_df <- separate_rows(all_molecules_df, `60`, sep = ",", convert=T )
all_molecules_df <- all_molecules_df %>% filter(`60` != "-") %>% distinct()

current_classes <- all_molecules_df %>% mutate(`60`=as.numeric(`60`)) %>%
  left_join(Drug_to_class_lookup, by=c("60"="Generic")) %>%
  select(patNum, Class) %>% distinct() %>%
  left_join(Class_names %>% mutate(class_id=as.numeric(class_id)), by=c("Class"="class_id")) %>%
  select(patNum, class) %>% distinct()

Pfizer_Migraine_Pat <- read_sav("Pfizer_Migraine_Pat v.2.0.sav")

US_df <- Pfizer_Migraine_Pat %>% filter(qcountries==28)

# PRF_I_1a, PRF_I_3a Prev

current_classes %>% 
  inner_join(US_df %>% select(patNum, PRF_I_1a) %>% mutate(patNum=as.numeric(patNum))) %>% drop_na() %>%
  group_by(class, PRF_I_1a) %>% count() %>% spread(key=PRF_I_1a, value=n)

# ---------------------

# UK Class penetrance and CGRP usage, different versions ---------------

Drug_to_class_lookup <- fread("Drug_to_class_lookup.txt", sep="\t")
Class_names <- fread("Class_names.txt", sep="\t")
Molecule_names <- fread("Molecule_names.txt", sep="\t")



all_molecules_df <- fread("UK_AllMolecules_OverTime_m60extended.txt", sep="\t")

Rx_exp <- all_molecules_df %>% gather(month, treat, `0`:`60`) %>% filter(treat!="-") %>%
  select(patNum) %>% distinct()

# Month 60

all_molecules_df <- Rx_exp %>% left_join(all_molecules_df) %>% select(patNum, `60`) 

all_molecules_df <- separate_rows(all_molecules_df, `60`, sep = ",", convert=T )

all_molecules_df <- all_molecules_df %>% filter(`60` != "-") %>% distinct()

current_classes <- all_molecules_df %>% mutate(`60`=as.numeric(`60`)) %>%
  left_join(Drug_to_class_lookup, by=c("60"="Generic")) %>%
  select(patNum, Class) %>% distinct() %>%
  left_join(Class_names %>% mutate(class_id=as.numeric(class_id)), by=c("Class"="class_id")) %>%
  select(patNum, class) %>% distinct()

unique(current_classes$class)

current_classes %>% group_by(class) %>% count() %>% arrange(-n)

current_classes %>% mutate(class2=ifelse(class=="Triptans", "Triptans",
                      ifelse(class=="Anti-CGRP mAb", "CGRPInj",
                             ifelse(class %in% c("Nsaid Inc Combs", "Non Opioid Analgesics inc combs",
                                                 "Opioid Analgesics inc combs", "Antimigraine",
                                                 "Triptan Combination"), "OtherAcute", "OtherPrev" )))) %>%
  select(patNum, class2) %>% distinct() %>% group_by(class2) %>% count() %>% mutate(n=n/944) 


current_classes %>% mutate(class2=ifelse(class=="Triptans", "Triptans",
                      ifelse(class=="Anti-CGRP mAb", "CGRPInj",
                             ifelse(class %in% c("Nsaid Inc Combs", "Non Opioid Analgesics inc combs",
                                                 "Opioid Analgesics inc combs", "Antimigraine",
                                                 "Triptan Combination"), "OtherAcute", "OtherPrev" )))) %>%
  select(patNum, class2) %>% filter(!is.na(class2)) %>% distinct() %>% mutate(exp=1) %>%
  spread(key=class2, value=exp) %>% 
  group_by(CGRPInj, OtherAcute, OtherPrev, Triptans) %>% count() %>% mutate(n=n/944)

# Last 12 months

all_molecules_df <- fread("UK_AllMolecules_OverTime_m60extended.txt", sep="\t")

all_molecules_df <- Rx_exp %>% left_join(all_molecules_df) %>% select(patNum, `49`:`60`) 

all_molecules_df <- gather(all_molecules_df, month, treat, `49`:`60`)

all_molecules_df <- separate_rows(all_molecules_df, treat, sep = ",", convert=T )

all_molecules_df <- all_molecules_df %>% filter(treat != "-") %>% distinct()

current_classes <- all_molecules_df %>% mutate(treat=as.numeric(treat)) %>%
  left_join(Drug_to_class_lookup, by=c("treat"="Generic")) %>%
  select(patNum, Class) %>% distinct() %>%
  left_join(Class_names %>% mutate(class_id=as.numeric(class_id)), by=c("Class"="class_id")) %>%
  select(patNum, class) %>% distinct()


current_classes %>% mutate(class2=ifelse(class=="Triptans", "Triptans",
                      ifelse(class=="Anti-CGRP mAb", "CGRPInj",
                             ifelse(class %in% c("Nsaid Inc Combs", "Non Opioid Analgesics inc combs",
                                                 "Opioid Analgesics inc combs", "Antimigraine",
                                                 "Triptan Combination"), "OtherAcute", "OtherPrev" )))) %>%
  select(patNum, class2) %>% filter(!is.na(class2)) %>% distinct() %>% mutate(exp=1) %>%
  spread(key=class2, value=exp) %>% 
  group_by(CGRPInj, OtherAcute, OtherPrev, Triptans) %>% count() %>% mutate(n=n/944)


# Current CGRP, which concomitant drugs?

all_molecules_df <- fread("UK_AllMolecules_OverTime_m60extended.txt", sep="\t")

CurrentCGRP <- all_molecules_df %>% select(patNum, `60`) 

CurrentCGRP <- separate_rows(CurrentCGRP, `60`, sep = ",", convert=T )

CurrentCGRP <- CurrentCGRP %>% filter(`60` != "-") %>% distinct()

CurrentCGRP <- CurrentCGRP %>% mutate(`60`=as.numeric(`60`)) %>%
  left_join(Drug_to_class_lookup, by=c("60"="Generic")) %>%
  select(patNum, Class) %>% distinct() %>%
  left_join(Class_names %>% mutate(class_id=as.numeric(class_id)), by=c("Class"="class_id")) %>%
  select(patNum, class) %>% distinct() %>% filter(class=="Anti-CGRP mAb") %>% select(patNum) %>% distinct()


all_molecules_df <- CurrentCGRP %>% left_join(all_molecules_df)  %>% select(patNum, `60`) 

all_molecules_df <- separate_rows(all_molecules_df, `60`, sep = ",", convert=T )

all_molecules_df <- all_molecules_df %>% filter(`60` != "-") %>% distinct()

all_molecules_df <- all_molecules_df %>% mutate(`60`=as.numeric(`60`)) %>%
  left_join(Drug_to_class_lookup, by=c("60"="Generic")) %>%
  select(patNum, Class) %>% distinct() %>%
  left_join(Class_names %>% mutate(class_id=as.numeric(class_id)), by=c("Class"="class_id")) %>%
  select(patNum, class) %>% distinct()

all_molecules_df %>% group_by(class) %>% count()  %>% mutate(n=n/246) %>% arrange(-n)


# using drug group 


all_molecules_df <- fread("UK_AllMolecules_OverTime_m60extended.txt", sep="\t")

CurrentCGRP <- all_molecules_df %>% select(patNum, `60`) 

CurrentCGRP <- separate_rows(CurrentCGRP, `60`, sep = ",", convert=T )

CurrentCGRP <- CurrentCGRP %>% filter(`60` != "-") %>% distinct()

CurrentCGRP <- CurrentCGRP %>% mutate(`60`=as.numeric(`60`)) %>%
  left_join(Drug_to_class_lookup, by=c("60"="Generic")) %>%
  select(patNum, Class) %>% distinct() %>%
  left_join(Class_names %>% mutate(class_id=as.numeric(class_id)), by=c("Class"="class_id")) %>%
  select(patNum, class) %>% distinct() %>% filter(class=="Anti-CGRP mAb") %>% select(patNum) %>% distinct()


all_molecules_df <- CurrentCGRP %>% left_join(all_molecules_df)  %>% select(patNum, `60`) 

all_molecules_df <- separate_rows(all_molecules_df, `60`, sep = ",", convert=T )

all_molecules_df <- all_molecules_df %>% filter(`60` != "-") %>% distinct()

all_molecules_df <- all_molecules_df %>% mutate(`60`=as.numeric(`60`)) %>%
  left_join(Drug_to_class_lookup, by=c("60"="Generic")) %>%
  select(patNum, Class) %>% distinct() %>%
  left_join(Class_names %>% mutate(class_id=as.numeric(class_id)), by=c("Class"="class_id")) %>%
  select(patNum, class) %>% distinct()

all_molecules_df %>% 
   mutate(class2=ifelse(class=="Triptans", "Triptans",
                      ifelse(class=="Anti-CGRP mAb", "CGRPInj",
                             ifelse(class %in% c("Nsaid Inc Combs", "Non Opioid Analgesics inc combs",
                                                 "Opioid Analgesics inc combs", "Antimigraine",
                                                 "Triptan Combination"), "OtherAcute", "OtherPrev" )))) %>%
  select(patNum, class2) %>% distinct() %>% drop_na() %>% 
  mutate(exp=1) %>%
  spread(key=class2, value=exp) %>% 
  group_by(CGRPInj, OtherAcute, OtherPrev, Triptans) %>% count() %>% mutate(n=n/246)




# Current CGRP, how many drugs?

all_molecules_df <- fread("UK_AllMolecules_OverTime_m60extended.txt", sep="\t")

CurrentCGRP <- all_molecules_df %>% select(patNum, `60`) 

CurrentCGRP <- separate_rows(CurrentCGRP, `60`, sep = ",", convert=T )

CurrentCGRP <- CurrentCGRP %>% filter(`60` != "-") %>% distinct()

CurrentCGRP <- CurrentCGRP %>% mutate(`60`=as.numeric(`60`)) %>%
  left_join(Drug_to_class_lookup, by=c("60"="Generic")) %>%
  select(patNum, Class) %>% distinct() %>%
  left_join(Class_names %>% mutate(class_id=as.numeric(class_id)), by=c("Class"="class_id")) %>%
  select(patNum, class) %>% distinct() %>% filter(class=="Anti-CGRP mAb") %>% select(patNum) %>% distinct()


all_molecules_df <- CurrentCGRP %>% left_join(all_molecules_df)  %>% select(patNum, `60`) 

all_molecules_df <- separate_rows(all_molecules_df, `60`, sep = ",", convert=T )

all_molecules_df <- all_molecules_df %>% filter(`60` != "-") %>% distinct()

CurrentCGRP %>% left_join(all_molecules_df %>% group_by(patNum) %>% count()) %>%
  ungroup() %>% summarise(n2=mean(n))


# What were the CGRP patients ON the month before?

# using extended m60 table

all_molecules_df <- fread("UK_AllMolecules_OverTime_m60extended.txt", sep="\t")

all_molecules_df <- gather(all_molecules_df, month, treat, `0`:`60`)

FirstCGRP <- separate_rows(all_molecules_df, treat, sep = ",", convert=T )

FirstCGRP <- FirstCGRP  %>%
  left_join(Drug_to_class_lookup %>% mutate(Generic=as.character(Generic)), by=c("treat"="Generic")) %>%
  select(patNum, month, Class) %>% distinct() %>%
  left_join(Class_names %>% mutate(class_id=as.numeric(class_id)), by=c("Class"="class_id")) %>%
  select(patNum, month, class) %>% distinct() %>% filter(class=="Anti-CGRP mAb") %>%
  group_by(patNum) %>% filter(month==min(month)) %>% select(patNum, month) %>% distinct() %>% rename("first"="month") %>%
  left_join(FirstCGRP) %>% ungroup() %>% filter(as.numeric(month) == as.numeric(first)-1) %>%
  select(patNum, treat) %>% 
  left_join(Drug_to_class_lookup %>% mutate(Generic=as.character(Generic)), by=c("treat"="Generic")) %>%
  select(patNum, Class) %>% distinct() %>%
  left_join(Class_names %>% mutate(class_id=as.numeric(class_id)), by=c("Class"="class_id")) %>%
  select(patNum, class) %>% distinct() %>%
  group_by(class) %>% count() %>% mutate(n=n/246) %>% arrange(-n)

  

# using original table

all_molecules_df <- fread("UK_AllMolecules_OverTime.txt", sep="\t")

all_molecules_df <- gather(all_molecules_df, month, treat, `0`:`60`)

FirstCGRP <- separate_rows(all_molecules_df, treat, sep = ",", convert=T )

FirstCGRP  %>%
  left_join(Drug_to_class_lookup %>% mutate(Generic=as.character(Generic)), by=c("treat"="Generic")) %>%
  select(patNum, month, Class) %>% distinct() %>%
  left_join(Class_names %>% mutate(class_id=as.numeric(class_id)), by=c("Class"="class_id")) %>%
  select(patNum, month, class) %>% distinct() %>% filter(class=="Anti-CGRP mAb") %>%
  group_by(patNum) %>% filter(month==min(month)) %>% select(patNum, month) %>% distinct() %>% rename("first"="month") %>%
  left_join(FirstCGRP) %>% ungroup() %>% filter(as.numeric(month) == as.numeric(first)-1) %>%
  select(patNum, treat) %>% 
  left_join(Drug_to_class_lookup %>% mutate(Generic=as.character(Generic)), by=c("treat"="Generic")) %>%
  select(patNum, Class) %>% distinct() %>%
  left_join(Class_names %>% mutate(class_id=as.numeric(class_id)), by=c("Class"="class_id")) %>%
  select(patNum, class) %>% distinct() %>%
  group_by(class) %>% count() %>% mutate(n=n/117) %>% arrange(-n)


# using drug group classification


all_molecules_df <- fread("UK_AllMolecules_OverTime.txt", sep="\t")

all_molecules_df <- gather(all_molecules_df, month, treat, `0`:`60`)

FirstCGRP <- separate_rows(all_molecules_df, treat, sep = ",", convert=T )

FirstCGRP  %>%
  left_join(Drug_to_class_lookup %>% mutate(Generic=as.character(Generic)), by=c("treat"="Generic")) %>%
  select(patNum, month, Class) %>% distinct() %>%
  left_join(Class_names %>% mutate(class_id=as.numeric(class_id)), by=c("Class"="class_id")) %>%
  select(patNum, month, class) %>% distinct() %>% filter(class=="Anti-CGRP mAb") %>%
  group_by(patNum) %>% filter(month==min(month)) %>% select(patNum, month) %>% distinct() %>% rename("first"="month") %>%
  left_join(FirstCGRP) %>% ungroup() %>% filter(as.numeric(month) == as.numeric(first)-1) %>%
  select(patNum, treat) %>% 
  left_join(Drug_to_class_lookup %>% mutate(Generic=as.character(Generic)), by=c("treat"="Generic")) %>%
  select(patNum, Class) %>% distinct() %>%
  left_join(Class_names %>% mutate(class_id=as.numeric(class_id)), by=c("Class"="class_id")) %>%
  select(patNum, class) %>% distinct() %>% 
   mutate(class2=ifelse(class=="Triptans", "Triptans",
                      ifelse(class=="Anti-CGRP mAb", "CGRPInj",
                             ifelse(class %in% c("Nsaid Inc Combs", "Non Opioid Analgesics inc combs",
                                                 "Opioid Analgesics inc combs", "Antimigraine",
                                                 "Triptan Combination"), "OtherAcute", "OtherPrev" )))) %>%
  select(patNum, class2) %>% distinct() %>% drop_na() %>% 
  mutate(exp=1) %>%
  spread(key=class2, value=exp) %>% 
  group_by(OtherAcute, OtherPrev, Triptans) %>% count() %>% mutate(n=n/117)







# using original table but classes ever up to CGRP

all_molecules_df <- fread("UK_AllMolecules_OverTime.txt", sep="\t")

all_molecules_df <- gather(all_molecules_df, month, treat, `0`:`60`)

FirstCGRP <- separate_rows(all_molecules_df, treat, sep = ",", convert=T )

FirstCGRP  %>%
  left_join(Drug_to_class_lookup %>% mutate(Generic=as.character(Generic)), by=c("treat"="Generic")) %>%
  select(patNum, month, Class) %>% distinct() %>%
  left_join(Class_names %>% mutate(class_id=as.numeric(class_id)), by=c("Class"="class_id")) %>%
  select(patNum, month, class) %>% distinct() %>% filter(class=="Anti-CGRP mAb") %>%
  group_by(patNum) %>% filter(month==min(month)) %>% select(patNum, month) %>% distinct() %>% rename("first"="month") %>%
  left_join(FirstCGRP) %>% ungroup() %>% filter(as.numeric(month) < as.numeric(first)) %>%
  select(patNum, treat) %>% 
  left_join(Drug_to_class_lookup %>% mutate(Generic=as.character(Generic)), by=c("treat"="Generic")) %>%
  select(patNum, Class) %>% distinct() %>%
  left_join(Class_names %>% mutate(class_id=as.numeric(class_id)), by=c("Class"="class_id")) %>%
  select(patNum, class) %>% distinct() %>%
  group_by(class) %>% count() %>% mutate(n=n/117) %>% arrange(-n)

# Time since diagnosis: Currently ON CGRP vs other 


all_molecules_df <- fread("UK_AllMolecules_OverTime_m60extended.txt", sep="\t")

CurrentCGRP <- all_molecules_df %>% select(patNum, `60`) 

CurrentCGRP <- separate_rows(CurrentCGRP, `60`, sep = ",", convert=T )

CurrentCGRP <- CurrentCGRP %>% filter(`60` != "-") %>% distinct()

CurrentCGRP <- CurrentCGRP %>% mutate(`60`=as.numeric(`60`)) %>%
  left_join(Drug_to_class_lookup, by=c("60"="Generic")) %>%
  select(patNum, Class) %>% distinct() %>%
  left_join(Class_names %>% mutate(class_id=as.numeric(class_id)), by=c("Class"="class_id")) %>%
  select(patNum, class) %>% distinct() %>% filter(class=="Anti-CGRP mAb") %>% select(patNum) %>% distinct()


Pfizer_Migraine_Pat <- read_sav("Pfizer_Migraine_Pat v.2.0.sav")

UK_df <- Pfizer_Migraine_Pat %>% filter(qcountries==27)

UK_df %>% mutate(patNum=as.numeric(patNum)) %>% select(patNum, PRF_B_2c_DaysSince) %>% drop_na() %>%
  left_join(CurrentCGRP %>% mutate(CGRP=1)) %>% group_by(CGRP) %>% summarise(n=mean(PRF_B_2c_DaysSince/(12*30.5)))


UK_df %>% mutate(patNum=as.numeric(patNum)) %>% select(patNum, PRF_B_2c_DaysSince) %>% drop_na() %>%
  left_join(CurrentCGRP %>% mutate(CGRP=1)) %>%
  ggplot(aes(PRF_B_2c_DaysSince/(30.5*12), colour=as.factor(CGRP), fill=as.factor(CGRP))) +
  geom_density(alpha=0.7) +
  theme_minimal() + xlab("\n Number of Years since Migraine Diagnosis") + ylab("Patient density \n") +
  scale_fill_manual(values=c("midnightblue", "deepskyblue3")) +
  scale_colour_manual(values=c("midnightblue", "deepskyblue3"))


# -------------------
  
# UK Volume : Physician specialty vs patient type vs type of drugs ----------

# Physicians

Pfizer_Migraine_Pat <- read_sav("Pfizer_Migraine_Pat v.2.0.sav")

UK_df <- Pfizer_Migraine_Pat %>% filter(qcountries==27)

UK_df %>% select(patNum, SCR_1_RC) %>% group_by(SCR_1_RC) %>% count()


UK_df %>% select(patNum, PhyPrimarySpeciality2) %>% group_by(PhyPrimarySpeciality2) %>% count()


all_molecules_df <- fread("UK_AllMolecules_OverTime_m60extended.txt", sep="\t")
CurrentCGRP <- all_molecules_df %>% select(patNum, `60`) 
CurrentCGRP <- separate_rows(CurrentCGRP, `60`, sep = ",", convert=T )
CurrentCGRP <- CurrentCGRP %>% filter(`60` != "-") %>% distinct()
CurrentCGRP <- CurrentCGRP %>% mutate(`60`=as.numeric(`60`)) %>%
  left_join(Drug_to_class_lookup, by=c("60"="Generic")) %>%
  select(patNum, Class) %>% distinct() %>%
  left_join(Class_names %>% mutate(class_id=as.numeric(class_id)), by=c("Class"="class_id")) %>%
  select(patNum, class) %>% distinct() %>% filter(class=="Anti-CGRP mAb") %>% select(patNum) %>% distinct()

UK_df %>% select(patNum, PhyPrimarySpeciality2) %>%  mutate(patNum=as.numeric(patNum)) %>%
  left_join(CurrentCGRP %>% mutate(group=1)) %>%
  group_by(PhyPrimarySpeciality2, group) %>% count() %>% mutate(n=n/1032)


Pats_Phys <- UK_df %>% select(patNum, PhyPrimarySpeciality2) %>%  mutate(patNum=as.numeric(patNum))


all_molecules_df <- fread("UK_AllMolecules_OverTime_m60extended.txt", sep="\t")
all_molecules_df <- all_molecules_df %>% select(patNum, `60`) 
all_molecules_df <- separate_rows(all_molecules_df, `60`, sep = ",", convert=T )
all_molecules_df <- all_molecules_df %>% filter(`60` != "-") %>% distinct()

current_classes <- all_molecules_df %>% mutate(`60`=as.numeric(`60`)) %>%
  left_join(Drug_to_class_lookup, by=c("60"="Generic")) %>%
  left_join(Class_names %>% mutate(class_id=as.numeric(class_id)), by=c("Class"="class_id"))

current_classes <- current_classes %>% mutate(class2=ifelse(class=="Triptans", "Triptans",
                      ifelse(class=="Anti-CGRP mAb", "CGRPInj",
                             ifelse(class %in% c("Nsaid Inc Combs", "Non Opioid Analgesics inc combs",
                                                 "Opioid Analgesics inc combs", "Antimigraine",
                                                 "Triptan Combination"), "OtherAcute", "OtherPrev" )))) 




groups <- current_classes %>% select(patNum, class2) %>% distinct() %>%
  mutate(class3=ifelse(class2=="CGRPInj", "Prev",
                       ifelse(class2=="OtherAcute", "Acute",
                              ifelse(class2=="Triptans", "Acute", "Prev")))) %>%
  select(patNum, class3) %>% distinct() %>% arrange(patNum, class3) %>%  drop_na() %>%
  group_by(patNum) %>% mutate(group=paste(class3, collapse = ",")) %>% ungroup() %>% select(patNum, group) %>% distinct() 

groups <- Pats_Phys %>% left_join(groups) %>% drop_na()

volume <- current_classes %>%
  mutate(class3=ifelse(class2=="CGRPInj", "CGRPInj",
                       ifelse(class2=="OtherAcute", "OtherAcute",
                              ifelse(class2=="Triptans", "Triptans", "OtherPrev")))) %>%
  group_by(patNum, class3) %>% count() %>% spread(key=class3, value=n)

volume[is.na(volume)] <- 0

groups <- groups %>% left_join(volume)

groups <- groups %>% select(-`<NA>`)

fwrite(groups, "UK_Volume_Summary.csv")

groups %>% group_by(group, PhyPrimarySpeciality2) %>% summarise(total=sum(Triptans))


# ----------
# US Class penetrance and CGRP usage, different versions ---------------

Drug_to_class_lookup <- fread("US_Drug_to_class_lookup.txt", sep="\t")
Class_names <- fread("Class_names.txt", sep="\t")
Molecule_names <- fread("Molecule_names.txt", sep="\t")


all_molecules_df <- fread("US_AllMolecules_OverTime_m60extended.txt", sep="\t")

Rx_exp <- all_molecules_df %>% gather(month, treat, `0`:`60`) %>% filter(treat!="-") %>%
  select(patNum) %>% distinct()

# Month 60

all_molecules_df <- Rx_exp %>% left_join(all_molecules_df) %>% select(patNum, `60`) 

all_molecules_df <- separate_rows(all_molecules_df, `60`, sep = ",", convert=T )

all_molecules_df <- all_molecules_df %>% filter(`60` != "-") %>% distinct()

current_classes <- all_molecules_df %>% mutate(`60`=as.numeric(`60`)) %>%
  left_join(Drug_to_class_lookup, by=c("60"="Generic")) %>%
  select(patNum, Class) %>% distinct() %>%
  left_join(Class_names %>% mutate(class_id=as.numeric(class_id)), by=c("Class"="class_id")) %>%
  select(patNum, class) %>% distinct()

unique(current_classes$class)

current_classes %>% group_by(class) %>% count() %>% arrange(-n)

current_classes %>% mutate(class2=ifelse(class=="Triptans", "Triptans",
                      ifelse(class=="Anti-CGRP mAb", "CGRPInj",
                             ifelse(class %in% c("Nsaid Inc Combs", "Non Opioid Analgesics inc combs",
                                                 "Opioid Analgesics inc combs", "Antimigraine",
                                                 "Triptan Combination"), "OtherAcute",
                                    ifelse(class=="Anti-CGRP gepant", "Gepant","OtherPrev" ))))) %>%
  select(patNum, class2) %>% distinct() %>% group_by(class2) %>% count() %>% mutate(n=n/1384) 


data.frame(current_classes %>% mutate(class2=ifelse(class=="Triptans", "Triptans",
                      ifelse(class=="Anti-CGRP mAb", "CGRPInj",
                             ifelse(class %in% c("Nsaid Inc Combs", "Non Opioid Analgesics inc combs",
                                                 "Opioid Analgesics inc combs", "Antimigraine",
                                                 "Triptan Combination"), "OtherAcute",
                                    ifelse(class=="Anti-CGRP gepant", "Gepant","OtherPrev" ))))) %>%
  select(patNum, class2) %>% filter(!is.na(class2)) %>% distinct() %>% mutate(exp=1) %>%
  spread(key=class2, value=exp) %>% 
  group_by(CGRPInj, Gepant, OtherPrev, OtherAcute , Triptans) %>% count() %>% mutate(n=n/1384))



# Last 12 months

all_molecules_df <- fread("US_AllMolecules_OverTime_m60extended.txt", sep="\t")

all_molecules_df <- Rx_exp %>% left_join(all_molecules_df) %>% select(patNum, `49`:`60`) 

all_molecules_df <- gather(all_molecules_df, month, treat, `49`:`60`)

all_molecules_df <- separate_rows(all_molecules_df, treat, sep = ",", convert=T )

all_molecules_df <- all_molecules_df %>% filter(treat != "-") %>% distinct()

current_classes <- all_molecules_df %>% mutate(treat=as.numeric(treat)) %>%
  left_join(Drug_to_class_lookup, by=c("treat"="Generic")) %>%
  select(patNum, Class) %>% distinct() %>%
  left_join(Class_names %>% mutate(class_id=as.numeric(class_id)), by=c("Class"="class_id")) %>%
  select(patNum, class) %>% distinct()


current_classes %>% mutate(class2=ifelse(class=="Triptans", "Triptans",
                      ifelse(class=="Anti-CGRP mAb", "CGRPInj",
                             ifelse(class %in% c("Nsaid Inc Combs", "Non Opioid Analgesics inc combs",
                                                 "Opioid Analgesics inc combs", "Antimigraine",
                                                 "Triptan Combination"), "OtherAcute",
                                    ifelse(class=="Anti-CGRP gepant", "Gepant","OtherPrev" ))))) %>%
  select(patNum, class2) %>% filter(!is.na(class2)) %>% distinct() %>% mutate(exp=1) %>%
  spread(key=class2, value=exp) %>% 
  group_by(CGRPInj, OtherPrev,OtherAcute, Triptans) %>% count() %>% mutate(n=n/1384)







# Current CGRP, which concomitant drugs?

all_molecules_df <- fread("US_AllMolecules_OverTime_m60extended.txt", sep="\t")

CurrentCGRP <- all_molecules_df %>% select(patNum, `60`) 

CurrentCGRP <- separate_rows(CurrentCGRP, `60`, sep = ",", convert=T )

CurrentCGRP <- CurrentCGRP %>% filter(`60` != "-") %>% distinct()

CurrentCGRP <- CurrentCGRP %>% mutate(`60`=as.numeric(`60`)) %>%
  left_join(Drug_to_class_lookup, by=c("60"="Generic")) %>%
  select(patNum, Class) %>% distinct() %>%
  left_join(Class_names %>% mutate(class_id=as.numeric(class_id)), by=c("Class"="class_id")) %>%
  select(patNum, class) %>% distinct() %>% filter(class=="Anti-CGRP mAb"|class=="Anti-CGRP gepant") %>% select(patNum) %>% distinct()


all_molecules_df <- CurrentCGRP %>% left_join(all_molecules_df)  %>% select(patNum, `60`) 

all_molecules_df <- separate_rows(all_molecules_df, `60`, sep = ",", convert=T )

all_molecules_df <- all_molecules_df %>% filter(`60` != "-") %>% distinct()

all_molecules_df <- all_molecules_df %>% mutate(`60`=as.numeric(`60`)) %>%
  left_join(Drug_to_class_lookup, by=c("60"="Generic")) %>%
  select(patNum, Class) %>% distinct() %>%
  left_join(Class_names %>% mutate(class_id=as.numeric(class_id)), by=c("Class"="class_id")) %>%
  select(patNum, class) %>% distinct()

all_molecules_df %>% group_by(class) %>% count()  %>% mutate(n=n/805) %>% arrange(-n)


# using drug group 


all_molecules_df <- fread("US_AllMolecules_OverTime_m60extended.txt", sep="\t")

CurrentCGRP <- all_molecules_df %>% select(patNum, `60`) 

CurrentCGRP <- separate_rows(CurrentCGRP, `60`, sep = ",", convert=T )

CurrentCGRP <- CurrentCGRP %>% filter(`60` != "-") %>% distinct()

CurrentCGRP <- CurrentCGRP %>% mutate(`60`=as.numeric(`60`)) %>%
  left_join(Drug_to_class_lookup, by=c("60"="Generic")) %>%
  select(patNum, Class) %>% distinct() %>%
  left_join(Class_names %>% mutate(class_id=as.numeric(class_id)), by=c("Class"="class_id")) %>%
  select(patNum, class) %>% distinct() %>% filter(class=="Anti-CGRP mAb"|class=="Anti-CGRP gepant") %>% select(patNum) %>% distinct()


all_molecules_df <- CurrentCGRP %>% left_join(all_molecules_df)  %>% select(patNum, `60`) 

all_molecules_df <- separate_rows(all_molecules_df, `60`, sep = ",", convert=T )

all_molecules_df <- all_molecules_df %>% filter(`60` != "-") %>% distinct()

all_molecules_df <- all_molecules_df %>% mutate(`60`=as.numeric(`60`)) %>%
  left_join(Drug_to_class_lookup, by=c("60"="Generic")) %>%
  select(patNum, Class) %>% distinct() %>%
  left_join(Class_names %>% mutate(class_id=as.numeric(class_id)), by=c("Class"="class_id")) %>%
  select(patNum, class) %>% distinct()

length(unique(all_molecules_df$patNum))

all_molecules_df %>% 
    mutate(class2=ifelse(class=="Triptans", "Triptans",
                      ifelse(class=="Anti-CGRP mAb", "CGRPInj",
                             ifelse(class %in% c("Nsaid Inc Combs", "Non Opioid Analgesics inc combs",
                                                 "Opioid Analgesics inc combs", "Antimigraine",
                                                 "Triptan Combination"), "OtherAcute",
                                    ifelse(class=="Anti-CGRP gepant", "Gepant","OtherPrev" ))))) %>%
  select(patNum, class2) %>% distinct() %>% drop_na() %>% 
  mutate(exp=1) %>%
  spread(key=class2, value=exp) %>% 
  group_by(CGRPInj, Gepant, OtherPrev, OtherAcute, Triptans) %>% count() %>% mutate(n=n/805)





# Current CGRP, how many drugs?

all_molecules_df <- fread("US_AllMolecules_OverTime_m60extended.txt", sep="\t")

CurrentCGRP <- all_molecules_df %>% select(patNum, `60`) 

CurrentCGRP <- separate_rows(CurrentCGRP, `60`, sep = ",", convert=T )

CurrentCGRP <- CurrentCGRP %>% filter(`60` != "-") %>% distinct()

CurrentCGRP <- CurrentCGRP %>% mutate(`60`=as.numeric(`60`)) %>%
  left_join(Drug_to_class_lookup, by=c("60"="Generic")) %>%
  select(patNum, Class) %>% distinct() %>%
  left_join(Class_names %>% mutate(class_id=as.numeric(class_id)), by=c("Class"="class_id")) %>%
  select(patNum, class) %>% distinct() %>% filter(class=="Anti-CGRP mAb"|class=="Anti-CGRP gepant") %>% select(patNum) %>% distinct()


all_molecules_df <- CurrentCGRP %>% left_join(all_molecules_df)  %>% select(patNum, `60`) 

all_molecules_df <- separate_rows(all_molecules_df, `60`, sep = ",", convert=T )

all_molecules_df <- all_molecules_df %>% filter(`60` != "-") %>% distinct()

CurrentCGRP %>% left_join(all_molecules_df %>% group_by(patNum) %>% count()) %>%
  ungroup() %>% summarise(n2=mean(n))





# What were the CGRP patients ON the month before?

# using extended m60 table

all_molecules_df <- fread("US_AllMolecules_OverTime_m60extended.txt", sep="\t")

all_molecules_df <- gather(all_molecules_df, month, treat, `0`:`60`)

FirstCGRP <- separate_rows(all_molecules_df, treat, sep = ",", convert=T )

length(unique(FirstCGRP$patNum))

FirstCGRP  %>%
  left_join(Drug_to_class_lookup %>% mutate(Generic=as.character(Generic)), by=c("treat"="Generic")) %>%
  select(patNum, month, Class) %>% distinct() %>%
  left_join(Class_names %>% mutate(class_id=as.numeric(class_id)), by=c("Class"="class_id")) %>%
  select(patNum, month, class) %>% distinct() %>% filter(class=="Anti-CGRP mAb"|class=="Anti-CGRP gepant") %>%
  group_by(patNum) %>% filter(month==min(month)) %>% select(patNum, month) %>% distinct() %>% rename("first"="month") %>%
  left_join(FirstCGRP) %>% ungroup() %>% filter(as.numeric(month) == as.numeric(first)-1) %>%
  select(patNum, treat) %>% 
  left_join(Drug_to_class_lookup %>% mutate(Generic=as.character(Generic)), by=c("treat"="Generic")) %>%
  select(patNum, Class) %>% distinct() %>%
  left_join(Class_names %>% mutate(class_id=as.numeric(class_id)), by=c("Class"="class_id")) %>%
  select(patNum, class) %>% distinct() %>%
  group_by(class) %>% count() %>% mutate(n=n/805) %>% arrange(-n)

  

# using original table

all_molecules_df <- fread("US_AllMolecules_OverTime.txt", sep="\t")

all_molecules_df <- gather(all_molecules_df, month, treat, `0`:`60`)

FirstCGRP <- separate_rows(all_molecules_df, treat, sep = ",", convert=T )

FirstCGRP  %>%
  left_join(Drug_to_class_lookup %>% mutate(Generic=as.character(Generic)), by=c("treat"="Generic")) %>%
  select(patNum, month, Class) %>% distinct() %>%
  left_join(Class_names %>% mutate(class_id=as.numeric(class_id)), by=c("Class"="class_id")) %>%
  select(patNum, month, class) %>% distinct() %>% filter(class=="Anti-CGRP mAb"|class=="Anti-CGRP gepant") %>%
  group_by(patNum) %>% filter(month==min(month)) %>% select(patNum, month) %>% distinct() %>% rename("first"="month") %>%
  left_join(FirstCGRP) %>% ungroup() %>% filter(as.numeric(month) == as.numeric(first)-1) %>%
  select(patNum, treat) %>% 
  left_join(Drug_to_class_lookup %>% mutate(Generic=as.character(Generic)), by=c("treat"="Generic")) %>%
  select(patNum, Class) %>% distinct() %>%
  left_join(Class_names %>% mutate(class_id=as.numeric(class_id)), by=c("Class"="class_id")) %>%
  select(patNum, class) %>% distinct() %>%
  group_by(class) %>% count() %>% mutate(n=n/568) %>% arrange(-n)


# using drug group classification


all_molecules_df <- fread("US_AllMolecules_OverTime.txt", sep="\t")

all_molecules_df <- gather(all_molecules_df, month, treat, `0`:`60`)

FirstCGRP <- separate_rows(all_molecules_df, treat, sep = ",", convert=T )

FirstCGRP  %>%
  left_join(Drug_to_class_lookup %>% mutate(Generic=as.character(Generic)), by=c("treat"="Generic")) %>%
  select(patNum, month, Class) %>% distinct() %>%
  left_join(Class_names %>% mutate(class_id=as.numeric(class_id)), by=c("Class"="class_id")) %>%
  select(patNum, month, class) %>% distinct() %>% filter(class=="Anti-CGRP mAb"|class=="Anti-CGRP gepant") %>%
  group_by(patNum) %>% filter(month==min(month)) %>% select(patNum, month) %>% distinct() %>% rename("first"="month") %>%
  left_join(FirstCGRP) %>% ungroup() %>% filter(as.numeric(month) == as.numeric(first)-1) %>%
  select(patNum, treat) %>% 
  left_join(Drug_to_class_lookup %>% mutate(Generic=as.character(Generic)), by=c("treat"="Generic")) %>%
  select(patNum, Class) %>% distinct() %>%
  left_join(Class_names %>% mutate(class_id=as.numeric(class_id)), by=c("Class"="class_id")) %>%
  select(patNum, class) %>% distinct() %>% 
    mutate(class2=ifelse(class=="Triptans", "Triptans",
                      ifelse(class=="Anti-CGRP mAb", "CGRPInj",
                             ifelse(class %in% c("Nsaid Inc Combs", "Non Opioid Analgesics inc combs",
                                                 "Opioid Analgesics inc combs", "Antimigraine",
                                                 "Triptan Combination"), "OtherAcute",
                                    ifelse(class=="Anti-CGRP gepant", "Gepant","OtherPrev" ))))) %>%
  select(patNum, class2) %>% distinct() %>% drop_na() %>% 
  mutate(exp=1) %>%
  spread(key=class2, value=exp) %>% 
  group_by(OtherAcute, Gepant, OtherPrev, Triptans) %>% count() %>% mutate(n=n/568)







# using original table but classes ever up to CGRP

all_molecules_df <- fread("US_AllMolecules_OverTime.txt", sep="\t")

all_molecules_df <- gather(all_molecules_df, month, treat, `0`:`60`)

FirstCGRP <- separate_rows(all_molecules_df, treat, sep = ",", convert=T )

FirstCGRP  %>%
  left_join(Drug_to_class_lookup %>% mutate(Generic=as.character(Generic)), by=c("treat"="Generic")) %>%
  select(patNum, month, Class) %>% distinct() %>%
  left_join(Class_names %>% mutate(class_id=as.numeric(class_id)), by=c("Class"="class_id")) %>%
  select(patNum, month, class) %>% distinct() %>% filter(class=="Anti-CGRP mAb"|class=="Anti-CGRP gepant") %>%
  group_by(patNum) %>% filter(month==min(month)) %>% select(patNum, month) %>% distinct() %>% rename("first"="month") %>%
  left_join(FirstCGRP) %>% ungroup() %>% filter(as.numeric(month) < as.numeric(first)) %>%
  select(patNum, treat) %>% 
  left_join(Drug_to_class_lookup %>% mutate(Generic=as.character(Generic)), by=c("treat"="Generic")) %>%
  select(patNum, Class) %>% distinct() %>%
  left_join(Class_names %>% mutate(class_id=as.numeric(class_id)), by=c("Class"="class_id")) %>%
  select(patNum, class) %>% distinct() %>%
  group_by(class) %>% count() %>% mutate(n=n/568) %>% arrange(-n)

# Time since diagnosis: Currently ON CGRP vs other 


all_molecules_df <- fread("US_AllMolecules_OverTime_m60extended.txt", sep="\t")

CurrentCGRP <- all_molecules_df %>% select(patNum, `60`) 

CurrentCGRP <- separate_rows(CurrentCGRP, `60`, sep = ",", convert=T )

CurrentCGRP <- CurrentCGRP %>% filter(`60` != "-") %>% distinct()

CurrentCGRP <- CurrentCGRP %>% mutate(`60`=as.numeric(`60`)) %>%
  left_join(Drug_to_class_lookup, by=c("60"="Generic")) %>%
  select(patNum, Class) %>% distinct() %>%
  left_join(Class_names %>% mutate(class_id=as.numeric(class_id)), by=c("Class"="class_id")) %>%
  select(patNum, class) %>% distinct() %>% filter(class=="Anti-CGRP mAb"|class=="Anti-CGRP gepant") %>% select(patNum) %>% distinct()


Pfizer_Migraine_Pat <- read_sav("Pfizer_Migraine_Pat v.2.0.sav")

US_df <- Pfizer_Migraine_Pat %>% filter(qcountries==28)

US_df %>% mutate(patNum=as.numeric(patNum)) %>% select(patNum, PRF_B_2c_DaysSince) %>% drop_na() %>%
  left_join(CurrentCGRP %>% mutate(CGRP=1)) %>% group_by(CGRP) %>% summarise(n=mean(PRF_B_2c_DaysSince/(12*30.5)))


US_df %>% mutate(patNum=as.numeric(patNum)) %>% select(patNum, PRF_B_2c_DaysSince) %>% drop_na() %>%
  left_join(CurrentCGRP %>% mutate(CGRP=1)) %>%
  ggplot(aes(PRF_B_2c_DaysSince/(30.5*12), colour=as.factor(CGRP), fill=as.factor(CGRP))) +
  geom_density(alpha=0.7) +
  xlim(0,20) +
  theme_minimal() + xlab("\n Number of Years since Migraine Diagnosis") + ylab("Patient density \n") +
  scale_fill_manual(values=c("midnightblue", "deepskyblue3")) +
  scale_colour_manual(values=c("midnightblue", "deepskyblue3"))

# ----------------------



# US Volume : Physician specialty vs patient type vs type of drugs ----------

# Physicians

Pfizer_Migraine_Pat <- read_sav("Pfizer_Migraine_Pat v.2.0.sav")

US_df <- Pfizer_Migraine_Pat %>% filter(qcountries==28)

US_df %>% select(patNum, SCR_1_RC) %>% group_by(SCR_1_RC) %>% count()

US_df %>% select(patNum, PhyPrimarySpeciality2) %>% group_by(PhyPrimarySpeciality2) %>% count()


all_molecules_df <- fread("US_AllMolecules_OverTime_m60extended.txt", sep="\t")
CurrentCGRP <- all_molecules_df %>% select(patNum, `60`) 
CurrentCGRP <- separate_rows(CurrentCGRP, `60`, sep = ",", convert=T )
CurrentCGRP <- CurrentCGRP %>% filter(`60` != "-") %>% distinct()
CurrentCGRP <- CurrentCGRP %>% mutate(`60`=as.numeric(`60`)) %>%
  left_join(Drug_to_class_lookup, by=c("60"="Generic")) %>%
  select(patNum, Class) %>% distinct() %>%
  left_join(Class_names %>% mutate(class_id=as.numeric(class_id)), by=c("Class"="class_id")) %>%
  select(patNum, class) %>% distinct() %>% filter(class=="Anti-CGRP mAb"|class=="Anti-CGRP gepant") %>% select(patNum) %>% distinct()

US_df %>% select(patNum, PhyPrimarySpeciality2) %>%  mutate(patNum=as.numeric(patNum)) %>%
  left_join(CurrentCGRP %>% mutate(group=1)) %>%
  group_by(PhyPrimarySpeciality2, group) %>% count() %>% mutate(n=n/1452)


Pats_Phys <- US_df %>% select(patNum, PhyPrimarySpeciality2) %>%  mutate(patNum=as.numeric(patNum))


all_molecules_df <- fread("US_AllMolecules_OverTime_m60extended.txt", sep="\t")
all_molecules_df <- all_molecules_df %>% select(patNum, `60`) 
all_molecules_df <- separate_rows(all_molecules_df, `60`, sep = ",", convert=T )
all_molecules_df <- all_molecules_df %>% filter(`60` != "-") %>% distinct()

current_classes <- all_molecules_df %>% mutate(`60`=as.numeric(`60`)) %>%
  left_join(Drug_to_class_lookup, by=c("60"="Generic")) %>%
  left_join(Class_names %>% mutate(class_id=as.numeric(class_id)), by=c("Class"="class_id"))

current_classes <- current_classes %>%   mutate(class2=ifelse(class=="Triptans", "Triptans",
                      ifelse(class=="Anti-CGRP mAb", "CGRPInj",
                             ifelse(class %in% c("Nsaid Inc Combs", "Non Opioid Analgesics inc combs",
                                                 "Opioid Analgesics inc combs", "Antimigraine",
                                                 "Triptan Combination"), "OtherAcute",
                                    ifelse(class=="Anti-CGRP gepant", "Gepant","OtherPrev" ))))) 



groups <- current_classes %>% select(patNum, class2) %>% distinct() %>%
  mutate(class3=ifelse(class2=="CGRPInj", "Prev",
                       ifelse(class2=="OtherAcute", "Acute",
                              ifelse(class2=="Triptans", "Acute",
                                    ifelse(class2=="Gepant", "Acute",   "Prev"))))) %>%
  select(patNum, class3) %>% distinct() %>% arrange(patNum, class3) %>%  drop_na() %>%
  group_by(patNum) %>% mutate(group=paste(class3, collapse = ",")) %>% ungroup() %>% select(patNum, group) %>% distinct() 

groups <- Pats_Phys %>% left_join(groups) %>% drop_na()

volume <- current_classes %>%
  mutate(class3=ifelse(class2=="CGRPInj", "CGRPInj",
                       ifelse(class2=="OtherAcute", "OtherAcute",
                              ifelse(class2=="Triptans", "Triptans",
                                    ifelse(class2=="Gepant", "Gepant",   "OtherPrev"))))) %>%
  group_by(patNum, class3) %>% count() %>% spread(key=class3, value=n)

volume[is.na(volume)] <- 0

groups <- groups %>% left_join(volume)

groups <- groups %>% select(-`<NA>`)

fwrite(groups, "US_Volume_Summary_patientLevel_Gepants.csv")

groups %>% group_by(group, PhyPrimarySpeciality2) %>% summarise(total=sum(CGRPInj+Gepant))




# ----------

# UK Class penetrance compare 3 methodologies ---------------

Drug_to_class_lookup <- fread("Drug_to_class_lookup.txt", sep="\t")
Class_names <- fread("Class_names.txt", sep="\t")
Molecule_names <- fread("Molecule_names.txt", sep="\t")

all_molecules_df <- fread("UK_AllMolecules_OverTime_m60extended.txt", sep="\t")


Rx_exp <- all_molecules_df %>% gather(month, treat, `0`:`60`) %>% filter(treat!="-") %>%
  select(patNum) %>% distinct()

# Month 60
all_molecules_df <- Rx_exp %>% left_join(all_molecules_df) %>% select(patNum, `60`) 
all_molecules_df <- separate_rows(all_molecules_df, `60`, sep = ",", convert=T )
all_molecules_df <- all_molecules_df %>% filter(`60` != "-") %>% distinct()

current_classes <- all_molecules_df %>% mutate(`60`=as.numeric(`60`)) %>%
  left_join(Drug_to_class_lookup, by=c("60"="Generic")) %>%
  select(patNum, Class) %>% distinct() %>%
  left_join(Class_names %>% mutate(class_id=as.numeric(class_id)), by=c("Class"="class_id")) %>%
  select(patNum, class) %>% distinct()

current_classes %>% group_by(class) %>% count() %>% arrange(-n) %>% mutate(n=n/1032)

current_classes %>% mutate(class2=ifelse(class=="Triptans", "Triptans",
                      ifelse(class=="Anti-CGRP mAb", "CGRPInj",
                             ifelse(class %in% c("Nsaid Inc Combs", "Non Opioid Analgesics inc combs",
                                                 "Opioid Analgesics inc combs", "Antimigraine",
                                                 "Triptan Combination"), "OtherAcute", "OtherPrev" )))) %>%
  select(patNum, class2) %>% distinct() %>% group_by(class2) %>% count() %>% mutate(n=n/1032) 


all_molecules_df <- fread("UK_AllMolecules_OverTime.txt", sep="\t")

Rx_exp <- all_molecules_df %>% gather(month, treat, `0`:`60`) %>% filter(treat!="-") %>%
  select(patNum) %>% distinct()

# Month 60
all_molecules_df <- Rx_exp %>% left_join(all_molecules_df) %>% select(patNum, `60`) 
all_molecules_df <- separate_rows(all_molecules_df, `60`, sep = ",", convert=T )
all_molecules_df <- all_molecules_df %>% filter(`60` != "-") %>% distinct()

current_classes <- all_molecules_df %>% mutate(`60`=as.numeric(`60`)) %>%
  left_join(Drug_to_class_lookup, by=c("60"="Generic")) %>%
  select(patNum, Class) %>% distinct() %>%
  left_join(Class_names %>% mutate(class_id=as.numeric(class_id)), by=c("Class"="class_id")) %>%
  select(patNum, class) %>% distinct()

current_classes %>% group_by(class) %>% count() %>% arrange(-n) %>% mutate(n=n/1032)

current_classes %>% mutate(class2=ifelse(class=="Triptans", "Triptans",
                      ifelse(class=="Anti-CGRP mAb", "CGRPInj",
                             ifelse(class %in% c("Nsaid Inc Combs", "Non Opioid Analgesics inc combs",
                                                 "Opioid Analgesics inc combs", "Antimigraine",
                                                 "Triptan Combination"), "OtherAcute", "OtherPrev" )))) %>%
  select(patNum, class2) %>% distinct() %>% group_by(class2) %>% count() %>% mutate(n=n/1032) 



Pfizer_Migraine_Pat <- read_sav("Pfizer_Migraine_Pat v.2.0.sav")
UK_df <- Pfizer_Migraine_Pat %>% filter(qcountries==27)

UK_df <- UK_df %>% select(patNum, PRF_D_3a_101:PRF_D_3a_123, PRF_E_3_201:PRF_E_3_220)

UK_df <- gather(UK_df, Drug, Molecule, PRF_D_3a_101:PRF_E_3_220)
UK_df <- UK_df %>% drop_na() %>% filter(Molecule==1)

UK_df <- UK_df %>% mutate( molecule_number = sub(".*_(\\d+)$", "\\1", Drug))

UK_df <- UK_df %>% select(patNum, Molecule, molecule_number) %>% distinct()

unique(UK_df$molecule_number)

length(unique(UK_df$patNum))

UK_df %>% mutate(molecule_number=as.numeric(molecule_number)) %>%
  mutate(class=ifelse(molecule_number %in% c(101,102,103,104,105,106,107,108), "Triptan", 
                              ifelse(molecule_number %in% c(109,110,111,115,116,123,220), "NSAID",
                                     ifelse(molecule_number %in% c(112,121,122,212,215,219), "Other",
                                            ifelse(molecule_number %in% c(113,114,204,205), "Gepant",
                                                   ifelse(molecule_number %in% c(117,118,119,120), "Opioid",
                                                          ifelse(molecule_number %in% c(201 ,202,203,206), "Mab",
                                                                 ifelse(molecule_number %in% c(207,213,216), "Anticonvulsant",
                                                                        ifelse(molecule_number %in% c(208,209,210), "BetaBlocker",
                                                                               ifelse(molecule_number %in% c(211,217,218), "Antidepressant",
                                                                                      ifelse(molecule_number %in% c(214), "Botox", NA )))))))))))  %>%
  select(patNum, class) %>% distinct() %>% group_by(class) %>% count() %>% mutate(n=n/1032)


# ---------------------------------
# Other trials ? ------------

# all_molecules_df <- fread("UK_AllMolecules_OverTime.txt", sep="\t")
# 
# all_molecules_df <- gather(all_molecules_df, month, treat, `0`:`60`)
# 
# FirstCGRP <- separate_rows(all_molecules_df, treat, sep = ",", convert=T )
# 
# FirstCGRP  %>%
#   left_join(Drug_to_class_lookup %>% mutate(Generic=as.character(Generic)), by=c("treat"="Generic")) %>%
#   select(patNum, month, Class) %>% distinct() %>%
#   left_join(Class_names %>% mutate(class_id=as.numeric(class_id)), by=c("Class"="class_id")) %>%
#   select(patNum, month, class) %>% distinct() %>% filter(class=="Anti-CGRP mAb")
# 

# - - - - - - - - - - - - -- - - - - 



Pfizer_Migraine_Pat <- read_sav("Pfizer_Migraine_Pat v.2.0.sav")
UK_df <- Pfizer_Migraine_Pat %>% filter(qcountries==27)
UK_df <- UK_df %>% select(patNum, PRF_D_3a_101:PRF_D_3a_123, PRF_E_3_201:PRF_E_3_220)
UK_df <- gather(UK_df, Drug, Molecule, PRF_D_3a_101:PRF_E_3_220)
UK_df <- UK_df %>% drop_na() %>% filter(Molecule==1)
UK_df <- UK_df %>% mutate( molecule_number = sub(".*_(\\d+)$", "\\1", Drug))
UK_df <- UK_df %>% select(patNum, Molecule, molecule_number) %>% distinct()
UK_df <- UK_df %>% select(patNum, molecule_number) %>% distinct()
PRF_data <- UK_df
length(unique(PRF_data$patNum))


Pfizer_Migraine_Pat <- read_sav("Pfizer_Migraine_Pat v.2.0.sav")
UK_df <- Pfizer_Migraine_Pat %>% filter(qcountries==27)
Current_df <- UK_df %>% select(patNum, Prevent_Generic_Curr_1, Acute_Generic_Curr_1) 
Current_df <- gather(Current_df, line, drug, Prevent_Generic_Curr_1:Acute_Generic_Curr_1)
Current_df <- Current_df %>% filter(drug!=9901)
Current_df <- Current_df %>% select(patNum, drug) %>% distinct()
length(unique(Current_df$patNum))
sum(Current_df)






Pfizer_Migraine_Pat <- read_sav("Pfizer_Migraine_Pat v.2.0.sav")
UK_df <- Pfizer_Migraine_Pat %>% filter(qcountries==27)
Acute_molecules <- UK_df %>% select(patNum, contains("acuteline") ) %>% 
  select(patNum, !contains("switch") ) %>%
  select(patNum, !contains("end") ) %>% 
  select(patNum, !contains("start") ) %>%
  select(patNum, !contains("headday") ) %>%
  select(patNum, !contains("dur") ) %>%
  select(patNum, !contains("lines") )

Acute_molecules <- gather(Acute_molecules, acuteline, use, acuteline1_1:acuteline6_9501)
Acute_molecules <- Acute_molecules %>% arrange(patNum)
Acute_molecules <- Acute_molecules %>% mutate(
  line_number = str_extract(acuteline, "(?<=acuteline)\\d+"),
  molecule_number = str_extract(acuteline, "(?<=_)\\d+")
  ) %>% 
  filter(use==1) %>%
  select(-c(acuteline, use)) %>% filter(molecule_number != "9501")

Acute_molecules <- Acute_molecules %>% arrange(patNum, line_number, molecule_number) %>%
  arrange(patNum, as.numeric(line_number), as.numeric(molecule_number)) %>% 
  group_by(patNum, line_number) %>% 
  mutate(molecule_number = paste0(molecule_number, collapse= ",")) %>% distinct()

Acute_molecules <- Acute_molecules %>% ungroup() %>% group_by(patNum) %>% filter(line_number==max(line_number)) %>% distinct()




Prev_molecules <- UK_df %>% select(patNum, contains("prevline") ) %>% 
  select(patNum, !contains("switch") ) %>%
  select(patNum, !contains("end") ) %>% 
  select(patNum, !contains("start") ) %>%
  select(patNum, !contains("headday") ) %>%
  select(patNum, !contains("dur") ) %>%
  select(patNum, !contains("lines") )

Prev_molecules <- gather(Prev_molecules, prevline, use, prevline1_1:prevline6_34801)
Prev_molecules <- Prev_molecules %>% arrange(patNum)
Prev_molecules <- Prev_molecules %>% mutate(
  line_number = str_extract(prevline, "(?<=prevline)\\d+"),
  molecule_number = str_extract(prevline, "(?<=_)\\d+")
  ) %>% 
  filter(use==1) %>%
  select(-c(prevline, use)) %>% filter(molecule_number != "9501" & molecule_number != "34801")

Prev_molecules <- Prev_molecules %>% arrange(patNum, as.numeric(line_number), as.numeric(molecule_number)) %>%
  group_by(patNum, line_number) %>% 
  mutate(molecule_number = paste0(molecule_number, collapse= ",")) %>% distinct()

Prev_molecules <- Prev_molecules %>% ungroup() %>% group_by(patNum) %>% filter(line_number==max(line_number)) %>% distinct()

Last_line <- Acute_molecules %>% select(-line_number) %>% bind_rows(Prev_molecules %>% select(-line_number)) 

Last_line <- separate_rows(Last_line, molecule_number, sep = ",", convert=T )

Last_line <- Last_line %>% distinct() %>% arrange(patNum, as.numeric(molecule_number)) %>% 
   group_by(patNum) %>% 
  mutate(molecule_number = paste0(molecule_number, collapse= ",")) %>% distinct()

# names(Last_line)[2] <- "drug"

data.frame(Last_line %>% full_join(Current_df))

# Last think they had was 1 CGRP and

Last_line



# - - - - - - - - - - - - - - - - - - - - -- - 


Pfizer_Migraine_Pat <- read_sav("Pfizer_Migraine_Pat v.2.0.sav")
UK_df <- Pfizer_Migraine_Pat %>% filter(qcountries==27)

Acute_molecules <- UK_df %>% select(patNum, contains("acuteline") ) %>% 
  select(patNum, !contains("switch") ) %>%
  select(patNum, !contains("end") ) %>% 
  select(patNum, !contains("start") ) %>%
  select(patNum, !contains("headday") ) %>%
  select(patNum, !contains("dur") ) %>%
  select(patNum, !contains("lines") )

Acute_molecules <- gather(Acute_molecules, acuteline, use, acuteline1_1:acuteline6_9501)

Acute_molecules <- Acute_molecules %>% arrange(patNum)
Acute_molecules <- Acute_molecules %>% filter(use==1)

Acute_molecules <- Acute_molecules %>% mutate(
  line_number = str_extract(acuteline, "(?<=acuteline)\\d+"),
  molecule_number = str_extract(acuteline, "(?<=_)\\d+")
  ) %>% 
  filter(use==1) %>%
  select(-c(acuteline, use)) %>% filter(molecule_number != "9501") %>%
  select(patNum, line_number) %>% distinct()




Prev_molecules <- UK_df %>% select(patNum, contains("prevline") ) %>% 
  select(patNum, !contains("switch") ) %>%
  select(patNum, !contains("end") ) %>% 
  select(patNum, !contains("start") ) %>%
  select(patNum, !contains("headday") ) %>%
  select(patNum, !contains("dur") ) %>%
  select(patNum, !contains("lines") )

Prev_molecules <- gather(Prev_molecules, prevline, use, prevline1_1:prevline6_34801)

Prev_molecules <- Prev_molecules %>% arrange(patNum)
Prev_molecules <- Prev_molecules %>% filter(use==1)



Prev_molecules <- Prev_molecules %>% mutate(
  line_number = str_extract(prevline, "(?<=prevline)\\d+"),
  molecule_number = str_extract(prevline, "(?<=_)\\d+")
  ) %>% 
  filter(use==1) %>%
  select(-c(prevline, use)) %>% filter(molecule_number != "9501") %>%
  select(patNum, line_number) %>% distinct()


Lines_per_pat <- Prev_molecules %>% bind_rows(Acute_molecules) %>% group_by(patNum) %>% count() 

mean(Lines_per_pat$n) # 878
sum(Lines_per_pat$n) # 2139




UK_df %>% select(startacuteline1, endacuteline1, acutelinedur1)
Starts_acute_lines <- UK_df %>% select(patNum, contains("startacuteline") )
Starts_acute_lines <- gather(Starts_acute_lines, acuteline, start, 
                             startacuteline1:startacuteline6, factor_key=FALSE)
Starts_acute_lines <- Starts_acute_lines %>% arrange(patNum) %>% drop_na() %>%
  mutate(acuteline=parse_number(acuteline)) %>% arrange(patNum)

Ends_acute_lines <- UK_df %>% select(patNum, contains("endacuteline") )
Ends_acute_lines <- gather(Ends_acute_lines, acuteline, end, 
                           endacuteline1:endacuteline6, factor_key=FALSE)
Ends_acute_lines <- Ends_acute_lines %>% arrange(patNum) %>%  drop_na() %>% 
  mutate(acuteline=parse_number(acuteline)) %>% arrange(patNum)

Duration_acute_lines <- UK_df %>% select(patNum, contains("acutelinedur") )
Duration_acute_lines <- gather(Duration_acute_lines, acuteline, 
                               duration, acutelinedur1:acutelinedur6, factor_key=FALSE)
Duration_acute_lines <- Duration_acute_lines %>% arrange(patNum) %>% drop_na() %>% 
  mutate(acuteline=parse_number(acuteline)) %>% arrange(patNum)

Lines_durations <- Starts_acute_lines %>%  
  left_join(Ends_acute_lines) %>%  
  left_join(Duration_acute_lines) %>% 
  arrange(patNum) %>% group_by(patNum) %>% 
  mutate(end2 = ifelse( is.na(duration), lead(start) - 1, end)) 

Lines_durations <- Lines_durations %>% group_by(patNum) %>% 
  mutate(end = ifelse( is.na(end), end2, end)) %>% 
  mutate(duration = ifelse( is.na(duration), start - end, duration))


Acute_lines_dur <- Lines_durations %>% filter(!is.na(duration)) %>% select(patNum, acuteline) %>% distinct()
names(Acute_lines_dur)[2] <- "line"


Starts_prev_lines <- UK_df %>% select(patNum, contains("startprevline") )
Starts_prev_lines <- gather(Starts_prev_lines, prevline, start, 
                             startprevline1:startprevline6, factor_key=FALSE)
Starts_prev_lines <- Starts_prev_lines %>% arrange(patNum) %>% drop_na() %>% 
  mutate(prevline=parse_number(prevline)) %>% arrange(patNum)
Ends_prev_lines <- UK_df %>% select(patNum, contains("endprevline") )
Ends_prev_lines <- gather(Ends_prev_lines, prevline, end, 
                           endprevline1:endprevline6, factor_key=FALSE)
Ends_prev_lines <- Ends_prev_lines %>% arrange(patNum) %>% drop_na() %>% 
  mutate(prevline=parse_number(prevline)) %>% arrange(patNum)
Duration_prev_lines <- UK_df %>% select(patNum, contains("prevlinedur") )
Duration_prev_lines <- gather(Duration_prev_lines, prevline, 
                               duration, prevlinedur1:prevlinedur6, factor_key=FALSE)
Duration_prev_lines <- Duration_prev_lines %>% arrange(patNum) %>% drop_na() %>% 
  mutate(prevline=parse_number(prevline)) %>% arrange(patNum)
Lines_durations <- Starts_prev_lines %>%  
  left_join(Ends_prev_lines) %>%  
  left_join(Duration_prev_lines) %>% 
  arrange(patNum) %>% group_by(patNum) %>% 
  mutate(end2 = ifelse( is.na(duration), lead(start) - 1, end)) 
Lines_durations <- Lines_durations %>% group_by(patNum) %>% 
  mutate(end = ifelse( is.na(end), end2, end)) %>% 
  mutate(duration = ifelse( is.na(duration), start - end, duration))

Prev_lines_dur <- Lines_durations %>% filter(!is.na(duration)) %>% select(patNum, prevline) %>% distinct()
names(Prev_lines_dur)[2] <- "line"

Lines_per_pat_dur <- Acute_lines_dur %>% bind_rows(Prev_lines_dur) %>% group_by(patNum) %>% count()

mean(Lines_per_pat_dur$n) # 
sum(Lines_per_pat_dur$n) # 1272

# - - - - - - - - - - - - - - - - - - -
# ------------
# China Acute Lines Long Format -----------------------------------------------------

Pfizer_Migraine_Pat <- read_sav("Pfizer_Migraine_Pat v.2.0.sav")

Pfizer_Migraine_Pat %>% select(qcountries) %>% distinct()

China_df <- Pfizer_Migraine_Pat %>% filter(qcountries==7)

China_df %>% select(startacuteline1, endacuteline1, acutelinedur1)

Starts_acute_lines <- China_df %>% select(patNum, contains("startacuteline") )

Starts_acute_lines <- gather(Starts_acute_lines, acuteline, start, 
                             startacuteline1:startacuteline6, factor_key=FALSE)

Starts_acute_lines <- Starts_acute_lines %>% arrange(patNum) %>% drop_na() %>% 
  mutate(acuteline=parse_number(acuteline)) %>% arrange(patNum)

Ends_acute_lines <- China_df %>% select(patNum, contains("endacuteline") )

Ends_acute_lines <- gather(Ends_acute_lines, acuteline, end, 
                           endacuteline1:endacuteline6, factor_key=FALSE)

Ends_acute_lines <- Ends_acute_lines %>% arrange(patNum) %>% drop_na() %>% 
  mutate(acuteline=parse_number(acuteline)) %>% arrange(patNum)

Duration_acute_lines <- China_df %>% select(patNum, contains("acutelinedur") )

Duration_acute_lines <- gather(Duration_acute_lines, acuteline, 
                               duration, acutelinedur1:acutelinedur6, factor_key=FALSE)

Duration_acute_lines <- Duration_acute_lines %>% arrange(patNum) %>% drop_na() %>% 
  mutate(acuteline=parse_number(acuteline)) %>% arrange(patNum)

Lines_durations <- Starts_acute_lines %>%  
  left_join(Ends_acute_lines) %>%  
  left_join(Duration_acute_lines) %>% 
  arrange(patNum) %>% group_by(patNum) %>% 
  mutate(end2 = ifelse( is.na(duration), lead(start) - 1, end)) 

Lines_durations <- Lines_durations %>% group_by(patNum) %>% 
  mutate(end = ifelse( is.na(end), end2, end)) %>% 
  mutate(duration = ifelse( is.na(duration), start - end, duration))

mean(Lines_durations$start) # 989.3072
mean(Lines_durations$end, na.rm = TRUE) # 981.7148
mean(Lines_durations$duration, na.rm = TRUE) # 597.7723

Lines_durations <- Lines_durations %>% select(-end2)

Lines_durations <- Lines_durations %>% 
  mutate(end = ifelse( is.na(end), 0, end )) %>%
  mutate(end = round (-end / 30.5, 0) ) %>% mutate(start = round(-start / 30.5, 0) ) %>%
  group_by(patNum) %>% mutate(end = ifelse( end == lead(start), end-1, end)) %>%
    mutate(end = ifelse( is.na(end), 0, end )) 

data.frame( Lines_durations %>% group_by(patNum) %>% count() %>% ungroup() %>% 
  filter(n>=3) %>% select(patNum) %>% left_join(Lines_durations) )

Lines_durations <- Lines_durations %>% select(-duration)

data.frame(Lines_durations)

df <- Lines_durations %>%
  mutate(month_number = map2(start, end, seq)) %>%
  unnest(month_number)

df <- df %>% select(patNum, acuteline, month_number)

df <- df %>% ungroup() %>% arrange(patNum, acuteline, month_number) %>% 
  group_by(patNum) %>% mutate(month_number2=ifelse(month_number==lead(month_number), 1, 0)) %>%
  filter(month_number2 == 0) %>% ungroup() %>% select(-month_number2)
  
df <- df %>% group_by(patNum, month_number) %>% filter(acuteline==max(acuteline)) %>% distinct() %>% ungroup()

wide_df <- pivot_wider(
  data = df,
  id_cols = c("patNum"),
  names_from = month_number,
  values_from = acuteline,
  values_fill = 0
)

wide_df <- wide_df[, order(as.numeric(colnames(wide_df)), decreasing = FALSE)]

wide_df[,360:376]

sum(is.na(wide_df))

names(wide_df)

wide_df <- wide_df[, 315:376]

names(wide_df)

wide_df %>% select(patNum, `-60`:`-1`)

wide_df <- gather(wide_df, Month, Line, `-61`:`-1`) %>% 
  mutate(Month=as.numeric(Month) + 61) %>%
  spread(key=Month, value=Line)

wide_df <- China_df %>% select(patNum)  %>% left_join(wide_df)

wide_df[is.na(wide_df)] <- 0

fwrite(wide_df, "China_AcuteLines_Wide.txt", sep="\t")

# --------------------

# China Preventive Lines Long Format -----------------------------------------------------

Pfizer_Migraine_Pat <- read_sav("Pfizer_Migraine_Pat v.2.0.sav")

China_df <- Pfizer_Migraine_Pat %>% filter(qcountries==7)

China_df %>% select(startprevline1,  endprevline1, prevlinedur1)

Starts_prev_lines <- China_df %>% select(patNum, contains("startprevline") )

Starts_prev_lines <- gather(Starts_prev_lines, prevline, start, 
                             startprevline1:startprevline6, factor_key=FALSE)

Starts_prev_lines <- Starts_prev_lines %>% arrange(patNum) %>% drop_na() %>% 
  mutate(prevline=parse_number(prevline)) %>% arrange(patNum)

Ends_prev_lines <- China_df %>% select(patNum, contains("endprevline") )

Ends_prev_lines <- gather(Ends_prev_lines, prevline, end, 
                           endprevline1:endprevline6, factor_key=FALSE)

Ends_prev_lines <- Ends_prev_lines %>% arrange(patNum) %>% drop_na() %>% 
  mutate(prevline=parse_number(prevline)) %>% arrange(patNum)

Duration_prev_lines <- China_df %>% select(patNum, contains("prevlinedur") )

Duration_prev_lines <- gather(Duration_prev_lines, prevline, 
                               duration, prevlinedur1:prevlinedur6, factor_key=FALSE)

Duration_prev_lines <- Duration_prev_lines %>% arrange(patNum) %>% drop_na() %>% 
  mutate(prevline=parse_number(prevline)) %>% arrange(patNum)

Lines_durations <- Starts_prev_lines %>%  
  left_join(Ends_prev_lines) %>%  
  left_join(Duration_prev_lines) %>% 
  arrange(patNum) %>% group_by(patNum) %>% 
  mutate(end2 = ifelse( is.na(duration), lead(start) - 1, end)) 

Lines_durations <- Lines_durations %>% group_by(patNum) %>% 
  mutate(end = ifelse( is.na(end), end2, end)) %>% 
  mutate(duration = ifelse( is.na(duration), start - end, duration))

mean(Lines_durations$start) # 681.8901
mean(Lines_durations$end, na.rm = TRUE) # 793.9309
mean(Lines_durations$duration, na.rm = TRUE) # 360.1934

Lines_durations <- Lines_durations %>% select(-end2)

Lines_durations <- Lines_durations %>% 
  mutate(end = ifelse( is.na(end), 0, end )) %>%
  mutate(end = round (-end / 30.5, 0) ) %>% mutate(start = round(-start / 30.5, 0) ) %>%
  group_by(patNum) %>% mutate(end = ifelse( end == lead(start), end-1, end)) %>%
    mutate(end = ifelse( is.na(end), 0, end )) 

data.frame( Lines_durations %>% group_by(patNum) %>% count() %>% ungroup() %>% 
  filter(n>=3) %>% select(patNum) %>% left_join(Lines_durations) )

Lines_durations <- Lines_durations %>% select(-duration)

data.frame(Lines_durations)

df <- Lines_durations %>%
  mutate(month_number = map2(start, end, seq)) %>%
  unnest(month_number)

df <- df %>% select(patNum, prevline, month_number)

df <- df %>% ungroup() %>% arrange(patNum, prevline, month_number) %>% 
  group_by(patNum) %>% mutate(month_number2=ifelse(month_number==lead(month_number), 1, 0)) %>%
  filter(month_number2 == 0) %>% ungroup() %>% select(-month_number2)
  
df <- df %>% group_by(patNum, month_number) %>% filter(prevline==max(prevline)) %>% ungroup() %>% distinct()


wide_df <- pivot_wider(
  data = df,
  id_cols = c("patNum"),
  names_from = month_number,
  values_from = prevline,
  values_fill = 0
)


wide_df <- wide_df[, order(as.numeric(colnames(wide_df)), decreasing = FALSE)]

sum(is.na(wide_df))

names(wide_df)

wide_df <- wide_df[, 85:146]

names(wide_df)

wide_df %>% select(patNum, `-60`:`-1`)

wide_df <- gather(wide_df, Month, Line, `-61`:`-1`) %>% 
  mutate(Month=as.numeric(Month) + 61) %>%
  spread(key=Month, value=Line)

wide_df <- China_df %>% select(patNum)  %>% left_join(wide_df)

wide_df[is.na(wide_df)] <- 0

fwrite(wide_df, "China_PrevLines_Wide.txt", sep="\t")

# --------------------
# China Molecule composition of Acute Lines ----------------------------------

Pfizer_Migraine_Pat <- read_sav("Pfizer_Migraine_Pat v.2.0.sav")

China_df <- Pfizer_Migraine_Pat %>% filter(qcountries==7)

Acute_molecules <- China_df %>% select(patNum, contains("acuteline") ) %>% 
  select(patNum, !contains("switch") ) %>%
  select(patNum, !contains("end") ) %>% 
  select(patNum, !contains("start") ) %>%
  select(patNum, !contains("headday") ) %>%
  select(patNum, !contains("dur") ) %>%
  select(patNum, !contains("lines") )

length(names(Acute_molecules)) == (1 + 6*345) 

Acute_molecules <- gather(Acute_molecules, acuteline, use, acuteline1_1:acuteline6_9501)

Acute_molecules <- Acute_molecules %>% arrange(patNum)

Acute_molecules <- Acute_molecules %>% mutate(
  line_number = str_extract(acuteline, "(?<=acuteline)\\d+"),
  molecule_number = str_extract(acuteline, "(?<=_)\\d+")
  ) %>% 
  filter(use==1) %>%
  select(-c(acuteline, use)) %>% filter(molecule_number != "9501")


unique(Acute_molecules$line_number)
unique(Acute_molecules$molecule_number)

Acute_molecules <- Acute_molecules %>% arrange(patNum, line_number, molecule_number) %>%
  group_by(patNum, line_number) %>% 
  mutate(molecule_number = paste0(molecule_number, collapse= ",")) %>% distinct()

fwrite(Acute_molecules , "China_Acute_Lines_MoleculeComposition.txt", sep="\t")

# --------------------
# China Molecule composition of Preventive Lines ----------------------------------

Pfizer_Migraine_Pat <- read_sav("Pfizer_Migraine_Pat v.2.0.sav")

China_df <- Pfizer_Migraine_Pat %>% filter(qcountries==7)

Prev_molecules <- China_df %>% select(patNum, contains("prevline") ) %>% 
  select(patNum, !contains("switch") ) %>%
  select(patNum, !contains("end") ) %>% 
  select(patNum, !contains("start") ) %>%
  select(patNum, !contains("headday") ) %>%
  select(patNum, !contains("dur") ) %>%
  select(patNum, !contains("lines") )

Prev_molecules <- gather(Prev_molecules, prevline, use, prevline1_1:prevline6_34801)

Prev_molecules <- Prev_molecules %>% arrange(patNum)

Prev_molecules <- Prev_molecules %>% mutate(
  line_number = str_extract(prevline, "(?<=prevline)\\d+"),
  molecule_number = str_extract(prevline, "(?<=_)\\d+")
  ) %>% 
  filter(use==1) %>%
  select(-c(prevline, use)) %>% filter(molecule_number != "9501" & molecule_number != "34801")


unique(Prev_molecules$line_number)
unique(Prev_molecules$molecule_number)

Prev_molecules <- Prev_molecules %>% arrange(patNum, line_number, molecule_number) %>%
  group_by(patNum, line_number) %>% 
  mutate(molecule_number = paste0(molecule_number, collapse= ",")) %>% distinct()

fwrite(Prev_molecules , "China_Prev_Lines_MoleculeComposition.txt", sep="\t")

# --------------------
# China Create Wide Format with all individual molecules joined -------

China_AcuteLines_Wide <- fread("China_AcuteLines_Wide.txt")
China_PrevLines_Wide <- fread("China_PrevLines_Wide.txt")
Acute_Lines_MoleculeComposition <- fread("China_Acute_Lines_MoleculeComposition.txt")
Prev_Lines_MoleculeComposition <- fread("China_Prev_Lines_MoleculeComposition.txt")


China_AcuteLines_Wide <- China_AcuteLines_Wide %>% gather(month, line, `0`:`60`) 

head(China_AcuteLines_Wide) ; head(Acute_Lines_MoleculeComposition)

China_AcuteLines_Wide <- China_AcuteLines_Wide %>% left_join(Acute_Lines_MoleculeComposition, by=c("patNum"="patNum", "line"="line_number"))

China_AcuteLines_Wide[is.na(China_AcuteLines_Wide)] <- "-"

China_AcuteLines_Wide <- China_AcuteLines_Wide %>% select(-line)

China_PrevLines_Wide <- China_PrevLines_Wide %>% gather(month, line, `0`:`60`) 

head(China_PrevLines_Wide) ; head(Prev_Lines_MoleculeComposition)

China_PrevLines_Wide <- China_PrevLines_Wide %>% left_join(Prev_Lines_MoleculeComposition, by=c("patNum"="patNum", "line"="line_number"))

China_PrevLines_Wide[is.na(China_PrevLines_Wide)] <- "-"

China_PrevLines_Wide <- China_PrevLines_Wide %>% select(-line)

all_molecules_df <- China_AcuteLines_Wide %>% bind_rows(China_PrevLines_Wide) %>% distinct()

head(all_molecules_df)

all_molecules_df <- separate_rows(all_molecules_df, molecule_number, sep = ",", convert=T )

all_molecules_df <- all_molecules_df %>% distinct()

all_molecules_df <- all_molecules_df %>% filter(molecule_number!="-" ) 

all_molecules_df <- all_molecules_df %>% arrange(patNum, month, molecule_number) %>%
  group_by(patNum, month) %>% mutate(molecule_number = paste(molecule_number, collapse = ",")) %>%
  distinct()

unique(all_molecules_df$molecule_number)

all_molecules_df <- all_molecules_df %>% spread(key=month, value=molecule_number)

all_molecules_df[is.na(all_molecules_df)] <- "-"

US_AcuteLines_Wide <- fread("China_AcuteLines_Wide.txt")

all_molecules_df <- China_AcuteLines_Wide %>% select(patNum) %>% left_join(all_molecules_df)

all_molecules_df[is.na(all_molecules_df)] <- "-"

names(all_molecules_df)

all_molecules_df <- all_molecules_df %>% select(patNum, as.character(0:60))

all_molecules_df %>% gather(month, line, `0`:`60`) %>%
  arrange(patNum, as.numeric(month)) %>% group_by(patNum) %>% filter(line=="-" & lag(line) != "-")


fwrite(all_molecules_df, "China_AllMolecules_OverTime.txt", sep="\t")


# ------------------------
# China Allocate EXTRA/MISSING patients ON month 60 based on "Current" columns -----------

Pfizer_Migraine_Pat <- read_sav("Pfizer_Migraine_Pat v.2.0.sav")

China_df <- Pfizer_Migraine_Pat %>% filter(qcountries==7)

all_molecules_df <- fread("China_AllMolecules_OverTime.txt", sep="\t")

Current_df <- China_df %>% select(patNum, Prevent_Generic_Curr_1, Acute_Generic_Curr_1) 

Current_df <- gather(Current_df, line, drug, Prevent_Generic_Curr_1:Acute_Generic_Curr_1)

Current_df <- Current_df %>% filter(drug!=9901)

Current_df <- Current_df %>% select(patNum, drug) %>%
  distinct() %>% arrange(patNum, drug) %>% group_by(patNum) %>%
  mutate(drug = paste(drug, collapse = ",")) %>% distinct()

to_check <- all_molecules_df %>% select(patNum, `60`) %>% 
  left_join(Current_df %>% mutate(patNum = as.numeric(patNum)))

to_check[is.na(to_check)] <- "-"

to_check <- gather(to_check, col, drugs, `60`:drug)

to_check <- to_check %>% select(-col) %>% distinct() %>% filter(drugs!="-")

to_check <- separate_rows(to_check, drugs, sep = ",", convert=T )

to_check <- to_check %>% distinct() %>% arrange(patNum, drugs) %>% 
  group_by(patNum) %>% mutate(drugs= paste(drugs, collapse = ","))  %>% distinct()

all_molecules_df <- all_molecules_df %>% select(-`60`) %>% left_join(to_check)

all_molecules_df[is.na(all_molecules_df)] <- "-"

names(all_molecules_df)[62] <- "60"

fwrite(all_molecules_df, "China_AllMolecules_OverTime_m60extended.txt", sep="\t")

all_molecules_df <- fread("China_AllMolecules_OverTime_m60extended.txt", sep="\t")

all_molecules_df[grepl("26", `60`), ]


# -------------------------
# China Drug to class mapping -----------

Pfizer_Migraine_Pat <- read_sav("Pfizer_Migraine_Pat v.2.0.sav")

China_df <- Pfizer_Migraine_Pat %>% filter(qcountries==7)

Acute_1 <- China_df %>% select(Acute_Generic_Curr_1, Acute_Class_Curr_1 ) %>% distinct() %>%
  mutate(Acute_Generic_Curr_1=as.numeric(Acute_Generic_Curr_1)) %>%
  mutate(Acute_Class_Curr_1=as.numeric(Acute_Class_Curr_1)) %>% 
  rename("Generic"="Acute_Generic_Curr_1", "Class"="Acute_Class_Curr_1") %>% 
  filter(Generic!=9901) %>% drop_na()
  
Acute_2 <- China_df %>% select(Acute_Generic_Curr_2, Acute_Class_Curr_2 ) %>% distinct() %>%
  mutate(Acute_Generic_Curr_2=as.numeric(Acute_Generic_Curr_2)) %>%
  mutate(Acute_Class_Curr_2=as.numeric(Acute_Class_Curr_2)) %>% 
  rename("Generic"="Acute_Generic_Curr_2", "Class"="Acute_Class_Curr_2") %>% 
  filter(Generic!=9901) %>% drop_na()

Acute_3 <- China_df %>% select(Acute_Generic_Curr_3, Acute_Class_Curr_3 ) %>% distinct() %>%
  mutate(Acute_Generic_Curr_3=as.numeric(Acute_Generic_Curr_3)) %>%
  mutate(Acute_Class_Curr_3=as.numeric(Acute_Class_Curr_3)) %>% 
  rename("Generic"="Acute_Generic_Curr_3", "Class"="Acute_Class_Curr_3") %>% 
  filter(Generic!=9901) %>% drop_na()

Acute_4 <- China_df %>% select(Acute_Generic_Curr_4, Acute_Class_Curr_4 ) %>% distinct() %>%
  mutate(Acute_Generic_Curr_4=as.numeric(Acute_Generic_Curr_4)) %>%
  mutate(Acute_Class_Curr_4=as.numeric(Acute_Class_Curr_4)) %>% 
  rename("Generic"="Acute_Generic_Curr_4", "Class"="Acute_Class_Curr_4") %>% 
  filter(Generic!=9901) %>% drop_na()

Acute_5 <- China_df %>% select(Acute_Generic_Curr_5, Acute_Class_Curr_5 ) %>% distinct() %>%
  mutate(Acute_Generic_Curr_5=as.numeric(Acute_Generic_Curr_5)) %>%
  mutate(Acute_Class_Curr_5=as.numeric(Acute_Class_Curr_5)) %>% 
  rename("Generic"="Acute_Generic_Curr_5", "Class"="Acute_Class_Curr_5") %>% 
  filter(Generic!=9901) %>% drop_na()

Acute_6 <- China_df %>% select(Acute_Generic_Curr_6, Acute_Class_Curr_6 ) %>% distinct() %>%
  mutate(Acute_Generic_Curr_6=as.numeric(Acute_Generic_Curr_6)) %>%
  mutate(Acute_Class_Curr_6=as.numeric(Acute_Class_Curr_6)) %>% 
  rename("Generic"="Acute_Generic_Curr_6", "Class"="Acute_Class_Curr_6") %>% 
  filter(Generic!=9901) %>% drop_na()


Acute_7 <- China_df %>% select(Acute_Generic_Curr_7, Acute_Class_Curr_7 ) %>% distinct() %>%
  mutate(Acute_Generic_Curr_7=as.numeric(Acute_Generic_Curr_7)) %>%
  mutate(Acute_Class_Curr_7=as.numeric(Acute_Class_Curr_7)) %>% 
  rename("Generic"="Acute_Generic_Curr_7", "Class"="Acute_Class_Curr_7") %>% 
  filter(Generic!=9901) %>% drop_na()

Acute_8 <- China_df %>% select(Acute_Generic_Curr_8, Acute_Class_Curr_8 ) %>% distinct() %>%
  mutate(Acute_Generic_Curr_8=as.numeric(Acute_Generic_Curr_8)) %>%
  mutate(Acute_Class_Curr_8=as.numeric(Acute_Class_Curr_8)) %>% 
  rename("Generic"="Acute_Generic_Curr_8", "Class"="Acute_Class_Curr_8") %>% 
  filter(Generic!=9901) %>% drop_na()



Acute_9 <- China_df %>% select(Acute_Generic_Curr_9, Acute_Class_Curr_9 ) %>% distinct() %>%
  mutate(Acute_Generic_Curr_9=as.numeric(Acute_Generic_Curr_9)) %>%
  mutate(Acute_Class_Curr_9=as.numeric(Acute_Class_Curr_9)) %>% 
  rename("Generic"="Acute_Generic_Curr_9", "Class"="Acute_Class_Curr_9") %>% 
  filter(Generic!=9901) %>% drop_na()


Acute_10 <- China_df %>% select(Acute_Generic_Curr_10, Acute_Class_Curr_10 ) %>% distinct() %>%
  mutate(Acute_Generic_Curr_10=as.numeric(Acute_Generic_Curr_10)) %>%
  mutate(Acute_Class_Curr_10=as.numeric(Acute_Class_Curr_10)) %>% 
  rename("Generic"="Acute_Generic_Curr_10", "Class"="Acute_Class_Curr_10") %>% 
  filter(Generic!=9901) %>% drop_na()


Prevent_1 <- China_df %>% select(Prevent_Generic_Curr_1, Prevent_Class_Curr_1 ) %>% distinct() %>%
  mutate(Prevent_Generic_Curr_1=as.numeric(Prevent_Generic_Curr_1)) %>%
  mutate(Prevent_Class_Curr_1=as.numeric(Prevent_Class_Curr_1)) %>% 
  rename("Generic"="Prevent_Generic_Curr_1", "Class"="Prevent_Class_Curr_1") %>% 
  filter(Generic!=9901) %>% drop_na()
  
Prevent_2 <- China_df %>% select(Prevent_Generic_Curr_2, Prevent_Class_Curr_2 ) %>% distinct() %>%
  mutate(Prevent_Generic_Curr_2=as.numeric(Prevent_Generic_Curr_2)) %>%
  mutate(Prevent_Class_Curr_2=as.numeric(Prevent_Class_Curr_2)) %>% 
  rename("Generic"="Prevent_Generic_Curr_2", "Class"="Prevent_Class_Curr_2") %>% 
  filter(Generic!=9901) %>% drop_na()

Prevent_3 <- China_df %>% select(Prevent_Generic_Curr_3, Prevent_Class_Curr_3 ) %>% distinct() %>%
  mutate(Prevent_Generic_Curr_3=as.numeric(Prevent_Generic_Curr_3)) %>%
  mutate(Prevent_Class_Curr_3=as.numeric(Prevent_Class_Curr_3)) %>% 
  rename("Generic"="Prevent_Generic_Curr_3", "Class"="Prevent_Class_Curr_3") %>% 
  filter(Generic!=9901) %>% drop_na()

Prevent_4 <- China_df %>% select(Prevent_Generic_Curr_4, Prevent_Class_Curr_4 ) %>% distinct() %>%
  mutate(Prevent_Generic_Curr_4=as.numeric(Prevent_Generic_Curr_4)) %>%
  mutate(Prevent_Class_Curr_4=as.numeric(Prevent_Class_Curr_4)) %>% 
  rename("Generic"="Prevent_Generic_Curr_4", "Class"="Prevent_Class_Curr_4") %>% 
  filter(Generic!=9901) %>% drop_na()

Prevent_5 <- China_df %>% select(Prevent_Generic_Curr_5, Prevent_Class_Curr_5 ) %>% distinct() %>%
  mutate(Prevent_Generic_Curr_5=as.numeric(Prevent_Generic_Curr_5)) %>%
  mutate(Prevent_Class_Curr_5=as.numeric(Prevent_Class_Curr_5)) %>% 
  rename("Generic"="Prevent_Generic_Curr_5", "Class"="Prevent_Class_Curr_5") %>% 
  filter(Generic!=9901) %>% drop_na()

Prevent_6 <- China_df %>% select(Prevent_Generic_Curr_6, Prevent_Class_Curr_6 ) %>% distinct() %>%
  mutate(Prevent_Generic_Curr_6=as.numeric(Prevent_Generic_Curr_6)) %>%
  mutate(Prevent_Class_Curr_6=as.numeric(Prevent_Class_Curr_6)) %>% 
  rename("Generic"="Prevent_Generic_Curr_6", "Class"="Prevent_Class_Curr_6") %>% 
  filter(Generic!=9901) %>% drop_na()


Prevent_7 <- China_df %>% select(Prevent_Generic_Curr_7, Prevent_Class_Curr_7 ) %>% distinct() %>%
  mutate(Prevent_Generic_Curr_7=as.numeric(Prevent_Generic_Curr_7)) %>%
  mutate(Prevent_Class_Curr_7=as.numeric(Prevent_Class_Curr_7)) %>% 
  rename("Generic"="Prevent_Generic_Curr_7", "Class"="Prevent_Class_Curr_7") %>% 
  filter(Generic!=9901) %>% drop_na()



Prevent_8 <- China_df %>% select(Prevent_Generic_Curr_8, Prevent_Class_Curr_8 ) %>% distinct() %>%
  mutate(Prevent_Generic_Curr_8=as.numeric(Prevent_Generic_Curr_8)) %>%
  mutate(Prevent_Class_Curr_8=as.numeric(Prevent_Class_Curr_8)) %>% 
  rename("Generic"="Prevent_Generic_Curr_8", "Class"="Prevent_Class_Curr_8") %>% 
  filter(Generic!=9901) %>% drop_na()



Prevent_9 <- China_df %>% select(Prevent_Generic_Curr_9, Prevent_Class_Curr_9 ) %>% distinct() %>%
  mutate(Prevent_Generic_Curr_9=as.numeric(Prevent_Generic_Curr_9)) %>%
  mutate(Prevent_Class_Curr_9=as.numeric(Prevent_Class_Curr_9)) %>% 
  rename("Generic"="Prevent_Generic_Curr_9", "Class"="Prevent_Class_Curr_9") %>% 
  filter(Generic!=9901) %>% drop_na()


Prevent_10 <- China_df %>% select(Prevent_Generic_Curr_10, Prevent_Class_Curr_10 ) %>% distinct() %>%
  mutate(Prevent_Generic_Curr_10=as.numeric(Prevent_Generic_Curr_10)) %>%
  mutate(Prevent_Class_Curr_10=as.numeric(Prevent_Class_Curr_10)) %>% 
  rename("Generic"="Prevent_Generic_Curr_10", "Class"="Prevent_Class_Curr_10") %>% 
  filter(Generic!=9901) %>% drop_na()


Drug_to_class_lookup <- bind_rows(Acute_1, Acute_2, Acute_3, Acute_4, Acute_5, Acute_6, Acute_7, Acute_8, Acute_9, Acute_10,
          Prevent_1, Prevent_2, Prevent_3, Prevent_4, Prevent_5, Prevent_6, Prevent_7, Prevent_8, Prevent_9, Prevent_10) %>% distinct() %>%
  arrange(Generic, Class)

fwrite(Drug_to_class_lookup, "China_Drug_to_class_lookup.txt", sep="\t")

length(unique(Drug_to_class_lookup$Generic))


# -------------------------
# China Class % Penetrance ON month 60 -----------------------------

Pfizer_Migraine_Pat <- read_sav("Pfizer_Migraine_Pat v.2.0.sav")

China_df <- Pfizer_Migraine_Pat %>% filter(qcountries==7)



Drug_to_class_lookup <- fread("China_Drug_to_class_lookup.txt", sep="\t")
Class_names <- fread("Class_names.txt", sep="\t")
Molecule_names <- fread("Molecule_names.txt", sep="\t")

all_molecules_df <- fread("China_AllMolecules_OverTime_m60extended.txt", sep="\t")
all_molecules_df <- all_molecules_df %>% distinct()
all_molecules_df <- all_molecules_df %>% select(patNum, `60`) 
all_molecules_df <- separate_rows(all_molecules_df, `60`, sep = ",", convert=T )
all_molecules_df <- all_molecules_df %>% filter(`60` != "-") %>% distinct()

length(unique(all_molecules_df$`60`))
length(unique(all_molecules_df$patNum))

all_molecules_df %>% mutate(`60`=as.numeric(`60`)) %>%
  left_join(Drug_to_class_lookup, by=c("60"="Generic")) %>% 
  select(patNum, Class) %>% distinct() %>%
  left_join(Class_names %>% mutate(class_id=as.numeric(class_id)), by=c("Class"="class_id")) %>%
  select(patNum, class) %>% distinct() %>%
  group_by(class) %>% count() %>% mutate(n=100 * n/1201) %>% arrange(-n)

# -----------------


# China Class penetrance and CGRP usage, different versions ---------------

Drug_to_class_lookup <- fread("China_Drug_to_class_lookup.txt", sep="\t")
Class_names <- fread("Class_names.txt", sep="\t")
Molecule_names <- fread("Molecule_names.txt", sep="\t")

length(unique(all_molecules_df$patNum))

all_molecules_df <- fread("China_AllMolecules_OverTime_m60extended.txt", sep="\t")
all_molecules_df <- all_molecules_df %>% distinct()

Rx_exp <- all_molecules_df %>% gather(month, treat, `0`:`60`) %>% filter(treat!="-") %>%
  select(patNum) %>% distinct()

length(unique(Rx_exp$patNum)) # 1155


# Month 60

all_molecules_df <- Rx_exp %>% left_join(all_molecules_df) %>% select(patNum, `60`) 

all_molecules_df <- separate_rows(all_molecules_df, `60`, sep = ",", convert=T )

all_molecules_df <- all_molecules_df %>% filter(`60` != "-") %>% distinct()

current_classes <- all_molecules_df %>% mutate(`60`=as.numeric(`60`)) %>%
  left_join(Drug_to_class_lookup, by=c("60"="Generic")) %>%
  select(patNum, Class) %>% distinct() %>%
  left_join(Class_names %>% mutate(class_id=as.numeric(class_id)), by=c("Class"="class_id")) %>%
  select(patNum, class) %>% distinct()

unique(current_classes$class)

current_classes %>% group_by(class) %>% count() %>% arrange(-n)

current_classes %>% mutate(class2=ifelse(class=="Triptans", "Triptans",
                      ifelse(class=="Anti-CGRP mAb", "CGRPInj",
                             ifelse(class %in% c("Nsaid Inc Combs", "Non Opioid Analgesics inc combs",
                                                 "Opioid Analgesics inc combs", "Antimigraine",
                                                 "Triptan Combination"), "OtherAcute",
                                    ifelse(class=="Anti-CGRP gepant", "Gepant","OtherPrev" ))))) %>%
  select(patNum, class2) %>% distinct() %>% group_by(class2) %>% count() %>% mutate(n=n/1155) 


data.frame(current_classes %>% mutate(class2=ifelse(class=="Triptans", "Triptans",
                      ifelse(class=="Anti-CGRP mAb", "CGRPInj",
                             ifelse(class %in% c("Nsaid Inc Combs", "Non Opioid Analgesics inc combs",
                                                 "Opioid Analgesics inc combs", "Antimigraine",
                                                 "Triptan Combination"), "OtherAcute",
                                    ifelse(class=="Anti-CGRP gepant", "Gepant","OtherPrev" ))))) %>%
  select(patNum, class2) %>% filter(!is.na(class2)) %>% distinct() %>% mutate(exp=1) %>%
  spread(key=class2, value=exp) %>% 
  group_by(CGRPInj, OtherPrev, OtherAcute , Triptans) %>% count() %>% mutate(n=n/1155))



# Last 12 months

all_molecules_df <- fread("China_AllMolecules_OverTime_m60extended.txt", sep="\t")
all_molecules_df <- all_molecules_df %>% distinct()
all_molecules_df <- Rx_exp %>% left_join(all_molecules_df) %>% select(patNum, `49`:`60`) 

all_molecules_df <- gather(all_molecules_df, month, treat, `49`:`60`)

all_molecules_df <- separate_rows(all_molecules_df, treat, sep = ",", convert=T )

all_molecules_df <- all_molecules_df %>% filter(treat != "-") %>% distinct()

current_classes <- all_molecules_df %>% mutate(treat=as.numeric(treat)) %>%
  left_join(Drug_to_class_lookup, by=c("treat"="Generic")) %>%
  select(patNum, Class) %>% distinct() %>%
  left_join(Class_names %>% mutate(class_id=as.numeric(class_id)), by=c("Class"="class_id")) %>%
  select(patNum, class) %>% distinct()


current_classes %>% mutate(class2=ifelse(class=="Triptans", "Triptans",
                      ifelse(class=="Anti-CGRP mAb", "CGRPInj",
                             ifelse(class %in% c("Nsaid Inc Combs", "Non Opioid Analgesics inc combs",
                                                 "Opioid Analgesics inc combs", "Antimigraine",
                                                 "Triptan Combination"), "OtherAcute",
                                    ifelse(class=="Anti-CGRP gepant", "Gepant","OtherPrev" ))))) %>%
  select(patNum, class2) %>% filter(!is.na(class2)) %>% distinct() %>% mutate(exp=1) %>%
  spread(key=class2, value=exp) %>% 
  group_by(CGRPInj, OtherPrev,OtherAcute, Triptans) %>% count() %>% mutate(n=n/1155)







# Current CGRP, which concomitant drugs?

all_molecules_df <- fread("China_AllMolecules_OverTime_m60extended.txt", sep="\t")
all_molecules_df <- all_molecules_df %>% distinct()

CurrentCGRP <- all_molecules_df %>% select(patNum, `60`) 

CurrentCGRP <- separate_rows(CurrentCGRP, `60`, sep = ",", convert=T )

CurrentCGRP <- CurrentCGRP %>% filter(`60` != "-") %>% distinct()

CurrentCGRP <- CurrentCGRP %>% mutate(`60`=as.numeric(`60`)) %>%
  left_join(Drug_to_class_lookup, by=c("60"="Generic")) %>%
  select(patNum, Class) %>% distinct() %>%
  left_join(Class_names %>% mutate(class_id=as.numeric(class_id)), by=c("Class"="class_id")) %>%
  select(patNum, class) %>% distinct() %>% filter(class=="Anti-CGRP mAb"|class=="Anti-CGRP gepant") %>% select(patNum) %>% distinct()


all_molecules_df <- CurrentCGRP %>% left_join(all_molecules_df)  %>% select(patNum, `60`) 

all_molecules_df <- separate_rows(all_molecules_df, `60`, sep = ",", convert=T )

all_molecules_df <- all_molecules_df %>% filter(`60` != "-") %>% distinct()

all_molecules_df <- all_molecules_df %>% mutate(`60`=as.numeric(`60`)) %>%
  left_join(Drug_to_class_lookup, by=c("60"="Generic")) %>%
  select(patNum, Class) %>% distinct() %>%
  left_join(Class_names %>% mutate(class_id=as.numeric(class_id)), by=c("Class"="class_id")) %>%
  select(patNum, class) %>% distinct()

all_molecules_df %>% group_by(class) %>% count()  %>% mutate(n=n/12) %>% arrange(-n)


# using drug group 


all_molecules_df <- fread("China_AllMolecules_OverTime_m60extended.txt", sep="\t")
all_molecules_df <- all_molecules_df %>% distinct()

CurrentCGRP <- all_molecules_df %>% select(patNum, `60`) 

CurrentCGRP <- separate_rows(CurrentCGRP, `60`, sep = ",", convert=T )

CurrentCGRP <- CurrentCGRP %>% filter(`60` != "-") %>% distinct()

CurrentCGRP <- CurrentCGRP %>% mutate(`60`=as.numeric(`60`)) %>%
  left_join(Drug_to_class_lookup, by=c("60"="Generic")) %>%
  select(patNum, Class) %>% distinct() %>%
  left_join(Class_names %>% mutate(class_id=as.numeric(class_id)), by=c("Class"="class_id")) %>%
  select(patNum, class) %>% distinct() %>% filter(class=="Anti-CGRP mAb"|class=="Anti-CGRP gepant") %>% select(patNum) %>% distinct()


all_molecules_df <- CurrentCGRP %>% left_join(all_molecules_df)  %>% select(patNum, `60`) 

all_molecules_df <- separate_rows(all_molecules_df, `60`, sep = ",", convert=T )

all_molecules_df <- all_molecules_df %>% filter(`60` != "-") %>% distinct()

all_molecules_df <- all_molecules_df %>% mutate(`60`=as.numeric(`60`)) %>%
  left_join(Drug_to_class_lookup, by=c("60"="Generic")) %>%
  select(patNum, Class) %>% distinct() %>%
  left_join(Class_names %>% mutate(class_id=as.numeric(class_id)), by=c("Class"="class_id")) %>%
  select(patNum, class) %>% distinct()

length(unique(all_molecules_df$patNum))

all_molecules_df %>% 
    mutate(class2=ifelse(class=="Triptans", "Triptans",
                      ifelse(class=="Anti-CGRP mAb", "CGRPInj",
                             ifelse(class %in% c("Nsaid Inc Combs", "Non Opioid Analgesics inc combs",
                                                 "Opioid Analgesics inc combs", "Antimigraine",
                                                 "Triptan Combination"), "OtherAcute",
                                    ifelse(class=="Anti-CGRP gepant", "Gepant","OtherPrev" ))))) %>%
  select(patNum, class2) %>% distinct() %>% drop_na() %>% 
  mutate(exp=1) %>%
  spread(key=class2, value=exp) %>% 
  group_by(CGRPInj, OtherPrev, OtherAcute, Triptans) %>% count() %>% mutate(n=n/12)





# Current CGRP, how many drugs?

all_molecules_df <- fread("China_AllMolecules_OverTime_m60extended.txt", sep="\t")
all_molecules_df <- all_molecules_df %>% distinct()
CurrentCGRP <- all_molecules_df %>% select(patNum, `60`) 

CurrentCGRP <- separate_rows(CurrentCGRP, `60`, sep = ",", convert=T )

CurrentCGRP <- CurrentCGRP %>% filter(`60` != "-") %>% distinct()

CurrentCGRP <- CurrentCGRP %>% mutate(`60`=as.numeric(`60`)) %>%
  left_join(Drug_to_class_lookup, by=c("60"="Generic")) %>%
  select(patNum, Class) %>% distinct() %>%
  left_join(Class_names %>% mutate(class_id=as.numeric(class_id)), by=c("Class"="class_id")) %>%
  select(patNum, class) %>% distinct() %>% filter(class=="Anti-CGRP mAb"|class=="Anti-CGRP gepant") %>% select(patNum) %>% distinct()


all_molecules_df <- CurrentCGRP %>% left_join(all_molecules_df)  %>% select(patNum, `60`) 

all_molecules_df <- separate_rows(all_molecules_df, `60`, sep = ",", convert=T )

all_molecules_df <- all_molecules_df %>% filter(`60` != "-") %>% distinct()

CurrentCGRP %>% left_join(all_molecules_df %>% group_by(patNum) %>% count()) %>%
  ungroup() %>% summarise(n2=mean(n))





# What were the CGRP patients ON the month before?

# using extended m60 table

all_molecules_df <- fread("China_AllMolecules_OverTime_m60extended.txt", sep="\t")
all_molecules_df <- all_molecules_df %>% distinct()
all_molecules_df <- gather(all_molecules_df, month, treat, `0`:`60`)

FirstCGRP <- separate_rows(all_molecules_df, treat, sep = ",", convert=T )

length(unique(FirstCGRP$patNum))

FirstCGRP  %>%
  left_join(Drug_to_class_lookup %>% mutate(Generic=as.character(Generic)), by=c("treat"="Generic")) %>%
  select(patNum, month, Class) %>% distinct() %>%
  left_join(Class_names %>% mutate(class_id=as.numeric(class_id)), by=c("Class"="class_id")) %>%
  select(patNum, month, class) %>% distinct() %>% filter(class=="Anti-CGRP mAb"|class=="Anti-CGRP gepant") %>%
  group_by(patNum) %>% filter(month==min(month)) %>% select(patNum, month) %>% distinct() %>% rename("first"="month") %>%
  left_join(FirstCGRP) %>% ungroup() %>% filter(as.numeric(month) == as.numeric(first)-1) %>%
  select(patNum, treat) %>% 
  left_join(Drug_to_class_lookup %>% mutate(Generic=as.character(Generic)), by=c("treat"="Generic")) %>%
  select(patNum, Class) %>% distinct() %>%
  left_join(Class_names %>% mutate(class_id=as.numeric(class_id)), by=c("Class"="class_id")) %>%
  select(patNum, class) %>% distinct() %>%
  group_by(class) %>% count() %>% mutate(n=n/12) %>% arrange(-n)

  

# using original table

all_molecules_df <- fread("China_AllMolecules_OverTime.txt", sep="\t")
all_molecules_df <- all_molecules_df %>% distinct()
all_molecules_df <- gather(all_molecules_df, month, treat, `0`:`60`)

FirstCGRP <- separate_rows(all_molecules_df, treat, sep = ",", convert=T )

FirstCGRP  %>%
  left_join(Drug_to_class_lookup %>% mutate(Generic=as.character(Generic)), by=c("treat"="Generic")) %>%
  select(patNum, month, Class) %>% distinct() %>%
  left_join(Class_names %>% mutate(class_id=as.numeric(class_id)), by=c("Class"="class_id")) %>%
  select(patNum, month, class) %>% distinct() %>% filter(class=="Anti-CGRP mAb"|class=="Anti-CGRP gepant") %>%
  group_by(patNum) %>% filter(month==min(month)) %>% select(patNum, month) %>% distinct() %>% rename("first"="month") %>%
  left_join(FirstCGRP) %>% ungroup() %>% filter(as.numeric(month) == as.numeric(first)-1) %>%
  select(patNum, treat) %>% 
  left_join(Drug_to_class_lookup %>% mutate(Generic=as.character(Generic)), by=c("treat"="Generic")) %>%
  select(patNum, Class) %>% distinct() %>%
  left_join(Class_names %>% mutate(class_id=as.numeric(class_id)), by=c("Class"="class_id")) %>%
  select(patNum, class) %>% distinct() %>%
  group_by(class) %>% count() %>% mutate(n=n/6) %>% arrange(-n)


# using drug group classification


all_molecules_df <- fread("China_AllMolecules_OverTime.txt", sep="\t")
all_molecules_df <- all_molecules_df %>% distinct()
all_molecules_df <- gather(all_molecules_df, month, treat, `0`:`60`)

FirstCGRP <- separate_rows(all_molecules_df, treat, sep = ",", convert=T )

FirstCGRP  %>%
  left_join(Drug_to_class_lookup %>% mutate(Generic=as.character(Generic)), by=c("treat"="Generic")) %>%
  select(patNum, month, Class) %>% distinct() %>%
  left_join(Class_names %>% mutate(class_id=as.numeric(class_id)), by=c("Class"="class_id")) %>%
  select(patNum, month, class) %>% distinct() %>% filter(class=="Anti-CGRP mAb"|class=="Anti-CGRP gepant") %>%
  group_by(patNum) %>% filter(month==min(month)) %>% select(patNum, month) %>% distinct() %>% rename("first"="month") %>%
  left_join(FirstCGRP) %>% ungroup() %>% filter(as.numeric(month) == as.numeric(first)-1) %>%
  select(patNum, treat) %>% 
  left_join(Drug_to_class_lookup %>% mutate(Generic=as.character(Generic)), by=c("treat"="Generic")) %>%
  select(patNum, Class) %>% distinct() %>%
  left_join(Class_names %>% mutate(class_id=as.numeric(class_id)), by=c("Class"="class_id")) %>%
  select(patNum, class) %>% distinct() %>% 
    mutate(class2=ifelse(class=="Triptans", "Triptans",
                      ifelse(class=="Anti-CGRP mAb", "CGRPInj",
                             ifelse(class %in% c("Nsaid Inc Combs", "Non Opioid Analgesics inc combs",
                                                 "Opioid Analgesics inc combs", "Antimigraine",
                                                 "Triptan Combination"), "OtherAcute",
                                    ifelse(class=="Anti-CGRP gepant", "Gepant","OtherPrev" ))))) %>%
  select(patNum, class2) %>% distinct() %>% drop_na() %>% 
  mutate(exp=1) %>%
  spread(key=class2, value=exp) %>% 
  group_by(OtherAcute, Triptans) %>% count() %>% mutate(n=n/6)







# using original table but classes ever up to CGRP

all_molecules_df <- fread("China_AllMolecules_OverTime.txt", sep="\t")
all_molecules_df <- all_molecules_df %>% distinct()

all_molecules_df <- gather(all_molecules_df, month, treat, `0`:`60`)

FirstCGRP <- separate_rows(all_molecules_df, treat, sep = ",", convert=T )

FirstCGRP  %>%
  left_join(Drug_to_class_lookup %>% mutate(Generic=as.character(Generic)), by=c("treat"="Generic")) %>%
  select(patNum, month, Class) %>% distinct() %>%
  left_join(Class_names %>% mutate(class_id=as.numeric(class_id)), by=c("Class"="class_id")) %>%
  select(patNum, month, class) %>% distinct() %>% filter(class=="Anti-CGRP mAb"|class=="Anti-CGRP gepant") %>%
  group_by(patNum) %>% filter(month==min(month)) %>% select(patNum, month) %>% distinct() %>% rename("first"="month") %>%
  left_join(FirstCGRP) %>% ungroup() %>% filter(as.numeric(month) < as.numeric(first)) %>%
  select(patNum, treat) %>% 
  left_join(Drug_to_class_lookup %>% mutate(Generic=as.character(Generic)), by=c("treat"="Generic")) %>%
  select(patNum, Class) %>% distinct() %>%
  left_join(Class_names %>% mutate(class_id=as.numeric(class_id)), by=c("Class"="class_id")) %>%
  select(patNum, class) %>% distinct() %>%
  group_by(class) %>% count() %>% mutate(n=n/6) %>% arrange(-n)

# Time since diagnosis: Currently ON CGRP vs other 


all_molecules_df <- fread("China_AllMolecules_OverTime_m60extended.txt", sep="\t")
all_molecules_df <- all_molecules_df %>% distinct()

CurrentCGRP <- all_molecules_df %>% select(patNum, `60`) 

CurrentCGRP <- separate_rows(CurrentCGRP, `60`, sep = ",", convert=T )

CurrentCGRP <- CurrentCGRP %>% filter(`60` != "-") %>% distinct()

CurrentCGRP <- CurrentCGRP %>% mutate(`60`=as.numeric(`60`)) %>%
  left_join(Drug_to_class_lookup, by=c("60"="Generic")) %>%
  select(patNum, Class) %>% distinct() %>%
  left_join(Class_names %>% mutate(class_id=as.numeric(class_id)), by=c("Class"="class_id")) %>%
  select(patNum, class) %>% distinct() %>% filter(class=="Anti-CGRP mAb"|class=="Anti-CGRP gepant") %>% select(patNum) %>% distinct()


Pfizer_Migraine_Pat <- read_sav("Pfizer_Migraine_Pat v.2.0.sav")

China_df <- Pfizer_Migraine_Pat %>% filter(qcountries==7)

China_df %>% mutate(patNum=as.numeric(patNum)) %>% select(patNum, PRF_B_2c_DaysSince) %>% drop_na() %>%
  left_join(CurrentCGRP %>% mutate(CGRP=1)) %>% group_by(CGRP) %>% summarise(n=mean(PRF_B_2c_DaysSince/(12*30.5)))


China_df %>% mutate(patNum=as.numeric(patNum)) %>% select(patNum, PRF_B_2c_DaysSince) %>% drop_na() %>%
  left_join(CurrentCGRP %>% mutate(CGRP=1)) %>%
  ggplot(aes(PRF_B_2c_DaysSince/(30.5*12), colour=as.factor(CGRP), fill=as.factor(CGRP))) +
  geom_density(alpha=0.7) +
  xlim(0,20) +
  theme_minimal() + xlab("\n Number of Years since Migraine Diagnosis") + ylab("Patient density \n") +
  scale_fill_manual(values=c("midnightblue", "deepskyblue3")) +
  scale_colour_manual(values=c("midnightblue", "deepskyblue3"))

# ----------------------



# China Volume : Physician specialty vs patient type vs type of drugs ----------

# Physicians

Pfizer_Migraine_Pat <- read_sav("Pfizer_Migraine_Pat v.2.0.sav")

China_df <- Pfizer_Migraine_Pat %>% filter(qcountries==7)

China_df %>% select(patNum, SCR_1_RC) %>% group_by(SCR_1_RC) %>% count()

China_df %>% select(patNum, PhyPrimarySpeciality2) %>% group_by(PhyPrimarySpeciality2) %>% count()


all_molecules_df <- fread("China_AllMolecules_OverTime_m60extended.txt", sep="\t")
all_molecules_df <- all_molecules_df %>% distinct()
CurrentCGRP <- all_molecules_df %>% select(patNum, `60`) 
CurrentCGRP <- separate_rows(CurrentCGRP, `60`, sep = ",", convert=T )
CurrentCGRP <- CurrentCGRP %>% filter(`60` != "-") %>% distinct()
CurrentCGRP <- CurrentCGRP %>% mutate(`60`=as.numeric(`60`)) %>%
  left_join(Drug_to_class_lookup, by=c("60"="Generic")) %>%
  select(patNum, Class) %>% distinct() %>%
  left_join(Class_names %>% mutate(class_id=as.numeric(class_id)), by=c("Class"="class_id")) %>%
  select(patNum, class) %>% distinct() %>% filter(class=="Anti-CGRP mAb"|class=="Anti-CGRP gepant") %>% select(patNum) %>% distinct()

China_df %>% select(patNum, PhyPrimarySpeciality2) %>%  mutate(patNum=as.numeric(patNum)) %>%
  left_join(CurrentCGRP %>% mutate(group=1)) %>%
  group_by(PhyPrimarySpeciality2, group) %>% count() %>% mutate(n=n/2201)


Pats_Phys <- China_df %>% select(patNum, PhyPrimarySpeciality2) %>%  mutate(patNum=as.numeric(patNum))


all_molecules_df <- fread("China_AllMolecules_OverTime_m60extended.txt", sep="\t")
all_molecules_df <- all_molecules_df %>% distinct()
all_molecules_df <- all_molecules_df %>% select(patNum, `60`) 
all_molecules_df <- separate_rows(all_molecules_df, `60`, sep = ",", convert=T )
all_molecules_df <- all_molecules_df %>% filter(`60` != "-") %>% distinct()

current_classes <- all_molecules_df %>% mutate(`60`=as.numeric(`60`)) %>%
  left_join(Drug_to_class_lookup, by=c("60"="Generic")) %>%
  left_join(Class_names %>% mutate(class_id=as.numeric(class_id)), by=c("Class"="class_id"))

current_classes <- current_classes %>%   mutate(class2=ifelse(class=="Triptans", "Triptans",
                      ifelse(class=="Anti-CGRP mAb", "CGRPInj",
                             ifelse(class %in% c("Nsaid Inc Combs", "Non Opioid Analgesics inc combs",
                                                 "Opioid Analgesics inc combs", "Antimigraine",
                                                 "Triptan Combination"), "OtherAcute",
                                    ifelse(class=="Anti-CGRP gepant", "Gepant","OtherPrev" ))))) 



groups <- current_classes %>% select(patNum, class2) %>% distinct() %>%
  mutate(class3=ifelse(class2=="CGRPInj", "Prev",
                       ifelse(class2=="OtherAcute", "Acute",
                              ifelse(class2=="Triptans", "Acute",
                                    ifelse(class2=="Gepant", "Acute",   "Prev"))))) %>%
  select(patNum, class3) %>% distinct() %>% arrange(patNum, class3) %>%  drop_na() %>%
  group_by(patNum) %>% mutate(group=paste(class3, collapse = ",")) %>% ungroup() %>% select(patNum, group) %>% distinct() 

groups <- Pats_Phys %>% left_join(groups) %>% drop_na()

volume <- current_classes %>%
  mutate(class3=ifelse(class2=="CGRPInj", "CGRPInj",
                       ifelse(class2=="OtherAcute", "OtherAcute",
                              ifelse(class2=="Triptans", "Triptans",
                                    ifelse(class2=="Gepant", "Gepant",   "OtherPrev"))))) %>%
  group_by(patNum, class3) %>% count() %>% spread(key=class3, value=n)

volume[is.na(volume)] <- 0

groups <- groups %>% left_join(volume)

groups <- groups %>% select(-`<NA>`)

fwrite(groups, "China_Volume_Summary_patientLevel_Gepants.csv")

groups %>% group_by(group, PhyPrimarySpeciality2) %>% summarise(total=sum(OtherPrev ))


# ------

# China compare patient and physician responses --------------------


# PHYSICIANS

Pfizer_Migraine_Pat <- read_sav("Pfizer_Migraine_Pat v.2.0.sav")
China_df <- Pfizer_Migraine_Pat %>% filter(qcountries==7)

length(unique(China_df$docNum))

China_df <- China_df %>% select(patNum, PhyPrimarySpeciality2, PRF_D_3a_101:PRF_D_3a_123, PRF_E_3_201:PRF_E_3_220)

China_df <- gather(China_df, Drug, Molecule, PRF_D_3a_101:PRF_E_3_220)
China_df <- China_df %>% drop_na() %>% filter(Molecule==1)

China_df <- China_df %>% mutate( molecule_number = sub(".*_(\\d+)$", "\\1", Drug))

China_df <- China_df %>% select(patNum, PhyPrimarySpeciality2, Molecule, molecule_number) %>% distinct()

unique(China_df$molecule_number)

length(unique(China_df$patNum))

China_df %>% mutate(molecule_number=as.numeric(molecule_number)) %>%
  mutate(class=ifelse(molecule_number %in% c(101,102,103,104,105,106,107,108), "Triptan", 
                              ifelse(molecule_number %in% c(109,110,111,115,116,123,220), "NSAID",
                                     ifelse(molecule_number %in% c(112,121,122), "Other_Acute",
                                            ifelse(molecule_number %in% c(212, 215, 219), "Other_Prev",
                                            ifelse(molecule_number %in% c(113,114,204,205), "Gepant",
                                                   ifelse(molecule_number %in% c(117,118,119,120), "Opioid",
                                                          ifelse(molecule_number %in% c(201 ,202,203,206), "Mab",
                                                                 ifelse(molecule_number %in% c(207,213,216), "Anticonvulsant",
                                                                        ifelse(molecule_number %in% c(208,209,210), "BetaBlocker",
                                                                               ifelse(molecule_number %in% c(211,217,218), "Antidepressant",
                                                                                      ifelse(molecule_number %in% c(214), "Botox", NA )))))))))))) %>%
  select(patNum, class) %>% distinct() %>% group_by(class) %>% count() %>% mutate(n=n/1018)



China_df %>% select(patNum, PhyPrimarySpeciality2) %>% distinct() %>% group_by(PhyPrimarySpeciality2) %>% count()


China_df %>% mutate(molecule_number=as.numeric(molecule_number)) %>%
  mutate(class=ifelse(molecule_number %in% c(101,102,103,104,105,106,107,108), "Triptan", 
                              ifelse(molecule_number %in% c(109,110,111,115,116,123,220), "NSAID",
                                     ifelse(molecule_number %in% c(112,121,122), "Other_Acute",
                                            ifelse(molecule_number %in% c(212, 215, 219), "Other_Prev",
                                            ifelse(molecule_number %in% c(113,114,204,205), "Gepant",
                                                   ifelse(molecule_number %in% c(117,118,119,120), "Opioid",
                                                          ifelse(molecule_number %in% c(201 ,202,203,206), "Mab",
                                                                 ifelse(molecule_number %in% c(207,213,216), "Anticonvulsant",
                                                                        ifelse(molecule_number %in% c(208,209,210), "BetaBlocker",
                                                                               ifelse(molecule_number %in% c(211,217,218), "Antidepressant",
                                                                                      ifelse(molecule_number %in% c(214), "Botox", NA )))))))))))) %>%
  select(patNum, PhyPrimarySpeciality2, class) %>% distinct() %>% group_by(PhyPrimarySpeciality2, class) %>% count() %>%
  mutate(n=ifelse(PhyPrimarySpeciality2==1, n/288, ifelse(PhyPrimarySpeciality2==3, n/522, n/208))) %>%
  spread(key=PhyPrimarySpeciality2, value=n)






# PATIENTS 

Drug_to_class_lookup <- fread("China_Drug_to_class_lookup.txt", sep="\t")
Class_names <- fread("Class_names.txt", sep="\t")
Molecule_names <- fread("Molecule_names.txt", sep="\t")

length(unique(all_molecules_df$patNum))

all_molecules_df <- fread("China_AllMolecules_OverTime_m60extended.txt", sep="\t")
all_molecules_df <- all_molecules_df %>% distinct()

Rx_exp <- all_molecules_df %>% gather(month, treat, `0`:`60`) %>% filter(treat!="-") %>%
  select(patNum) %>% distinct()

length(unique(Rx_exp$patNum)) # 1155

all_molecules_df <- Rx_exp %>% left_join(all_molecules_df) %>% select(patNum, `60`) 

all_molecules_df <- separate_rows(all_molecules_df, `60`, sep = ",", convert=T )

all_molecules_df <- all_molecules_df %>% filter(`60` != "-") %>% distinct()

current_classes <- all_molecules_df %>% mutate(`60`=as.numeric(`60`)) %>%
  left_join(Drug_to_class_lookup, by=c("60"="Generic")) %>%
  select(patNum, Class) %>% distinct() %>%
  left_join(Class_names %>% mutate(class_id=as.numeric(class_id)), by=c("Class"="class_id")) %>%
  select(patNum, class) %>% distinct()

unique(current_classes$class)

current_classes %>% group_by(class) %>% count() %>% arrange(-n) %>% mutate(n=n/1155) 

current_classes %>% mutate(class2=ifelse(class=="Triptans", "Triptans",
                      ifelse(class=="Anti-CGRP mAb", "CGRPInj",
                             ifelse(class %in% c("Nsaid Inc Combs", "Non Opioid Analgesics inc combs",
                                                 "Opioid Analgesics inc combs", "Antimigraine",
                                                 "Triptan Combination"), "OtherAcute",
                                    ifelse(class=="Anti-CGRP gepant", "Gepant","OtherPrev" ))))) %>%
  select(patNum, class2) %>% distinct() %>% group_by(class2) %>% count() %>% mutate(n=n/1155) 



# OTHER 

Pfizer_Migraine_Pat <- read_sav("Pfizer_Migraine_Pat v.2.0.sav")

China_df <- Pfizer_Migraine_Pat %>% filter(qcountries==7)
China_df %>% select(patNum, Acute_vs_Prophy) %>% group_by(Acute_vs_Prophy) %>% count() %>% mutate(n=n/1201)
China_df %>% select(patNum, PhyPrimarySpeciality2, Acute_vs_Prophy) %>% group_by(PhyPrimarySpeciality2, Acute_vs_Prophy) %>% count() %>%
  mutate(n=ifelse(PhyPrimarySpeciality2==1, n/400, ifelse(PhyPrimarySpeciality2==3,n/581, n/220)))


# -----------
# Physician breakdown Dx and Rx per country ---------



data.frame(Pfizer_Migraine_Pat %>% select(patNum, PhyPrimarySpeciality2, qcountries,  PRF_B_3a_YRC) %>%
  group_by(qcountries, PhyPrimarySpeciality2) %>% count())

Pfizer_Migraine_Pat %>% select(qcountries) %>% distinct()
Pfizer_Migraine_Pat %>% select(PhyPrimarySpeciality2) %>% distinct()

Pfizer_Migraine_Pat %>% select(PRF_B_3a_YRC) %>% distinct()
Pfizer_Migraine_Pat %>% select(PRF_B_3b_YRC) %>% distinct()
Pfizer_Migraine_Pat %>% select(PRF_B_3c_YRC) %>% distinct()
Pfizer_Migraine_Pat %>% select(PRF_B_3d_YRC) %>% distinct()

data.frame(
  Pfizer_Migraine_Pat %>% select(patNum, qcountries,  PRF_B_3a_YRC) %>%
  group_by(qcountries, PRF_B_3a_YRC) %>% count() %>% rename("PRF_B_3a_YRC_n"="n") %>% rename("PRF_B"="PRF_B_3a_YRC") %>%
  full_join(
     Pfizer_Migraine_Pat %>% select(patNum, qcountries,  PRF_B_3b_YRC) %>%
  group_by(qcountries, PRF_B_3b_YRC) %>% count() %>% rename("PRF_B_3b_YRC_n"="n")  %>% rename("PRF_B"="PRF_B_3b_YRC")
  ) %>%
    full_join(
     Pfizer_Migraine_Pat %>% select(patNum, qcountries,  PRF_B_3c_YRC) %>%
  group_by(qcountries, PRF_B_3c_YRC) %>% count() %>% rename("PRF_B_3c_YRC_n"="n")  %>% rename("PRF_B"="PRF_B_3c_YRC")
  ) %>%
    full_join(
     Pfizer_Migraine_Pat %>% select(patNum, qcountries,  PRF_B_3d_YRC) %>%
  group_by(qcountries, PRF_B_3d_YRC) %>% count() %>% rename("PRF_B_3d_YRC_n"="n")  %>% rename("PRF_B"="PRF_B_3d_YRC")
  )
) %>% arrange(qcountries, PRF_B)

# --------

# Physician breakdown Dx and Rx per country % SEEN ANY PCP ---------

# PRF_B_3a_YRC. First consultation (Healthcare professionals (HCPs) responsible for this patient's migraine)
# PRF_B_3b_YRC. Diagnosis of migraine (Healthcare professionals (HCPs) responsible for this patient's migraine)
# PRF_B_3c_YRC. Initiated first acute treatment (Healthcare professionals (HCPs) responsible for this patient's migraine)
# PRF_B_3d_YRC. Initiated first preventive treatment (Healthcare professionals (HCPs) responsible for this patient's migraine)

Pfizer_Migraine_Pat <- read_sav("Pfizer_Migraine_Pat v.2.0.sav")
Pfizer_Migraine_Pat <- Pfizer_Migraine_Pat %>% filter(qcountries==28)

data.frame(Pfizer_Migraine_Pat %>% select(patNum, PhyPrimarySpeciality2,  PRF_B_3a_YRC, PRF_B_3b_YRC, PRF_B_3c_YRC, PRF_B_3d_YRC) %>%
  group_by(qcountries, PhyPrimarySpeciality2) %>% count())

Pfizer_Migraine_Pat %>% select(PhyPrimarySpeciality2) %>% distinct()

Pfizer_Migraine_Pat %>% select(PRF_B_3a_YRC) %>% distinct()
Pfizer_Migraine_Pat %>% select(PRF_B_3b_YRC) %>% distinct()
Pfizer_Migraine_Pat %>% select(PRF_B_3c_YRC) %>% distinct()
Pfizer_Migraine_Pat %>% select(PRF_B_3d_YRC) %>% distinct()

Pfizer_Migraine_Pat %>% select(patNum, qcountries,  PRF_B_3a_YRC,  PRF_B_3b_YRC, PRF_B_3c_YRC, PRF_B_3d_YRC) %>%
  filter(PRF_B_3a_YRC != 99 & PRF_B_3a_YRC != 98) %>%  # count()  # 1373
  mutate(PRF_B_3b_YRC=ifelse(PRF_B_3b_YRC==2,2,0)) %>%
  mutate(PRF_B_3c_YRC=ifelse(PRF_B_3c_YRC==2,2,0)) %>%
  mutate(PRF_B_3d_YRC=ifelse(PRF_B_3d_YRC==2,2,0)) %>%
  filter(PRF_B_3a_YRC==2|PRF_B_3b_YRC==2|PRF_B_3c_YRC==2|PRF_B_3d_YRC==2) %>% count() # 954


Pfizer_Migraine_Pat %>% select(patNum, qcountries,  PRF_B_3a_YRC,  PRF_B_3b_YRC, PRF_B_3c_YRC, PRF_B_3d_YRC) %>%
  filter(PRF_B_3b_YRC != 99 & PRF_B_3b_YRC != 98) %>%  # count()  # 1378
  mutate(PRF_B_3a_YRC=ifelse(PRF_B_3a_YRC==2,2,0)) %>%
  mutate(PRF_B_3c_YRC=ifelse(PRF_B_3c_YRC==2,2,0)) %>%
  mutate(PRF_B_3d_YRC=ifelse(PRF_B_3d_YRC==2,2,0)) %>%
  filter(PRF_B_3a_YRC==2|PRF_B_3b_YRC==2|PRF_B_3c_YRC==2|PRF_B_3d_YRC==2) %>% count() # 956


Pfizer_Migraine_Pat %>% select(patNum, qcountries,  PRF_B_3a_YRC,  PRF_B_3b_YRC, PRF_B_3c_YRC, PRF_B_3d_YRC) %>%
  filter(PRF_B_3c_YRC != 99 & PRF_B_3c_YRC != 98) %>%  # count()  # 1335
  mutate(PRF_B_3a_YRC=ifelse(PRF_B_3a_YRC==2,2,0)) %>%
  mutate(PRF_B_3b_YRC=ifelse(PRF_B_3b_YRC==2,2,0)) %>%
  mutate(PRF_B_3d_YRC=ifelse(PRF_B_3d_YRC==2,2,0)) %>%
  filter(PRF_B_3a_YRC==2|PRF_B_3b_YRC==2|PRF_B_3c_YRC==2|PRF_B_3d_YRC==2) %>% count() # 919


Pfizer_Migraine_Pat %>% select(patNum, qcountries,  PRF_B_3a_YRC,  PRF_B_3b_YRC, PRF_B_3c_YRC, PRF_B_3d_YRC) %>%
  filter(PRF_B_3d_YRC != 99 & PRF_B_3d_YRC != 98) %>%  # count()  # 1122
  mutate(PRF_B_3a_YRC=ifelse(PRF_B_3a_YRC==2,2,0)) %>%
  mutate(PRF_B_3b_YRC=ifelse(PRF_B_3b_YRC==2,2,0)) %>%
  mutate(PRF_B_3c_YRC=ifelse(PRF_B_3c_YRC==2,2,0)) %>%
  filter(PRF_B_3a_YRC==2|PRF_B_3b_YRC==2|PRF_B_3c_YRC==2|PRF_B_3d_YRC==2) %>% count() # 766

# --------

# First Diagnosed vs First Rx ------------

Pfizer_Migraine_Pat <- read_sav("Pfizer_Migraine_Pat v.2.0.sav")
Pfizer_Migraine_Pat %>% select(qcountries) %>% distinct()
Pfizer_Migraine_Pat <- Pfizer_Migraine_Pat %>% filter(qcountries==24)

Pfizer_Migraine_Pat <- Pfizer_Migraine_Pat %>% select(patNum,  PRF_B_3b_YRC, PRF_B_3c_YRC, PRF_B_3d_YRC)

Pfizer_Migraine_Pat %>% select(PRF_B_3b_YRC) %>% distinct()
Pfizer_Migraine_Pat %>% select(PRF_B_3c_YRC) %>% distinct()

data.frame(Pfizer_Migraine_Pat %>%
             mutate(PRF_B_3b_YRC=ifelse(PRF_B_3b_YRC==2,2,
                                        ifelse(PRF_B_3b_YRC==3,3,
                                               ifelse(PRF_B_3b_YRC==4,4,9501)))) %>%
             mutate(PRF_B_3c_YRC=ifelse(PRF_B_3c_YRC==2,2,
                                        ifelse(PRF_B_3c_YRC==3,3,
                                               ifelse(PRF_B_3c_YRC==4,4,9501)))) %>%
             group_by(PRF_B_3b_YRC, PRF_B_3c_YRC) %>% count())


data.frame(Pfizer_Migraine_Pat %>%
             mutate(PRF_B_3d_YRC=ifelse(PRF_B_3d_YRC==2,2,
                                        ifelse(PRF_B_3d_YRC==3,3,
                                               ifelse(PRF_B_3d_YRC==4,4,9501)))) %>%
             mutate(PRF_B_3c_YRC=ifelse(PRF_B_3c_YRC==2,2,
                                        ifelse(PRF_B_3c_YRC==3,3,
                                               ifelse(PRF_B_3c_YRC==4,4,9501)))) %>%
             group_by(PRF_B_3c_YRC, PRF_B_3d_YRC) %>% count())


# -----------
# How many have acute vs preventive migraine, are on acute ve preventive classes? ---------


Pfizer_Migraine_Pat <- read_sav("Pfizer_Migraine_Pat v.2.0.sav")
Pfizer_Migraine_Pat <- Pfizer_Migraine_Pat %>% filter(qcountries==28)

Pfizer_Migraine_Pat %>% select(patNum, PRF_C_4a_1) %>%
  ggplot(aes(PRF_C_4a_1)) + 
  geom_density()

Pfizer_Migraine_Pat <- Pfizer_Migraine_Pat %>%  mutate(PRF_D_3b=ifelse(is.na(PRF_D_3b),0,
                                                                       ifelse(PRF_D_3b==2,0,PRF_D_3b)))

Pfizer_Migraine_Pat %>% group_by(PRF_D_3b) %>% count()
   
Pfizer_Migraine_Pat <- Pfizer_Migraine_Pat %>% select(patNum, PRF_C_4a_1, PRF_D_3b) %>%
  mutate(PRF_C_4a_1=ifelse(PRF_C_4a_1<4, "Acute" , 
                           ifelse(PRF_C_4a_1<15, "Prev", "Chronic")))

Pfizer_Migraine_Pat %>% group_by(PRF_C_4a_1) %>% count()


Drug_to_class_lookup <- fread("US_Drug_to_class_lookup.txt", sep="\t")
Class_names <- fread("Class_names.txt", sep="\t")
Molecule_names <- fread("Molecule_names.txt", sep="\t")


all_molecules_df <- fread("US_AllMolecules_OverTime_m60extended.txt", sep="\t")
all_molecules_df <- all_molecules_df %>% select(patNum, `60`) 
all_molecules_df <- separate_rows(all_molecules_df, `60`, sep = ",", convert=T )
all_molecules_df <- all_molecules_df %>% filter(`60` != "-") %>% distinct()

length(unique(all_molecules_df$patNum))

all_molecules_df %>% select(patNum) %>% distinct() %>%
  inner_join(Pfizer_Migraine_Pat %>% mutate(patNum=as.numeric(patNum))) %>% 
  group_by(PRF_C_4a_1) %>% count()


all_molecules_df <- all_molecules_df %>% mutate(`60`=as.numeric(`60`)) %>%
  left_join(Drug_to_class_lookup, by=c("60"="Generic"))

all_molecules_df <- all_molecules_df %>% 
  left_join(Class_names %>% mutate(class_id=as.numeric(class_id)), by=c("Class"="class_id")) 

unique(all_molecules_df$class)


all_molecules_df <- all_molecules_df %>% 
  inner_join(Pfizer_Migraine_Pat %>% mutate(patNum=as.numeric(patNum))) %>%
  mutate(class2=ifelse(class=="Triptans", "Triptans",
                      ifelse(class=="Anti-CGRP mAb", "CGRPInj",
                             ifelse(class %in% c("Nsaid Inc Combs", "Non Opioid Analgesics inc combs",
                                                 "Opioid Analgesics inc combs", "Antimigraine",
                                                 "Triptan Combination"), "OtherAcute",
                                    ifelse(class=="Anti-CGRP gepant" & PRF_D_3b==1 , "Gepant_Prev",
                                           ifelse(class=="Anti-CGRP gepant" & PRF_D_3b==0 , "Gepant_Acute","OtherPrev" )))))) %>%
   filter(!is.na(class2)) %>% distinct() 


Groups <- all_molecules_df %>% mutate(exp=1) %>%
  arrange(patNum) %>% select(patNum, class2, exp) %>% distinct() %>%
  spread(key=class2, value=exp)

Groups[is.na(Groups)] <- 0

Groups <- Groups %>% mutate(Prev=ifelse(CGRPInj==1|OtherPrev==1|Gepant_Prev==1, 1,0)) %>%
  mutate(Acute=ifelse(Gepant_Acute ==1|Triptans==1|OtherAcute==1, 1, 0))

Groups %>% group_by(Prev, Acute) %>% count()

Pfizer_Migraine_Pat %>% mutate(patNum=as.numeric(patNum)) %>%
  inner_join(Groups) %>% 
  filter(Gepant_Acute==1|Gepant_Prev==1) %>%
  group_by(PRF_C_4a_1, Prev, Acute) %>% count()


Pfizer_Migraine_Pat %>% mutate(patNum=as.numeric(patNum)) %>%
  inner_join(Groups) %>% 
   filter(Prev==1&Acute==1) %>%
  filter(Gepant_Acute==1|Gepant_Prev==1) %>%
  group_by(PRF_C_4a_1, Gepant_Acute, Gepant_Prev) %>% count()

Pfizer_Migraine_Pat %>% mutate(patNum=as.numeric(patNum)) %>%
  inner_join(Groups) %>% 
  filter(Triptans==1) %>%
  group_by(PRF_C_4a_1, Prev, Acute) %>% count()


Pfizer_Migraine_Pat %>% mutate(patNum=as.numeric(patNum)) %>%
  inner_join(Groups) %>% 
  filter(CGRPInj==1) %>%
  group_by(PRF_C_4a_1, Prev, Acute) %>% count()


Pfizer_Migraine_Pat <- read_sav("Pfizer_Migraine_Pat v.2.0.sav")
Pfizer_Migraine_Pat <- Pfizer_Migraine_Pat %>% filter(qcountries==28)


# ----------

# How many MIG days per month per country ? ---------

Pfizer_Migraine_Pat <- read_sav("Pfizer_Migraine_Pat v.2.0.sav")

Pfizer_Migraine_Pat %>% select(patNum, qcountries ,PRF_C_4a_1) %>%
  mutate(PRF_C_4a_1=ifelse(PRF_C_4a_1<=3, 3,
                           ifelse(PRF_C_4a_1<=14,14,15))) %>%
  group_by(qcountries,PRF_C_4a_1) %>%
  count() %>% spread(key=PRF_C_4a_1, value=n)

# -----------
