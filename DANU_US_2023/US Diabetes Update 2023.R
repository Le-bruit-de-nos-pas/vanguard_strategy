library(tidyverse)
library(data.table)
library(hacksaw)
library(splitstackshape)
library(spatstat)
library(lubridate)
options(scipen = 999)


# Treatment-experienced ----------------

DIA_Drug_Histories <- fread("DIA Analysis Results 1.1/DIA Drug Histories.txt", integer64 = "character", stringsAsFactors = F)

sum(DIA_Drug_Histories$weight) # 46029709

DIA_Drug_Histories <- DIA_Drug_Histories %>% select(4:63)
DIA_Drug_Histories[DIA_Drug_Histories != "-"] <- 1  # on drug 
DIA_Drug_Histories[DIA_Drug_Histories == "-"] <- 0  # no drug
DIA_Drug_Histories[] <- lapply(DIA_Drug_Histories,as.numeric)
DIA_Drug_Histories$SUM <- rowSums(DIA_Drug_Histories)

DIA_Drug_Histories_LONG <- fread("DIA Analysis Results 1.1/DIA Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
Pats_vec <- DIA_Drug_Histories_LONG %>% select(patient, weight)
DIA_Drug_Histories <- Pats_vec %>% bind_cols(DIA_Drug_Histories)
DIA_Drug_Histories <- DIA_Drug_Histories %>% filter(SUM != 0)

sum(DIA_Drug_Histories$weight) # 30120087

Treatment_exp_Vector <- DIA_Drug_Histories %>% select(patient, weight)
fwrite(Treatment_exp_Vector, "DIA Analysis Results 1.1/Treatment_exp_Vector.txt")

# -----------------------------------
# Stocks month-over-month --------------

Treatment_exp_Vector <- fread("DIA Analysis Results 1.1/Treatment_exp_Vector.txt")
DIA_Drug_Histories     <- fread("DIA Analysis Results 1.1/DIA Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
DIA_Drug_Histories <- Treatment_exp_Vector %>% left_join(DIA_Drug_Histories)

length(unique(DIA_Drug_Histories$patient)) # 236983
sum(as.numeric(DIA_Drug_Histories$weight)) # 30120087

DANU_Ingredients <- fread("DIA Analysis Results 1.1/DANU Ingredients.txt", integer64 = "character", stringsAsFactors = F)
DANU_Ingredients <- DANU_Ingredients %>%  separate(drug_id, c('class', 'molecule'))
DANU_Ingredients  <- DANU_Ingredients %>% select(molecule, drug_group)
names(DANU_Ingredients)[1] <- "Drugs"

DIA_Drug_Histories <- gather(DIA_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
DIA_Drug_Histories <- separate_rows(DIA_Drug_Histories, Drugs, sep = ",", convert=T)

DIA_Drug_Histories <- DIA_Drug_Histories %>% select(patient, weight, Month, Drugs) %>% 
  distinct() %>% filter(Drugs!="-") %>% left_join(DANU_Ingredients) 

DIA_Drug_Histories <- DIA_Drug_Histories %>% select(patient, weight, Month, drug_group) %>% distinct() 

data.frame(DIA_Drug_Histories %>% mutate(Month=parse_number(as.character(Month))) %>%
  group_by(Month, drug_group) %>% summarise(n=sum(as.numeric(weight))) %>%
  ungroup() %>%
  spread(key=Month, value=n))


data.frame(DIA_Drug_Histories %>% select(patient, weight, Month) %>% distinct() %>%
             mutate(Month=parse_number(as.character(Month))) %>%
  group_by(Month) %>% summarise(n=sum(as.numeric(weight))) %>%
  ungroup() %>%
  spread(key=Month, value=n)) %>%
  transpose()

# ---------------------

# Stocks vs Lines of Therapy vs Combos vs Experience -------------------------

Treatment_exp_Vector <- fread("DIA Analysis Results 1.1/Treatment_exp_Vector.txt")
DIA_Box_Histories     <- fread("DIA Analysis Results 1.1/DIA Box Histories.txt", integer64 = "character", stringsAsFactors = F)
DIA_Box_Histories <- Treatment_exp_Vector %>% left_join(DIA_Box_Histories)
DIA_Box_Histories <- DIA_Box_Histories %>% select(patient, weight, month60)
DIA_Box_Histories <- DIA_Box_Histories %>% mutate(month60 = str_sub(month60, 2L, 2L))

DIA_Box_Histories %>% group_by(month60) %>% summarise(n=sum(weight))

DIA_Drug_Histories <- fread("DIA Analysis Results 1.1/DIA Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
DIA_Drug_Histories <- DIA_Drug_Histories %>% select(patient, weight, month60)
DIA_Drug_Histories <- DIA_Drug_Histories %>% mutate(Combo=ifelse(grepl(",", month60),1,0)) %>% select(patient, Combo)

DIA_Box_Histories %>% left_join(DIA_Drug_Histories) %>% group_by(month60, Combo) %>% summarise(n=sum(weight)) %>%
  ungroup() %>% spread(key=Combo, value=n) %>% mutate(`1`=ifelse(is.na(`1`),0,`1`)) %>%
  mutate(Percent=`1`/(`1`+`0`))

DIA_Box_Histories %>% left_join(DIA_Drug_Histories) %>% group_by(Combo) %>% summarise(n=sum(weight)) %>%
  ungroup() %>% spread(key=Combo, value=n) %>% 
  mutate(Percent=`1`/(`1`+`0`))

DIA_Drug_Histories <- fread("DIA Analysis Results 1.1/DIA Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
DIA_Drug_Histories <- DIA_Drug_Histories %>% select(patient, weight, month60)
DIA_Drug_Histories <- DIA_Drug_Histories %>% filter(month60!="-")
DIA_Drug_Histories <- separate_rows(DIA_Drug_Histories, month60, sep = ",", convert=T)
DIA_Drug_Histories <- DIA_Drug_Histories %>% group_by(patient) %>% count()

DIA_Box_Histories %>% left_join(DIA_Drug_Histories) %>% mutate(n=ifelse(is.na(n), 0, n)) %>%
  group_by(month60) %>% summarise(n2=weighted.mean(n, weight))

DIA_Box_Histories %>% left_join(DIA_Drug_Histories) %>% mutate(n=ifelse(is.na(n), 0, n)) %>%
  summarise(n2=weighted.mean(n, weight))

DIA_Drug_Histories <- fread("DIA Analysis Results 1.1/DIA Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
DIA_Drug_Histories <- gather(DIA_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
DIA_Drug_Histories <- DIA_Drug_Histories %>% filter(Drugs!="-")
DIA_Drug_Histories <- separate_rows(DIA_Drug_Histories, Drugs, sep = ",", convert=T)

DIA_Drug_Histories <- DIA_Drug_Histories %>% distinct() %>% select(-disease) %>%
  arrange(patient, weight, Month, Drugs) %>%
  group_by(patient, weight, Month) %>% mutate(treat_new = paste(Drugs, collapse=",")) 

DIA_Drug_Histories <- DIA_Drug_Histories %>% ungroup() %>% select(patient, weight, Month, treat_new) %>% distinct()

DIA_Drug_Histories <- DIA_Drug_Histories %>% spread(key=Month, value=treat_new)

DIA_Drug_Histories[is.na(DIA_Drug_Histories)] <- "-"

sum(as.numeric(DIA_Drug_Histories$weight))

DIA_Drug_Histories <- gather(DIA_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)

DIA_Drug_Histories <- DIA_Drug_Histories %>% select(patient, Drugs) %>% distinct() %>% filter(Drugs!="-") %>%  group_by(patient) %>% count()

DIA_Box_Histories %>% left_join(DIA_Drug_Histories) %>% mutate(n=ifelse(is.na(n), 0, n)) %>%
  group_by(month60) %>% summarise(n2=weighted.mean(n, weight))

DIA_Box_Histories %>% left_join(DIA_Drug_Histories) %>% mutate(n=ifelse(is.na(n), 0, n)) %>%
  summarise(n2=weighted.mean(n, weight))

names(DIA_Box_Histories)[3] <- "Box"

DIA_Drug_Histories <- fread("DIA Analysis Results 1.1/DIA Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
DIA_Drug_Histories <- DIA_Drug_Histories %>% select(-c(disease)) %>% select(1,2,62)
DIA_Box_Histories <- DIA_Box_Histories %>% left_join(DIA_Drug_Histories)

DIA_Box_Histories %>% group_by(Box) %>% summarise(n=sum(as.numeric(weight)))

DIA_Box_Histories <- DIA_Box_Histories %>% filter(month60!="-")

DIA_Box_Histories <- separate_rows(DIA_Box_Histories, month60, sep = ",", convert=T)
names(DIA_Box_Histories)[4] <- "molecule"

DANU_Ingredients <- fread("DIA Analysis Results 1.1/DANU Ingredients.txt", integer64 = "character", stringsAsFactors = F)
DANU_Ingredients <- DANU_Ingredients %>%  separate(drug_id, c('class', 'molecule'))
DANU_Ingredients <- DANU_Ingredients %>% select(molecule, drug_group)
DANU_Ingredients$molecule <- as.numeric(DANU_Ingredients$molecule)

data.frame(DIA_Box_Histories %>% left_join(DANU_Ingredients) %>% 
             select(patient, weight, Box, drug_group) %>% distinct() %>%
             group_by(Box, drug_group) %>% summarise(n=sum(as.numeric(weight)))) %>%
  spread(key=Box, value=n)


data.frame(DIA_Box_Histories %>% left_join(DANU_Ingredients) %>% 
             select(patient, weight, Box, drug_group) %>% distinct() %>%
             group_by(drug_group) %>% summarise(n=sum(as.numeric(weight))/17769429))
  

# -----------------------------------
# Newly diagnosed year-over-year ----------------------

DANU_Demographics <- DANU_Demographics %>% filter(grepl("Diabetes", diagnosis))
range(DANU_Demographics$diabetes_onset)

DANU_Demographics %>% filter(diabetes_onset>="2021-05-01" & diabetes_onset<="2022-04-30") %>% summarise(n=sum(weight)) # 3805386
DANU_Demographics %>% filter(diabetes_onset>="2020-05-01" & diabetes_onset<="2021-04-30") %>% summarise(n=sum(weight)) # 3820870
DANU_Demographics %>% filter(diabetes_onset>="2019-05-01" & diabetes_onset<="2020-04-30") %>% summarise(n=sum(weight)) # 3853292
DANU_Demographics %>% filter(diabetes_onset>="2018-05-01" & diabetes_onset<="2019-04-30") %>% summarise(n=sum(weight)) # 4462034
DANU_Demographics %>% filter(diabetes_onset>="2017-05-01" & diabetes_onset<="2018-04-30") %>% summarise(n=sum(weight)) # 6132548
DANU_Demographics %>% filter(diabetes_onset>="2016-05-01" & diabetes_onset<="2017-04-30") %>% summarise(n=sum(weight)) # 23955578

# -----------------------------------------

# Drug Experience ~ stock month60 --------

DANU_Ingredients <- fread("DIA Analysis Results 1.1/DANU Ingredients.txt", integer64 = "character", stringsAsFactors = F)
DANU_Ingredients <- DANU_Ingredients %>%  separate(drug_id, c('class', 'molecule'))
DANU_Ingredients <- DANU_Ingredients %>% select(molecule, drug_group)
DANU_Ingredients$molecule <- as.numeric(DANU_Ingredients$molecule)

DIA_Drug_Histories <- fread("DIA Analysis Results 1.1/DIA Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
Treatment_exp_Vector <- fread("DIA Analysis Results 1.1/Treatment_exp_Vector.txt")
DIA_Drug_Histories <- Treatment_exp_Vector %>% left_join(DIA_Drug_Histories)
DIA_Drug_Histories <- gather(DIA_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
DIA_Drug_Histories <- DIA_Drug_Histories %>% select(patient, weight, Drugs) %>% distinct() %>% filter(Drugs!="-")
DIA_Drug_Histories <- separate_rows(DIA_Drug_Histories, Drugs, sep = ",", convert=T)
DIA_Drug_Histories$Drugs <- as.numeric(DIA_Drug_Histories$Drugs)
DIA_Drug_Histories <- DIA_Drug_Histories %>% select(patient, weight, Drugs) %>% distinct() 
DIA_Drug_Histories <- DIA_Drug_Histories %>% left_join(DANU_Ingredients, by=c("Drugs"="molecule")) %>% select(patient, weight, drug_group) %>% distinct()


Treatment_exp_Vector <- fread("DIA Analysis Results 1.1/Treatment_exp_Vector.txt")
DIA_Box_Histories     <- fread("DIA Analysis Results 1.1/DIA Box Histories.txt", integer64 = "character", stringsAsFactors = F)
DIA_Box_Histories <- Treatment_exp_Vector %>% left_join(DIA_Box_Histories)
DIA_Box_Histories <- DIA_Box_Histories %>% select(patient, weight, month60)
DIA_Box_Histories <- DIA_Box_Histories %>% mutate(month60 = str_sub(month60, 2L, 2L))

DIA_Drug_Histories <- DIA_Drug_Histories %>% filter(drug_group=="SGLT2"|drug_group=="GLP1 Injectable"|drug_group=="GLP1 Oral"|drug_group=="Insulin") %>%
  select(patient, weight) %>% distinct() %>% mutate(drug_group="Advanced_Exp") %>%
  bind_rows(DIA_Drug_Histories)

DIA_Box_Histories %>% left_join(DIA_Drug_Histories) %>%
  group_by(month60, drug_group) %>% summarise(n=sum(weight)) %>%
  ungroup() %>%spread(key = drug_group, value=n) %>%
  left_join(
    DIA_Box_Histories %>% group_by(month60) %>% summarise(Total=sum(weight))
  )

DIA_Box_Histories %>% left_join(DIA_Drug_Histories) %>%
  group_by(drug_group) %>% summarise(n=sum(weight)) %>%
  ungroup() 

Stock_vs_Cum_Exp <- fread("DIA Analysis Results 1.1/Stock_vs_Cum_Exp.csv")
row.names(Stock_vs_Cum_Exp) <- Stock_vs_Cum_Exp$month60
Stock_vs_Cum_Exp <- Stock_vs_Cum_Exp %>% select(-c(month60))
row.names(Stock_vs_Cum_Exp)



grid.bubble.plot <- function(df, 
                              axis_labels_size=7, 
                              aspect_ratio=1/1,
                              values_text_size=4,
                              values_text_color="black",
                              x_axis_position="top", # or "bottom",
                              bubble_size_range=c(5, 30),
                              bubble_alpha=0.7,
                              bubble_shape=21,
                              bubble_edge_stroke=0) {
   col_names <- colnames(df)
   row_names <- rownames(df)
   values <- as.vector(as.matrix(df))
   values_x <- as.vector(sapply(col_names, function(i) rep(i, nrow(df))))
   values_y <- as.vector(rep(row_names, dim(df)[2]))
   res_df <- data.frame(values = values, values_x = values_x, values_y)
   res_df <- data.frame(res_df %>% mutate(values_x=fct_relevel(values_x,c("Biguanide_Exp","Antidiabetic_Exp","DPP4_Exp","SGLT2_Exp","Insulin_Exp","GLP1_Oral_Exp","GLP1_Injectable_Exp","Advanced_Exp"))) %>%
                          mutate(values_y=fct_relevel(values_y,c("Lapsed","Biguanide","Antidiabetic","DPP4","SGLT2","Insulin","GLP1_Oral","GLP1_Injectable"))))
   gg <- ggplot(res_df, aes(x=values_x, y=values_y, size = values, fill=factor(values_x))) +
     geom_point(alpha=bubble_alpha, shape=bubble_shape, stroke=bubble_edge_stroke) +
     scale_size(range = bubble_size_range) +
      scale_fill_brewer(palette = "RdBu") +
     scale_x_discrete(position = x_axis_position) +
     scale_y_discrete(limits=rev)+
     geom_text(aes(label=paste0(values,"%")), fontface="bold", size=values_text_size, color=values_text_color,) +
     theme(line=element_blank(), 
           panel.background=element_blank(),
           legend.position="none",
           axis.title=element_blank(),
           axis.text=element_text(size=axis_labels_size),
           aspect.ratio=aspect_ratio)
   gg
}


grid.bubble.plot(Stock_vs_Cum_Exp)
  









# --------------------------
# Generating Long Flows Table ------------------------
DIA_Drug_Histories     <- fread("DIA Analysis Results 1.1/DIA Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
DIA_Box_Histories     <- fread("DIA Analysis Results 1.1/DIA Box Histories.txt", integer64 = "character", stringsAsFactors = F)

# Flows table in long format
flDIA <- DIA_Drug_Histories
flDIA <- flDIA[,disease := NULL]

flDIA <- melt(flDIA, id = c("patient","weight"))
names(flDIA)[c(3,4)] <- c("p1","v1")
flDIA <- flDIA[, p1 := str_extrat(p1,"[:digit:]+")]
flDIA$p1 <- as.numeric(flDIA$p1)
flDIA <- data.frame(cbind(flDIA[p1 < 60], flDIA[p1 > 1,.(p2 = p1, v2 = v1)]), stringsAsFactors = F)
flDIA <- flDIA[,c(1:3,5,4,6)]

# Any flow flag and stops flag
flDIA <- setDT(flDIA)[, flow := (v1 != v2)*1]
flDIA <- flDIA[, stops := (flow == 1 & v2 == "-")*1]

# Treatment experience
RxExp <- data.frame(DIA_Drug_Histories, stringsAsFactors = F)
RxExp$month1 <- (RxExp$month1 != "-")*1

for(i in 2:60){
  cat(i)
  RxExp[,i+2] <- (((RxExp[,i+2] != "-")*1 + RxExp[,i+2-1]) > 0)*1
}

RxExp <- setDT(RxExp)
RxExp <- melt(RxExp, id = c("patient","weight"))
RxExp <- RxExp[, month := str_extract(variable,"[:digit:]+")]
RxExp$month <- as.numeric(RxExp$month)
names(RxExp)[4] <- "DIA_RxExp"

flDIA <- RxExp[,.(patient,month,DIA_RxExp)][flDIA, on = .(patient, month = p1)]
flDIA <- flDIA[,.(patient, weight, p1 = month, p2, v1, v2, p1_RxExp = DIA_RxExp, flow, stops)]

# Starts and re-starts flag
flDIA <- flDIA[, starts := (flow == 1 & v1 == "-" & p1_RxExp == 0)*1]
flDIA <- flDIA[, re_starts := (flow == 1 & v1 == "-" & p1_RxExp == 1)*1]
flDIA <- flDIA[, disease := "DIA US"]
flDIA <- flDIA[,c(12,1:11)]

# Bring Therapy classes (Stocks) to the table
DIA_Box_Histories <- DIA_Box_Histories[,disease := NULL]
DIA_Box_Histories <- data.frame(DIA_Box_Histories, stringsAsFactors = F)

for(i in 1:60){
  cat(i)
  DIA_Box_Histories[,i+2] <- unlist(lapply(DIA_Box_Histories[,i+2],function(x) str_sub(x, 2L, 2L)))
}

setDT(DIA_Box_Histories) 
DIA_Box_Histories <- melt(DIA_Box_Histories, id = c("patient","weight"))
names(DIA_Box_Histories)[c(3,4)] <- c("p","s")
DIA_Box_Histories <- DIA_Box_Histories[, p := str_extract(p,"[:digit:]+")]
DIA_Box_Histories$p <- as.numeric(DIA_Box_Histories$p)

flDIA <- DIA_Box_Histories[,.(patient,p,s)][flDIA, on = .(patient, p = p1)]
names(flDIA)[c(2,3)] <- c("p1","s1")
flDIA <- DIA_Box_Histories[,.(patient,p,s)][flDIA, on = .(patient, p = p2)]
names(flDIA)[c(2,3)] <- c("p2","s2")

flDIA <- flDIA[,.(disease, patient, weight, p1, p2, v1, v2, s1, s2, p1_RxExp, flow, stops, starts, re_starts)]
names(flDIA)[c(6,7)] <- c("d1","d2")


fwrite(flDIA,"DIA Analysis Results 1.1/DIA_Flows_Aux_Long.txt")

# -------------------------------------
# Flows Last 12 months ----------------------------

DIA_Flows_Aux_Long <- fread("DIA Analysis Results 1.1/DIA_Flows_Aux_Long.txt", integer64 = "character", stringsAsFactors = F)
Treatment_exp_Vector <- fread("DIA Analysis Results 1.1/Treatment_exp_Vector.txt")
DIA_Flows_Aux_Long <- Treatment_exp_Vector %>% inner_join(DIA_Flows_Aux_Long)
DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% filter(p1 >=48)

DIA_Flows_Aux_Long %>% filter(stops==1) %>% summarise(n=sum(weight))
DIA_Flows_Aux_Long %>% filter(flow==1&stops==0&(starts==1|re_starts==1)) %>% summarise(n=sum(weight))
DIA_Flows_Aux_Long %>% filter(flow==1) %>% summarise(n=sum(weight))


data.frame(DIA_Flows_Aux_Long %>% 
  mutate(s1=ifelse(s1=="g"|s1=="G", "G", s1)) %>% mutate(s2=ifelse(s2=="g"|s2=="G", "G", s2)) %>%
  mutate(s1=ifelse(s1=="D"|s1=="S", "DS", s1)) %>% mutate(s2=ifelse(s2=="D"|s2=="S", "DS", s2)) %>%
  mutate(s1=ifelse(s1=="b"|s1=="d", "E", s1)) %>% mutate(s2=ifelse(s2=="b"|s2=="d", "E", s2)) %>%
  filter(flow==1) %>% group_by(s1,s2) %>% summarise(n=sum(weight)))

data.frame(DIA_Flows_Aux_Long %>% filter(flow==1&stops!=1) %>%
             group_by(s1,s2) %>% summarise(n=sum(weight))) %>%
  ungroup() %>% spread(key=s1, value=n)

DIA_Flows_Aux_Long %>% filter(flow==1) %>% filter(!grepl("47", d1) & grepl("47", d2)) %>% group_by(s1) %>% summarise(n=sum(weight)/(22533+26047+38891+57624+66684+41786+105464))

DIA_Flows_Aux_Long %>% filter(flow==1) %>% filter(grepl("47", d1) & !grepl("47", d2)) %>% group_by(s2) %>% summarise(n=sum(weight)/(10156+27786+31640+36053+38892+25716+78840))


# ----------------------------
# Off label use Injectable GLP1 ------------------

DIA_Flows_Aux_Long <- fread("DIA Analysis Results 1.1/DIA_Flows_Aux_Long.txt", integer64 = "character", stringsAsFactors = F)
DIA_Flows_Aux_Long %>% filter(s1=="G"|s2=="G") %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(weight)) # 6444437
GLP1_pats <- DIA_Flows_Aux_Long %>% filter(s1=="G"|s2=="G") %>% select(patient, weight) %>% distinct()

# Pick patients that had *ONLY* |Biguanide| AND |GLP1|
DANU_Ingredients <- fread("DIA Analysis Results 1.1/DANU Ingredients.txt", integer64 = "character", stringsAsFactors = F)
DANU_Ingredients <- DANU_Ingredients %>%  separate(drug_id, c('class', 'molecule'))

string_Biguanide       <- paste0("\\b(",paste0(DANU_Ingredients$molecule[DANU_Ingredients$drug_group == "Biguanide"], collapse = "|"),")\\b")
string_Antidiabetic    <- paste0("\\b(",paste0(DANU_Ingredients$molecule[DANU_Ingredients$drug_group == "Antidiabetic"], collapse = "|"),")\\b")
string_DPP4            <- paste0("\\b(",paste0(DANU_Ingredients$molecule[DANU_Ingredients$drug_group == "DPP4"], collapse = "|"),")\\b")
string_SGLT2           <- paste0("\\b(",paste0(DANU_Ingredients$molecule[DANU_Ingredients$drug_group == "SGLT2"], collapse = "|"),")\\b")
string_Insulin         <- paste0("\\b(",paste0(DANU_Ingredients$molecule[DANU_Ingredients$drug_group == "Insulin"], collapse = "|"),")\\b")
string_OralGLP1        <- paste0("\\b(",paste0(DANU_Ingredients$molecule[DANU_Ingredients$drug_group == "GLP1 Oral"], collapse = "|"),")\\b")
string_InjectableGLP1  <- paste0("\\b(",paste0(DANU_Ingredients$molecule[DANU_Ingredients$drug_group == "GLP1 Injectable"], collapse = "|"),")\\b")

DIA_Drug_Histories <- fread("DIA Analysis Results 1.1/DIA Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
DIA_Drug_Histories <- GLP1_pats %>% left_join(DIA_Drug_Histories) %>% select(-c(disease))
DIA_Drug_Histories <- gather(DIA_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
DIA_Drug_Histories$Month <- as.character(DIA_Drug_Histories$Month)
DIA_Drug_Histories$Month <- parse_number(DIA_Drug_Histories$Month)
DIA_Drug_Histories <- DIA_Drug_Histories %>% filter(Drugs != "-")

Pats_to_remove <- DIA_Drug_Histories %>% 
  mutate(Toremove = ifelse(grepl(string_Antidiabetic, Drugs)|grepl(string_DPP4, Drugs)|
                             grepl(string_Insulin, Drugs)|grepl(string_SGLT2, Drugs)|grepl(string_OralGLP1, Drugs),1,0)) %>% 
  filter(Toremove==1) %>% select(patient) %>% distinct()

Pats_Biguanide_GLP1_only <- DIA_Drug_Histories %>%  anti_join(Pats_to_remove)  %>% select(patient, weight) %>% distinct()

GLP1_pats <- Pats_Biguanide_GLP1_only %>% inner_join(GLP1_pats)

sum(GLP1_pats$weight) # 355926.9 # GLP1 and/or Biguanide

GLP1_pats %>%
  anti_join(
    DIA_Drug_Histories %>% filter(grepl(string_Biguanide,Drugs)) %>% select(patient) %>% distinct()
  ) %>% summarise(n=sum(weight))


Off_label_Injectable_pats <- GLP1_pats %>%
  anti_join(
    DIA_Drug_Histories %>% filter(grepl(string_Biguanide,Drugs)) %>% select(patient) %>% distinct()
  )

DIA_Drug_Histories %>% filter(grepl(string_Biguanide,Drugs)) %>% group_by(patient) %>% filter(Month==min(Month)) %>% 
  select(patient, Month) %>% distinct() %>% rename("First_Big"="Month") %>%
  inner_join(
    DIA_Drug_Histories %>% filter(grepl(string_InjectableGLP1,Drugs)) %>% group_by(patient) %>% filter(Month==min(Month)) %>% 
  select(patient, Month) %>% distinct() %>% rename("First_GLP1"="Month")
  ) %>% left_join(DIA_Drug_Histories %>% select(patient, weight) %>% distinct()) %>%
  ungroup() %>% 
  inner_join(GLP1_pats) %>%
  filter(First_Big<First_GLP1) %>%
  summarise(n=sum(weight))

DIA_Drug_Histories <- fread("DIA Analysis Results 1.1/DIA Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
DIA_Drug_Histories <- gather(DIA_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
DIA_Drug_Histories <- DIA_Drug_Histories %>% filter(Drugs != "-")
DIA_Drug_Histories %>% filter(grepl(string_InjectableGLP1,Drugs)) %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(weight))

# ---------------------------------
# Off label use Oral GLP1 ------------------

DIA_Flows_Aux_Long <- fread("DIA Analysis Results 1.1/DIA_Flows_Aux_Long.txt", integer64 = "character", stringsAsFactors = F)
DIA_Flows_Aux_Long %>% filter(grepl("47",d1)|grepl("47",d2)) %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(weight)) # 544894.7
GLP1_pats <- DIA_Flows_Aux_Long %>% filter(grepl("47",d1)|grepl("47",d2)) %>% select(patient, weight) %>% distinct()

# Pick patients that had *ONLY* |Biguanide| AND |GLP1|
DANU_Ingredients <- fread("DIA Analysis Results 1.1/DANU Ingredients.txt", integer64 = "character", stringsAsFactors = F)
DANU_Ingredients <- DANU_Ingredients %>%  separate(drug_id, c('class', 'molecule'))

string_Biguanide       <- paste0("\\b(",paste0(DANU_Ingredients$molecule[DANU_Ingredients$drug_group == "Biguanide"], collapse = "|"),")\\b")
string_Antidiabetic    <- paste0("\\b(",paste0(DANU_Ingredients$molecule[DANU_Ingredients$drug_group == "Antidiabetic"], collapse = "|"),")\\b")
string_DPP4            <- paste0("\\b(",paste0(DANU_Ingredients$molecule[DANU_Ingredients$drug_group == "DPP4"], collapse = "|"),")\\b")
string_SGLT2           <- paste0("\\b(",paste0(DANU_Ingredients$molecule[DANU_Ingredients$drug_group == "SGLT2"], collapse = "|"),")\\b")
string_Insulin         <- paste0("\\b(",paste0(DANU_Ingredients$molecule[DANU_Ingredients$drug_group == "Insulin"], collapse = "|"),")\\b")
string_OralGLP1        <- paste0("\\b(",paste0(DANU_Ingredients$molecule[DANU_Ingredients$drug_group == "GLP1 Oral"], collapse = "|"),")\\b")
string_InjectableGLP1  <- paste0("\\b(",paste0(DANU_Ingredients$molecule[DANU_Ingredients$drug_group == "GLP1 Injectable"], collapse = "|"),")\\b")

DIA_Drug_Histories <- fread("DIA Analysis Results 1.1/DIA Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
DIA_Drug_Histories <- GLP1_pats %>% left_join(DIA_Drug_Histories) %>% select(-c(disease))
DIA_Drug_Histories <- gather(DIA_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
DIA_Drug_Histories$Month <- as.character(DIA_Drug_Histories$Month)
DIA_Drug_Histories$Month <- parse_number(DIA_Drug_Histories$Month)
DIA_Drug_Histories <- DIA_Drug_Histories %>% filter(Drugs != "-")

Pats_to_remove <- DIA_Drug_Histories %>% 
  mutate(Toremove = ifelse(grepl(string_Antidiabetic, Drugs)|grepl(string_DPP4, Drugs)|
                             grepl(string_Insulin, Drugs)|grepl(string_SGLT2, Drugs)|grepl(string_InjectableGLP1, Drugs),1,0)) %>% 
  filter(Toremove==1) %>% select(patient) %>% distinct()

Pats_Biguanide_GLP1_only <- DIA_Drug_Histories %>%  anti_join(Pats_to_remove)  %>% select(patient, weight) %>% distinct()

GLP1_pats <- Pats_Biguanide_GLP1_only %>% inner_join(GLP1_pats)

sum(GLP1_pats$weight) # 102653 # GLP1 and/or Biguanide

GLP1_pats %>%
  anti_join(
    DIA_Drug_Histories %>% filter(grepl(string_Biguanide,Drugs)) %>% select(patient) %>% distinct()
  ) %>% summarise(n=sum(weight)) # 24112.88

Off_label_Oral_pats <- GLP1_pats %>%
  anti_join(
    DIA_Drug_Histories %>% filter(grepl(string_Biguanide,Drugs)) %>% select(patient) %>% distinct()
  )

fwrite(Off_label_Injectable_pats, "Off_label_Injectable_pats.txt", sep="\t")
fwrite(Off_label_Oral_pats, "Off_label_Oral_pats.txt", sep="\t")


DIA_Drug_Histories %>% filter(grepl(string_Biguanide,Drugs)) %>% group_by(patient) %>% filter(Month==min(Month)) %>% 
  select(patient, Month) %>% distinct() %>% rename("First_Big"="Month") %>%
  inner_join(
    DIA_Drug_Histories %>% filter(grepl(string_OralGLP1,Drugs)) %>% group_by(patient) %>% filter(Month==min(Month)) %>% 
  select(patient, Month) %>% distinct() %>% rename("First_GLP1"="Month")
  ) %>% left_join(DIA_Drug_Histories %>% select(patient, weight) %>% distinct()) %>%
  ungroup() %>% 
  inner_join(GLP1_pats) %>%
  filter(First_Big<First_GLP1) %>%
  summarise(n=sum(weight)) # 67991

DIA_Drug_Histories <- fread("DIA Analysis Results 1.1/DIA Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
DIA_Drug_Histories <- gather(DIA_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
DIA_Drug_Histories <- DIA_Drug_Histories %>% filter(Drugs != "-")
DIA_Drug_Histories %>% filter(grepl(string_InjectableGLP1,Drugs)) %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(weight))

# ---------------------------------
# Last HbA1c & Last BMI ----------------------------

DIA_Box_Histories <- fread("DIA Analysis Results 1.1/DIA Box Histories.txt", integer64 = "character", stringsAsFactors = F)
DIA_Box_Histories <- DIA_Box_Histories %>% select(patient, month60)
DIA_Box_Histories <- DIA_Box_Histories %>% mutate(month60 = str_sub(month60, 2L, 2L))

DANU_Measures <- fread("DANU Measures 1.1/DANU Measures.txt",  integer64 = "character", stringsAsFactors = F)
unique(DANU_Measures$test)

BMI <- DANU_Measures %>% filter(test=="BMI")
HbA1c <- DANU_Measures %>% filter(test=="HbA1c Level")

BMI <- BMI %>% select(patid, value) %>% distinct() %>% group_by(patid ) %>% filter(value==max(value)) %>% slice(1)
HbA1c <- HbA1c %>% select(patid, value) %>% distinct() %>% group_by(patid ) %>% filter(value==max(value)) %>% slice(1)

BMI <- DIA_Box_Histories %>% inner_join(BMI, by=c("patient"="patid"))
HbA1c <- DIA_Box_Histories %>% inner_join(HbA1c, by=c("patient"="patid"))


BMI %>% group_by(month60) %>% summarise(n=mean(as.numeric(value))) %>% arrange(-n)
HbA1c %>% group_by(month60) %>% summarise(n=mean(as.numeric(value))) %>% arrange(-n)

BMI %>% group_by(month60) %>% summarise(n=median(as.numeric(value))) %>% arrange(-n)
HbA1c %>% group_by(month60) %>% summarise(n=median(as.numeric(value))) %>% arrange(-n)


HbA1c %>% ungroup() %>% select(month60, value) %>% 
  mutate(month60 = factor(month60, levels=c("x", "b", "d", "D", "S", "I", "g", "G"))) %>% group_by(month60) %>%
  ggplot(aes(value))+
  geom_density(aes(fill = month60, colour=month60), alpha =0.7, show.legend = F, size=2)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  facet_wrap(~month60, ncol = 8)+
  scale_colour_brewer(palette = "RdBu") +
  scale_fill_brewer(palette = "RdBu") +
  coord_flip()+
  xlim(4,15)+
  xlab("HbA1c (%)\n")+ ylab("\nProportion of patients")

# --------------------------------
# Number of months / lines until each class initiation ---------------------------
DANU_Ingredients <- fread("DIA Analysis Results 1.1/DANU Ingredients.txt", integer64 = "character", stringsAsFactors = F)
DANU_Ingredients <- DANU_Ingredients %>%  separate(drug_id, c('class', 'molecule'))
string_Biguanide <- paste0("\\b(",paste0(DANU_Ingredients$molecule[DANU_Ingredients$drug_group == "Biguanide"], collapse = "|"),")\\b")
string_Antidiabetic <- paste0("\\b(",paste0(DANU_Ingredients$molecule[DANU_Ingredients$drug_group == "Antidiabetic"], collapse = "|"),")\\b")
string_DPP4 <- paste0("\\b(",paste0(DANU_Ingredients$molecule[DANU_Ingredients$drug_group == "DPP4"], collapse = "|"),")\\b")
string_SGLT2 <- paste0("\\b(",paste0(DANU_Ingredients$molecule[DANU_Ingredients$drug_group == "SGLT2"], collapse = "|"),")\\b")
string_Insulin <- paste0("\\b(",paste0(DANU_Ingredients$molecule[DANU_Ingredients$drug_group == "Insulin"], collapse = "|"),")\\b")
string_OralGLP1 <- paste0("\\b(",paste0(DANU_Ingredients$molecule[DANU_Ingredients$drug_group == "GLP1 Oral"], collapse = "|"),")\\b")
string_InjectableGLP1 <- paste0("\\b(",paste0(DANU_Ingredients$molecule[DANU_Ingredients$drug_group == "GLP1 Injectable"], collapse = "|"),")\\b")

# All pats
DIA_Drug_Histories <- fread("DIA Analysis Results 1.1/DIA Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
Treatment_exp_Vector <-fread("DIA Analysis Results 1.1/Treatment_exp_Vector.txt")
DIA_Drug_Histories <- Treatment_exp_Vector %>% left_join(DIA_Drug_Histories)
DIA_Drug_Histories <- DIA_Drug_Histories %>% select(-c(disease))

DIA_Drug_Histories <- gather(DIA_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
DIA_Drug_Histories$Month <- as.character(DIA_Drug_Histories$Month)
DIA_Drug_Histories$Month <- parse_number(DIA_Drug_Histories$Month)
DIA_Drug_Histories <- DIA_Drug_Histories %>% group_by(patient) %>% filter(Month<=12)
DIA_Drug_Histories <- DIA_Drug_Histories %>% filter(Drugs == "-")
Naive_until_12 <- DIA_Drug_Histories %>% group_by(patient) %>% count() %>% filter(n==12) %>% select(patient)

# Number of months
DIA_Drug_Histories <- fread("DIA Analysis Results 1.1/DIA Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
DIA_Drug_Histories <- DIA_Drug_Histories %>% select(-c(disease))
DIA_Drug_Histories <- Naive_until_12 %>% left_join(DIA_Drug_Histories)
DIA_Drug_Histories <- gather(DIA_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
DIA_Drug_Histories$Month <- as.character(DIA_Drug_Histories$Month)
DIA_Drug_Histories$Month <- parse_number(DIA_Drug_Histories$Month)

# Time to first  Biguanide
DIA_Drug_Histories_Biguanide <- DIA_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_Biguanide,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_Biguanide,Drugs)) else NA) 
DIA_Drug_Histories_Biguanide <- DIA_Drug_Histories_Biguanide %>% group_by(patient, weight) %>% count() %>% arrange(-n)
DIA_Drug_Histories_Biguanide %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
DIA_Drug_Histories_Biguanide %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) # 1.9
DIA_Drug_Histories_Biguanide %>% ungroup() %>% summarise(median=weighted.median(n, weight)) # 1
sum(DIA_Drug_Histories_Biguanide$weight) #8767251

# Time to first  Antidiabetic
DIA_Drug_Histories_Antidiabetic <- DIA_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_Antidiabetic,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_Antidiabetic,Drugs)) else NA) 
DIA_Drug_Histories_Antidiabetic <- DIA_Drug_Histories_Antidiabetic %>% group_by(patient, weight) %>% count() %>% arrange(-n)
DIA_Drug_Histories_Antidiabetic %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
DIA_Drug_Histories_Antidiabetic %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) # 6.65
DIA_Drug_Histories_Antidiabetic %>% ungroup() %>% summarise(median=weighted.median(n, weight)) # 1
sum(DIA_Drug_Histories_Antidiabetic$weight) #1880554

# Time to first  DPP4
DIA_Drug_Histories_DPP4 <- DIA_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_DPP4,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_DPP4,Drugs)) else NA) 
DIA_Drug_Histories_DPP4 <- DIA_Drug_Histories_DPP4 %>% group_by(patient, weight) %>% count() %>% arrange(-n)
DIA_Drug_Histories_DPP4 %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
DIA_Drug_Histories_DPP4 %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) # 7.33
DIA_Drug_Histories_DPP4 %>% ungroup() %>% summarise(median=weighted.median(n, weight)) # 1
sum(DIA_Drug_Histories_DPP4$weight) #950927.7

# Time to first  SGLT2
DIA_Drug_Histories_SGLT2 <- DIA_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_SGLT2,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_SGLT2,Drugs)) else NA) 
DIA_Drug_Histories_SGLT2 <- DIA_Drug_Histories_SGLT2 %>% group_by(patient, weight) %>% count() %>% arrange(-n)
DIA_Drug_Histories_SGLT2 %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
DIA_Drug_Histories_SGLT2 %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) # 10.6
DIA_Drug_Histories_SGLT2 %>% ungroup() %>% summarise(median=weighted.median(n, weight)) # 3.5
sum(DIA_Drug_Histories_SGLT2$weight) #1280760

# Time to first  Insulin
DIA_Drug_Histories_Insulin <- DIA_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_Insulin,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_Insulin,Drugs)) else NA) 
DIA_Drug_Histories_Insulin <- DIA_Drug_Histories_Insulin %>% group_by(patient, weight) %>% count() %>% arrange(-n)
DIA_Drug_Histories_Insulin %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
DIA_Drug_Histories_Insulin %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) # 3.80
DIA_Drug_Histories_Insulin %>% ungroup() %>% summarise(median=weighted.median(n, weight)) # 1
sum(DIA_Drug_Histories_Insulin$weight) #3682418

# Time to first  OralGLP1
DIA_Drug_Histories_OralGLP1 <- DIA_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_OralGLP1,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_OralGLP1,Drugs)) else NA) 
DIA_Drug_Histories_OralGLP1 <- DIA_Drug_Histories_OralGLP1 %>% group_by(patient, weight) %>% count() %>% arrange(-n)
DIA_Drug_Histories_OralGLP1 %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
DIA_Drug_Histories_OralGLP1 %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) # 14.3
DIA_Drug_Histories_OralGLP1 %>% ungroup() %>% summarise(median=weighted.median(n, weight)) # 10.5
sum(DIA_Drug_Histories_OralGLP1$weight) #207330.2

# Time to first  InjectableGLP1
DIA_Drug_Histories_InjectableGLP1 <- DIA_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_InjectableGLP1,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_InjectableGLP1,Drugs)) else NA) 
DIA_Drug_Histories_InjectableGLP1 <- DIA_Drug_Histories_InjectableGLP1 %>% group_by(patient, weight) %>% count() %>% arrange(-n)
DIA_Drug_Histories_InjectableGLP1 %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
DIA_Drug_Histories_InjectableGLP1 %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) # 9.08
DIA_Drug_Histories_InjectableGLP1 %>% ungroup() %>% summarise(median=weighted.median(n, weight)) # 1.5
sum(DIA_Drug_Histories_InjectableGLP1$weight) #1896628

# Number of lines
DIA_Drug_Histories <- fread("DIA Analysis Results 1.1/DIA Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
DIA_Drug_Histories <- DIA_Drug_Histories %>% select(-c(disease))
DIA_Drug_Histories <- Naive_until_12 %>% left_join(DIA_Drug_Histories)
DIA_Drug_Histories <- gather(DIA_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
DIA_Drug_Histories$Month <- as.character(DIA_Drug_Histories$Month)
DIA_Drug_Histories$Month <- parse_number(DIA_Drug_Histories$Month)

# Lines to first  Biguanide
DIA_Drug_Histories_Biguanide <- DIA_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_Biguanide,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_Biguanide,Drugs)) else NA) 
DIA_Drug_Histories_Biguanide <- DIA_Drug_Histories_Biguanide %>% select(-c(Month)) %>% filter(Drugs!="-") %>% group_by(patient, weight) %>% distinct() %>% count() %>% arrange(-n)
DIA_Drug_Histories_Biguanide %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
DIA_Drug_Histories_Biguanide %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) # 1.12
DIA_Drug_Histories_Biguanide %>% ungroup() %>% summarise(median=weighted.median(n, weight)) # 1
sum(DIA_Drug_Histories_Biguanide$weight) #8767251

# Time to first  Antidiabetic
DIA_Drug_Histories_Antidiabetic <- DIA_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_Antidiabetic,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_Antidiabetic,Drugs)) else NA) 
DIA_Drug_Histories_Antidiabetic <- DIA_Drug_Histories_Antidiabetic %>% select(-c(Month)) %>% filter(Drugs!="-") %>% group_by(patient, weight) %>% distinct() %>% count() %>% arrange(-n)
DIA_Drug_Histories_Antidiabetic %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
DIA_Drug_Histories_Antidiabetic %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) # 1.67
DIA_Drug_Histories_Antidiabetic %>% ungroup() %>% summarise(median=weighted.median(n, weight)) # 1
sum(DIA_Drug_Histories_Antidiabetic$weight) #1880554

# Time to first  DPP4
DIA_Drug_Histories_DPP4 <- DIA_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_DPP4,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_DPP4,Drugs)) else NA) 
DIA_Drug_Histories_DPP4 <- DIA_Drug_Histories_DPP4 %>% select(-c(Month)) %>% filter(Drugs!="-") %>% group_by(patient, weight) %>% distinct() %>% count() %>% arrange(-n)
DIA_Drug_Histories_DPP4 %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
DIA_Drug_Histories_DPP4 %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) # 1.78
DIA_Drug_Histories_DPP4 %>% ungroup() %>% summarise(median=weighted.median(n, weight)) # 1.5
sum(DIA_Drug_Histories_DPP4$weight) #950927.7

# Time to first  SGLT2
DIA_Drug_Histories_SGLT2 <- DIA_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_SGLT2,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_SGLT2,Drugs)) else NA) 
DIA_Drug_Histories_SGLT2 <- DIA_Drug_Histories_SGLT2 %>% select(-c(Month)) %>% filter(Drugs!="-") %>% group_by(patient, weight) %>% distinct() %>% count() %>% arrange(-n)
DIA_Drug_Histories_SGLT2 %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
DIA_Drug_Histories_SGLT2 %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) # 2.21
DIA_Drug_Histories_SGLT2 %>% ungroup() %>% summarise(median=weighted.median(n, weight)) # 1.5
sum(DIA_Drug_Histories_SGLT2$weight) #97264

# Time to first  Insulin
DIA_Drug_Histories_Insulin <- DIA_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_Insulin,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_Insulin,Drugs)) else NA) 
DIA_Drug_Histories_Insulin <- DIA_Drug_Histories_Insulin %>% select(-c(Month)) %>% filter(Drugs!="-") %>% group_by(patient, weight) %>% distinct() %>% count() %>% arrange(-n)
DIA_Drug_Histories_Insulin %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
DIA_Drug_Histories_Insulin %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) # 2.16
DIA_Drug_Histories_Insulin %>% ungroup() %>% summarise(median=weighted.median(n, weight)) # 1.5
sum(DIA_Drug_Histories_Insulin$weight) #1280760

# Time to first  OralGLP1
DIA_Drug_Histories_OralGLP1 <- DIA_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_OralGLP1,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_OralGLP1,Drugs)) else NA) 
DIA_Drug_Histories_OralGLP1 <- DIA_Drug_Histories_OralGLP1 %>% select(-c(Month)) %>% filter(Drugs!="-") %>% group_by(patient, weight) %>% distinct() %>% count() %>% arrange(-n)
DIA_Drug_Histories_OralGLP1 %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
DIA_Drug_Histories_OralGLP1 %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) # 2.54
DIA_Drug_Histories_OralGLP1 %>% ungroup() %>% summarise(median=weighted.median(n, weight)) # 1.5
sum(DIA_Drug_Histories_OralGLP1$weight) #207330.2

# Time to first  InjectableGLP1
DIA_Drug_Histories_InjectableGLP1 <- DIA_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_InjectableGLP1,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_InjectableGLP1,Drugs)) else NA) 
DIA_Drug_Histories_InjectableGLP1 <- DIA_Drug_Histories_InjectableGLP1 %>% select(-c(Month)) %>%  filter(Drugs!="-") %>% group_by(patient, weight) %>% distinct() %>% count() %>% arrange(-n)
DIA_Drug_Histories_InjectableGLP1 %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
DIA_Drug_Histories_InjectableGLP1 %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) # 1.94
DIA_Drug_Histories_InjectableGLP1 %>% ungroup() %>% summarise(median=weighted.median(n, weight)) # 1.5
sum(DIA_Drug_Histories_InjectableGLP1$weight) #1896628



Months_vs_Lines_to_class <- fread("DIA Analysis Results 1.1/Months_vs_Lines_to_class.csv")

library(ggrepel)
library(hrbrthemes)
library(viridis)

ggplot(Months_vs_Lines_to_class, aes(x=average_months_to_class_all, y=average_lines_to_class_all, size = pop_all, fill=drug_class, colour=drug_class)) +
  geom_point(alpha=1)+
  geom_text_repel(aes(label = drug_class), 
                  colour = "black", 
                  size = 4,
                  hjust = -1,
                  vjust=0.1,
                  fontface=2)+ 
  scale_size(range = c(.1, 15))+
  theme(legend.position = "none",
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        text = element_text(size = 15))+
  scale_colour_brewer(palette = "YlGnBu") +
  scale_fill_brewer(palette = "YlGnBu") +
  xlim(0,15) +
  xlab("\nAverage Number of Months to Class Initiation")+
  ylab("Average Number of Therapy Lines to Class Initiation\n")

# ----------------------------------------------

# NEW definition lines of therapy ------------------------------------
DANU_Ingredients <- fread("DIA Analysis Results 1.1/DANU Ingredients.txt", integer64 = "character", stringsAsFactors = F)
DANU_Ingredients <- DANU_Ingredients %>%  separate(drug_id, c('class', 'molecule'))
DANU_Ingredients <- DANU_Ingredients %>% select(molecule, drug_group) 

DIA_Drug_Histories <- fread("DIA Analysis Results 1.1/DIA Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
Treatment_exp_Vector <- fread("DIA Analysis Results 1.1/Treatment_exp_Vector.txt")
DIA_Drug_Histories <- DIA_Drug_Histories %>% select(-disease) %>% gather(Month, Drugs, month1:month60) %>% filter(Drugs != "-")
DIA_Drug_Histories <- Treatment_exp_Vector %>% inner_join(DIA_Drug_Histories)
DIA_Drug_Histories <- separate_rows(DIA_Drug_Histories, Drugs, sep = ",", convert=T)
DIA_Drug_Histories <- DIA_Drug_Histories %>% left_join(DANU_Ingredients %>% mutate(molecule=as.numeric(molecule)), by=c("Drugs"="molecule"))
DIA_Drug_Histories <- DIA_Drug_Histories %>% select(-Drugs) %>% distinct()

DIA_Drug_Histories <- DIA_Drug_Histories %>% mutate(drug_group=ifelse(drug_group=="Insulin", "I",
                                                ifelse(drug_group=="Biguanide", "b",
                                                       ifelse(drug_group=="SGLT2", "S",
                                                              ifelse(drug_group=="Antidiabetic", "d",
                                                                     ifelse(drug_group=="DPP4", "D",
                                                                            ifelse(drug_group=="GLP1 Injectable", "G", "g")))))))

DIA_Drug_Histories <- DIA_Drug_Histories %>% arrange(patient, weight, Month, drug_group) %>%
  group_by(patient, weight, Month) %>% mutate(drug_group=paste(drug_group, collapse=",")) 

DIA_Drug_Histories <- DIA_Drug_Histories %>% ungroup() 
DIA_Drug_Histories$Month <- as.character(DIA_Drug_Histories$Month)
DIA_Drug_Histories$Month <- parse_number(DIA_Drug_Histories$Month)
DIA_Drug_Histories %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(weight))

DIA_Drug_Histories %>% group_by(patient) %>% filter(Month==min(Month)) %>%
  ungroup() %>% separate_rows(drug_group, sep = ",", convert=T) %>%
  group_by(drug_group) %>% summarise(n=sum(weight)/30120087) %>% arrange(-n)

temp <- DIA_Drug_Histories

temp <- temp %>% select(patient, weight, drug_group) %>% distinct() %>%
   group_by(patient, weight) %>% mutate(grp = rle(drug_group)$lengths %>% {rep(seq(length(.)), .)})

temp %>% ungroup() %>% 
  separate_rows(drug_group, sep = ",", convert=T) %>%
  group_by(grp, drug_group) %>% summarise(n=sum(weight)) %>%
  left_join(temp %>% ungroup() %>%  group_by(grp) %>% summarise(Total=sum(weight))) %>%
  mutate(Perc=100*n/Total) %>% select(-c(n, Total)) %>%
  spread(key=grp, value=Perc)

data.frame(temp %>% group_by(patient) %>% filter(grp==max(grp)) %>% ungroup() %>%  group_by(grp) %>% summarise(Total=sum(weight)/30120087))

DANU_Demographics <- fread("DANU Demographics 1.1/DANU Demographics.txt")
DANU_Demographics <- DANU_Demographics %>% select(patid, weight, diabetes_onset) %>% filter(diabetes_onset>="2017-05-01" & diabetes_onset<="2018-04-30")


InsulinStart_PlusOther <- temp %>% ungroup() %>% filter(grp==1) %>% filter(grepl("I", drug_group)) %>%
  select(patient, weight) %>% distinct() %>% 
  left_join(temp) %>% ungroup() %>%
  filter(grepl("b", drug_group)|grepl("d", drug_group)|grepl("D", drug_group)|grepl("S", drug_group)|grepl("g", drug_group)|grepl("G", drug_group)) %>%
  select(patient, weight) %>% distinct() 

InsulinStart_Only <- temp %>% ungroup() %>% filter(grp==1) %>% filter(grepl("I", drug_group)) %>%
  select(patient, weight) %>% distinct() %>% anti_join(InsulinStart_PlusOther)


temp %>% group_by(grp) %>% summarise(n=sum(weight))
temp %>%  group_by(patient) %>% filter(grp==max(grp)) %>%  mutate(grp=ifelse(grp>=5,5,grp)) %>% group_by(grp) %>% summarise(n=sum(weight)/30120087)


temp %>% anti_join(InsulinStart_Only) %>% ungroup() %>% inner_join(DANU_Demographics %>% select(patid), by=c("patient"="patid")) %>%
  group_by(patient) %>% filter(grp==max(grp)) %>%
  mutate(grp=ifelse(grp>=5,5,grp)) %>%
  group_by(grp) %>% summarise(Total=sum(weight)) %>%
  left_join(
    temp %>% ungroup() %>% anti_join(InsulinStart_Only) %>% inner_join(DANU_Demographics %>% select(patid), by=c("patient"="patid")) %>%
      group_by(patient) %>% filter(grp==max(grp)) %>%
      separate_rows(drug_group, sep = ",", convert=T) %>%   mutate(grp=ifelse(grp>=5,5,grp)) %>% group_by(grp, drug_group) %>% summarise(n=sum(weight))
  ) %>%
    mutate(Perc=100*n/Total) %>% select(-c(n, Total)) %>%
  spread(key=drug_group, value=Perc)



temp %>% anti_join(InsulinStart_Only) %>% ungroup() %>% inner_join(DANU_Demographics %>% select(patid), by=c("patient"="patid"))  %>%
  mutate(grp=ifelse(grp>=5,5,grp)) %>% 
  separate_rows(drug_group, sep = ",", convert=T) %>%
  group_by(drug_group  , grp) %>% 
  summarise(Total=sum(weight)) %>%
  spread(key=grp, value=Total)




temp %>% ungroup() %>% 
  anti_join(InsulinStart_Only) %>%
  inner_join(DANU_Demographics %>% select(patid), by=c("patient"="patid")) %>%
  #select(patient, weight) %>% distinct() %>% summarise(n=sum(weight))
  separate_rows(drug_group, sep = ",", convert=T) %>%
  group_by(grp, drug_group) %>% summarise(n=sum(weight)) %>%
  left_join(
    temp %>% ungroup() %>% inner_join(DANU_Demographics %>% select(patid), by=c("patient"="patid")) %>% group_by(grp) %>% summarise(Total=sum(weight))) %>%
  mutate(Perc=100*n/Total) %>% select(-c(n, Total)) %>%
  mutate(Perc=ifelse(is.na(Perc), 0, Perc)) %>%
  mutate(drug_group=ifelse(drug_group=="I", "Insulin",
                                                ifelse(drug_group=="b", "Biguanide",
                                                       ifelse(drug_group=="S", "SGLT2",
                                                              ifelse(drug_group=="d", "Antidiabetic",
                                                                     ifelse(drug_group=="D", "DPP4",
                                                                            ifelse(drug_group=="G", "GLP1 Injectable", "GLP1 Oral"))))))) %>%
  mutate(drug_group=factor(drug_group, levels=c("Biguanide", "Antidiabetic", "DPP4", "SGLT2", "Insulin", "GLP1 Oral", "GLP1 Injectable"))) %>%
  #filter(drug_group!="Insulin") %>%
  ggplot(aes(grp, Perc, colour=drug_group, fill=drug_group)) +
  geom_smooth(size=2,alpha=0.9, se=F) +
  theme_minimal() +
  xlim(1,5) +
  ylim(0,70) +
  xlab("\n No. Unique Class Combinations Tried") + ylab("Class Penetrance (%) \n") +
  scale_colour_manual(values=c("#f2eded","#7d95be","#0d2b4e","#daa520","#D1F0D1", "#f6546a","#a52a2a")) 

# ----------------------------------------------


# Stops and Returns to Injectables  ------------------------------
DIA_Drug_Histories <- fread("DIA Analysis Results 1.1/DIA Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
Treatment_exp_Vector <- fread("DIA Analysis Results 1.1/Treatment_exp_Vector.txt")
DIA_Drug_Histories <- Treatment_exp_Vector %>% left_join(DIA_Drug_Histories) %>% select(-c(disease))
DIA_Drug_Histories %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(weight))
DIA_Drug_Histories <- gather(DIA_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
DIA_Drug_Histories <- DIA_Drug_Histories %>% filter(Drugs!="-")
DIA_Drug_Histories$Month <- as.character(DIA_Drug_Histories$Month)
DIA_Drug_Histories$Month <- parse_number(DIA_Drug_Histories$Month)
  
DANU_Ingredients       <- fread("DIA Analysis Results 1.1/DANU Ingredients.txt", integer64 = "character", stringsAsFactors = F)
DANU_Ingredients       <- DANU_Ingredients %>%  separate(drug_id, c('class', 'molecule'))
string_Injectables       <- paste0("\\b(",paste0(DANU_Ingredients$molecule[DANU_Ingredients$drug_group == "GLP1 Injectable" | DANU_Ingredients$drug_group == "Insulin"], collapse = "|"),")\\b")
string_Oral    <- paste0("\\b(",paste0(DANU_Ingredients$molecule[DANU_Ingredients$drug_group != "Insulin" & DANU_Ingredients$drug_group != "GLP1 Injectable"], collapse = "|"),")\\b")

DIA_Drug_Histories <- DIA_Drug_Histories %>% mutate(Inj = ifelse( grepl(string_Injectables, Drugs)&grepl(string_Oral, Drugs), "I+O",
                                                                  ifelse(grepl(string_Injectables, Drugs),"I", 
                                                                        ifelse(grepl(string_Oral,Drugs), "O", NA))))

DIA_Drug_Histories_2 <- DIA_Drug_Histories %>% select(patient, weight, Month, Inj) %>%
  filter(grepl("I", Inj)) %>% select(patient) %>% distinct() %>%
  left_join(DIA_Drug_Histories %>% select(patient, weight, Month, Inj))

DIA_Drug_Histories_2 <- DIA_Drug_Histories_2 %>% spread(key=Month, value=Inj)
DIA_Drug_Histories_2[is.na(DIA_Drug_Histories_2)] <- "-"
DIA_Drug_Histories_2 <- gather(DIA_Drug_Histories_2, Month, Drugs, `1`:`60`, factor_key=TRUE)
DIA_Drug_Histories_2 <- DIA_Drug_Histories_2 %>% arrange(patient, Month)

DIA_Drug_Histories_3 <- DIA_Drug_Histories_2 %>% group_by(patient, weight) %>% 
  slice(if(any(grepl("I",Drugs))) which.max(grepl("I",Drugs)):which.max(Month=="60") else NA) 

DIA_Drug_Histories_3 <- DIA_Drug_Histories_3 %>% group_by(patient) %>% mutate(grp = rle(Drugs)$lengths %>% {rep(seq(length(.)), .)})
DIA_Drug_Histories_3 <- DIA_Drug_Histories_3 %>% arrange(patient, Month)
DIA_Drug_Histories_3 %>% ungroup() %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(weight)) # 14402790 Inj exp

DIA_Drug_Histories_3 %>% ungroup() %>% filter(grp!=1) %>%
   filter(!grepl("I", Drugs)) %>% select(patient, weight, Drugs, grp) %>%
  group_by(patient, weight, grp) %>% count() %>% ungroup() %>%
  filter(n>=6) %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(weight)) # stopped >= 6 months 7601646 


DIA_Drug_Histories_3 %>% ungroup() %>% filter(grp!=1) %>%
   filter(!grepl("I", Drugs)) %>% select(patient, weight, Drugs, grp) %>%
  group_by(patient, weight, grp) %>% count() %>% ungroup() %>%
  filter(n>=6) %>% select(patient, weight, grp) %>% distinct() %>%
  group_by(patient, weight) %>% filter(grp==min(grp)) %>%
  rename("Stopgrp"="grp") %>%
  inner_join(
    DIA_Drug_Histories_3 %>% ungroup() %>% filter(grp!=1) %>%
   filter(grepl("I", Drugs)) %>% select(patient, weight, grp) %>% distinct() %>%
  rename("Injgrp"="grp")
  ) %>%
  filter(Injgrp>Stopgrp) %>%
  select(patient, weight)  %>% distinct() %>% ungroup() %>% summarise(n=sum(weight)) # 3376958 


# ---------------------------------------------------------------
# Interaction between rybelsus and different glp1 injectables -------------
DANU_Ingredients <- fread("DIA Analysis Results 1.1/DANU Ingredients.txt", integer64 = "character", stringsAsFactors = F)
DANU_Ingredients <- DANU_Ingredients %>%  separate(drug_id, c('class', 'molecule'))
DANU_Ingredients <- DANU_Ingredients %>% select(molecule, generic_name, drug_group)
DANU_Ingredients$molecule <- as.numeric(DANU_Ingredients$molecule)

DIA_Flows_Aux_Long <- fread("DIA Analysis Results 1.1/DIA_Flows_Aux_Long.txt", integer64 = "character", stringsAsFactors = F)
DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% select(-c(disease, starts, stops, re_starts))
DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% filter(p1>=48) %>% filter(!grepl("47",d1)) %>% filter(grepl("47",d2)) %>% filter(s1=="G") %>%
  select(patient, weight, d1)

sum(DIA_Flows_Aux_Long$weight) # 26046.61

DIA_Flows_Aux_Long <- separate_rows(DIA_Flows_Aux_Long, d1, sep = ",", convert=T)
DIA_Flows_Aux_Long %>% left_join(DANU_Ingredients, by=c("d1"="molecule")) %>% filter(drug_group=="GLP1 Injectable") %>%
  group_by(generic_name) %>% summarise(n=sum(weight)/26046.61)


DIA_Flows_Aux_Long <- fread("DIA Analysis Results 1.1/DIA_Flows_Aux_Long.txt", integer64 = "character", stringsAsFactors = F)
DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% select(-c(disease, starts, stops, re_starts))
DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% filter(p1>=48) %>% filter(grepl("47",d1)) %>% filter(!grepl("47",d2)) %>% filter(s2=="G") %>%
  select(patient, weight, d2)

sum(DIA_Flows_Aux_Long$weight) # 27786.38

DIA_Flows_Aux_Long <- separate_rows(DIA_Flows_Aux_Long, d2, sep = ",", convert=T)
DIA_Flows_Aux_Long %>% left_join(DANU_Ingredients, by=c("d2"="molecule")) %>% filter(drug_group=="GLP1 Injectable") %>%
  group_by(generic_name) %>% summarise(n=sum(weight)/27786.38)

# ------------------------------------------

# Semaglutide Oral dosages over time ----------------------------------------------------------------------


DIA_Medications <- fread("DIA Analysis Results 1.1/DANU Medications.txt")
DIA_Medications <- DIA_Medications %>% filter(brand_name =="Rybelsus") %>% select(drug_id, med_ingredient, med_strength)

Dia_US_Doses <- fread("DIA Doses 1.1/DIA Doses.txt")
Dia_US_Doses <- Dia_US_Doses %>% filter(drug_group == "GLP1 Oral") %>% left_join(DIA_Medications) %>% mutate(from_dt = as.Date(from_dt))
Dia_US_Doses$doses <- parse_number(Dia_US_Doses$med_strength)

Dia_US_Doses %>% filter(drug_group == "GLP1 Oral") %>% 
  select(generic_name, dayssup, pat_id, from_dt, doses) %>% group_by(pat_id) %>%  summarise(n=n()) %>% arrange(-n)

unique(Dia_US_Doses$doses) # 14, 7, 3
weighted.mean(Dia_US_Doses$doses, Dia_US_Doses$weight) #7.851905
weighted.median(Dia_US_Doses$doses, Dia_US_Doses$weight) #5

Dia_US_Doses_semaglutide_Oral <- Dia_US_Doses %>% filter(drug_group == "GLP1 Oral") %>% 
  select(generic_name, dayssup, pat_id, weight, from_dt, doses) 

Dia_US_Doses_semaglutide_Oral <- Dia_US_Doses_semaglutide_Oral %>% filter(!is.na(doses))

Dia_US_Doses_semaglutide_Oral_summary <- Dia_US_Doses_semaglutide_Oral %>% group_by(pat_id) %>% arrange(pat_id, from_dt) %>% 
  mutate(index = from_dt-lag(from_dt)) %>% ungroup() %>% mutate(index = as.numeric(index)) %>%
  mutate(index = ifelse(is.na(index), 0, index)) %>% group_by(pat_id) %>%
  mutate(time_progression = cumsum(index)) %>% select(-c(generic_name, dayssup, index))

Dia_US_Doses_semaglutide_Oral_summary %>% ungroup() %>% select(pat_id, doses, time_progression) %>%
  ggplot(aes(x=time_progression, y=doses, fill=doses, colour=-doses))+
  geom_jitter(height =2, width = 1.5, show.legend = F, alpha=0.2, size=2)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        text = element_text(size = 20))+
  xlab("\nTime since therapy initiation (days)")+
  ylab("Dosage Prescribed\n")+
  xlim(0,600) +
  scale_y_continuous(breaks = c(3, 7, 14))

length(unique(Dia_US_Doses_semaglutide_Oral_summary$pat_id)) #5102

Dia_US_Doses_semaglutide_Oral_summary %>% ungroup() %>% 
  left_join(Dia_US_Doses %>% select(pat_id, weight) %>% distinct()) %>%
  group_by(pat_id) %>% slice_head() %>% ungroup() %>% group_by(doses) %>% summarise(n=sum(as.numeric(weight)))


Dia_US_Doses_semaglutide_Oral_summary %>% ungroup() %>% 
  left_join(Dia_US_Doses %>% select(pat_id, weight) %>% distinct()) %>%
  group_by(pat_id) %>% filter(time_progression <=30) %>% slice_tail() %>% ungroup() %>% group_by(doses) %>% summarise(n=sum(as.numeric(weight)))

Dia_US_Doses_semaglutide_Oral_summary %>% ungroup() %>% 
  left_join(Dia_US_Doses %>% select(pat_id, weight) %>% distinct()) %>%
  group_by(pat_id) %>% filter(time_progression <=60) %>% slice_tail() %>% ungroup() %>% group_by(doses) %>% summarise(n=sum(as.numeric(weight)))

Dia_US_Doses_semaglutide_Oral_summary %>% ungroup() %>% 
  left_join(Dia_US_Doses %>% select(pat_id, weight) %>% distinct()) %>%
  group_by(pat_id) %>% filter(time_progression <=90) %>% slice_tail() %>% ungroup() %>% group_by(doses) %>% summarise(n=sum(as.numeric(weight)))

Dia_US_Doses_semaglutide_Oral_summary %>% ungroup() %>% 
  left_join(Dia_US_Doses %>% select(pat_id, weight) %>% distinct()) %>%
  group_by(pat_id) %>% filter(time_progression <=120) %>% slice_tail() %>% ungroup() %>% group_by(doses) %>% summarise(n=sum(as.numeric(weight)))

Dia_US_Doses_semaglutide_Oral_summary %>% ungroup() %>% 
  left_join(Dia_US_Doses %>% select(pat_id, weight) %>% distinct()) %>%
  group_by(pat_id) %>% filter(time_progression <=150) %>% slice_tail() %>% ungroup() %>% group_by(doses) %>% summarise(n=sum(as.numeric(weight)))

Dia_US_Doses_semaglutide_Oral_summary %>% ungroup() %>% 
  left_join(Dia_US_Doses %>% select(pat_id, weight) %>% distinct()) %>%
  group_by(pat_id) %>% filter(time_progression <=180) %>% slice_tail() %>% ungroup() %>% group_by(doses) %>% summarise(n=sum(as.numeric(weight)))

Dia_US_Doses_semaglutide_Oral_summary %>% ungroup() %>% 
  left_join(Dia_US_Doses %>% select(pat_id, weight) %>% distinct()) %>%
  group_by(pat_id) %>% filter(time_progression <=210) %>% slice_tail() %>% ungroup() %>% group_by(doses) %>% summarise(n=sum(as.numeric(weight)))

# ----------------------------
# Number treated, GLP1 and Oral GLP1 Year-over-year -----------------------

Dia_US_Doses <- fread("DIA Doses 1.1/DIA Doses.txt")
Treatment_exp_Vector <- fread("DIA Analysis Results 1.1/Treatment_exp_Vector.txt")
Dia_US_Doses <- Treatment_exp_Vector %>% inner_join(Dia_US_Doses, by=c("patient"="pat_id"))
Dia_US_Doses <- Dia_US_Doses %>% select(drug_class, patient , weight.x, from_dt, paid) %>% filter(paid=="P")

Dia_US_Doses %>% filter(from_dt>="2018-01-01"&from_dt<="2018-12-31") %>% select(patient, weight.x) %>% distinct() %>% summarise(n=sum(weight.x)) # 17716844
Dia_US_Doses %>% filter(from_dt>="2019-01-01"&from_dt<="2019-12-31") %>% select(patient, weight.x) %>% distinct() %>% summarise(n=sum(weight.x)) # 18769868
Dia_US_Doses %>% filter(from_dt>="2020-01-01"&from_dt<="2020-12-31") %>% select(patient, weight.x) %>% distinct() %>% summarise(n=sum(weight.x)) # 19442917
Dia_US_Doses %>% filter(from_dt>="2021-01-01"&from_dt<="2021-12-31") %>% select(patient, weight.x) %>% distinct() %>% summarise(n=sum(weight.x)) # 21608708
Dia_US_Doses %>% filter(from_dt>="2021-05-31"&from_dt<="2022-04-30") %>% select(patient, weight.x) %>% distinct() %>% summarise(n=sum(weight.x)) # 21867948

Dia_US_Doses %>% filter(grepl("GLP", drug_class)) %>% filter(from_dt>="2018-01-01"&from_dt<="2018-12-31") %>% select(patient, weight.x) %>% distinct() %>% summarise(n=sum(weight.x)) # 2040236
Dia_US_Doses %>% filter(grepl("GLP", drug_class)) %>% filter(from_dt>="2019-01-01"&from_dt<="2019-12-31") %>% select(patient, weight.x) %>% distinct() %>% summarise(n=sum(weight.x)) # 2598573
Dia_US_Doses %>% filter(grepl("GLP", drug_class)) %>% filter(from_dt>="2020-01-01"&from_dt<="2020-12-31") %>% select(patient, weight.x) %>% distinct() %>% summarise(n=sum(weight.x)) # 3162672
Dia_US_Doses %>% filter(grepl("GLP", drug_class)) %>% filter(from_dt>="2021-01-01"&from_dt<="2021-12-31") %>% select(patient, weight.x) %>% distinct() %>% summarise(n=sum(weight.x)) # 4413704
Dia_US_Doses %>% filter(grepl("GLP", drug_class)) %>% filter(from_dt>="2021-05-31"&from_dt<="2022-04-30") %>% select(patient, weight.x) %>% distinct() %>% summarise(n=sum(weight.x)) # 4805453

unique(Dia_US_Doses$drug_class)
Dia_US_Doses %>% filter(drug_class=="GLP1 Oral") %>% filter(from_dt>="2018-01-01"&from_dt<="2018-12-31") %>% select(patient, weight.x) %>% distinct() %>% summarise(n=sum(weight.x)) # 0
Dia_US_Doses %>% filter(drug_class=="GLP1 Oral") %>% filter(from_dt>="2019-01-01"&from_dt<="2019-12-31") %>% select(patient, weight.x) %>% distinct() %>% summarise(n=sum(weight.x)) # 9671.75
Dia_US_Doses %>% filter(drug_class=="GLP1 Oral") %>% filter(from_dt>="2020-01-01"&from_dt<="2020-12-31") %>% select(patient, weight.x) %>% distinct() %>% summarise(n=sum(weight.x)) # 147802.9
Dia_US_Doses %>% filter(drug_class=="GLP1 Oral") %>% filter(from_dt>="2021-01-01"&from_dt<="2021-12-31") %>% select(patient, weight.x) %>% distinct() %>% summarise(n=sum(weight.x)) # 347046.4
Dia_US_Doses %>% filter(drug_class=="GLP1 Oral") %>% filter(from_dt>="2021-05-31"&from_dt<="2022-04-30") %>% select(patient, weight.x) %>% distinct() %>% summarise(n=sum(weight.x)) # 396135

# ---------------------
# Diagnosed ON each year -----------------------------------
DANU_Events <- fread("DANU Events 1.1/DANU Events.txt")
names(DANU_Events)[1] <- "patient"
DIA_Drug_Histories <- fread("DIA Analysis Results 1.1/DIA Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
DIA_Drug_Histories <- DIA_Drug_Histories[,2]
DANU_Events <- DANU_Events %>% inner_join(DIA_Drug_Histories)
DANU_Diagnosis_Codes <- DANU_Diagnosis_Codes %>% filter(diagnosis=="Diabetes"&source=="Diagnosis") %>% select(code) %>% distinct()
DANU_Events <- DANU_Diagnosis_Codes %>% inner_join(DANU_Events) %>% select(-prov)


DANU_Events %>% filter(claimed>="2018-01-01"&claimed<="2018-12-31") %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(weight)) # 24618080
DANU_Events %>% filter(claimed>="2019-01-01"&claimed<="2019-12-31") %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(weight)) # 25786206
DANU_Events %>% filter(claimed>="2020-01-01"&claimed<="2020-12-31") %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(weight)) # 26134803
DANU_Events %>% filter(claimed>="2021-01-01"&claimed<="2021-12-31") %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(weight)) # 27896157
DANU_Events %>% filter(claimed>="2021-05-31"&claimed<="2022-04-30")  %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(weight)) # 27720607

# ---------------------------------
# No. diabetes diagnoses - Treatment experienced vs no treatment (OLD vs NEW data) --------------------

DANU_Events <- fread("DANU Events 1.1/DANU Events.txt")
names(DANU_Events)[1] <- "patient"
DIA_Drug_Histories <- fread("DIA Analysis Results 1.1/DIA Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
DIA_Drug_Histories <- DIA_Drug_Histories[,2]
DANU_Events <- DANU_Events %>% inner_join(DIA_Drug_Histories)
DANU_Diagnosis_Codes <- fread("DANU Demographics 1.1/DANU Diagnosis Codes.txt", integer64 = "character", stringsAsFactors = F)
DANU_Diagnosis_Codes <- DANU_Diagnosis_Codes %>% filter(diagnosis=="Diabetes"&source=="Diagnosis") %>% select(code) %>% distinct()
DANU_Events <- DANU_Diagnosis_Codes %>% inner_join(DANU_Events) %>% select(-prov)

DANU_Events <- DANU_Events %>% group_by(patient, weight) %>% count()

DIA_Drug_Histories <- fread("DIA Analysis Results 1.1/DIA Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
DIA_Drug_Histories <- DIA_Drug_Histories[,2:3]
DIA_Drug_Histories$group <- "No_treatment"
sum(DIA_Drug_Histories$weight)
DIA_Drug_Histories <- DIA_Drug_Histories %>% anti_join(Treatment_exp_Vector %>% select(patient))
sum(DIA_Drug_Histories$weight) # 15909622
DIA_Drug_Histories <- DIA_Drug_Histories %>% left_join(DANU_Events) %>% mutate(n=ifelse(is.na(n),0,n))
weighted.mean(DIA_Drug_Histories$n, DIA_Drug_Histories$weight)

Treatment_exp_Vector <- fread("DIA Analysis Results 1.1/Treatment_exp_Vector.txt")
Treatment_exp_Vector$group <- "Treated"
sum(Treatment_exp_Vector$weight) # 30120087
Treatment_exp_Vector <- Treatment_exp_Vector %>% left_join(DANU_Events) %>% mutate(n=ifelse(is.na(n),0,n))
weighted.mean(Treatment_exp_Vector$n, Treatment_exp_Vector$weight)

NEW_data <- DIA_Drug_Histories %>% bind_rows(Treatment_exp_Vector)

DIA_Drug_Histories %>% bind_rows(Treatment_exp_Vector) %>%
  #mutate(n=ifelse(n>=10, "+10", ifelse(n>=6, "+6", ifelse(n>=2, "+2", n)))) %>%
  ggplot(aes(n, colour=group, fill=group)) +
  geom_density(alpha=0.5) +
  ylab("Patient density \n") + xlab("\n No. Diabetes Diagnoses") +
  xlim(0,200) +
  theme_minimal() +
  facet_wrap(~group, scales = "free_y") +
  ggsci::scale_color_lancet() + ggsci::scale_fill_lancet()



DANU_Events <- fread("./DANU Events.txt")
names(DANU_Events)[1] <- "patient"
DIA_Drug_Histories <- fread("./DIA Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
DIA_Drug_Histories <- DIA_Drug_Histories[,2]
DANU_Events <- DANU_Events %>% inner_join(DIA_Drug_Histories)
DANU_Diagnosis_Codes <- fread("./DANU Diagnosis Codes.txt", integer64 = "character", stringsAsFactors = F)
DANU_Diagnosis_Codes <- DANU_Diagnosis_Codes %>% filter(diagnosis=="Diabetes"&source=="Diagnosis") %>% select(code) %>% distinct()
DANU_Events <- DANU_Diagnosis_Codes %>% inner_join(DANU_Events) 

DANU_Events <- DANU_Events %>% group_by(patient, weight) %>% count()

Treatment_exp_Vector <- fread("./Treatment_exp_Vector.txt")
Treatment_exp_Vector$group <- "Treated"
sum(Treatment_exp_Vector$weight) # 30625690
Treatment_exp_Vector <- Treatment_exp_Vector %>% left_join(DANU_Events) %>% mutate(n=ifelse(is.na(n),0,n))
weighted.mean(Treatment_exp_Vector$n, Treatment_exp_Vector$weight) # 46.10145


DIA_Drug_Histories <- fread("./DIA Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
DIA_Drug_Histories <- DIA_Drug_Histories[,2:3]
DIA_Drug_Histories$group <- "No_treatment"
sum(DIA_Drug_Histories$weight)
Treatment_exp_Vector <- fread("./Treatment_exp_Vector.txt")
DIA_Drug_Histories <- DIA_Drug_Histories %>% anti_join(Treatment_exp_Vector %>% select(patient))
sum(DIA_Drug_Histories$weight) # 17618734
DIA_Drug_Histories <- DIA_Drug_Histories %>% left_join(DANU_Events) %>% mutate(n=ifelse(is.na(n),0,n))
weighted.mean(DIA_Drug_Histories$n, DIA_Drug_Histories$weight) # 11.00344
Treatment_exp_Vector$group <- "Treated"
sum(Treatment_exp_Vector$weight) # 30625690
Treatment_exp_Vector <- Treatment_exp_Vector %>% left_join(DANU_Events) %>% mutate(n=ifelse(is.na(n),0,n))
weighted.mean(Treatment_exp_Vector$n, Treatment_exp_Vector$weight) # 46.10145


OLD_data <- DIA_Drug_Histories %>% bind_rows(Treatment_exp_Vector)


OLD_data %>%
  #mutate(n=ifelse(n>=10, "+10", ifelse(n>=6, "+6", ifelse(n>=2, "+2", n)))) %>%
  ggplot(aes(n, colour=group, fill=group)) +
  geom_density(alpha=0.5) +
  ylab("Patient density \n") + xlab("\n No. Diabetes Diagnoses") +
  xlim(0,200) +
  theme_minimal() +
  facet_wrap(~group, scales = "free_y") +
  ggsci::scale_color_lancet() + ggsci::scale_fill_lancet()

# --------------------------

# Number of months until each class moving window ------------
# Drugs / Classes to lookup
DANU_Ingredients       <- fread("DIA Analysis Results 1.1/DANU Ingredients.txt", integer64 = "character", stringsAsFactors = F)
DANU_Ingredients       <- DANU_Ingredients %>%  separate(drug_id, c('class', 'molecule'))

string_Biguanide    <- paste0("\\b(",paste0(DANU_Ingredients$molecule[DANU_Ingredients$drug_group == "Biguanide"], collapse = "|"),")\\b")
string_Antidiabetic    <- paste0("\\b(",paste0(DANU_Ingredients$molecule[DANU_Ingredients$drug_group == "Antidiabetic"], collapse = "|"),")\\b")
string_DPP4    <- paste0("\\b(",paste0(DANU_Ingredients$molecule[DANU_Ingredients$drug_group == "DPP4"], collapse = "|"),")\\b")
string_SGLT2    <- paste0("\\b(",paste0(DANU_Ingredients$molecule[DANU_Ingredients$drug_group == "SGLT2"], collapse = "|"),")\\b")
string_Insulin    <- paste0("\\b(",paste0(DANU_Ingredients$molecule[DANU_Ingredients$drug_group == "Insulin"], collapse = "|"),")\\b")
string_OralGLP1 <- paste0("\\b(",paste0(DANU_Ingredients$molecule[DANU_Ingredients$drug_group == "GLP1 Oral"], collapse = "|"),")\\b")
string_InjectableGLP1 <- paste0("\\b(",paste0(DANU_Ingredients$molecule[DANU_Ingredients$drug_group == "GLP1 Injectable"], collapse = "|"),")\\b")

# All pats
DIA_Drug_Histories     <- fread("DIA Analysis Results 1.1/DIA Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
Treatment_exp_Vector   <-fread("DIA Analysis Results 1.1/Treatment_exp_Vector.txt")
DIA_Drug_Histories     <- Treatment_exp_Vector %>% left_join(DIA_Drug_Histories)
DIA_Drug_Histories     <- DIA_Drug_Histories %>% select(-c(disease))

DIA_Drug_Histories       <- gather(DIA_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
DIA_Drug_Histories$Month <- as.character(DIA_Drug_Histories$Month)
DIA_Drug_Histories$Month <- parse_number(DIA_Drug_Histories$Month)

DIA_Drug_Histories <- DIA_Drug_Histories %>% filter(Month<=36&Month>=25)
DIA_Drug_Histories <-  DIA_Drug_Histories %>% filter(Drugs == "-")

Naive <- DIA_Drug_Histories %>% group_by(patient) %>% count() %>% filter(n==12) %>% select(patient)


# Number of months 
                             
DIA_Drug_Histories     <- fread("DIA Analysis Results 1.1/DIA Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
DIA_Drug_Histories     <- DIA_Drug_Histories %>% select(-c(disease))
Treatment_exp_Vector   <-fread("DIA Analysis Results 1.1/Treatment_exp_Vector.txt")
DIA_Drug_Histories     <- Treatment_exp_Vector %>% left_join(DIA_Drug_Histories)
DIA_Drug_Histories <- Naive %>% inner_join(DIA_Drug_Histories)
DIA_Drug_Histories     <- gather(DIA_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
DIA_Drug_Histories$Month <- as.character(DIA_Drug_Histories$Month)
DIA_Drug_Histories$Month <- parse_number(DIA_Drug_Histories$Month)
DIA_Drug_Histories <- DIA_Drug_Histories %>% filter(Month>=36&Month<=60)

# Time to first  Biguanide
DIA_Drug_Histories_Biguanide <- DIA_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_Biguanide,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_Biguanide,Drugs)) else NA) 
DIA_Drug_Histories_Biguanide <- DIA_Drug_Histories_Biguanide %>% group_by(patient, weight) %>% count() %>% arrange(-n)
DIA_Drug_Histories_Biguanide %>% ungroup() %>% summarise(pats=sum(weight)) 
DIA_Drug_Histories_Biguanide %>% ungroup() %>% summarise(mean=weighted.mean(n, weight))  # 1.30 # 1.36 # 1.39


# Time to first  Antidiabetic
DIA_Drug_Histories_Antidiabetic <- DIA_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_Antidiabetic,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_Antidiabetic,Drugs)) else NA) 
DIA_Drug_Histories_Antidiabetic <- DIA_Drug_Histories_Antidiabetic %>% group_by(patient, weight) %>% count() %>% arrange(-n)
DIA_Drug_Histories_Antidiabetic %>% ungroup() %>%  summarise(pats=sum(weight)) 
DIA_Drug_Histories_Antidiabetic %>% ungroup() %>% summarise(mean=weighted.mean(n, weight))  # 2.92 # 2.97 # 2.96

# Time to first  DPP4
DIA_Drug_Histories_DPP4 <- DIA_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_DPP4,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_DPP4,Drugs)) else NA) 
DIA_Drug_Histories_DPP4 <- DIA_Drug_Histories_DPP4 %>% group_by(patient, weight) %>% count() %>% arrange(-n)
DIA_Drug_Histories_DPP4 %>% ungroup() %>%  summarise(pats=sum(weight)) 
DIA_Drug_Histories_DPP4 %>% ungroup() %>% summarise(mean=weighted.mean(n, weight))  # 3.38 # 3.28 # 3.09

# Time to first  SGLT2
DIA_Drug_Histories_SGLT2 <- DIA_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_SGLT2,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_SGLT2,Drugs)) else NA) 
DIA_Drug_Histories_SGLT2 <- DIA_Drug_Histories_SGLT2 %>% group_by(patient, weight) %>% count() %>% arrange(-n)
data.frame(DIA_Drug_Histories_SGLT2 %>% ungroup() %>%  summarise(pats=sum(weight))) 
DIA_Drug_Histories_SGLT2 %>% ungroup() %>% summarise(mean=weighted.mean(n, weight))  # 4.62 # 4.30 # 4.11
 
# Time to first  Insulin
DIA_Drug_Histories_Insulin <- DIA_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_Insulin,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_Insulin,Drugs)) else NA) 
DIA_Drug_Histories_Insulin <- DIA_Drug_Histories_Insulin %>% group_by(patient, weight) %>% count() %>% arrange(-n)
DIA_Drug_Histories_Insulin %>% ungroup() %>% summarise(pats=sum(weight)) 
DIA_Drug_Histories_Insulin %>% ungroup() %>% summarise(mean=weighted.mean(n, weight))  # 1.88 # 1.92 # 1.94

# Time to first  OralGLP1
DIA_Drug_Histories_OralGLP1 <- DIA_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_OralGLP1,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_OralGLP1,Drugs)) else NA) 
DIA_Drug_Histories_OralGLP1 <- DIA_Drug_Histories_OralGLP1 %>% group_by(patient, weight) %>% count() %>% arrange(-n)
DIA_Drug_Histories_OralGLP1 %>% ungroup() %>%  summarise(pats=sum(weight)) 
DIA_Drug_Histories_OralGLP1 %>% ungroup() %>% summarise(mean=weighted.mean(n, weight))  # 6.85 # 5.64 # 4.94
 
# Time to first  InjectableGLP1
DIA_Drug_Histories_InjectableGLP1 <- DIA_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_InjectableGLP1,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_InjectableGLP1,Drugs)) else NA) 
DIA_Drug_Histories_InjectableGLP1 <- DIA_Drug_Histories_InjectableGLP1 %>% group_by(patient, weight) %>% count() %>% arrange(-n)
DIA_Drug_Histories_InjectableGLP1 %>% ungroup() %>%  summarise(pats=sum(weight))
DIA_Drug_Histories_InjectableGLP1 %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) # 3.98 # 3.63 # 3.71

# ---------------------------

# Oral GLP1 starts, number combinations tried until then ---------------------

DANU_Ingredients <- fread("DIA Analysis Results 1.1/DANU Ingredients.txt", integer64 = "character", stringsAsFactors = F)
DANU_Ingredients <- DANU_Ingredients %>%  separate(drug_id, c('class', 'molecule'))

string_Biguanide       <- paste0("\\b(",paste0(DANU_Ingredients$molecule[DANU_Ingredients$drug_group == "Biguanide"], collapse = "|"),")\\b")
string_Antidiabetic    <- paste0("\\b(",paste0(DANU_Ingredients$molecule[DANU_Ingredients$drug_group == "Antidiabetic"], collapse = "|"),")\\b")
string_DPP4            <- paste0("\\b(",paste0(DANU_Ingredients$molecule[DANU_Ingredients$drug_group == "DPP4"], collapse = "|"),")\\b")
string_SGLT2           <- paste0("\\b(",paste0(DANU_Ingredients$molecule[DANU_Ingredients$drug_group == "SGLT2"], collapse = "|"),")\\b")
string_Insulin         <- paste0("\\b(",paste0(DANU_Ingredients$molecule[DANU_Ingredients$drug_group == "Insulin"], collapse = "|"),")\\b")
string_OralGLP1        <- paste0("\\b(",paste0(DANU_Ingredients$molecule[DANU_Ingredients$drug_group == "GLP1 Oral"], collapse = "|"),")\\b")
string_InjectableGLP1  <- paste0("\\b(",paste0(DANU_Ingredients$molecule[DANU_Ingredients$drug_group == "GLP1 Injectable"], collapse = "|"),")\\b")

DANU_Ingredients <- DANU_Ingredients %>%  separate(drug_id, c('class', 'molecule'))
DANU_Ingredients <- DANU_Ingredients %>% select(molecule, drug_group) 

DIA_Drug_Histories <- fread("DIA Analysis Results 1.1/DIA Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
Treatment_exp_Vector <- fread("DIA Analysis Results 1.1/Treatment_exp_Vector.txt")
DIA_Drug_Histories <- DIA_Drug_Histories %>% select(-disease) %>% gather(Month, Drugs, month1:month60) %>% filter(Drugs != "-")
DIA_Drug_Histories <- Treatment_exp_Vector %>% inner_join(DIA_Drug_Histories)
DIA_Drug_Histories <- separate_rows(DIA_Drug_Histories, Drugs, sep = ",", convert=T)
DIA_Drug_Histories <- DIA_Drug_Histories %>% left_join(DANU_Ingredients %>% mutate(molecule=as.numeric(molecule)), by=c("Drugs"="molecule"))
DIA_Drug_Histories <- DIA_Drug_Histories %>% select(-Drugs) %>% distinct()

DIA_Drug_Histories <- DIA_Drug_Histories %>% mutate(drug_group=ifelse(drug_group=="Insulin", "I",
                                                ifelse(drug_group=="Biguanide", "b",
                                                       ifelse(drug_group=="SGLT2", "S",
                                                              ifelse(drug_group=="Antidiabetic", "d",
                                                                     ifelse(drug_group=="DPP4", "D",
                                                                            ifelse(drug_group=="GLP1 Injectable", "G", "g")))))))

DIA_Drug_Histories <- DIA_Drug_Histories %>% arrange(patient, weight, Month, drug_group) %>%
  group_by(patient, weight, Month) %>% mutate(drug_group=paste(drug_group, collapse=",")) 

DIA_Drug_Histories <- DIA_Drug_Histories %>% ungroup() 
DIA_Drug_Histories$Month <- as.character(DIA_Drug_Histories$Month)
DIA_Drug_Histories$Month <- parse_number(DIA_Drug_Histories$Month)
DIA_Drug_Histories %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(weight))

temp <- DIA_Drug_Histories

temp <- temp %>% select(patient, weight, drug_group) %>% distinct() %>% 
   group_by(patient, weight) %>% mutate(grp = rle(drug_group)$lengths %>% {rep(seq(length(.)), .)})

temp <- temp %>% left_join(DIA_Drug_Histories) 

temp <- temp %>% arrange(patient, weight, Month, drug_group, grp)
temp <- temp %>% distinct()

DIA_Flows_Aux_Long <- fread("DIA Analysis Results 1.1/DIA_Flows_Aux_Long.txt", integer64 = "character", stringsAsFactors = F)
Treatment_exp_Vector <- fread("DIA Analysis Results 1.1/Treatment_exp_Vector.txt")
DIA_Flows_Aux_Long <- Treatment_exp_Vector %>% inner_join(DIA_Flows_Aux_Long)
DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% select(patient, weight, p1, p2, d1, d2) 

DIA_Flows_Aux_Long %>% filter(!grepl(string_OralGLP1, d1) & grepl(string_OralGLP1, d2)) %>% group_by(patient) %>% filter(p1==min(p1) & p1 >=24) %>%
  select(patient, weight, p1) %>%
  inner_join(temp %>% select(patient, weight, Month, grp), by=c("patient"="patient", "weight"="weight", "p1"="Month")) %>%
  ungroup() %>% mutate(grp=ifelse(grp>=10, 10, grp)) %>% group_by(grp) %>% summarise(n=sum(weight))




# -----------------------------

# Class Penetrance by  number of classes tried -----------------------------------

DANU_Ingredients <- fread("DIA Analysis Results 1.1/DANU Ingredients.txt", integer64 = "character", stringsAsFactors = F)
DANU_Ingredients <- DANU_Ingredients %>%  separate(drug_id, c('class', 'molecule'))

string_Biguanide       <- paste0("\\b(",paste0(DANU_Ingredients$molecule[DANU_Ingredients$drug_group == "Biguanide"], collapse = "|"),")\\b")
string_Antidiabetic    <- paste0("\\b(",paste0(DANU_Ingredients$molecule[DANU_Ingredients$drug_group == "Antidiabetic"], collapse = "|"),")\\b")
string_DPP4            <- paste0("\\b(",paste0(DANU_Ingredients$molecule[DANU_Ingredients$drug_group == "DPP4"], collapse = "|"),")\\b")
string_SGLT2           <- paste0("\\b(",paste0(DANU_Ingredients$molecule[DANU_Ingredients$drug_group == "SGLT2"], collapse = "|"),")\\b")
string_Insulin         <- paste0("\\b(",paste0(DANU_Ingredients$molecule[DANU_Ingredients$drug_group == "Insulin"], collapse = "|"),")\\b")
string_OralGLP1        <- paste0("\\b(",paste0(DANU_Ingredients$molecule[DANU_Ingredients$drug_group == "GLP1 Oral"], collapse = "|"),")\\b")
string_InjectableGLP1  <- paste0("\\b(",paste0(DANU_Ingredients$molecule[DANU_Ingredients$drug_group == "GLP1 Injectable"], collapse = "|"),")\\b")


DIA_Flows_Aux_Long <- fread("DIA Analysis Results 1.1/DIA_Flows_Aux_Long.txt", integer64 = "character", stringsAsFactors = F)
Treatment_exp_Vector <- fread("DIA Analysis Results 1.1/Treatment_exp_Vector.txt")
DIA_Flows_Aux_Long <- Treatment_exp_Vector %>% inner_join(DIA_Flows_Aux_Long)
DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% select(patient, weight, p1, p2, d1, d2) 

DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% mutate(BiguanideExp = ifelse(grepl(string_Biguanide,d1)|grepl(string_Biguanide,d2),1,0))
DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% group_by(patient) %>% mutate(BiguanideExp = cumsum(BiguanideExp))
DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% group_by(patient) %>% mutate(BiguanideExp = ifelse(BiguanideExp==0,0,1))

DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% mutate(AntidiabeticExp = ifelse(grepl(string_Antidiabetic,d1)|grepl(string_Antidiabetic,d2),1,0))
DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% group_by(patient) %>% mutate(AntidiabeticExp = cumsum(AntidiabeticExp))
DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% group_by(patient) %>% mutate(AntidiabeticExp = ifelse(AntidiabeticExp==0,0,1))
 
DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% mutate(DPP4Exp = ifelse(grepl(string_DPP4,d1)|grepl(string_DPP4,d2),1,0))
DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% group_by(patient) %>% mutate(DPP4Exp = cumsum(DPP4Exp))
DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% group_by(patient) %>% mutate(DPP4Exp = ifelse(DPP4Exp==0,0,1))

DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% mutate(SGLT2Exp = ifelse(grepl(string_SGLT2,d1)|grepl(string_SGLT2,d2),1,0))
DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% group_by(patient) %>% mutate(SGLT2Exp = cumsum(SGLT2Exp))
DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% group_by(patient) %>% mutate(SGLT2Exp = ifelse(SGLT2Exp==0,0,1))
 
DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% mutate(OralExp = ifelse(grepl(string_OralGLP1,d1)|grepl(string_OralGLP1,d2),1,0))
DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% group_by(patient) %>% mutate(OralExp = ifelse(OralExp==0,0,1))
 
DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% mutate(InjExp = ifelse(grepl(string_InjectableGLP1,d1)|grepl(string_InjectableGLP1,d2),1,0))
DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% group_by(patient) %>% mutate(InjExp = cumsum(InjExp))
DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% group_by(patient) %>% mutate(InjExp = ifelse(InjExp==0,0,1))

DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% mutate(InsulinExp = ifelse(grepl(string_Insulin,d1)|grepl(string_Insulin,d2),1,0))
DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% group_by(patient) %>% mutate(InsulinExp = cumsum(InsulinExp))
DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% group_by(patient) %>% mutate(InsulinExp = ifelse(InsulinExp==0,0,1))

DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% filter(p2==60) %>% mutate(lines=BiguanideExp+AntidiabeticExp+DPP4Exp+SGLT2Exp+OralExp+InjExp+InsulinExp)
DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% select(-c(p1,p2,d1,d2))

DIA_Flows_Aux_Long %>% mutate(lines=ifelse(lines>=5,5,lines)) %>%
  group_by(lines) %>% summarise(n=sum(weight))

data.frame(data.frame(DIA_Flows_Aux_Long %>% mutate(lines=ifelse(lines>=5,5,lines)) %>%
  group_by(lines, BiguanideExp, AntidiabeticExp, DPP4Exp, SGLT2Exp, OralExp, InjExp, InsulinExp) %>%
  summarise(n=sum(weight)) %>%
  gather(Group, Exp, BiguanideExp:InsulinExp) %>% filter(Exp==1)) %>%
  group_by(lines, Group) %>% summarise(n=sum(n))) %>%
  spread(key=Group, value=n)

# --------------------------------
# NEW GLP1-based Architecture --------------------------------------------

DIA_Flows_Aux_Long <- fread("DIA Analysis Results 1.1/DIA_Flows_Aux_Long.txt", integer64 = "character", stringsAsFactors = F)
Treatment_exp_Vector <- fread("DIA Analysis Results 1.1/Treatment_exp_Vector.txt")
DIA_Flows_Aux_Long <- Treatment_exp_Vector %>% inner_join(DIA_Flows_Aux_Long)
DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% select(-c(disease,s1,s2))
DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% select(-c(starts,stops,re_starts, p1_RxExp))
DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% arrange(patient, p1)
data.frame(DIA_Flows_Aux_Long)

# DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% group_by(patient) %>% mutate(d1=ifelse(d1=="-" & lag(d1)=="-" & lag(lag(d1))!="-",lag(lag(d1)),d1 ))
# DIA_Flows_Aux_Long[is.na(DIA_Flows_Aux_Long)] <- "-"
# DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% group_by(patient) %>% mutate(d2=ifelse(d2=="-" & lag(d2)=="-" & lag(lag(d2))!="-",lag(lag(d2)),d2 ))
# DIA_Flows_Aux_Long[is.na(DIA_Flows_Aux_Long)] <- "-"

# DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% group_by(patient) %>% mutate(d1=ifelse(d1=="-" & lag(d1)!="-",lag(d1),d1 ))
# DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% group_by(patient) %>% mutate(d2=ifelse(d2=="-" & lag(d2)!="-",lag(d2),d2 ))
# DIA_Flows_Aux_Long[is.na(DIA_Flows_Aux_Long)] <- "-"

DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% mutate(flow=ifelse(flow==1 & (d1 == d2), 0, flow)) %>% mutate(flow=ifelse(flow==0 & (d1!=d2), 1, flow))

DANU_Ingredients       <- fread("DIA Analysis Results 1.1/DANU Ingredients.txt", integer64 = "character", stringsAsFactors = F)
DANU_Ingredients       <- DANU_Ingredients %>%  separate(drug_id, c('class', 'molecule'))
string_Biguanide    <- paste0("\\b(",paste0(DANU_Ingredients$molecule[DANU_Ingredients$drug_group == "Biguanide"], collapse = "|"),")\\b")
string_Antidiabetic    <- paste0("\\b(",paste0(DANU_Ingredients$molecule[DANU_Ingredients$drug_group == "Antidiabetic"], collapse = "|"),")\\b")
string_DPP4    <- paste0("\\b(",paste0(DANU_Ingredients$molecule[DANU_Ingredients$drug_group == "DPP4"], collapse = "|"),")\\b")
string_SGLT2    <- paste0("\\b(",paste0(DANU_Ingredients$molecule[DANU_Ingredients$drug_group == "SGLT2"], collapse = "|"),")\\b")
string_Insulin    <- paste0("\\b(",paste0(DANU_Ingredients$molecule[DANU_Ingredients$drug_group == "Insulin"], collapse = "|"),")\\b")
string_OralGLP1 <- paste0("\\b(",paste0(DANU_Ingredients$molecule[DANU_Ingredients$drug_group == "GLP1 Oral"], collapse = "|"),")\\b")
string_InjectableGLP1 <- paste0("\\b(",paste0(DANU_Ingredients$molecule[DANU_Ingredients$drug_group == "GLP1 Injectable"], collapse = "|"),")\\b")


DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>%
  mutate(s1=ifelse(grepl(string_InjectableGLP1, d1), "GLP1_Inj",
                   ifelse(grepl(string_OralGLP1, d1), "GLP1_Oral",
                   ifelse(grepl("-", d1), "Lapsed", "Other")))) %>%
  mutate(s2=ifelse(grepl(string_InjectableGLP1, d2), "GLP1_Inj",
                   ifelse(grepl(string_OralGLP1, d2), "GLP1_Oral",
                   ifelse(grepl("-", d2), "Lapsed", "Other"))))

DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% group_by(patient, weight) %>% 
  mutate(s2_GLP1_exp=ifelse(cumsum(s2=="GLP1_Inj")>0|cumsum(s2=="GLP1_Oral")>0,1,0)) %>%  
  mutate(s1_GLP1_exp=ifelse(cumsum(s1=="GLP1_Inj")>0|cumsum(s1=="GLP1_Oral")>0,1,0))

DIA_Flows_Aux_Long %>% filter(p2==60) %>%
  group_by(s2, s2_GLP1_exp) %>% summarise(n=sum(weight))

# 1 GLP1_Inj            1  3,318,178.
# 2 GLP1_Oral           1   228,223.
# 3 Lapsed              0 10,694,350.
# 4 Lapsed              1  1,656,308.
# 5 Other               0 12,655,982.
# 6 Other               1  1,567,046.


DIA_Flows_Aux_Long %>% filter(p2>=49 & flow==1) %>%
  mutate(s1=paste(s1, s1_GLP1_exp, sep="_")) %>%
  mutate(s2=paste(s2, s2_GLP1_exp, sep="_")) %>%
  group_by(s1, s2) %>% summarise(n=sum(weight)) %>%
  ungroup() %>% spread(key=s2, value=n)


#   s1          GLP1_Inj_1 GLP1_Oral_1 Lapsed_0 Lapsed_1   Other_0  Other_1
# 1 GLP1_Inj_1    4638523.      20836.      0   816942.       0  1344394.
# 2 GLP1_Oral_1     22879.     206445.      0    76798.       0   138051.
# 3 Lapsed_0       654673.      68577.      0       0   8263606.      0 
# 4 Lapsed_1       492964.      32307.      0       0         0   694826
# 5 Other_0        972704.     154177. 7192491.      0  11318020.      0 
# 6 Other_1        888460.      67773.       0  849921           0 2277295

# -------------------------------------------------------
# SGLT2+GLP1 vs GLP1 vs SGLT2 --------------------------------------------------------------------------
Treatment_exp_Vector <- fread("DIA Analysis Results 1.1/Treatment_exp_Vector.txt")
names(Treatment_exp_Vector)[1] <- "patid"


DANU_Demographics <- fread("DANU Demographics 1.1/DANU Demographics.txt", integer64 = "character", stringsAsFactors = F)
DANU_Demographics <- DANU_Demographics %>% select(c(patid, age, gender, diagnosis))
DANU_Demographics <- Treatment_exp_Vector %>% inner_join(DANU_Demographics)


DANU_Measures <- fread("DANU Measures 1.1/DANU Measures.txt",  integer64 = "character", stringsAsFactors = F)
unique(DANU_Measures$test)
DANU_Measures <- Treatment_exp_Vector %>% inner_join(DANU_Measures)
BMI <- DANU_Measures %>% filter(test=="BMI")
HbA1c <- DANU_Measures %>% filter(test=="HbA1c Level")
BMI <- BMI %>% select(patid, value) %>% distinct() %>% group_by(patid ) %>% filter(value==max(value)) %>% slice(1)
names(BMI)[2] <- "BMI"
HbA1c <- HbA1c %>% select(patid, value) %>% distinct() %>% group_by(patid ) %>% filter(value==max(value)) %>% slice(1)
names(HbA1c)[2] <- "HbA1c"

temp <- DANU_Demographics %>% inner_join(BMI) %>% inner_join(HbA1c)


DANU_Ingredients <- fread("DIA Analysis Results 1.1/DANU Ingredients.txt", integer64 = "character", stringsAsFactors = F)
DANU_Ingredients <- DANU_Ingredients %>%  separate(drug_id, c('class', 'molecule'))
string_Biguanide       <- paste0("\\b(",paste0(DANU_Ingredients$molecule[DANU_Ingredients$drug_group == "Biguanide"], collapse = "|"),")\\b")
string_Antidiabetic    <- paste0("\\b(",paste0(DANU_Ingredients$molecule[DANU_Ingredients$drug_group == "Antidiabetic"], collapse = "|"),")\\b")
string_DPP4            <- paste0("\\b(",paste0(DANU_Ingredients$molecule[DANU_Ingredients$drug_group == "DPP4"], collapse = "|"),")\\b")
string_SGLT2           <- paste0("\\b(",paste0(DANU_Ingredients$molecule[DANU_Ingredients$drug_group == "SGLT2"], collapse = "|"),")\\b")
string_Insulin         <- paste0("\\b(",paste0(DANU_Ingredients$molecule[DANU_Ingredients$drug_group == "Insulin"], collapse = "|"),")\\b")
string_OralGLP1        <- paste0("\\b(",paste0(DANU_Ingredients$molecule[DANU_Ingredients$drug_group == "GLP1 Oral"], collapse = "|"),")\\b")
string_InjectableGLP1  <- paste0("\\b(",paste0(DANU_Ingredients$molecule[DANU_Ingredients$drug_group == "GLP1 Injectable"], collapse = "|"),")\\b")

DIA_Flows_Aux_Long <- fread("DIA Analysis Results 1.1/DIA_Flows_Aux_Long.txt", integer64 = "character", stringsAsFactors = F)
DIA_Flows_Aux_Long <- temp %>% select(patid) %>% inner_join(DIA_Flows_Aux_Long, by=c("patid"="patient"))
DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% select(patid, weight, p1, p2, d1, d2, flow) 

DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% mutate(BiguanideExp = ifelse(grepl(string_Biguanide,d1)|grepl(string_Biguanide,d2),1,0))
DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% group_by(patid) %>% mutate(BiguanideExp = cumsum(BiguanideExp))
DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% group_by(patid) %>% mutate(BiguanideExp = ifelse(BiguanideExp==0,0,1))

DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% mutate(AntidiabeticExp = ifelse(grepl(string_Antidiabetic,d1)|grepl(string_Antidiabetic,d2),1,0))
DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% group_by(patid) %>% mutate(AntidiabeticExp = cumsum(AntidiabeticExp))
DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% group_by(patid) %>% mutate(AntidiabeticExp = ifelse(AntidiabeticExp==0,0,1))
 
DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% mutate(DPP4Exp = ifelse(grepl(string_DPP4,d1)|grepl(string_DPP4,d2),1,0))
DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% group_by(patid) %>% mutate(DPP4Exp = cumsum(DPP4Exp))
DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% group_by(patid) %>% mutate(DPP4Exp = ifelse(DPP4Exp==0,0,1))

DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% mutate(SGLT2Exp = ifelse(grepl(string_SGLT2,d1)|grepl(string_SGLT2,d2),1,0))
DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% group_by(patid) %>% mutate(SGLT2Exp = cumsum(SGLT2Exp))
DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% group_by(patid) %>% mutate(SGLT2Exp = ifelse(SGLT2Exp==0,0,1))
 
DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% mutate(OralExp = ifelse(grepl(string_OralGLP1,d1)|grepl(string_OralGLP1,d2),1,0))
DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% group_by(patid) %>% mutate(OralExp = cumsum(OralExp))
DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% group_by(patid) %>% mutate(OralExp = ifelse(OralExp==0,0,1))
 
DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% mutate(InjExp = ifelse(grepl(string_InjectableGLP1,d1)|grepl(string_InjectableGLP1,d2),1,0))
DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% group_by(patid) %>% mutate(InjExp = cumsum(InjExp))
DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% group_by(patid) %>% mutate(InjExp = ifelse(InjExp==0,0,1))

DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% mutate(InsulinExp = ifelse(grepl(string_Insulin,d1)|grepl(string_Insulin,d2),1,0))
DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% group_by(patid) %>% mutate(InsulinExp = cumsum(InsulinExp))
DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% group_by(patid) %>% mutate(InsulinExp = ifelse(InsulinExp==0,0,1))

DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% group_by(patid) %>% mutate(flow = cumsum(flow))

DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% filter(p2==60) %>%  select(-c(p1,p2,d1,d2))

temp <- temp %>% inner_join(DIA_Flows_Aux_Long)

DIA_Drug_Histories <- fread("DIA Analysis Results 1.1/DIA Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
DIA_Drug_Histories <- temp %>% select(patid) %>% left_join(DIA_Drug_Histories, by=c("patid"="patient"))
DIA_Drug_Histories <- gather(DIA_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)

temp <- DIA_Drug_Histories %>% filter(Drugs!="-") %>% select(patid, Drugs) %>% distinct() %>% 
  group_by(patid) %>% count() %>% inner_join(temp) %>% rename("lines"="n")

DIA_Drug_Histories <- DIA_Drug_Histories %>% filter(Drugs!="-") %>% select(patid, Drugs) %>% distinct() 
DIA_Drug_Histories <- separate_rows(DIA_Drug_Histories, Drugs, sep = ",", convert=T)

temp <- DIA_Drug_Histories %>% distinct() %>% group_by(patid) %>% count() %>% inner_join(temp) %>% rename("drugs"="n")



DIA_Comorbidity_Inventories <- fread("DIA Analysis Results 1.1/DIA Comorbidity Inventories.txt")
DIA_Comorbidity_Inventories <- temp %>% select(patid) %>% left_join(DIA_Comorbidity_Inventories)

DIA_Comorbidity_Inventories <- DIA_Comorbidity_Inventories %>% filter(grepl("I", diagnosis) | grepl("N", diagnosis))

DIA_Comorbidity_Inventories <- DIA_Comorbidity_Inventories %>% mutate(label=1) %>% spread(key=diagnosis, value=label)
DIA_Comorbidity_Inventories[is.na(DIA_Comorbidity_Inventories)] <-0




DANU_Ingredients       <- fread("DIA Analysis Results 1.1/DANU Ingredients.txt", integer64 = "character", stringsAsFactors = F)
DANU_Ingredients$drug_id <- unlist(lapply(DANU_Ingredients$drug_id, function(x) as.numeric(unlist(str_extract_all(x,"[:digit:]+$")))))
string_SGLT2 <- paste0("\\b(",paste0(DANU_Ingredients$drug_id[DANU_Ingredients$drug_group == "SGLT2"], collapse = "|"),")\\b")
string_GLP1 <- paste0("\\b(",paste0(DANU_Ingredients$drug_id[DANU_Ingredients$drug_group == "GLP1 Injectable"|DANU_Ingredients$drug_group == "GLP1 Oral"], collapse = "|"),")\\b")


DIA_Drug_Histories <- fread("DIA Analysis Results 1.1/DIA Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
DIA_Drug_Histories <- DIA_Drug_Histories %>% select(-c(disease)) 
DIA_Drug_Histories <- gather(DIA_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
DIA_Drug_Histories <- DIA_Drug_Histories %>% select(patient, Treat, weight) %>% distinct() %>% filter(Treat!="-")
DIA_Drug_Histories <- DIA_Drug_Histories %>% mutate(GLP1_SGLT2_Status = ifelse(grepl(string_GLP1, Treat)&grepl(string_SGLT2, Treat), "Combo", 
                                                                               ifelse(grepl(string_GLP1, Treat), "GLP1",
                                                                                            ifelse(grepl(string_SGLT2, Treat), "SGLT2", "none"))))

DIA_Drug_Histories <- DIA_Drug_Histories %>% select(patient, GLP1_SGLT2_Status, weight) %>% distinct() 

Combos <- DIA_Drug_Histories %>% filter(GLP1_SGLT2_Status=="Combo") %>% select(patient, weight) %>% distinct()
GLP1 <- DIA_Drug_Histories %>% filter(GLP1_SGLT2_Status=="GLP1") %>% select(patient, weight) %>% distinct()
SGLT2 <- DIA_Drug_Histories %>% filter(GLP1_SGLT2_Status=="SGLT2") %>% select(patient, weight) %>% distinct()


SGLT2 %>% inner_join(GLP1) %>% full_join(Combos) %>% summarise(n=sum(weight)) # 2597105

GLP1_Pats <- GLP1 %>% anti_join(Combos) %>% anti_join(SGLT2)
SGLT2_Pats <- SGLT2 %>% anti_join(Combos) %>% anti_join(GLP1)
Combo_Pats <- Combos

sum(GLP1_Pats$weight)
sum(SGLT2_Pats$weight)
sum(Combo_Pats$weight)



SGLT2_Pats$group <- 0
GLP1_Pats$group <- 1
Combo_Pats$group <- 2


temp2 <- SGLT2_Pats %>% full_join(GLP1_Pats) %>% full_join(Combo_Pats) %>% select(-weight) %>% rename("patid"="patient") %>%
  inner_join(temp)

temp2 <- temp2 %>% left_join(DIA_Comorbidity_Inventories)
temp2[is.na(temp2)] <-0



temp2$group <- as.factor(temp2$group)

temp2 <- temp2 %>% select(-c(SGLT2Exp, OralExp, InjExp))

temp2 <- temp2 %>% mutate(diagnosis = ifelse(diagnosis=="Diabetes + Obesity", 1, 0))
temp2 <- temp2 %>% mutate(gender = ifelse(gender=="M", 1, 0))


temp2 <- temp2 %>% select(-patid)
temp2 <- temp2 %>% select(-weight)

temp2 %>% group_by(group) %>% count()

temp2 <- temp2 %>% group_by(group) %>% sample_n(2000)
temp2 <- temp2 %>% ungroup()

temp2 <- temp2 %>% sample_n(6000)

modelAll_1_randomForest <- randomForest(group ~ ., data = temp2)
summary(modelAll_1_randomForest)
data.frame(modelAll_1_randomForest$importance) %>% arrange(-MeanDecreaseGini)


names(temp2)

temp2 %>% group_by(group) %>% summarise(n=mean(HbA1c))

temp2 %>% group_by(group, I70) %>% count()


# -----------------------





# Split Diabetes into comorb, BMI, etc ---------------

DANU_Demographics <- fread("DANU Demographics 1.1/DANU Demographics.txt", integer64 = "character", stringsAsFactors = F)
DANU_Demographics <- DANU_Demographics %>% filter(grepl("Diabetes", diagnosis)) %>% select(patid, weight, diagnosis)

Treatment_exp_Vector <- fread("DIA Analysis Results 1.1/Treatment_exp_Vector.txt")
names(Treatment_exp_Vector)[1] <- "patid"
Treatment_exp_Vector$Treat <- "Treat"

DANU_Demographics <- DANU_Demographics %>% left_join(Treatment_exp_Vector)


DANU_Measures <- fread("DANU Measures 1.1/DANU Measures.txt",  integer64 = "character", stringsAsFactors = F)
DANU_Measures <- DANU_Demographics %>% select(patid) %>% inner_join(DANU_Measures)
DANU_Measures <- DANU_Measures %>% filter(test=="BMI")
DANU_Measures <- DANU_Measures %>% select(patid, value) %>% distinct() %>% group_by(patid ) %>% filter(value==max(value)) %>% slice(1)
DANU_Measures <- DANU_Measures %>% filter(value>=25) %>% mutate(value=ifelse(value>=25&value<27, "25-27", ifelse(value>=27&value<30,"27-30", ">30" )))

DANU_Demographics <- DANU_Demographics %>% inner_join(DANU_Measures)


DIA_Comorbidity_Inventories <- fread("DIA Analysis Results 1.1/DIA Comorbidity Inventories.txt")
DIA_Comorbidity_Inventories <- DIA_Comorbidity_Inventories %>% inner_join(DANU_Demographics %>% select(patid))

CKD <- unique(DIA_Comorbidity_Inventories[grepl("N18",diagnosis), c("patid","weight")])
POS <- unique(DIA_Comorbidity_Inventories[grepl("E28",diagnosis), c("patid","weight")])
PAD <- unique(DIA_Comorbidity_Inventories[grepl("I70",diagnosis)|grepl("I73",diagnosis), c("patid","weight")])
SLEEPAPNEA <- unique(DIA_Comorbidity_Inventories[grepl("G47",diagnosis), c("patid","weight")])
HF <- unique(DIA_Comorbidity_Inventories[grepl("I5",diagnosis), c("patid","weight")])
DISLIPIDEMIA <- unique(DIA_Comorbidity_Inventories[grepl("E78",diagnosis), c("patid","weight")])
HTN <- unique(DIA_Comorbidity_Inventories[grepl("I10",diagnosis), c("patid","weight")])
OA <- unique(DIA_Comorbidity_Inventories[grepl("M15", diagnosis)|grepl("M16", diagnosis)|grepl("M17", diagnosis)|
                                     grepl("M18", diagnosis)| grepl("M19", diagnosis), c("patid","weight")])
NASH <- unique(DIA_Comorbidity_Inventories[grepl("K75",diagnosis), c("patid","weight")])
PREDIABETES <- unique(DIA_Comorbidity_Inventories[grepl("R73",diagnosis), c("patid","weight")])
IHD <- unique(DIA_Comorbidity_Inventories[grepl("I20", diagnosis)|grepl("I21", diagnosis)| grepl("I22", diagnosis)|
                                   grepl("I23", diagnosis)| grepl("I24", diagnosis)| grepl("I25", diagnosis), c("patid","weight")])


Comorb <- Reduce(function (...) { merge(..., all = TRUE) },    list(CKD, POS, PAD, HF, SLEEPAPNEA, DISLIPIDEMIA, HTN, OA, NASH, IHD, PREDIABETES)) 


Comorb$Comorb <- "Comorb"

DANU_Demographics <- DANU_Demographics %>% left_join(Comorb) %>% mutate(Treat=ifelse(is.na(Treat),"no", Treat)) %>%
  mutate(Comorb=ifelse(is.na(Comorb), "no", Comorb)) %>% mutate(Comorb=ifelse(value=="27-30", Comorb, "ignore"))

DANU_Demographics %>% group_by(diagnosis, Treat, value, Comorb) %>% summarise(n=sum(weight))

# ------------



# Highest class tried -------------------

DANU_Ingredients <- fread("DIA Analysis Results 1.1/DANU Ingredients.txt", integer64 = "character", stringsAsFactors = F)
DANU_Ingredients <- DANU_Ingredients %>%  separate(drug_id, c('class', 'molecule'))
DANU_Ingredients <- DANU_Ingredients %>% select(molecule, drug_group)
DANU_Ingredients$molecule <- as.numeric(DANU_Ingredients$molecule)

DIA_Drug_Histories <- fread("DIA Analysis Results 1.1/DIA Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
Treatment_exp_Vector <- fread("DIA Analysis Results 1.1/Treatment_exp_Vector.txt")
DIA_Drug_Histories <- Treatment_exp_Vector %>% left_join(DIA_Drug_Histories)
DIA_Drug_Histories <- gather(DIA_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
DIA_Drug_Histories <- DIA_Drug_Histories %>% select(patient, weight, Drugs) %>% distinct() %>% filter(Drugs!="-")
DIA_Drug_Histories <- separate_rows(DIA_Drug_Histories, Drugs, sep = ",", convert=T)
DIA_Drug_Histories$Drugs <- as.numeric(DIA_Drug_Histories$Drugs)
DIA_Drug_Histories <- DIA_Drug_Histories %>% select(patient, weight, Drugs) %>% distinct() 
DIA_Drug_Histories <- DIA_Drug_Histories %>% left_join(DANU_Ingredients, by=c("Drugs"="molecule")) %>% select(patient, weight, drug_group) %>% distinct()

DIA_Drug_Histories %>% mutate(drug_group=ifelse(drug_group=="GLP1 Injectable", 1, 
                                                ifelse(drug_group=="Insulin", 2,
                                                       ifelse(drug_group=="GLP1 Oral", 3,
                                                              ifelse(drug_group=="SGLT2", 4,
                                                                     ifelse(drug_group=="DPP4"|drug_group=="Antidiabetc",5,6)))))) %>%
  group_by(patient) %>% filter(drug_group==min(drug_group)) %>% slice(1) %>% ungroup() %>% group_by(drug_group) %>% summarise(n=sum(weight)/30120087)

Ranks <- DIA_Drug_Histories %>% mutate(drug_group=ifelse(drug_group=="GLP1 Injectable", 1, 
                                                ifelse(drug_group=="Insulin", 2,
                                                       ifelse(drug_group=="GLP1 Oral", 3,
                                                              ifelse(drug_group=="SGLT2", 4,
                                                                     ifelse(drug_group=="DPP4"|drug_group=="Antidiabetc",5,6)))))) %>%
  group_by(patient) %>% filter(drug_group==min(drug_group)) %>% slice(1) %>% ungroup()

DANU_Demographics <- fread("DANU Demographics 1.1/DANU Demographics.txt", integer64 = "character", stringsAsFactors = F)
DANU_Demographics <- DANU_Demographics %>% select(patid, weight, age, gender) %>% rename("patient"="patid")

Ranks %>% left_join(DANU_Demographics) %>% group_by(drug_group) %>% summarise(n=mean(age))

Ranks %>% left_join(DANU_Demographics) %>% group_by(drug_group, gender) %>% count()


DANU_Ingredients <- fread("DIA Analysis Results 1.1/DANU Ingredients.txt", integer64 = "character", stringsAsFactors = F)
DANU_Ingredients <- DANU_Ingredients %>%  separate(drug_id, c('class', 'molecule'))

string_Biguanide       <- paste0("\\b(",paste0(DANU_Ingredients$molecule[DANU_Ingredients$drug_group == "Biguanide"], collapse = "|"),")\\b")
string_Antidiabetic    <- paste0("\\b(",paste0(DANU_Ingredients$molecule[DANU_Ingredients$drug_group == "Antidiabetic"], collapse = "|"),")\\b")
string_DPP4            <- paste0("\\b(",paste0(DANU_Ingredients$molecule[DANU_Ingredients$drug_group == "DPP4"], collapse = "|"),")\\b")
string_SGLT2           <- paste0("\\b(",paste0(DANU_Ingredients$molecule[DANU_Ingredients$drug_group == "SGLT2"], collapse = "|"),")\\b")
string_Insulin         <- paste0("\\b(",paste0(DANU_Ingredients$molecule[DANU_Ingredients$drug_group == "Insulin"], collapse = "|"),")\\b")
string_OralGLP1        <- paste0("\\b(",paste0(DANU_Ingredients$molecule[DANU_Ingredients$drug_group == "GLP1 Oral"], collapse = "|"),")\\b")
string_InjectableGLP1  <- paste0("\\b(",paste0(DANU_Ingredients$molecule[DANU_Ingredients$drug_group == "GLP1 Injectable"], collapse = "|"),")\\b")


DIA_Flows_Aux_Long <- fread("DIA Analysis Results 1.1/DIA_Flows_Aux_Long.txt", integer64 = "character", stringsAsFactors = F)
Treatment_exp_Vector <- fread("DIA Analysis Results 1.1/Treatment_exp_Vector.txt")
DIA_Flows_Aux_Long <- Treatment_exp_Vector %>% inner_join(DIA_Flows_Aux_Long)
DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% select(patient, weight, p1, p2, d1, d2) 

DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% mutate(BiguanideExp = ifelse(grepl(string_Biguanide,d1),1,0))
DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% group_by(patient) %>% mutate(BiguanideExp = cumsum(BiguanideExp))
DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% group_by(patient) %>% mutate(BiguanideExp = ifelse(BiguanideExp==0,0,1))

DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% mutate(AntidiabeticExp = ifelse(grepl(string_Antidiabetic,d1),1,0))
DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% group_by(patient) %>% mutate(AntidiabeticExp = cumsum(AntidiabeticExp))
DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% group_by(patient) %>% mutate(AntidiabeticExp = ifelse(AntidiabeticExp==0,0,1))
 
DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% mutate(DPP4Exp = ifelse(grepl(string_DPP4,d1),1,0))
DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% group_by(patient) %>% mutate(DPP4Exp = cumsum(DPP4Exp))
DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% group_by(patient) %>% mutate(DPP4Exp = ifelse(DPP4Exp==0,0,1))

DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% mutate(SGLT2Exp = ifelse(grepl(string_SGLT2,d1),1,0))
DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% group_by(patient) %>% mutate(SGLT2Exp = cumsum(SGLT2Exp))
DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% group_by(patient) %>% mutate(SGLT2Exp = ifelse(SGLT2Exp==0,0,1))
 
DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% mutate(OralExp = ifelse(grepl(string_OralGLP1,d1),1,0))
DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% group_by(patient) %>% mutate(OralExp = cumsum(OralExp))
DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% group_by(patient) %>% mutate(OralExp = ifelse(OralExp==0,0,1))
 
DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% mutate(InjExp = ifelse(grepl(string_InjectableGLP1,d1),1,0))
DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% group_by(patient) %>% mutate(InjExp = cumsum(InjExp))
DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% group_by(patient) %>% mutate(InjExp = ifelse(InjExp==0,0,1))

DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% mutate(InsulinExp = ifelse(grepl(string_Insulin,d1),1,0))
DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% group_by(patient) %>% mutate(InsulinExp = cumsum(InsulinExp))
DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% group_by(patient) %>% mutate(InsulinExp = ifelse(InsulinExp==0,0,1))

temp <- DIA_Flows_Aux_Long %>% filter(p1>=48) %>% filter(!grepl("47",d1)&grepl("47",d2)) %>% select(-c(p1,p2,d1,d2))
temp <- DIA_Flows_Aux_Long %>% filter(p1>=48) %>% filter(!grepl(string_InjectableGLP1,d1)&grepl(string_InjectableGLP1,d2)) %>% select(-c(p1,p2,d1,d2))

sum(temp$weight) # 359028.1

temp %>% mutate(group=ifelse(InjExp==1, 1,
                             ifelse(InsulinExp==1,2,
                                    ifelse(OralExp==1,3,
                                           ifelse(SGLT2Exp==1,4,
                                                  ifelse(DPP4Exp==1|AntidiabeticExp==1,5,6)))))) %>%
  group_by(group) %>% summarise(n=sum(weight)/3031681)



# ---------------------
# COmorbidities -----------------------------
DANU_Demographics <- fread("DANU Demographics 1.1/DANU Demographics.txt", integer64 = "character", stringsAsFactors = F)
DANU_Demographics <- DANU_Demographics %>% filter(diagnosis!="-") %>% select(patid, weight, diagnosis)
DANU_Demographics %>% group_by(diagnosis) %>% summarise(n=sum(weight))
DANU_Demographics <- DANU_Demographics %>% filter(grepl("Diabetes", diagnosis)) %>% select(patid, weight)

DIA_Comorbidity_Inventories <- fread("DIA Analysis Results 1.1/DIA Comorbidity Inventories.txt")

names(DIA_Comorbidity_Inventories)[3] <- "Dxs"


Comorbidity_Inventories %>% filter(grepl("I50", Dxs)) %>% inner_join(DANU_Demographics) %>% group_by(diagnosis) %>% summarise(n=sum(weight))
HF <- DIA_Comorbidity_Inventories %>% filter(grepl("I50", Dxs)) %>% inner_join(DANU_Demographics)  %>% select(patid, weight) %>% distinct()

# 1 Diabetes            784164.
# 2 Diabetes + Obesity 6000794.
# 3 Obesity            3777803

Comorbidity_Inventories %>% filter(grepl("N18", Dxs)) %>% inner_join(DANU_Demographics) %>% group_by(diagnosis) %>% summarise(n=sum(weight))
CKD <- DIA_Comorbidity_Inventories %>% filter(grepl("N18", Dxs)) %>% inner_join(DANU_Demographics)  %>% select(patid, weight) %>% distinct()

# 1 Diabetes           1190729.
# 2 Diabetes + Obesity 8132717.
# 3 Obesity            5467795.

Comorbidity_Inventories %>% filter(grepl("I70", Dxs)|grepl("I73", Dxs)) %>% inner_join(DANU_Demographics) %>% group_by(diagnosis) %>% summarise(n=sum(weight))
PAD <- DIA_Comorbidity_Inventories %>% filter(grepl("I70", Dxs)|grepl("I73", Dxs)) %>% inner_join(DANU_Demographics)  %>% select(patid, weight) %>% distinct()

# 1 Diabetes            1925399.
# 2 Diabetes + Obesity 11364070.
# 3 Obesity             8446560.

Comorbidity_Inventories %>% filter(grepl("G47", Dxs)) %>% inner_join(DANU_Demographics) %>% group_by(diagnosis) %>% summarise(n=sum(weight))
SA <- DIA_Comorbidity_Inventories %>% filter(grepl("G47", Dxs)) %>% inner_join(DANU_Demographics)  %>% select(patid, weight) %>% distinct()

# 1 Diabetes            1624704.
# 2 Diabetes + Obesity 16410072.
# 3 Obesity            24322915.

Comorbidity_Inventories %>% filter(grepl("E28", Dxs)) %>% inner_join(DANU_Demographics) %>% group_by(diagnosis) %>% summarise(n=sum(weight))
POS <- DIA_Comorbidity_Inventories %>% filter(grepl("E28", Dxs)) %>% inner_join(DANU_Demographics)  %>% select(patid, weight) %>% distinct()

# 1 Diabetes            275887.
# 2 Diabetes + Obesity 2065556.
# 3 Obesity            2019958.

sum(DANU_Demographics$weight)

No_Comorb <- DANU_Demographics %>% anti_join(POS) %>%
  anti_join(SA) %>% anti_join(HF) %>% anti_join(CKD) %>% anti_join(PAD)

sum(No_Comorb$weight)

HF$group <- "HF"
CKD$group <- "CKD"
PAD$group <- "PAD"
POS$group <- "POS"
SA$group <- "SA"
No_Comorb$group <- "No_comorb"

Comorbs <- HF %>% bind_rows(CKD) %>% bind_rows(PAD) %>% bind_rows(POS) %>% bind_rows(SA) %>% bind_rows(No_Comorb)

DIA_Drug_Histories     <- fread("DIA Analysis Results 1.1/DIA Drug Histories.txt", integer64 = "character", stringsAsFactors = F)

length(unique(DIA_Drug_Histories$patient)) # 359284
sum(as.numeric(DIA_Drug_Histories$weight)) # 46029709

DANU_Ingredients <- fread("DIA Analysis Results 1.1/DANU Ingredients.txt", integer64 = "character", stringsAsFactors = F)
DANU_Ingredients <- DANU_Ingredients %>%  separate(drug_id, c('class', 'molecule'))
DANU_Ingredients  <- DANU_Ingredients %>% select(molecule, drug_group)
names(DANU_Ingredients)[1] <- "Drugs"
DANU_Ingredients$Drugs <- as.numeric(DANU_Ingredients$Drugs)

DIA_Drug_Histories <- gather(DIA_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
DIA_Drug_Histories <- DIA_Drug_Histories %>% select(patient, weight, Drugs) %>% distinct() %>% filter(Drugs!="-")
DIA_Drug_Histories <- separate_rows(DIA_Drug_Histories, Drugs, sep = ",", convert=T)
DIA_Drug_Histories <- DIA_Drug_Histories %>% left_join(DANU_Ingredients) 
DIA_Drug_Histories <- DIA_Drug_Histories %>% select(patient, weight, drug_group) %>% distinct() %>% rename("patid"="patient")

DIA_Drug_Histories <- DIA_Drug_Histories %>% inner_join(Comorbs)

DIA_Drug_Histories %>% select(patid, group, weight) %>% distinct() %>% group_by(group) %>% summarise(n=sum(weight))

DIA_Drug_Histories %>% group_by(group, drug_group) %>% summarise(n=sum(weight)) %>%
  spread(key=drug_group, value=n)


CKD	9323446
HF	6784958
PAD	13289469
POS	2341443
SA	18034776
No Comorb	17443243

# ------------------------------------
# Prevalence DIA / OBE / CKD / HF / PAD / SA / POS per age group -----------------------------------

# Diabetes
DANU_Demographics <- fread("DANU Demographics 1.1/DANU Demographics.txt")
Diabetes <- DANU_Demographics %>% select(patid, weight, diagnosis, age) %>% filter(grepl("Diabetes", diagnosis))
Diabetes <- Diabetes %>% group_by(age) %>% summarise(DIA_Population=sum(weight))
data.frame(Diabetes)

# Obesity
DANU_Demographics <- fread("DANU Demographics 1.1/DANU Demographics.txt")
Obesity <- DANU_Demographics %>% select(patid, weight, diagnosis, age) %>% filter(grepl("Obesity", diagnosis))
Obesity <- Obesity %>% group_by(age) %>% summarise(OBE_Population=sum(weight))

DANU_Measures <- fread("DANU Measures 1.1/DANU Measures.txt")
length(unique(DANU_Measures$patid))
DANU_Measures <- DANU_Measures %>% filter(test=="BMI") %>% select(patid, weight, value, claimed) %>%
  mutate(claimed=as.Date(claimed)) %>% group_by(patid) %>% filter(claimed==max(claimed)) %>% filter(value==max(value))
DANU_Measures <- DANU_Measures %>% distinct()
DANU_Measures <- DANU_Measures %>% select(patid, weight, value)

DANU_Demographics <- fread("DANU Demographics 1.1/DANU Demographics.txt")

Obesity_30_share <- DANU_Demographics %>% filter(grepl("Obesity", diagnosis)) %>% select(patid, weight, age) %>%
  inner_join(DANU_Measures) %>%
  group_by(age) %>% summarise(n=sum(weight)) %>%
  left_join(
    DANU_Demographics %>% filter(grepl("Obesity", diagnosis)) %>% select(patid, weight, age) %>%
  inner_join(DANU_Measures %>% filter(value>=30)) %>%
  group_by(age) %>% summarise(n2=sum(weight))
  ) %>% mutate(share=n2/n)

Obesity_30_share <- Obesity_30_share %>% select(age, share)


Mkt_Clarity_PAD_PCOS_SLAP_CKD_HFpEF_ce18_pts <- fread("DIA Analysis Results 1.1/Mkt_Clarity_PAD_PCOS_SLAP_CKD_HFpEF_ce18_pts.txt")
Mkt_Clarity_PAD_PCOS_SLAP_CKD_HFpEF_ce18_pts <- Mkt_Clarity_PAD_PCOS_SLAP_CKD_HFpEF_ce18_pts %>% select(-c(weight2))
Weights_Comorbidity_Pedro <- fread("DIA Analysis Results 1.1/Weights_Comorbidity_Pedro.csv")
Weights_Comorbidity_Pedro <- Weights_Comorbidity_Pedro %>% select(age, gender, weight)
Mkt_Clarity_PAD_PCOS_SLAP_CKD_HFpEF_ce18_pts <- Mkt_Clarity_PAD_PCOS_SLAP_CKD_HFpEF_ce18_pts %>% left_join(Weights_Comorbidity_Pedro)
names(Mkt_Clarity_PAD_PCOS_SLAP_CKD_HFpEF_ce18_pts)[1] <- "patid"



CKD <- Mkt_Clarity_PAD_PCOS_SLAP_CKD_HFpEF_ce18_pts %>% filter(CKD==1) %>% select(patid, weight, age) %>% distinct() %>% 
  group_by(age)  %>% summarise(CKD_Population=sum(weight))  
Heart_Failure <- Mkt_Clarity_PAD_PCOS_SLAP_CKD_HFpEF_ce18_pts %>% filter(HFpEF==1) %>% select(patid, weight, age) %>% distinct() %>% 
  group_by(age)  %>% summarise(HF_Population=sum(weight))
PAD <- Mkt_Clarity_PAD_PCOS_SLAP_CKD_HFpEF_ce18_pts %>% filter(PAD_restrict==1) %>% select(patid, weight, age) %>% distinct() %>% group_by(age) %>% summarise(PAD_Population=sum(weight))
POS <- Mkt_Clarity_PAD_PCOS_SLAP_CKD_HFpEF_ce18_pts %>% filter(PCOS==1) %>% select(patid, weight, age) %>% distinct() %>% group_by(age) %>% summarise(POS_Population=sum(weight))
SA <- Mkt_Clarity_PAD_PCOS_SLAP_CKD_HFpEF_ce18_pts %>% filter(OSLAP ==1) %>% select(patid, weight, age) %>% distinct() %>% group_by(age) %>% summarise(SA_Population=sum(weight))


US_Population <- fread("DANU weights.txt")
sum(US_Population$insured_population)
US_Population <- US_Population %>% group_by(age) %>% summarise(TOTAL_US=sum(insured_population))
sum(US_Population$TOTAL_US)


data.frame(Diabetes %>% 
             left_join(Obesity_30_share) %>% 
             left_join(CKD) %>% 
             left_join(Heart_Failure) %>% 
             left_join(POS) %>% 
             left_join(PAD) %>% 
             left_join(SA) %>% 
             left_join(US_Population)) %>%
  mutate(DIA_Population = DIA_Population / TOTAL_US,
         OBE_Population  = share  ,
         CKD_Population  = CKD_Population  / TOTAL_US,
         PAD_Population  = PAD_Population  / TOTAL_US,
         POS_Population  = POS_Population  / TOTAL_US,
         SA_Population  = SA_Population  / TOTAL_US,
         HF_Population  = HF_Population  / TOTAL_US) %>% select(-c(TOTAL_US, share)) %>%
  select(age, DIA_Population, OBE_Population, CKD_Population, HF_Population, PAD_Population, SA_Population, POS_Population) %>%
  gather(Disease, Prevalence, DIA_Population:POS_Population, factor_key=TRUE) %>%
  ggplot((aes(age, Prevalence, colour=Disease))) +
  geom_smooth(size=2, se = F) +
  theme_classic() +
  scale_y_continuous(labels = scales::percent)+
  xlab("\n \n Age (years)") +  ylab("US Insured Population Prevalence\n \n") +
  scale_colour_manual(values=c("#009dd6","#7849b8","#138468","#ec111a","#ffd42f","#fb6330","#f2609e")) 

# ---------------------------------------


# Total Durations on each class over 60 months------------------------------------------------------------
DANU_Ingredients <- fread("DIA Analysis Results 1.1/DANU Ingredients.txt", integer64 = "character", stringsAsFactors = F)
DANU_Ingredients <- DANU_Ingredients %>%  separate(drug_id, c('class', 'molecule'))
DANU_Ingredients <- DANU_Ingredients %>% select(molecule, drug_group)
names(DANU_Ingredients)[1] <- "Drugs"
DANU_Ingredients$Drugs <- as.numeric(DANU_Ingredients$Drugs)

string_Biguanide <- paste0("\\b(",paste0(DANU_Ingredients$molecule[DANU_Ingredients$drug_group == "Biguanide"], collapse = "|"),")\\b")
string_Antidiabetic <- paste0("\\b(",paste0(DANU_Ingredients$molecule[DANU_Ingredients$drug_group == "Antidiabetic"], collapse = "|"),")\\b")
string_DPP4 <- paste0("\\b(",paste0(DANU_Ingredients$molecule[DANU_Ingredients$drug_group == "DPP4"], collapse = "|"),")\\b")
string_SGLT2 <- paste0("\\b(",paste0(DANU_Ingredients$molecule[DANU_Ingredients$drug_group == "SGLT2"], collapse = "|"),")\\b")
string_Insulin <- paste0("\\b(",paste0(DANU_Ingredients$molecule[DANU_Ingredients$drug_group == "Insulin"], collapse = "|"),")\\b")
string_OralGLP1 <- paste0("\\b(",paste0(DANU_Ingredients$molecule[DANU_Ingredients$drug_group == "GLP1 Oral"], collapse = "|"),")\\b")
string_InjectableGLP1 <- paste0("\\b(",paste0(DANU_Ingredients$molecule[DANU_Ingredients$drug_group == "GLP1 Injectable"], collapse = "|"),")\\b")


DIA_Drug_Histories <- read.table("DIA Analysis Results 1.1/DIA Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
DIA_Drug_Histories <- gather(DIA_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
DIA_Drug_Histories$Month <- as.character(DIA_Drug_Histories$Month)
DIA_Drug_Histories$Month <- parse_number(DIA_Drug_Histories$Month)
#DIA_Drug_Histories <- DIA_Drug_Histories %>% filter(Month>=37)
DIA_Drug_Histories <- DIA_Drug_Histories %>% filter(Drugs != "-") %>% select(-c(disease))
DIA_Drug_Histories <- separate_rows(DIA_Drug_Histories, Drugs, sep = ",", convert=T)
DIA_Drug_Histories <- DIA_Drug_Histories %>% left_join(DANU_Ingredients)
DIA_Drug_Histories <- DIA_Drug_Histories %>% select(-Drugs) %>% distinct()

DIA_Drug_Histories <- DIA_Drug_Histories %>% group_by(patient, weight, drug_group) %>% count()

DIA_Drug_Histories %>% ungroup() %>% group_by(drug_group) %>% summarise(mean=weighted.mean(n, as.numeric(weight)))

# 1 Antidiabetic    14.8 
# 2 Biguanide       14.7 
# 3 DPP4            13.2 
# 4 GLP1 Injectable 11.5 
# 5 GLP1 Oral        6.00
# 6 Insulin         12.8 
# 7 SGLT2           12.0 


DIA_Drug_Histories <- DIA_Drug_Histories %>% ungroup() %>% group_by(drug_group, n) %>% summarise(Total=sum(as.numeric(weight))) %>% arrange(drug_group, n)



temp <- data.frame(DIA_Drug_Histories %>% group_by(drug_group) %>% mutate(Total_cum=cumsum(Total)) %>% mutate(grand_Total=sum(Total)) %>%
             mutate(Remain=grand_Total-lag(Total_cum)) %>% mutate(Remain=ifelse(is.na(Remain), grand_Total, Remain)) %>%
             mutate(Remain=Remain/grand_Total) %>%
             select(drug_group, n, Remain)) 


temp %>% filter(n==12)

temp %>%
  mutate(drug_group=factor(drug_group, levels=c("Biguanide", "Antidiabetic", "DPP4", "SGLT2", "Insulin", "GLP1 Oral", "GLP1 Injectable"))) %>%
  ggplot(aes(n, Remain, colour=drug_group)) +
  geom_smooth(se=F, size=2) +
  ylim(0,1) +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent, breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)) +
  #scale_x_continuous(breaks = c(-60, -50, -40, -30, -20, -10, 0)) +
  scale_colour_manual(values=c("#c49a7c","#7d95be","#0d2b4e","#ff9933","#7cc49a", "#e87496","#a52a2a")) +
  xlab("\n Number of Months Elapsed Since Therapy Initiation ") + ylab("Proportion of Patients Still ON Drugs \n")














DIA_Drug_Histories <- DIA_Drug_Histories %>% select(-c(disease))
DIA_Drug_Histories <-  DIA_Drug_Histories %>%  select(3:62)

DIA_Drug_Histories <- DIA_Drug_Histories %>% 
  mutate_if(grepl(string_InjectableGLP1,.), ~replace(., grepl(string_InjectableGLP1, .), "GLP1 Injectable"))

DIA_Drug_Histories <-  DIA_Drug_Histories %>% mutate_all(function(x) ifelse(x=="GLP1 Injectable",1,0))
DIA_Drug_Histories[] <-  lapply(DIA_Drug_Histories,as.numeric)

DIA_Drug_Histories_LONG <- read.table("DIA Analysis Results 1.1/DIA Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
DIA_Drug_Histories_LONG <- DIA_Drug_Histories_LONG %>% select(patient, weight)
DIA_Drug_Histories <- DIA_Drug_Histories_LONG %>% bind_cols(DIA_Drug_Histories)
rm(DIA_Drug_Histories_LONG)

DIA_Drug_Histories <- gather(DIA_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
DIA_Drug_Histories <- DIA_Drug_Histories %>% group_by(patient) %>% arrange(patient)

DIA_Drug_Histories <- DIA_Drug_Histories %>% group_by(patient) %>% mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

DIA_Drug_Histories <- DIA_Drug_Histories %>% filter(Treat == 1)

GLP1_Injectable_Periods <- DIA_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())
names(GLP1_Injectable_Periods)[3] <- "Duration"
GLP1_Injectable_Periods <- GLP1_Injectable_Periods %>% select(patient, Duration) 

DIA_Drug_Histories <- DIA_Drug_Histories %>% select(patient, weight) %>% distinct()
GLP1_Injectable_Periods <- GLP1_Injectable_Periods %>% left_join(DIA_Drug_Histories) 
GLP1_Injectable_Periods <- GLP1_Injectable_Periods %>% mutate(weight = as.numeric(weight))
GLP1_Injectable_Periods <- GLP1_Injectable_Periods %>% group_by(patient) %>% mutate(Total_Duration = sum(Duration)) %>% select(patient, weight, Total_Duration)
GLP1_Injectable_Periods <- GLP1_Injectable_Periods %>% distinct()

library(spatstat)
weighted.mean(GLP1_Injectable_Periods$Total_Duration, GLP1_Injectable_Periods$weight)  # 17.3716
weighted.median(GLP1_Injectable_Periods$Total_Duration, GLP1_Injectable_Periods$weight) # 10.5

data.frame(GLP1_Injectable_Periods %>% distinct() %>% group_by(Total_Duration) %>% summarise(pats = sum(weight)))



# GLP1 Oral
DIA_Drug_Histories <- read.table("DIA Analysis Results 1.1/DIA Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
DIA_Drug_Histories <- DIA_Drug_Histories %>% select(-c(disease))
DIA_Drug_Histories <-  DIA_Drug_Histories %>%  select(3:62)

DIA_Drug_Histories <- DIA_Drug_Histories %>% 
  mutate_if(grepl('47',.), ~replace(., grepl('47', .), "GLP1 Oral"))

DIA_Drug_Histories <-  DIA_Drug_Histories %>% mutate_all(function(x) ifelse(x=="GLP1 Oral",1,0))
DIA_Drug_Histories[] <-  lapply(DIA_Drug_Histories,as.numeric)

DIA_Drug_Histories_LONG <- read.table("DIA Analysis Results 1.1/DIA Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
DIA_Drug_Histories_LONG <- DIA_Drug_Histories_LONG %>% select(patient, weight)
DIA_Drug_Histories <- DIA_Drug_Histories_LONG %>% bind_cols(DIA_Drug_Histories)
rm(DIA_Drug_Histories_LONG)

DIA_Drug_Histories <- gather(DIA_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
DIA_Drug_Histories <- DIA_Drug_Histories %>% group_by(patient) %>% arrange(patient)

DIA_Drug_Histories <- DIA_Drug_Histories %>% group_by(patient) %>% mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

DIA_Drug_Histories <- DIA_Drug_Histories %>% filter(Treat == 1)

GLP1_Oral_Periods <- DIA_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())
names(GLP1_Oral_Periods)[3] <- "Duration"
GLP1_Oral_Periods <- GLP1_Oral_Periods %>% select(patient, Duration) 

DIA_Drug_Histories <- DIA_Drug_Histories %>% select(patient, weight) %>% distinct()
GLP1_Oral_Periods <- GLP1_Oral_Periods %>% left_join(DIA_Drug_Histories) 
GLP1_Oral_Periods <- GLP1_Oral_Periods %>% mutate(weight = as.numeric(weight))
GLP1_Oral_Periods <- GLP1_Oral_Periods %>% group_by(patient) %>% mutate(Total_Duration = sum(Duration)) %>% select(patient, weight, Total_Duration)
GLP1_Oral_Periods <- GLP1_Oral_Periods %>% distinct()

library(spatstat)
weighted.mean(GLP1_Oral_Periods$Total_Duration, GLP1_Oral_Periods$weight)  # 4.285284
weighted.median(GLP1_Oral_Periods$Total_Duration, GLP1_Oral_Periods$weight) # 2.5

data.frame(GLP1_Oral_Periods %>% distinct() %>% group_by(Total_Duration) %>% summarise(pats = sum(weight)))



# Insulin
DIA_Drug_Histories <- read.table("DIA Analysis Results 1.1/DIA Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
DIA_Drug_Histories <- DIA_Drug_Histories %>% select(-c(disease))
DIA_Drug_Histories <-  DIA_Drug_Histories %>%  select(3:62)

DIA_Drug_Histories <- DIA_Drug_Histories %>% 
  mutate_if(grepl(string_Insulin,.), ~replace(., grepl(string_Insulin, .), "Insulin"))

DIA_Drug_Histories <-  DIA_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Insulin",1,0))
DIA_Drug_Histories[] <-  lapply(DIA_Drug_Histories,as.numeric)

DIA_Drug_Histories_LONG <- read.table("DIA Analysis Results 1.1/DIA Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
DIA_Drug_Histories_LONG <- DIA_Drug_Histories_LONG %>% select(patient, weight)
DIA_Drug_Histories <- DIA_Drug_Histories_LONG %>% bind_cols(DIA_Drug_Histories)
rm(DIA_Drug_Histories_LONG)

DIA_Drug_Histories <- gather(DIA_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
DIA_Drug_Histories <- DIA_Drug_Histories %>% group_by(patient) %>% arrange(patient)

DIA_Drug_Histories <- DIA_Drug_Histories %>% group_by(patient) %>% mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

DIA_Drug_Histories <- DIA_Drug_Histories %>% filter(Treat == 1)

Insulin_Periods <- DIA_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())
names(Insulin_Periods)[3] <- "Duration"
Insulin_Periods <- Insulin_Periods %>% select(patient, Duration) 

DIA_Drug_Histories <- DIA_Drug_Histories %>% select(patient, weight) %>% distinct()
Insulin_Periods <- Insulin_Periods %>% left_join(DIA_Drug_Histories) 
Insulin_Periods <- Insulin_Periods %>% mutate(weight = as.numeric(weight))
Insulin_Periods <- Insulin_Periods %>% group_by(patient) %>% mutate(Total_Duration = sum(Duration)) %>% select(patient, weight, Total_Duration)
Insulin_Periods <- Insulin_Periods %>% distinct()

library(spatstat)
weighted.mean(Insulin_Periods$Total_Duration, Insulin_Periods$weight)  # 22.82028
weighted.median(Insulin_Periods$Total_Duration, Insulin_Periods$weight) #  13.5

data.frame(Insulin_Periods %>% distinct() %>% group_by(Total_Duration) %>% summarise(pats = sum(weight)))


# SGLT2
DIA_Drug_Histories <- read.table("DIA Analysis Results 1.1/DIA Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
DIA_Drug_Histories <- DIA_Drug_Histories %>% select(-c(disease))
DIA_Drug_Histories <-  DIA_Drug_Histories %>%  select(3:62)

DIA_Drug_Histories <- DIA_Drug_Histories %>% 
  mutate_if(grepl(string_SGLT2,.), ~replace(., grepl(string_SGLT2, .), "SGLT2"))
 
DIA_Drug_Histories <-  DIA_Drug_Histories %>% mutate_all(function(x) ifelse(x=="SGLT2",1,0))
DIA_Drug_Histories[] <-  lapply(DIA_Drug_Histories,as.numeric)

DIA_Drug_Histories_LONG <- read.table("DIA Analysis Results 1.1/DIA Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
DIA_Drug_Histories_LONG <- DIA_Drug_Histories_LONG %>% select(patient, weight)
DIA_Drug_Histories <- DIA_Drug_Histories_LONG %>% bind_cols(DIA_Drug_Histories)
rm(DIA_Drug_Histories_LONG)

DIA_Drug_Histories <- gather(DIA_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
DIA_Drug_Histories <- DIA_Drug_Histories %>% group_by(patient) %>% arrange(patient)

DIA_Drug_Histories <- DIA_Drug_Histories %>% group_by(patient) %>% mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

DIA_Drug_Histories <- DIA_Drug_Histories %>% filter(Treat == 1)

SGLT2_Periods <- DIA_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())
names(SGLT2_Periods)[3] <- "Duration"
SGLT2_Periods <- SGLT2_Periods %>% select(patient, Duration) 

DIA_Drug_Histories <- DIA_Drug_Histories %>% select(patient, weight) %>% distinct()
SGLT2_Periods <- SGLT2_Periods %>% left_join(DIA_Drug_Histories) 
SGLT2_Periods <- SGLT2_Periods %>% mutate(weight = as.numeric(weight))
SGLT2_Periods <- SGLT2_Periods %>% group_by(patient) %>% mutate(Total_Duration = sum(Duration)) %>% select(patient, weight, Total_Duration)
SGLT2_Periods <- SGLT2_Periods %>% distinct()

library(spatstat)
weighted.mean(SGLT2_Periods$Total_Duration, SGLT2_Periods$weight)  # 18.29962
weighted.median(SGLT2_Periods$Total_Duration, SGLT2_Periods$weight) # 10.5

data.frame(SGLT2_Periods %>% distinct() %>% group_by(Total_Duration) %>% summarise(pats = sum(weight)))



# DPP4
DIA_Drug_Histories <- read.table("DIA Analysis Results 1.1/DIA Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
DIA_Drug_Histories <- DIA_Drug_Histories %>% select(-c(disease))
DIA_Drug_Histories <-  DIA_Drug_Histories %>%  select(3:62)

DIA_Drug_Histories <- DIA_Drug_Histories %>% 
  mutate_if(grepl(string_DPP4,.), ~replace(., grepl(string_DPP4, .), "DPP4")) 

DIA_Drug_Histories <-  DIA_Drug_Histories %>% mutate_all(function(x) ifelse(x=="DPP4",1,0))
DIA_Drug_Histories[] <-  lapply(DIA_Drug_Histories,as.numeric)

DIA_Drug_Histories_LONG <- read.table("DIA Analysis Results 1.1/DIA Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
DIA_Drug_Histories_LONG <- DIA_Drug_Histories_LONG %>% select(patient, weight)
DIA_Drug_Histories <- DIA_Drug_Histories_LONG %>% bind_cols(DIA_Drug_Histories)
rm(DIA_Drug_Histories_LONG)

DIA_Drug_Histories <- gather(DIA_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
DIA_Drug_Histories <- DIA_Drug_Histories %>% group_by(patient) %>% arrange(patient)

DIA_Drug_Histories <- DIA_Drug_Histories %>% group_by(patient) %>% mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

DIA_Drug_Histories <- DIA_Drug_Histories %>% filter(Treat == 1)

DPP4_Periods <- DIA_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())
names(DPP4_Periods)[3] <- "Duration"
DPP4_Periods <- DPP4_Periods %>% select(patient, Duration) 

DIA_Drug_Histories <- DIA_Drug_Histories %>% select(patient, weight) %>% distinct()
DPP4_Periods <- DPP4_Periods %>% left_join(DIA_Drug_Histories) 
DPP4_Periods <- DPP4_Periods %>% mutate(weight = as.numeric(weight))
DPP4_Periods <- DPP4_Periods %>% group_by(patient) %>% mutate(Total_Duration = sum(Duration)) %>% select(patient, weight, Total_Duration)
DPP4_Periods <- DPP4_Periods %>% distinct()

library(spatstat)
weighted.mean(DPP4_Periods$Total_Duration, DPP4_Periods$weight)  # 21.73646
weighted.median(DPP4_Periods$Total_Duration, DPP4_Periods$weight) #14.5

data.frame(DPP4_Periods %>% distinct() %>% group_by(Total_Duration) %>% summarise(pats = sum(weight)))




# Antidiabetic
DIA_Drug_Histories <- read.table("DIA Analysis Results 1.1/DIA Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
DIA_Drug_Histories <- DIA_Drug_Histories %>% select(-c(disease))
DIA_Drug_Histories <-  DIA_Drug_Histories %>%  select(3:62)

DIA_Drug_Histories <- DIA_Drug_Histories %>% 
  mutate_if(grepl(string_Antidiabetic,.), ~replace(., grepl(string_Antidiabetic, .), "Antidiabetic")) 

DIA_Drug_Histories <-  DIA_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Antidiabetic",1,0))
DIA_Drug_Histories[] <-  lapply(DIA_Drug_Histories,as.numeric)

DIA_Drug_Histories_LONG <- read.table("DIA Analysis Results 1.1/DIA Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
DIA_Drug_Histories_LONG <- DIA_Drug_Histories_LONG %>% select(patient, weight)
DIA_Drug_Histories <- DIA_Drug_Histories_LONG %>% bind_cols(DIA_Drug_Histories)
rm(DIA_Drug_Histories_LONG)

DIA_Drug_Histories <- gather(DIA_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
DIA_Drug_Histories <- DIA_Drug_Histories %>% group_by(patient) %>% arrange(patient)

DIA_Drug_Histories <- DIA_Drug_Histories %>% group_by(patient) %>% mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

DIA_Drug_Histories <- DIA_Drug_Histories %>% filter(Treat == 1)

Antidiabetic_Periods <- DIA_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())
names(Antidiabetic_Periods)[3] <- "Duration"
Antidiabetic_Periods <- Antidiabetic_Periods %>% select(patient, Duration) 

DIA_Drug_Histories <- DIA_Drug_Histories %>% select(patient, weight) %>% distinct()
Antidiabetic_Periods <- Antidiabetic_Periods %>% left_join(DIA_Drug_Histories) 
Antidiabetic_Periods <- Antidiabetic_Periods %>% mutate(weight = as.numeric(weight))
Antidiabetic_Periods <- Antidiabetic_Periods %>% group_by(patient) %>% mutate(Total_Duration = sum(Duration)) %>% select(patient, weight, Total_Duration)
Antidiabetic_Periods <- Antidiabetic_Periods %>% distinct()

library(spatstat)
weighted.mean(Antidiabetic_Periods$Total_Duration, Antidiabetic_Periods$weight)  # 26.74429
weighted.median(Antidiabetic_Periods$Total_Duration, Antidiabetic_Periods$weight) # 22.5

data.frame(Antidiabetic_Periods %>% distinct() %>% group_by(Total_Duration) %>% summarise(pats = sum(weight)))



# Biguanide
DIA_Drug_Histories <- read.table("DIA Analysis Results 1.1/DIA Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
DIA_Drug_Histories <- DIA_Drug_Histories %>% select(-c(disease))
DIA_Drug_Histories <-  DIA_Drug_Histories %>%  select(3:62)

DIA_Drug_Histories <- DIA_Drug_Histories %>% 
  mutate_if(grepl('(^|\\D)(1{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(1{1})(\\D|$)', .), "Biguanide"))

DIA_Drug_Histories <-  DIA_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Biguanide",1,0))
DIA_Drug_Histories[] <-  lapply(DIA_Drug_Histories,as.numeric)

DIA_Drug_Histories_LONG <- read.table("DIA Analysis Results 1.1/DIA Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
DIA_Drug_Histories_LONG <- DIA_Drug_Histories_LONG %>% select(patient, weight)
DIA_Drug_Histories <- DIA_Drug_Histories_LONG %>% bind_cols(DIA_Drug_Histories)
rm(DIA_Drug_Histories_LONG)

DIA_Drug_Histories <- gather(DIA_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
DIA_Drug_Histories <- DIA_Drug_Histories %>% group_by(patient) %>% arrange(patient)

DIA_Drug_Histories <- DIA_Drug_Histories %>% group_by(patient) %>% mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

DIA_Drug_Histories <- DIA_Drug_Histories %>% filter(Treat == 1)

Biguanide_Periods <- DIA_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())
names(Biguanide_Periods)[3] <- "Duration"
Biguanide_Periods <- Biguanide_Periods %>% select(patient, Duration) 

DIA_Drug_Histories <- DIA_Drug_Histories %>% select(patient, weight) %>% distinct()
Biguanide_Periods <- Biguanide_Periods %>% left_join(DIA_Drug_Histories) 
Biguanide_Periods <- Biguanide_Periods %>% mutate(weight = as.numeric(weight))
Biguanide_Periods <- Biguanide_Periods %>% group_by(patient) %>% mutate(Total_Duration = sum(Duration)) %>% select(patient, weight, Total_Duration)
Biguanide_Periods <- Biguanide_Periods %>% distinct()

library(spatstat)
weighted.mean(Biguanide_Periods$Total_Duration, Biguanide_Periods$weight)  # 27.60358
weighted.median(Biguanide_Periods$Total_Duration, Biguanide_Periods$weight) # 23.5

data.frame(Biguanide_Periods %>% distinct() %>% group_by(Total_Duration) %>% summarise(pats = sum(weight)))



# ----------------------
# ------------------------------------
# Pathways ----------------------------------

DANU_Ingredients <- fread("DIA Analysis Results 1.1/DANU Ingredients.txt", integer64 = "character", stringsAsFactors = F)
DANU_Ingredients <- DANU_Ingredients %>%  separate(drug_id, c('class', 'molecule'))
DANU_Ingredients <- DANU_Ingredients %>% select(molecule, drug_group) 

DIA_Drug_Histories <- fread("DIA Analysis Results 1.1/DIA Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
Treatment_exp_Vector <- fread("DIA Analysis Results 1.1/Treatment_exp_Vector.txt")

DIA_Drug_Histories <- DIA_Drug_Histories %>% select(-disease) %>% gather(Month, Drugs, month1:month60) %>% filter(Drugs != "-")

DIA_Drug_Histories <- Treatment_exp_Vector %>% inner_join(DIA_Drug_Histories) %>% select(-Month) %>% distinct()
DIA_Drug_Histories <- separate_rows(DIA_Drug_Histories, Drugs, sep = ",", convert=T)

DIA_Drug_Histories <- DIA_Drug_Histories %>%  left_join(DANU_Ingredients %>% mutate(molecule=as.numeric(molecule)), by=c("Drugs"="molecule"))
                                                       
DIA_Drug_Histories <- DIA_Drug_Histories %>% select(-Drugs) %>% distinct()

unique(DIA_Drug_Histories$drug_group)

DIA_Drug_Histories <- DIA_Drug_Histories %>% mutate(drug_group=ifelse(drug_group=="Insulin", "I",
                                                ifelse(drug_group=="Biguanide", "b",
                                                       ifelse(drug_group=="SGLT2", "S",
                                                              ifelse(drug_group=="Antidiabetic", "d",
                                                                     ifelse(drug_group=="DPP4", "D",
                                                                            ifelse(drug_group=="GLP1 Injectable", "G", "g")))))))

DIA_Drug_Histories <- DIA_Drug_Histories %>% distinct() %>%
  group_by(patient, weight) %>% mutate(drug_group=paste(drug_group, collapse=",")) 

DIA_Drug_Histories <- DIA_Drug_Histories %>% ungroup() 

DIA_Drug_Histories %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(weight))

temp <- DIA_Drug_Histories

length(unique(temp$patient))

temp <- temp %>% distinct()

DANU_Demographics <- fread("DANU Demographics 1.1/DANU Demographics.txt")
names(DANU_Demographics)[1] <- "patient"
temp <- temp %>% left_join(DANU_Demographics %>% select(patient, age, gender, diagnosis))

DIA_Disorders <- fread("DIA Analysis Results 1.1/DIA Disorders.txt")

DIA_Disorders <- DIA_Disorders %>% mutate(heart_attack=ifelse(is.na(heart_attack),0,1))
DIA_Disorders <- DIA_Disorders %>% mutate(heart_failure =ifelse(is.na(heart_failure ),0,1))
DIA_Disorders <- DIA_Disorders %>% mutate(vascular_disease=ifelse(is.na(vascular_disease),0,1))
DIA_Disorders <- DIA_Disorders %>% mutate(stroke   =ifelse(is.na(stroke   ),0,1))
DIA_Disorders <- DIA_Disorders %>% mutate(dementia =ifelse(is.na(dementia ),0,1))
DIA_Disorders <- DIA_Disorders %>% mutate(pulmonary_disease =ifelse(is.na(pulmonary_disease ),0,1))
DIA_Disorders <- DIA_Disorders %>% mutate(rheumatic_disease  =ifelse(is.na(rheumatic_disease  ),0,1))
DIA_Disorders <- DIA_Disorders %>% mutate(peptic_ulcer  =ifelse(is.na(peptic_ulcer  ),0,1))
DIA_Disorders <- DIA_Disorders %>% mutate(liver_disease    =ifelse(is.na(liver_disease    ),0,1))
DIA_Disorders <- DIA_Disorders %>% mutate(diabetes =ifelse(is.na(diabetes ),0,1))
DIA_Disorders <- DIA_Disorders %>% mutate(kidney_disease  =ifelse(is.na(kidney_disease  ),0,1))
DIA_Disorders <- DIA_Disorders %>% mutate(diabetes_complications    =ifelse(is.na(diabetes_complications    ),0,1))
DIA_Disorders <- DIA_Disorders %>% mutate(paralysis       =ifelse(is.na(paralysis       ),0,1))
DIA_Disorders <- DIA_Disorders %>% mutate(cancer   =ifelse(is.na(cancer   ),0,1))
DIA_Disorders <- DIA_Disorders %>% mutate(severe_liver_disease  =ifelse(is.na(severe_liver_disease  ),0,1))
DIA_Disorders <- DIA_Disorders %>% mutate(severe_kidney_disease   =ifelse(is.na(severe_kidney_disease   ),0,1))
DIA_Disorders <- DIA_Disorders %>% mutate(metastatic_cancer   =ifelse(is.na(metastatic_cancer   ),0,1))

DIA_Disorders <- DIA_Disorders %>% select(-diabetes)
DIA_Disorders <- DIA_Disorders %>% select(-hiv_infection)

DIA_Disorders <- DIA_Disorders %>% select(-weight)
DIA_Disorders <- DIA_Disorders %>% select(-charlson_index_without_age)

DIA_Disorders <- DIA_Disorders %>% select(patient, charlson_index, kidney_disease, heart_failure , stroke ,  rheumatic_disease , liver_disease, vascular_disease)

temp <- temp %>% left_join(DIA_Disorders)


DANU_Measures <- fread("DANU Measures 1.1/DANU Measures.txt",  integer64 = "character", stringsAsFactors = F)
unique(DANU_Measures$test)
BMI <- DANU_Measures %>% filter(test=="BMI")
HbA1c <- DANU_Measures %>% filter(test=="HbA1c Level")
BMI <- BMI %>% select(patid, value) %>% distinct() %>% group_by(patid ) %>% filter(value==max(value)) %>% slice(1)
HbA1c <- HbA1c %>% select(patid, value) %>% distinct() %>% group_by(patid ) %>% filter(value==max(value)) %>% slice(1)
names(BMI)[2] <- "BMI"
names(HbA1c)[2] <- "HbA1c"
names(BMI)[1] <- "patient"
names(HbA1c)[1] <- "patient"

temp <- temp %>% inner_join(BMI) %>% inner_join(HbA1c)



data.frame(temp %>% ungroup() %>% 
  separate_rows(drug_group, sep = ",", convert=T) %>%
  select(patient, weight, drug_group) %>%
  distinct() %>%
  arrange(patient, weight, drug_group) %>%
  group_by(patient, weight) %>% mutate(drug_group=paste(drug_group, collapse=",")) %>% distinct() %>%
  ungroup() %>%
  group_by(drug_group) %>% summarise(n=sum(weight))) %>%
  arrange(-n)  %>% select(-n)
  
data.frame(temp %>% ungroup() %>% 
  separate_rows(drug_group, sep = ",", convert=T) %>%
  select(patient, weight, drug_group, charlson_index) %>%
  distinct() %>%
  arrange(patient, weight, drug_group) %>%
  group_by(patient, weight) %>% mutate(drug_group=paste(drug_group, collapse=",")) %>% distinct() %>%
  ungroup() %>%
  group_by(drug_group) %>% summarise(n=sum(weight), n2=weighted.mean(charlson_index, weight))) %>%
  arrange(-n)  %>% select(-n) 


data.frame(temp %>% ungroup() %>% 
  separate_rows(drug_group, sep = ",", convert=T) %>%
  select(patient, weight, drug_group) %>%
  distinct() %>%
  arrange(patient, weight, drug_group) %>%
  group_by(patient, weight) %>% mutate(drug_group=paste(drug_group, collapse=",")) %>% distinct() %>%
  ungroup() %>%
  group_by(drug_group) %>% summarise(TOTAL=sum(weight))) %>%
  arrange(-TOTAL)  %>%
  left_join(
data.frame(temp %>% ungroup() %>% 
             filter(kidney_disease==1) %>%
  separate_rows(drug_group, sep = ",", convert=T) %>%
  select(patient, weight, drug_group) %>%
  distinct() %>%
  arrange(patient, weight, drug_group) %>%
  group_by(patient, weight) %>% mutate(drug_group=paste(drug_group, collapse=",")) %>% distinct() %>%
  ungroup() %>%
  group_by(drug_group) %>% summarise(kidney=sum(weight)))
  ) %>%
  left_join(
data.frame(temp %>% ungroup() %>% 
             filter(heart_failure==1) %>%
  separate_rows(drug_group, sep = ",", convert=T) %>%
  select(patient, weight, drug_group) %>%
  distinct() %>%
  arrange(patient, weight, drug_group) %>%
  group_by(patient, weight) %>% mutate(drug_group=paste(drug_group, collapse=",")) %>% distinct() %>%
  ungroup() %>%
  group_by(drug_group) %>% summarise(heart=sum(weight)))
  )  %>%
  left_join(
data.frame(temp %>% ungroup() %>% 
             filter(stroke==1) %>%
  separate_rows(drug_group, sep = ",", convert=T) %>%
  select(patient, weight, drug_group) %>%
  distinct() %>%
  arrange(patient, weight, drug_group) %>%
  group_by(patient, weight) %>% mutate(drug_group=paste(drug_group, collapse=",")) %>% distinct() %>%
  ungroup() %>%
  group_by(drug_group) %>% summarise(stroke=sum(weight)))
  ) %>%
  left_join(
data.frame(temp %>% ungroup() %>% 
             filter(rheumatic_disease==1) %>%
  separate_rows(drug_group, sep = ",", convert=T) %>%
  select(patient, weight, drug_group) %>%
  distinct() %>%
  arrange(patient, weight, drug_group) %>%
  group_by(patient, weight) %>% mutate(drug_group=paste(drug_group, collapse=",")) %>% distinct() %>%
  ungroup() %>%
  group_by(drug_group) %>% summarise(rheumatic_disease=sum(weight)))
  ) %>%
  left_join(
data.frame(temp %>% ungroup() %>% 
             filter(liver_disease==1) %>%
  separate_rows(drug_group, sep = ",", convert=T) %>%
  select(patient, weight, drug_group) %>%
  distinct() %>%
  arrange(patient, weight, drug_group) %>%
  group_by(patient, weight) %>% mutate(drug_group=paste(drug_group, collapse=",")) %>% distinct() %>%
  ungroup() %>%
  group_by(drug_group) %>% summarise(liver_disease=sum(weight)))
  ) %>%
  left_join(
data.frame(temp %>% ungroup() %>% 
             filter(vascular_disease==1) %>%
  separate_rows(drug_group, sep = ",", convert=T) %>%
  select(patient, weight, drug_group) %>%
  distinct() %>%
  arrange(patient, weight, drug_group) %>%
  group_by(patient, weight) %>% mutate(drug_group=paste(drug_group, collapse=",")) %>% distinct() %>%
  ungroup() %>%
  group_by(drug_group) %>% summarise(vascular_disease=sum(weight)))
  )







data.frame(temp %>% ungroup() %>% 
  separate_rows(drug_group, sep = ",", convert=T) %>%
  select(patient, weight, drug_group) %>%
  distinct() %>%
  arrange(patient, weight, drug_group) %>%
  group_by(patient, weight) %>% mutate(drug_group=paste(drug_group, collapse=",")) %>% distinct() %>%
  ungroup() %>%
  group_by(drug_group) %>% summarise(TOTAL=sum(weight))) %>%
  arrange(-TOTAL)  %>%
  left_join(
data.frame(temp %>% ungroup() %>% 
             filter(gender=="F") %>%
  separate_rows(drug_group, sep = ",", convert=T) %>%
  select(patient, weight, drug_group) %>%
  distinct() %>%
  arrange(patient, weight, drug_group) %>%
  group_by(patient, weight) %>% mutate(drug_group=paste(drug_group, collapse=",")) %>% distinct() %>%
  ungroup() %>%
  group_by(drug_group) %>% summarise(gender=sum(weight)))
  )

# ------------------------------
# HbA1c & BMI reductions --------------------------------------

DANU_Ingredients <- fread("DIA Analysis Results 1.1/DANU Ingredients.txt", integer64 = "character", stringsAsFactors = F)
DANU_Ingredients <- DANU_Ingredients %>%  separate(drug_id, c('class', 'molecule'))
DANU_Ingredients <- DANU_Ingredients %>% select(molecule, drug_group)
names(DANU_Ingredients)[1] <- "Drugs"
DANU_Ingredients$Drugs <- as.numeric(DANU_Ingredients$Drugs)


DIA_Drug_Histories <- read.table("DIA Analysis Results 1.1/DIA Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
DIA_Drug_Histories <- gather(DIA_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
DIA_Drug_Histories$Month <- as.character(DIA_Drug_Histories$Month)
DIA_Drug_Histories$Month <- parse_number(DIA_Drug_Histories$Month)
DIA_Drug_Histories <- DIA_Drug_Histories %>% filter(Drugs != "-")
DIA_Drug_Histories <- separate_rows(DIA_Drug_Histories, Drugs, sep = ",", convert=T)
DIA_Drug_Histories <- DIA_Drug_Histories %>% left_join(DANU_Ingredients)
DIA_Drug_Histories <- DIA_Drug_Histories %>% select(-Drugs) %>% distinct()
DIA_Drug_Histories <- DIA_Drug_Histories %>% select(-c(disease, Month)) %>% distinct()
DIA_Drug_Histories <- DIA_Drug_Histories %>% mutate(Exp=1) %>% spread(key=drug_group, value=Exp)
DIA_Drug_Histories[is.na(DIA_Drug_Histories)] <- 0
DIA_Drug_Histories <- DIA_Drug_Histories %>% mutate(SUM=Antidiabetic+Biguanide+DPP4+SGLT2+Insulin+`GLP1 Injectable`+`GLP1 Oral`)

Mono_Big_Pats <- DIA_Drug_Histories %>% filter(SUM==1 | (SUM==2&Biguanide==1))


DIA_Drug_Histories <- read.table("DIA Analysis Results 1.1/DIA Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
DIA_Drug_Histories <- gather(DIA_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
DIA_Drug_Histories$Month <- as.character(DIA_Drug_Histories$Month)
DIA_Drug_Histories$Month <- parse_number(DIA_Drug_Histories$Month)
DIA_Drug_Histories <- DIA_Drug_Histories %>% filter(Drugs != "-")
DIA_Drug_Histories <- separate_rows(DIA_Drug_Histories, Drugs, sep = ",", convert=T)
DIA_Drug_Histories <- DIA_Drug_Histories %>% left_join(DANU_Ingredients)
DIA_Drug_Histories <- DIA_Drug_Histories %>% select(-c(disease, Drugs)) %>% distinct()

DIA_Drug_Histories <- DIA_Drug_Histories %>% filter(drug_group!="Biguanide")
DIA_Drug_Histories <- DIA_Drug_Histories %>% group_by(patient, drug_group) %>% filter(Month==min(Month)) %>% ungroup()
DIA_Drug_Histories <- DIA_Drug_Histories %>% select(-c(weight))

length(unique(DIA_Drug_Histories$patient))
max(DIA_Drug_Histories$Month)


Months_lookup <- fread("Months_lookup.txt")
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")
  
DANU_Measures <- fread("DANU Measures 1.1/DANU Measures.txt",  integer64 = "character", stringsAsFactors = F)
DANU_Measures <- DANU_Measures %>% select(patid, weight, value, test, claimed) %>% distinct()
HbA1c <- DANU_Measures %>% filter(test=="HbA1c Level")
rm(DANU_Measures)
HbA1c <- DIA_Drug_Histories %>% select(patient) %>% inner_join(HbA1c, by=c("patient"="patid"))
HbA1c$Month_Yr <- format(as.Date(HbA1c$claimed), "%Y-%m")
HbA1c <- HbA1c %>% left_join(Months_lookup, by = c("Month_Yr" = "Month")) %>%  filter(!is.na(Exact_Month)) 
HbA1c <- HbA1c %>% select(patient, weight, value, Exact_Month)
HbA1c <- HbA1c %>% group_by(patient, weight, Exact_Month) %>% summarise(HbA1c=mean(value))
HbA1c <- HbA1c %>% ungroup() %>% select(-weight)


temp <- DIA_Drug_Histories %>% left_join(HbA1c) %>% ungroup() %>% arrange(patient) %>% drop_na() 
temp <- temp %>% mutate(Elapsed=Exact_Month-Month) %>% select(-c(Month, Exact_Month))


temp %>% 
  filter(Elapsed>(-12) & Elapsed<12) %>%
  filter(Elapsed<0) %>% select(patient) %>% distinct() %>%
  inner_join(temp %>% filter(Elapsed>0) %>% select(patient) %>% distinct()) %>%
  left_join(temp) %>%
   filter(Elapsed>(-12) & Elapsed<12) %>%
   #filter(drug_group!="Insulin") %>%
    mutate(drug_group=factor(drug_group, levels=c("Antidiabetic", "DPP4", "SGLT2", "Insulin", "GLP1 Oral", "GLP1 Injectable"))) %>%
  ggplot(aes(Elapsed, HbA1c , colour=drug_group, fill=drug_group)) +
  geom_smooth(se=F, size=2) +
  theme_minimal() +
  scale_colour_manual(values=c("#7d95be","#0d2b4e","#daa520","#D1F0D1", "#f6546a","#a52a2a")) +
  xlab("\n No. Elapsed Months \n From HbA1c record to Class Initiation") +
  ylab("HbA1c \n (local linear regression smoothers) \n")


temp %>% filter(Elapsed<0) %>%
  group_by(patient, drug_group) %>%
  filter(BMI==max(BMI)) %>%
  ungroup() %>% group_by(drug_group) %>%
  summarise(n=mean(BMI))


# 1 Antidiabetic     34.9
# 2 DPP4             34.6
# 3 GLP1 Injectable  37.8
# 4 GLP1 Oral        36.7
# 5 Insulin          34.6
# 6 SGLT2            35.8


temp %>% group_by(patient) %>% filter(BMI==max(BMI)) %>% slice(1) %>% rename("MAX"="BMI") %>% select(-Elapsed) %>%
  left_join(
temp %>% filter(Elapsed<0) %>%
  group_by(patient, drug_group) %>%
  filter(Elapsed==max(Elapsed)) %>%
  ungroup()) %>% drop_na() %>%
  mutate(Diff=MAX-BMI) %>%
  #group_by(drug_group) %>% summarise(n=mean(Diff)) %>%
  filter(drug_group=="GLP1 Injectable")

temp %>% filter(Elapsed<0) %>%
  group_by(patient, drug_group) %>%
  filter(Elapsed==max(Elapsed)) %>%
  ungroup() %>% 
  filter(BMI>22&BMI<50) %>%
  ggplot(aes(drug_group, BMI, colour=drug_group, fill=drug_group)) +
  geom_boxplot() +
  facet_wrap(~drug_group)

temp %>% filter(Elapsed>12) %>%
  group_by(patient, drug_group) %>%
  filter(Elapsed==min(Elapsed)) %>%
  ungroup() %>% group_by(drug_group) %>%
  summarise(n=mean(BMI))



temp %>% filter(Elapsed<0) %>%
  group_by(patient, drug_group) %>%
  filter(BMI ==max(BMI )) %>%
  ungroup() %>% select(-Elapsed) %>% rename("Before"="BMI") %>%
  left_join(temp %>% filter(Elapsed>12) %>%
  group_by(patient, drug_group) %>%
  filter(BMI ==min(BMI )) %>%
  ungroup() %>% select(-Elapsed) %>% rename("After"="BMI") 
  ) %>% drop_na() %>% mutate(Diff=After-Before) %>%
  #filter(Before>8&Before<10) %>%
  group_by(drug_group)  %>%
  summarise(n=mean(Diff))


# -----------------------------
# % Paid scripts ---------------------------------------

# Doses All
DIA_US_Doses <- fread("DIA Doses 1.1/DIA Doses.txt")
DIA_US_Doses <- DIA_US_Doses %>% filter(status != "G")
DIA_US_Doses <- DIA_US_Doses %>% filter(paid != "?")
DIA_US_Doses <- DIA_US_Doses %>% select(drug_group, pat_id, from_dt, paid)
DIA_US_Doses$from_dt <- as.Date(DIA_US_Doses$from_dt)
DIA_US_Doses <- DIA_US_Doses %>% arrange(drug_group, pat_id, from_dt)

unique(DIA_US_Doses$drug_group)

# Injectable GLP1
DIA_US_Doses_GLP1_Injectable <- DIA_US_Doses %>% filter(drug_group=="GLP1 Injectable")
Paid_dates_GLP1 <- DIA_US_Doses_GLP1_Injectable %>% filter(paid=="P") %>% select(-paid)
names(Paid_dates_GLP1)[3] <- "Paid_dates"
DIA_US_Doses_GLP1_Injectable <- DIA_US_Doses_GLP1_Injectable %>% left_join(Paid_dates_GLP1)
DIA_US_Doses_GLP1_Injectable <- DIA_US_Doses_GLP1_Injectable %>% group_by(pat_id) %>% mutate(ElapsedTime=as.numeric(from_dt-Paid_dates))
To_keep <- DIA_US_Doses_GLP1_Injectable %>% filter(paid=="D"&ElapsedTime<=0&ElapsedTime>=(-90)) %>% select(-c(Paid_dates, ElapsedTime)) %>% distinct()
To_keep$tokeep <- "Yes"
DIA_US_Doses_GLP1_Injectable <- DIA_US_Doses %>% filter(drug_group=="GLP1 Injectable")
DIA_US_Doses_GLP1_Injectable <- DIA_US_Doses_GLP1_Injectable %>% left_join(To_keep)
DIA_US_Doses_GLP1_Injectable <- DIA_US_Doses_GLP1_Injectable %>% filter(!(paid=="D"&is.na(tokeep)))

DIA_US_Doses_GLP1_Injectable %>% group_by(paid) %>% count()

# 1 D     201026
# 2 P     797903

# Oral GLP1
DIA_US_Doses_GLP1_Oral <- DIA_US_Doses %>% filter(drug_group=="GLP1 Oral")
Paid_dates_GLP1 <- DIA_US_Doses_GLP1_Oral %>% filter(paid=="P") %>% select(-paid)
names(Paid_dates_GLP1)[3] <- "Paid_dates"
DIA_US_Doses_GLP1_Oral <- DIA_US_Doses_GLP1_Oral %>% left_join(Paid_dates_GLP1)
DIA_US_Doses_GLP1_Oral <- DIA_US_Doses_GLP1_Oral %>% group_by(pat_id) %>% mutate(ElapsedTime=as.numeric(from_dt-Paid_dates))
To_keep <- DIA_US_Doses_GLP1_Oral %>% filter(paid=="D"&ElapsedTime<=0&ElapsedTime>=(-90)) %>% select(-c(Paid_dates, ElapsedTime)) %>% distinct()
To_keep$tokeep <- "Yes"
DIA_US_Doses_GLP1_Oral <- DIA_US_Doses %>% filter(drug_group=="GLP1 Oral")
DIA_US_Doses_GLP1_Oral <- DIA_US_Doses_GLP1_Oral %>% left_join(To_keep)
DIA_US_Doses_GLP1_Oral <- DIA_US_Doses_GLP1_Oral %>% filter(!(paid=="D"&is.na(tokeep)))

DIA_US_Doses_GLP1_Oral %>% group_by(paid) %>% count()

# 1 D      7458
# 2 P     25581


# Biguanide
DIA_US_Doses_Biguanide <- DIA_US_Doses %>% filter(drug_group=="Biguanide")
Paid_dates_Biguanide <- DIA_US_Doses_Biguanide %>% filter(paid=="P") %>% select(-paid)
names(Paid_dates_Biguanide)[3] <- "Paid_dates"
DIA_US_Doses_Biguanide <- DIA_US_Doses_Biguanide %>% left_join(Paid_dates_Biguanide)
DIA_US_Doses_Biguanide <- DIA_US_Doses_Biguanide %>% group_by(pat_id) %>% mutate(ElapsedTime=as.numeric(from_dt-Paid_dates))
To_keep <- DIA_US_Doses_Biguanide %>% filter(paid=="D"&ElapsedTime<=0&ElapsedTime>=(-90)) %>% select(-c(Paid_dates, ElapsedTime)) %>% distinct()
To_keep$tokeep <- "Yes"
DIA_US_Doses_Biguanide <- DIA_US_Doses %>% filter(drug_group=="Biguanide")
DIA_US_Doses_Biguanide <- DIA_US_Doses_Biguanide %>% left_join(To_keep)
DIA_US_Doses_Biguanide <- DIA_US_Doses_Biguanide %>% filter(!(paid=="D"&is.na(tokeep)))

DIA_US_Doses_Biguanide %>% group_by(paid) %>% count()

# 1 D      439796
# 2 P     3293694



# Antidiabetic
DIA_US_Doses_Antidiabetic <- DIA_US_Doses %>% filter(drug_group=="Antidiabetic")
Paid_dates_Antidiabetic <- DIA_US_Doses_Antidiabetic %>% filter(paid=="P") %>% select(-paid)
names(Paid_dates_Antidiabetic)[3] <- "Paid_dates"
DIA_US_Doses_Antidiabetic <- DIA_US_Doses_Antidiabetic %>% left_join(Paid_dates_Antidiabetic)
DIA_US_Doses_Antidiabetic <- DIA_US_Doses_Antidiabetic %>% group_by(pat_id) %>% mutate(ElapsedTime=as.numeric(from_dt-Paid_dates))
To_keep <- DIA_US_Doses_Antidiabetic %>% filter(paid=="D"&ElapsedTime<=0&ElapsedTime>=(-90)) %>% select(-c(Paid_dates, ElapsedTime)) %>% distinct()
To_keep$tokeep <- "Yes"
DIA_US_Doses_Antidiabetic <- DIA_US_Doses %>% filter(drug_group=="Antidiabetic")
DIA_US_Doses_Antidiabetic <- DIA_US_Doses_Antidiabetic %>% left_join(To_keep)
DIA_US_Doses_Antidiabetic <- DIA_US_Doses_Antidiabetic %>% filter(!(paid=="D"&is.na(tokeep)))

DIA_US_Doses_Antidiabetic %>% group_by(paid) %>% count()

# 1 D      165743
# 2 P     1294090


# Insulin
DIA_US_Doses_Insulin <- DIA_US_Doses %>% filter(drug_group=="Insulin")
Paid_dates_Insulin <- DIA_US_Doses_Insulin %>% filter(paid=="P") %>% select(-paid)
names(Paid_dates_Insulin)[3] <- "Paid_dates"
DIA_US_Doses_Insulin <- DIA_US_Doses_Insulin %>% left_join(Paid_dates_Insulin)
DIA_US_Doses_Insulin <- DIA_US_Doses_Insulin %>% group_by(pat_id) %>% mutate(ElapsedTime=as.numeric(from_dt-Paid_dates))
To_keep <- DIA_US_Doses_Insulin %>% filter(paid=="D"&ElapsedTime<=0&ElapsedTime>=(-90)) %>% select(-c(Paid_dates, ElapsedTime)) %>% distinct()
To_keep$tokeep <- "Yes"
DIA_US_Doses_Insulin <- DIA_US_Doses %>% filter(drug_group=="Insulin")
DIA_US_Doses_Insulin <- DIA_US_Doses_Insulin %>% left_join(To_keep)
DIA_US_Doses_Insulin <- DIA_US_Doses_Insulin %>% filter(!(paid=="D"&is.na(tokeep)))

DIA_US_Doses_Insulin %>% group_by(paid) %>% count()


# 1 D      607390
# 2 P     2266034



# DPP4
DIA_US_Doses_DPP4 <- DIA_US_Doses %>% filter(drug_group=="DPP4")
Paid_dates_DPP4 <- DIA_US_Doses_DPP4 %>% filter(paid=="P") %>% select(-paid)
names(Paid_dates_DPP4)[3] <- "Paid_dates"
DIA_US_Doses_DPP4 <- DIA_US_Doses_DPP4 %>% left_join(Paid_dates_DPP4)
DIA_US_Doses_DPP4 <- DIA_US_Doses_DPP4 %>% group_by(pat_id) %>% mutate(ElapsedTime=as.numeric(from_dt-Paid_dates))
To_keep <- DIA_US_Doses_DPP4 %>% filter(paid=="D"&ElapsedTime<=0&ElapsedTime>=(-90)) %>% select(-c(Paid_dates, ElapsedTime)) %>% distinct()
To_keep$tokeep <- "Yes"
DIA_US_Doses_DPP4 <- DIA_US_Doses %>% filter(drug_group=="DPP4")
DIA_US_Doses_DPP4 <- DIA_US_Doses_DPP4 %>% left_join(To_keep)
DIA_US_Doses_DPP4 <- DIA_US_Doses_DPP4 %>% filter(!(paid=="D"&is.na(tokeep)))

DIA_US_Doses_DPP4 %>% group_by(paid) %>% count()

# 1 D     118616
# 2 P     645055




# SGLT2
DIA_US_Doses_SGLT2 <- DIA_US_Doses %>% filter(drug_group=="SGLT2")
Paid_dates_SGLT2 <- DIA_US_Doses_SGLT2 %>% filter(paid=="P") %>% select(-paid)
names(Paid_dates_SGLT2)[3] <- "Paid_dates"
DIA_US_Doses_SGLT2 <- DIA_US_Doses_SGLT2 %>% left_join(Paid_dates_SGLT2)
DIA_US_Doses_SGLT2 <- DIA_US_Doses_SGLT2 %>% group_by(pat_id) %>% mutate(ElapsedTime=as.numeric(from_dt-Paid_dates))
To_keep <- DIA_US_Doses_SGLT2 %>% filter(paid=="D"&ElapsedTime<=0&ElapsedTime>=(-90)) %>% select(-c(Paid_dates, ElapsedTime)) %>% distinct()
To_keep$tokeep <- "Yes"
DIA_US_Doses_SGLT2 <- DIA_US_Doses %>% filter(drug_group=="SGLT2")
DIA_US_Doses_SGLT2 <- DIA_US_Doses_SGLT2 %>% left_join(To_keep)
DIA_US_Doses_SGLT2 <- DIA_US_Doses_SGLT2 %>% filter(!(paid=="D"&is.na(tokeep)))

DIA_US_Doses_SGLT2 %>% group_by(paid) %>% count()


# 1 D     128788
# 2 P     651207




DIA_US_Doses_GLP1_Injectable # From above, but ove time now

DIA_US_Doses_GLP1_Injectable$Month_Yr <-  format(as.Date(DIA_US_Doses_GLP1_Injectable$from_dt), "%Y-%m")
DIA_US_Doses_GLP1_Injectable <- DIA_US_Doses_GLP1_Injectable %>% select(-from_dt)

temp <- DIA_US_Doses_GLP1_Injectable %>% group_by(Month_Yr, paid) %>% count() %>%
  spread(key=paid, value=n) %>%
  mutate(total=D+P) %>% 
  mutate(Percent_paid=100*P/total) 

temp$Month_Yr <- paste0("\'",temp$Month_Yr) 

data.frame(temp)

temp %>% ggplot(aes(x=Month_Yr     , y=Percent_paid))+
  geom_point()+
  theme_minimal() +
  ylim(0,100) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  xlab("\n Date") + ylab("% of Paid Injectable GLP1 Scripts \n")

fwrite(temp,"Percent_PaidDenied_GLP1_OverTime.txt", sep="\t")



# --------------------------
# Market segmentation Injectable Naive vs Experienced - Early vs Late --------------------------
DANU_Ingredients <- fread("DIA Analysis Results 1.1/DANU Ingredients.txt", integer64 = "character", stringsAsFactors = F)
DANU_Ingredients <- DANU_Ingredients %>%  separate(drug_id, c('class', 'molecule'))
DANU_Ingredients <- DANU_Ingredients %>% select(molecule, drug_group)
DANU_Ingredients$molecule <- as.numeric(DANU_Ingredients$molecule)

DIA_Drug_Histories <- fread("DIA Analysis Results 1.1/DIA Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
Treatment_exp_Vector <- fread("DIA Analysis Results 1.1/Treatment_exp_Vector.txt")
DIA_Drug_Histories <- Treatment_exp_Vector %>% left_join(DIA_Drug_Histories)
DIA_Drug_Histories <- gather(DIA_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
DIA_Drug_Histories <- DIA_Drug_Histories %>% select(patient, weight, Month, Drugs) %>% distinct() %>% filter(Drugs!="-")
DIA_Drug_Histories <- separate_rows(DIA_Drug_Histories, Drugs, sep = ",", convert=T)
DIA_Drug_Histories$Drugs <- as.numeric(DIA_Drug_Histories$Drugs)
DIA_Drug_Histories$Month <- as.character(DIA_Drug_Histories$Month)
DIA_Drug_Histories$Month <- parse_number(DIA_Drug_Histories$Month)

DIA_Drug_Histories <- DIA_Drug_Histories %>% left_join(DANU_Ingredients, by=c("Drugs"="molecule")) %>% select(patient, weight, Month, drug_group) %>% distinct()


DIA_Drug_Histories <- DIA_Drug_Histories %>% mutate(rank=ifelse(drug_group=="GLP1 Injectable", 1, 
                                                ifelse(drug_group=="Insulin", 2,
                                                       ifelse(drug_group=="GLP1 Oral", 3,
                                                              ifelse(drug_group=="SGLT2", 4,
                                                                     ifelse(drug_group=="DPP4"|drug_group=="Antidiabetc",5,6)))))) 


Inj_naive_early <- DIA_Drug_Histories %>%group_by(patient) %>% filter(rank==min(rank)) %>% slice(1) %>% ungroup() %>%
  filter(rank==6|rank==5) %>% select(patient, weight) %>% distinct()

sum(Inj_naive_early$weight) # 13449533

Inj_naive_advanced <- DIA_Drug_Histories %>% anti_join(Inj_naive_early) %>% group_by(patient) %>% 
  filter(rank==min(rank)) %>% slice(1) %>% ungroup() %>%
  filter(rank==4|rank==3) %>% select(patient, weight) %>% distinct()

sum(Inj_naive_advanced$weight) # 2267763


DIA_Drug_Histories <- DIA_Drug_Histories %>% anti_join(Inj_naive_advanced) %>% anti_join(Inj_naive_early)


GLP1_Inj <- DIA_Drug_Histories  %>% group_by(patient) %>% 
  filter(rank==min(rank)) %>% slice(1) %>% ungroup() %>%
  filter(rank==1) %>% select(patient, weight) %>% distinct() 

sum(GLP1_Inj$weight) # 6444437


Insulins <- DIA_Drug_Histories %>% anti_join(GLP1_Inj) %>% group_by(patient) %>% 
  filter(rank==min(rank)) %>% slice(1) %>% ungroup() %>%
  filter(rank==2) %>% select(patient, weight) %>% distinct() 

sum(Insulins$weight) # 7958353




DIA_Drug_Histories <- fread("DIA Analysis Results 1.1/DIA Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
Treatment_exp_Vector <- fread("DIA Analysis Results 1.1/Treatment_exp_Vector.txt")
DIA_Drug_Histories <- Treatment_exp_Vector %>% left_join(DIA_Drug_Histories) %>% select(-c(disease))
DIA_Drug_Histories %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(weight))
DIA_Drug_Histories <- gather(DIA_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
DIA_Drug_Histories <- DIA_Drug_Histories %>% filter(Drugs!="-")
DIA_Drug_Histories$Month <- as.character(DIA_Drug_Histories$Month)
DIA_Drug_Histories$Month <- parse_number(DIA_Drug_Histories$Month)
  
DANU_Ingredients       <- fread("DIA Analysis Results 1.1/DANU Ingredients.txt", integer64 = "character", stringsAsFactors = F)
DANU_Ingredients       <- DANU_Ingredients %>%  separate(drug_id, c('class', 'molecule'))
string_Injectables       <- paste0("\\b(",paste0(DANU_Ingredients$molecule[DANU_Ingredients$drug_group == "GLP1 Injectable"], collapse = "|"),")\\b")
string_Oral    <- paste0("\\b(",paste0(DANU_Ingredients$molecule[DANU_Ingredients$drug_group != "GLP1 Injectable"], collapse = "|"),")\\b")

DIA_Drug_Histories <- DIA_Drug_Histories %>% mutate(Inj = ifelse( grepl(string_Injectables, Drugs)&grepl(string_Oral, Drugs), "I+O",
                                                                  ifelse(grepl(string_Injectables, Drugs),"I", 
                                                                        ifelse(grepl(string_Oral,Drugs), "O", NA))))

DIA_Drug_Histories_2 <- DIA_Drug_Histories %>% select(patient, weight, Month, Inj) %>%
  filter(grepl("I", Inj)) %>% select(patient) %>% distinct() %>%
  left_join(DIA_Drug_Histories %>% select(patient, weight, Month, Inj))

DIA_Drug_Histories_2 <- DIA_Drug_Histories_2 %>% spread(key=Month, value=Inj)
DIA_Drug_Histories_2[is.na(DIA_Drug_Histories_2)] <- "-"
DIA_Drug_Histories_2 <- gather(DIA_Drug_Histories_2, Month, Drugs, `1`:`60`, factor_key=TRUE)
DIA_Drug_Histories_2 <- DIA_Drug_Histories_2 %>% arrange(patient, Month)

DIA_Drug_Histories_3 <- DIA_Drug_Histories_2 %>% group_by(patient, weight) %>% 
  slice(if(any(grepl("I",Drugs))) which.max(grepl("I",Drugs)):which.max(Month=="60") else NA) 

DIA_Drug_Histories_3 <- DIA_Drug_Histories_3 %>% group_by(patient) %>% mutate(grp = rle(Drugs)$lengths %>% {rep(seq(length(.)), .)})
DIA_Drug_Histories_3 <- DIA_Drug_Histories_3 %>% arrange(patient, Month)
DIA_Drug_Histories_3 %>% ungroup() %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(weight)) # 6444437 Inj exp

DIA_Drug_Histories_3 %>% ungroup() %>% filter(grp!=1) %>%
   filter(!grepl("I", Drugs)) %>% select(patient, weight, Drugs, grp) %>%
  group_by(patient, weight, grp) %>% count() %>% ungroup() %>%
  filter(n>=6) %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(weight)) 


Stopped_Inj_GLP1 <- DIA_Drug_Histories_3 %>% ungroup() %>% filter(grp!=1) %>%
   filter(!grepl("I", Drugs)) %>% select(patient, weight, Drugs, grp) %>%
  group_by(patient, weight, grp) %>% count() %>% ungroup() %>%
  filter(n>=6) %>% select(patient, weight) %>% distinct()

Returned_Inj_GLP1 <- DIA_Drug_Histories_3 %>% ungroup() %>% filter(grp!=1) %>%
   filter(!grepl("I", Drugs)) %>% select(patient, weight, Drugs, grp) %>%
  group_by(patient, weight, grp) %>% count() %>% ungroup() %>%
  filter(n>=6) %>% select(patient, weight, grp) %>% distinct() %>%
  group_by(patient, weight) %>% filter(grp==min(grp)) %>%
  rename("Stopgrp"="grp") %>%
  inner_join(
    DIA_Drug_Histories_3 %>% ungroup() %>% filter(grp!=1) %>%
   filter(grepl("I", Drugs)) %>% select(patient, weight, grp) %>% distinct() %>%
  rename("Injgrp"="grp")
  ) %>%
  filter(Injgrp>Stopgrp) %>%
  select(patient, weight)  %>% distinct() 

Stopped_Inj_GLP1 <- Stopped_Inj_GLP1 %>% anti_join(Returned_Inj_GLP1)












DIA_Drug_Histories <- fread("DIA Analysis Results 1.1/DIA Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
Treatment_exp_Vector <- fread("DIA Analysis Results 1.1/Treatment_exp_Vector.txt")
DIA_Drug_Histories <- Treatment_exp_Vector %>% left_join(DIA_Drug_Histories) %>% select(-c(disease))
DIA_Drug_Histories %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(weight))
DIA_Drug_Histories <- gather(DIA_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
DIA_Drug_Histories <- DIA_Drug_Histories %>% filter(Drugs!="-")
DIA_Drug_Histories$Month <- as.character(DIA_Drug_Histories$Month)
DIA_Drug_Histories$Month <- parse_number(DIA_Drug_Histories$Month)
DIA_Drug_Histories <- Insulins %>% inner_join(DIA_Drug_Histories)

DANU_Ingredients       <- fread("DIA Analysis Results 1.1/DANU Ingredients.txt", integer64 = "character", stringsAsFactors = F)
DANU_Ingredients       <- DANU_Ingredients %>%  separate(drug_id, c('class', 'molecule'))
string_Injectables       <- paste0("\\b(",paste0(DANU_Ingredients$molecule[DANU_Ingredients$drug_group == "Insulin"], collapse = "|"),")\\b")
string_Oral    <- paste0("\\b(",paste0(DANU_Ingredients$molecule[DANU_Ingredients$drug_group != "Insulin"], collapse = "|"),")\\b")

DIA_Drug_Histories <- DIA_Drug_Histories %>% mutate(Inj = ifelse( grepl(string_Injectables, Drugs)&grepl(string_Oral, Drugs), "I+O",
                                                                  ifelse(grepl(string_Injectables, Drugs),"I", 
                                                                        ifelse(grepl(string_Oral,Drugs), "O", NA))))

DIA_Drug_Histories_2 <- DIA_Drug_Histories %>% select(patient, weight, Month, Inj) %>%
  filter(grepl("I", Inj)) %>% select(patient) %>% distinct() %>%
  left_join(DIA_Drug_Histories %>% select(patient, weight, Month, Inj))

DIA_Drug_Histories_2 <- DIA_Drug_Histories_2 %>% spread(key=Month, value=Inj)
DIA_Drug_Histories_2[is.na(DIA_Drug_Histories_2)] <- "-"
DIA_Drug_Histories_2 <- gather(DIA_Drug_Histories_2, Month, Drugs, `1`:`60`, factor_key=TRUE)
DIA_Drug_Histories_2 <- DIA_Drug_Histories_2 %>% arrange(patient, Month)

DIA_Drug_Histories_3 <- DIA_Drug_Histories_2 %>% group_by(patient, weight) %>% 
  slice(if(any(grepl("I",Drugs))) which.max(grepl("I",Drugs)):which.max(Month=="60") else NA) 

DIA_Drug_Histories_3 <- DIA_Drug_Histories_3 %>% group_by(patient) %>% mutate(grp = rle(Drugs)$lengths %>% {rep(seq(length(.)), .)})
DIA_Drug_Histories_3 <- DIA_Drug_Histories_3 %>% arrange(patient, Month)
DIA_Drug_Histories_3 %>% ungroup() %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(weight)) # 7958353 Insulin
DIA_Drug_Histories_3 %>% ungroup() %>% filter(grp!=1) %>%
   filter(!grepl("I", Drugs)) %>% select(patient, weight, Drugs, grp) %>%
  group_by(patient, weight, grp) %>% count() %>% ungroup() %>%
  filter(n>=6) %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(weight))  # 4860376 stop


Stopped_Insulin <- DIA_Drug_Histories_3 %>% ungroup() %>% filter(grp!=1) %>%
   filter(!grepl("I", Drugs)) %>% select(patient, weight, Drugs, grp) %>%
  group_by(patient, weight, grp) %>% count() %>% ungroup() %>%
  filter(n>=6) %>% select(patient, weight) %>% distinct()

Returned_Insulin <- DIA_Drug_Histories_3 %>% ungroup() %>% filter(grp!=1) %>%
   filter(!grepl("I", Drugs)) %>% select(patient, weight, Drugs, grp) %>%
  group_by(patient, weight, grp) %>% count() %>% ungroup() %>%
  filter(n>=6) %>% select(patient, weight, grp) %>% distinct() %>%
  group_by(patient, weight) %>% filter(grp==min(grp)) %>%
  rename("Stopgrp"="grp") %>%
  inner_join(
    DIA_Drug_Histories_3 %>% ungroup() %>% filter(grp!=1) %>%
   filter(grepl("I", Drugs)) %>% select(patient, weight, grp) %>% distinct() %>%
  rename("Injgrp"="grp")
  ) %>%
  filter(Injgrp>Stopgrp) %>%
  select(patient, weight)  %>% distinct() 

Stopped_Insulin <- Stopped_Insulin %>% anti_join(Returned_Insulin)


sum(Inj_naive_early$weight) # 13449533
sum(Inj_naive_advanced$weight) # 2267763
 
sum(GLP1_Inj$weight) # 6444437   # 4622001
sum(Stopped_Inj_GLP1$weight) # 1822436

sum(Insulins$weight) # 7958353  4928148
sum(Stopped_Insulin$weight) # 3030205

GLP1_Inj <- GLP1_Inj %>% anti_join(Stopped_Inj_GLP1)
Insulins <- Insulins %>% anti_join(Stopped_Insulin)

Inj_naive_early$group <- "Early"
Inj_naive_advanced$group <- "Advanced"
GLP1_Inj$group <- "GLP1_Inj"
Insulins$group <- "Insulins"
Stopped_Inj_GLP1$group <- "Stopped_Inj_GLP1"
Stopped_Insulin$group <- "Stopped_Insulin"

New_Segmentation <- Inj_naive_early %>% bind_rows(Inj_naive_advanced) %>% bind_rows(GLP1_Inj) %>% bind_rows(Insulins) %>% bind_rows(Stopped_Inj_GLP1) %>% bind_rows(Stopped_Insulin)
fwrite(New_Segmentation, "DIA Analysis Results 1.1/New_Segmentation.txt", sep="\t")

# -------------------------------------------


# New Segmentation per BMI groups --------------------------------
New_Segmentation <- fread("DIA Analysis Results 1.1/New_Segmentation.txt", sep="\t")

DIA_Comorbidity_Inventories <- fread("DIA Analysis Results 1.1/DIA Comorbidity Inventories.txt")

CKD <- unique(DIA_Comorbidity_Inventories[grepl("N18",diagnosis), c("patid","weight")])
POS <- unique(DIA_Comorbidity_Inventories[grepl("E28",diagnosis), c("patid","weight")])
PAD <- unique(DIA_Comorbidity_Inventories[grepl("I70",diagnosis)|grepl("I73",diagnosis), c("patid","weight")])
SLEEPAPNEA <- unique(DIA_Comorbidity_Inventories[grepl("G47",diagnosis), c("patid","weight")])
HF <- unique(DIA_Comorbidity_Inventories[grepl("I5",diagnosis), c("patid","weight")])
DISLIPIDEMIA <- unique(DIA_Comorbidity_Inventories[grepl("E78",diagnosis), c("patid","weight")])
HTN <- unique(DIA_Comorbidity_Inventories[grepl("I10",diagnosis), c("patid","weight")])
OA <- unique(DIA_Comorbidity_Inventories[grepl("M15", diagnosis)|grepl("M16", diagnosis)|grepl("M17", diagnosis)|
                                     grepl("M18", diagnosis)| grepl("M19", diagnosis), c("patid","weight")])
NASH <- unique(DIA_Comorbidity_Inventories[grepl("K75",diagnosis), c("patid","weight")])
PREDIABETES <- unique(DIA_Comorbidity_Inventories[grepl("R73",diagnosis), c("patid","weight")])
IHD <- unique(DIA_Comorbidity_Inventories[grepl("I20", diagnosis)|grepl("I21", diagnosis)| grepl("I22", diagnosis)|
                                   grepl("I23", diagnosis)| grepl("I24", diagnosis)| grepl("I25", diagnosis), c("patid","weight")])


Comorb <- Reduce(function (...) { merge(..., all = TRUE) },    list(CKD, POS, PAD, HF, SLEEPAPNEA, DISLIPIDEMIA, HTN, OA, NASH, IHD, PREDIABETES)) 


Comorb <-  Comorb %>% select(patid, weight) %>% distinct() %>% mutate(comorb="comorb") %>% rename("patient"="patid")

New_Segmentation <- New_Segmentation %>% left_join(Comorb)


DANU_Measures <- fread("DANU Measures 1.1/DANU Measures.txt",  integer64 = "character", stringsAsFactors = F)
DANU_Measures <- DANU_Measures %>% inner_join(New_Segmentation %>% select(patient), by=c("patid"="patient"))
DANU_Measures <- DANU_Measures %>% filter(test=="BMI")
DANU_Measures <- DANU_Measures %>% select(patid, value) %>% distinct() %>% group_by(patid ) %>% filter(value==max(value)) %>% slice(1)
DANU_Measures <- DANU_Measures %>% mutate(value=ifelse(value<25, "<25",
                                                       ifelse(value>=25&value<27, "25-27",
                                                              ifelse(value>=27&value<30, "27-30", ">=30")))) 

New_Segmentation <- New_Segmentation %>% inner_join(DANU_Measures, by=c("patient"="patid"))

data.frame(New_Segmentation %>% mutate(comorb=ifelse(value=="27-30", comorb, NA)) %>%
  group_by(group, value, comorb) %>% summarise(n=sum(weight)) %>%
    spread(key=value, value=n))

# ------------------------------

# Highest rank per HbA1c / BMI bucket --------------------
DANU_Measures <- fread("DANU Measures 1.1/DANU Measures.txt",  integer64 = "character", stringsAsFactors = F)

DANU_Measures <- DANU_Measures %>% filter(test=="BMI")
DANU_Measures <- DANU_Measures %>% select(patid, value) %>% distinct() %>% group_by(patid ) %>% filter(value==max(value)) %>% slice(1)
DANU_Measures <- DANU_Measures %>% mutate(value=ifelse(value<25, 25, 
                                                       ifelse(value<27,27,
                                                              ifelse(value<30,30,
                                                                     ifelse(value<40,40,50)))))

DANU_Ingredients <- fread("DIA Analysis Results 1.1/DANU Ingredients.txt", integer64 = "character", stringsAsFactors = F)
DANU_Ingredients <- DANU_Ingredients %>%  separate(drug_id, c('class', 'molecule'))
DANU_Ingredients <- DANU_Ingredients %>% select(molecule, drug_group)
DANU_Ingredients$molecule <- as.numeric(DANU_Ingredients$molecule)

DIA_Drug_Histories <- fread("DIA Analysis Results 1.1/DIA Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
Treatment_exp_Vector <- fread("DIA Analysis Results 1.1/Treatment_exp_Vector.txt")
DIA_Drug_Histories <- Treatment_exp_Vector %>% left_join(DIA_Drug_Histories)
DIA_Drug_Histories <- gather(DIA_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
DIA_Drug_Histories <- DIA_Drug_Histories %>% select(patient, weight, Drugs) %>% distinct() %>% filter(Drugs!="-")
DIA_Drug_Histories <- separate_rows(DIA_Drug_Histories, Drugs, sep = ",", convert=T)
DIA_Drug_Histories$Drugs <- as.numeric(DIA_Drug_Histories$Drugs)
DIA_Drug_Histories <- DIA_Drug_Histories %>% select(patient, weight, Drugs) %>% distinct() 
DIA_Drug_Histories <- DIA_Drug_Histories %>% left_join(DANU_Ingredients, by=c("Drugs"="molecule")) %>% select(patient, weight, drug_group) %>% distinct()


DIA_Drug_Histories %>% mutate(drug_group=ifelse(drug_group=="GLP1 Injectable", 1, 
                                                ifelse(drug_group=="Insulin", 2,
                                                       ifelse(drug_group=="GLP1 Oral", 3,
                                                              ifelse(drug_group=="SGLT2", 4,
                                                                     ifelse(drug_group=="DPP4"|drug_group=="Antidiabetc",5,6)))))) %>%
  group_by(patient) %>% filter(drug_group==min(drug_group)) %>% slice(1) %>% ungroup() %>% group_by(drug_group) %>% summarise(n=sum(weight))



Ranks <- DIA_Drug_Histories %>% mutate(drug_group=ifelse(drug_group=="GLP1 Injectable", 1, 
                                                ifelse(drug_group=="Insulin", 2,
                                                       ifelse(drug_group=="GLP1 Oral", 3,
                                                              ifelse(drug_group=="SGLT2", 4,
                                                                     ifelse(drug_group=="DPP4"|drug_group=="Antidiabetc",5,6)))))) %>%
  group_by(patient) %>% filter(drug_group==min(drug_group)) %>% slice(1) %>% ungroup()

Ranks <- Ranks %>% inner_join(DANU_Measures, by=c("patient"="patid"))
Ranks %>% group_by(value, drug_group) %>% summarise(n=sum(weight)) %>% spread(key=value, value=n)
# ------------------
# Individual comorbidities / ICD10s per rank class----------------------------------
DANU_Ingredients <- fread("DIA Analysis Results 1.1/DANU Ingredients.txt", integer64 = "character", stringsAsFactors = F)
DANU_Ingredients <- DANU_Ingredients %>%  separate(drug_id, c('class', 'molecule'))
DANU_Ingredients <- DANU_Ingredients %>% select(molecule, drug_group)
DANU_Ingredients$molecule <- as.numeric(DANU_Ingredients$molecule)

DIA_Drug_Histories <- fread("DIA Analysis Results 1.1/DIA Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
Treatment_exp_Vector <- fread("DIA Analysis Results 1.1/Treatment_exp_Vector.txt")
DIA_Drug_Histories <- Treatment_exp_Vector %>% left_join(DIA_Drug_Histories)
DIA_Drug_Histories <- gather(DIA_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
DIA_Drug_Histories <- DIA_Drug_Histories %>% select(patient, weight, Drugs) %>% distinct() %>% filter(Drugs!="-")
DIA_Drug_Histories <- separate_rows(DIA_Drug_Histories, Drugs, sep = ",", convert=T)
DIA_Drug_Histories$Drugs <- as.numeric(DIA_Drug_Histories$Drugs)
DIA_Drug_Histories <- DIA_Drug_Histories %>% select(patient, weight, Drugs) %>% distinct() 
DIA_Drug_Histories <- DIA_Drug_Histories %>% left_join(DANU_Ingredients, by=c("Drugs"="molecule")) %>% select(patient, weight, drug_group) %>% distinct()

Ranks <- DIA_Drug_Histories %>% mutate(drug_group=ifelse(drug_group=="GLP1 Injectable", 1, 
                                                ifelse(drug_group=="Insulin", 2,
                                                       ifelse(drug_group=="GLP1 Oral", 3,
                                                              ifelse(drug_group=="SGLT2", 4,
                                                                     ifelse(drug_group=="DPP4"|drug_group=="Antidiabetc",5,6)))))) %>%
  group_by(patient) %>% filter(drug_group==min(drug_group)) %>% slice(1) %>% ungroup()


DIA_Comorbidity_Inventories <- fread("DIA Analysis Results 1.1/DIA Comorbidity Inventories.txt")
names(DIA_Comorbidity_Inventories)[1] <- "patient"
DIA_Comorbidity_Inventories <- DIA_Comorbidity_Inventories %>% inner_join(Ranks)



DIA_Comorbidity_Inventories %>% select(patient, weight, drug_group) %>% distinct() %>%  group_by(drug_group) %>% summarise(n=sum(weight)) %>%
  left_join(
    DIA_Comorbidity_Inventories %>% filter(grepl("N18",diagnosis)) %>%  group_by(drug_group) %>% summarise(n2=sum(weight))
  ) %>%
  mutate(percent=(n2/n))


DIA_Comorbidity_Inventories %>% select(patient, weight, drug_group) %>% distinct() %>%  group_by(drug_group) %>% summarise(n=sum(weight)) %>%
  left_join(
    DIA_Comorbidity_Inventories %>% filter(grepl("I10",diagnosis)) %>%  group_by(drug_group) %>% summarise(n2=sum(weight))
  ) %>%
  mutate(percent=(n2/n))

DIA_Comorbidity_Inventories %>% select(patient, weight, drug_group) %>% distinct() %>%  group_by(drug_group) %>% summarise(n=sum(weight)) %>%
  left_join(
    DIA_Comorbidity_Inventories %>% filter(grepl("I20", diagnosis)|grepl("I21", diagnosis)| grepl("I22", diagnosis)|
                                   grepl("I23", diagnosis)| grepl("I24", diagnosis)| grepl("I25", diagnosis)) %>% 
      select(patient, weight, drug_group)  %>%  group_by(drug_group) %>% summarise(n2=sum(weight))
  ) %>%
  mutate(percent=(n2/n))




temp <- data.frame(
DIA_Comorbidity_Inventories %>% select(patient, weight, drug_group) %>% distinct() %>%  group_by(drug_group) %>% summarise(n=sum(weight)) %>%
  left_join(
    DIA_Comorbidity_Inventories %>% group_by(drug_group, diagnosis) %>% summarise(n2=sum(weight)) 
  )  %>%
  mutate(percent=(n2/n)) %>% select(-c(n, n2)) %>%
  spread(key=drug_group , value=percent)
) %>%
  filter(grepl("E", diagnosis)|grepl("F", diagnosis)| grepl("G", diagnosis)| grepl("I", diagnosis)| grepl("J", diagnosis)|
           grepl("K", diagnosis)|grepl("M", diagnosis)| grepl("N", diagnosis)|grepl("R", diagnosis))

temp[is.na(temp)] <- 0

temp$fold_change <- temp$X1/temp$X6

temp %>% filter(X1>0.1) %>% 
  arrange(-fold_change)


# -----------------
# Age ditribution per rank --------------------------

DANU_Ingredients <- fread("DIA Analysis Results 1.1/DANU Ingredients.txt", integer64 = "character", stringsAsFactors = F)
DANU_Ingredients <- DANU_Ingredients %>%  separate(drug_id, c('class', 'molecule'))
DANU_Ingredients <- DANU_Ingredients %>% select(molecule, drug_group)
DANU_Ingredients$molecule <- as.numeric(DANU_Ingredients$molecule)

DIA_Drug_Histories <- fread("DIA Analysis Results 1.1/DIA Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
Treatment_exp_Vector <- fread("DIA Analysis Results 1.1/Treatment_exp_Vector.txt")
DIA_Drug_Histories <- Treatment_exp_Vector %>% left_join(DIA_Drug_Histories)
DIA_Drug_Histories <- gather(DIA_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
DIA_Drug_Histories <- DIA_Drug_Histories %>% select(patient, weight, Drugs) %>% distinct() %>% filter(Drugs!="-")
DIA_Drug_Histories <- separate_rows(DIA_Drug_Histories, Drugs, sep = ",", convert=T)
DIA_Drug_Histories$Drugs <- as.numeric(DIA_Drug_Histories$Drugs)
DIA_Drug_Histories <- DIA_Drug_Histories %>% select(patient, weight, Drugs) %>% distinct() 
DIA_Drug_Histories <- DIA_Drug_Histories %>% left_join(DANU_Ingredients, by=c("Drugs"="molecule")) %>% select(patient, weight, drug_group) %>% distinct()

Ranks <- DIA_Drug_Histories %>% mutate(drug_group=ifelse(drug_group=="GLP1 Injectable", 1, 
                                                ifelse(drug_group=="Insulin", 2,
                                                       ifelse(drug_group=="GLP1 Oral", 3,
                                                              ifelse(drug_group=="SGLT2", 4,
                                                                     ifelse(drug_group=="DPP4"|drug_group=="Antidiabetc",5,6)))))) %>%
  group_by(patient) %>% filter(drug_group==min(drug_group)) %>% slice(1) %>% ungroup()


DANU_Demographics <- fread("DANU Demographics 1.1/DANU Demographics.txt", integer64 = "character", stringsAsFactors = F)
DANU_Demographics <- DANU_Demographics %>% select(patid, weight, age) %>% rename("patient"="patid")

Ranks %>% inner_join(DANU_Demographics) %>% group_by(drug_group) %>% summarise(n=weighted.mean(age, weight))
max(DANU_Demographics$age)



Ranks %>% inner_join(DANU_Demographics) %>% mutate(new_bin = ifelse(age<50, "<50",
                                                                    ifelse(age<65, "<65",
                                                                           ifelse(age<75, "<75", ">=75")))) %>%
  group_by(drug_group, new_bin) %>% summarise(n=sum(weight)) %>%
  spread(key=new_bin, value=n)

# ------------------
# How many start SGLT2 first vs GLP1 first --------------------

DANU_Ingredients <- fread("DIA Analysis Results 1.1/DANU Ingredients.txt", integer64 = "character", stringsAsFactors = F)
DANU_Ingredients <- DANU_Ingredients %>%  separate(drug_id, c('class', 'molecule'))
string_Biguanide       <- paste0("\\b(",paste0(DANU_Ingredients$molecule[DANU_Ingredients$drug_group == "Biguanide"], collapse = "|"),")\\b")
string_Antidiabetic    <- paste0("\\b(",paste0(DANU_Ingredients$molecule[DANU_Ingredients$drug_group == "Antidiabetic"], collapse = "|"),")\\b")
string_DPP4            <- paste0("\\b(",paste0(DANU_Ingredients$molecule[DANU_Ingredients$drug_group == "DPP4"], collapse = "|"),")\\b")
string_SGLT2           <- paste0("\\b(",paste0(DANU_Ingredients$molecule[DANU_Ingredients$drug_group == "SGLT2"], collapse = "|"),")\\b")
string_Insulin         <- paste0("\\b(",paste0(DANU_Ingredients$molecule[DANU_Ingredients$drug_group == "Insulin"], collapse = "|"),")\\b")
string_OralGLP1        <- paste0("\\b(",paste0(DANU_Ingredients$molecule[DANU_Ingredients$drug_group == "GLP1 Oral"], collapse = "|"),")\\b")
string_InjectableGLP1  <- paste0("\\b(",paste0(DANU_Ingredients$molecule[DANU_Ingredients$drug_group == "GLP1 Injectable"], collapse = "|"),")\\b")


DIA_Drug_Histories <- fread("DIA Analysis Results 1.1/DIA Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
DIA_Drug_Histories <- DIA_Drug_Histories %>% select(-c(disease)) 
DIA_Drug_Histories <- gather(DIA_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
DIA_Drug_Histories <- DIA_Drug_Histories %>% select(patient, Treat, Month, weight) %>% distinct() %>% filter(Treat!="-")
DIA_Drug_Histories$Month <- as.character(DIA_Drug_Histories$Month)
DIA_Drug_Histories$Month <- parse_number(DIA_Drug_Histories$Month)

SGLT2_Inj_GLP1_pats <- DIA_Drug_Histories %>% filter(grepl(string_Antidiabetic, Treat)) %>% select(patient, weight) %>% distinct() %>% 
  inner_join(DIA_Drug_Histories %>% filter(grepl(string_InjectableGLP1, Treat)) %>% select(patient, weight) %>% distinct())
sum(SGLT2_Inj_GLP1_pats$weight) 

SGLT2_Inj_GLP1_pats_Combos <- SGLT2_Inj_GLP1_pats %>% inner_join(DIA_Drug_Histories) %>% 
  filter(grepl(string_Antidiabetic, Treat)&grepl(string_InjectableGLP1, Treat)) %>%
  select(patient, weight) %>% distinct()
sum(SGLT2_Inj_GLP1_pats_Combos$weight) 

  SGLT2_Inj_GLP1_pats_Combos %>% inner_join(DIA_Drug_Histories) %>% filter(grepl(string_Antidiabetic, Treat)) %>%
  group_by(patient) %>% filter(Month==min(Month)) %>% select(patient, weight, Month) %>% rename("First_SGLT2"="Month") %>%
  inner_join(
    SGLT2_Inj_GLP1_pats_Combos %>% inner_join(DIA_Drug_Histories) %>% filter(grepl(string_InjectableGLP1, Treat)) %>%
  group_by(patient) %>% filter(Month==min(Month)) %>% select(patient, weight, Month) %>% rename("First_GLP1_Inj"="Month")
  ) %>% mutate(First=ifelse(First_SGLT2<First_GLP1_Inj, "SGLT2",
                            ifelse(First_SGLT2>First_GLP1_Inj, "GLP1", "Both"))) %>%
  group_by(First) %>% summarise(n=sum(weight))
  
# -------------------

# How many start SGLT2 first vs Biguanide first --------------------
    DANU_Ingredients <- fread("DIA Analysis Results 1.1/DANU Ingredients.txt", integer64 = "character", stringsAsFactors = F)
DANU_Ingredients <- DANU_Ingredients %>%  separate(drug_id, c('class', 'molecule'))
string_Biguanide       <- paste0("\\b(",paste0(DANU_Ingredients$molecule[DANU_Ingredients$drug_group == "Biguanide"], collapse = "|"),")\\b")
string_Antidiabetic    <- paste0("\\b(",paste0(DANU_Ingredients$molecule[DANU_Ingredients$drug_group == "Antidiabetic"], collapse = "|"),")\\b")
string_DPP4            <- paste0("\\b(",paste0(DANU_Ingredients$molecule[DANU_Ingredients$drug_group == "DPP4"], collapse = "|"),")\\b")
string_SGLT2           <- paste0("\\b(",paste0(DANU_Ingredients$molecule[DANU_Ingredients$drug_group == "SGLT2"], collapse = "|"),")\\b")
string_Insulin         <- paste0("\\b(",paste0(DANU_Ingredients$molecule[DANU_Ingredients$drug_group == "Insulin"], collapse = "|"),")\\b")
string_OralGLP1        <- paste0("\\b(",paste0(DANU_Ingredients$molecule[DANU_Ingredients$drug_group == "GLP1 Oral"], collapse = "|"),")\\b")
string_InjectableGLP1  <- paste0("\\b(",paste0(DANU_Ingredients$molecule[DANU_Ingredients$drug_group == "GLP1 Injectable"], collapse = "|"),")\\b")


DIA_Drug_Histories <- fread("DIA Analysis Results 1.1/DIA Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
DIA_Drug_Histories <- DIA_Drug_Histories %>% select(-c(disease)) 
DIA_Drug_Histories <- gather(DIA_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
DIA_Drug_Histories <- DIA_Drug_Histories %>% select(patient, Treat, Month, weight) %>% distinct() %>% filter(Treat!="-")
DIA_Drug_Histories$Month <- as.character(DIA_Drug_Histories$Month)
DIA_Drug_Histories$Month <- parse_number(DIA_Drug_Histories$Month)

SGLT2_Big_pats <- DIA_Drug_Histories %>% filter(grepl(string_SGLT2, Treat)) %>% select(patient, weight) %>% distinct() %>% 
  inner_join(DIA_Drug_Histories %>% filter(grepl(string_Biguanide, Treat)) %>% select(patient, weight) %>% distinct())
sum(SGLT2_Big_pats$weight) 

SGLT2_Big_pats_Combos <- SGLT2_Big_pats %>% inner_join(DIA_Drug_Histories) %>% 
  filter(grepl(string_SGLT2, Treat)&grepl(string_Biguanide, Treat)) %>%
  select(patient, weight) %>% distinct()
sum(SGLT2_Big_pats_Combos$weight) 

SGLT2_Big_pats_Combos %>% inner_join(DIA_Drug_Histories) %>% filter(grepl(string_SGLT2, Treat)) %>%
  group_by(patient) %>% filter(Month==min(Month)) %>% select(patient, weight, Month) %>% rename("First_SGLT2"="Month") %>%
  inner_join(
    SGLT2_Big_pats_Combos %>% inner_join(DIA_Drug_Histories) %>% filter(grepl(string_Biguanide, Treat)) %>%
  group_by(patient) %>% filter(Month==min(Month)) %>% select(patient, weight, Month) %>% rename("First_Big"="Month")
  ) %>% mutate(First=ifelse(First_SGLT2<First_Big, "SGLT2",
                            ifelse(First_SGLT2>First_Big, "BIG", "Both"))) %>%
  group_by(First) %>% summarise(n=sum(weight))



SGLT2_Big_pats_Combos %>% inner_join(DIA_Drug_Histories) %>% filter(grepl(string_SGLT2, Treat)) %>%
  group_by(patient) %>% filter(Month==min(Month)) %>% select(patient, weight, Month) %>% rename("First_SGLT2"="Month") %>%
  inner_join(
    SGLT2_Big_pats_Combos %>% inner_join(DIA_Drug_Histories) %>% filter(grepl(string_Biguanide, Treat)) %>%
  group_by(patient) %>% filter(Month==min(Month)) %>% select(patient, weight, Month) %>% rename("First_Big"="Month")
  ) %>% mutate(First=ifelse(First_SGLT2<First_Big, "SGLT2",
                            ifelse(First_SGLT2>First_Big, "BIG", "Both"))) %>% 
  ungroup() %>% summarise(mean=mean(First_Big))



groups <- SGLT2_Big_pats_Combos %>% inner_join(DIA_Drug_Histories) %>% filter(grepl(string_SGLT2, Treat)) %>%
  group_by(patient) %>% filter(Month==min(Month)) %>% select(patient, weight, Month) %>% rename("First_SGLT2"="Month") %>%
  inner_join(
    SGLT2_Big_pats_Combos %>% inner_join(DIA_Drug_Histories) %>% filter(grepl(string_Biguanide, Treat)) %>%
  group_by(patient) %>% filter(Month==min(Month)) %>% select(patient, weight, Month) %>% rename("First_Big"="Month")
  ) %>% mutate(First=ifelse(First_SGLT2<First_Big, "SGLT2",
                            ifelse(First_SGLT2>First_Big, "BIG", "Both"))) %>% 
  ungroup() %>% select(patient, weight, First)



Dia_US_Doses <- fread("DIA Doses 1.1/DIA Doses.txt")
DIA_Medications <- fread("DIA Analysis Results 1.1/DANU Medications.txt")

DIA_Medications <- DIA_Medications %>% filter(drug_group=="SGLT2")
DIA_Medications <- DIA_Medications %>% select(drug_id, med_ingredient, med_explanation)
DIA_Medications <- DIA_Medications %>% filter(grepl("/", med_ingredient))
DIA_Medications <- DIA_Medications %>% filter(grepl("METFORMIN", med_ingredient))
DIA_Medications <- DIA_Medications %>% select(drug_id)

range(as.Date(Dia_US_Doses$from_dt))


Dia_US_Doses %>% mutate(from_dt=as.Date(from_dt)) %>% filter(from_dt>="2021-07-01") %>%
  inner_join(DIA_Medications) %>%
  select(pat_id, weight) %>% distinct() %>%
  inner_join(groups %>% rename("pat_id"="patient")) %>%
  group_by(First) %>%
  summarise(n=sum(weight)) 

# ---------------------
# -----------------
# Comorbidities everyone from market clarity --------------------

Mkt_Clarity_PAD_PCOS_SLAP_CKD_HFpEF_ce18_pts <- fread("DIA Analysis Results 1.1/Mkt_Clarity_PAD_PCOS_SLAP_CKD_HFpEF_ce18_pts.txt")
names(Mkt_Clarity_PAD_PCOS_SLAP_CKD_HFpEF_ce18_pts)[1] <- "patid"
sum(Mkt_Clarity_PAD_PCOS_SLAP_CKD_HFpEF_ce18_pts$weight2)

PAD_restrict <- Mkt_Clarity_PAD_PCOS_SLAP_CKD_HFpEF_ce18_pts %>% filter(PAD_restrict ==1) %>% select(patid, weight2)
PCOS <- Mkt_Clarity_PAD_PCOS_SLAP_CKD_HFpEF_ce18_pts %>% filter(PCOS ==1) %>% select(patid, weight2)
OSLAP <- Mkt_Clarity_PAD_PCOS_SLAP_CKD_HFpEF_ce18_pts %>% filter(OSLAP ==1) %>% select(patid, weight2)
CKD  <- Mkt_Clarity_PAD_PCOS_SLAP_CKD_HFpEF_ce18_pts %>% filter(CKD ==1) %>% select(patid, weight2)
HFpEF <- Mkt_Clarity_PAD_PCOS_SLAP_CKD_HFpEF_ce18_pts %>% filter(HFpEF==1) %>% select(patid, weight2)

sum(PAD_restrict$weight2) # 26731570
sum(PCOS$weight2) # 6653417
sum(OSLAP$weight2) # 55756184
sum(CKD$weight2) # 29297524
sum(HFpEF$weight2) # 3623025

PAD_restrict$group <- "PAD_restrict"
PCOS$group <- "PCOS"
CKD$group <- "CKD"
OSLAP$group <- "OSLAP"
HFpEF$group <- "HFpEF"

Comorbs <- PAD_restrict %>% bind_rows(PCOS) %>% bind_rows(CKD) %>% bind_rows(OSLAP) %>% bind_rows(HFpEF)

DANU_Demographics <- fread("DANU Demographics 1.1/DANU Demographics.txt")
DANU_Demographics <- DANU_Demographics %>% select(patid, weight, diagnosis) %>% filter(diagnosis!="-") 
DANU_Demographics %>%  group_by(diagnosis) %>% summarise(n=sum(weight))

OBE2_Demographics_All <- fread("OBE2 Demographics All.txt")
OBE2_Demographics_All <- OBE2_Demographics_All %>%  filter(diagnosis=="Obesity") %>% select(patid, weight) %>% mutate(weight=weight*1.515)
names(OBE2_Demographics_All)[2] <- "OBE_weight"

DANU_Demographics <- DANU_Demographics %>% left_join(OBE2_Demographics_All) %>% mutate(weight=ifelse(is.na(OBE_weight), weight, OBE_weight))


DANU_Demographics <- DANU_Demographics%>% filter(diagnosis!="-")
DANU_Demographics %>% group_by(diagnosis) %>% summarise(n=sum(weight))

DANU_Demographics %>% inner_join(PAD_restrict) %>% group_by(diagnosis) %>% summarise(n=sum(weight))
DANU_Demographics %>% inner_join(PCOS) %>% group_by(diagnosis) %>% summarise(n=sum(weight))
DANU_Demographics %>% inner_join(OSLAP) %>% group_by(diagnosis) %>% summarise(n=sum(weight))
DANU_Demographics %>% inner_join(CKD) %>% group_by(diagnosis) %>% summarise(n=sum(weight))
DANU_Demographics %>% inner_join(HFpEF) %>% group_by(diagnosis) %>% summarise(n=sum(weight))


DANU_Demographics %>% left_join(Comorbs) %>% group_by(diagnosis, group) %>% summarise(n=sum(weight)) %>%
  mutate(n=ifelse(diagnosis=="Diabetes", n/7354170, 
                  ifelse(diagnosis=="Obesity", n/200931666, n/38675539))) %>%
  spread(key=group, value=n)


DANU_Demographics %>% left_join(Comorbs) %>% group_by(diagnosis, group) %>% summarise(n=sum(weight)) %>% filter(group=="CKD")


DIA_Comorbidity_Inventories <- fread("DIA Analysis Results 1.1/DIA Comorbidity Inventories.txt")
DIA_Comorbidity_Inventories <- DIA_Comorbidity_Inventories %>% filter(grepl("N18", diagnosis))

DIA_Comorbidity_Inventories %>% select(patid) %>% distinct() %>% 
  inner_join(DANU_Demographics) %>% group_by(diagnosis) %>% summarise(n=sum(weight)) 


# ------------
# Create New_Comorbidity_Groups_Jun1 -----------------

Mkt_Clarity_PAD_PCOS_SLAP_CKD_HFpEF_ce18_pts <- fread("DIA Analysis Results 1.1/Mkt_Clarity_PAD_PCOS_SLAP_CKD_HFpEF_ce18_pts.txt")
names(Mkt_Clarity_PAD_PCOS_SLAP_CKD_HFpEF_ce18_pts)[1] <- "patid"
sum(Mkt_Clarity_PAD_PCOS_SLAP_CKD_HFpEF_ce18_pts$weight2)

PAD_restrict <- Mkt_Clarity_PAD_PCOS_SLAP_CKD_HFpEF_ce18_pts %>% filter(PAD_restrict ==1) %>% select(patid, weight2)
PCOS <- Mkt_Clarity_PAD_PCOS_SLAP_CKD_HFpEF_ce18_pts %>% filter(PCOS ==1) %>% select(patid, weight2)
OSLAP <- Mkt_Clarity_PAD_PCOS_SLAP_CKD_HFpEF_ce18_pts %>% filter(OSLAP ==1) %>% select(patid, weight2)
CKD  <- Mkt_Clarity_PAD_PCOS_SLAP_CKD_HFpEF_ce18_pts %>% filter(CKD ==1) %>% select(patid, weight2)
HFpEF <- Mkt_Clarity_PAD_PCOS_SLAP_CKD_HFpEF_ce18_pts %>% filter(HFpEF==1) %>% select(patid, weight2)

sum(PAD_restrict$weight2) # 26731570
sum(PCOS$weight2) # 6653417
sum(OSLAP$weight2) # 55756184
sum(CKD$weight2) # 29297524
sum(HFpEF$weight2) # 3623025

PAD_restrict$group <- "PAD_restrict"
PCOS$group <- "PCOS"
CKD$group <- "CKD"
OSLAP$group <- "OSLAP"
HFpEF$group <- "HFpEF"
Comorbs <- PAD_restrict %>% bind_rows(PCOS) %>% bind_rows(CKD) %>% bind_rows(OSLAP) %>% bind_rows(HFpEF)



DANU_Demographics <- fread("DANU Demographics 1.1/DANU Demographics.txt")
DANU_Demographics <- DANU_Demographics %>% select(patid, weight, diagnosis) %>% filter(diagnosis!="-")
DANU_Demographics %>%  group_by(diagnosis) %>% summarise(n=sum(weight))

Treatment_exp_Vector <- fread("DIA Analysis Results 1.1/Treatment_exp_Vector.txt")
Treatment_exp_Vector$Exp <- "Exp"
names(Treatment_exp_Vector)[1] <- "patid"

DANU_Demographics <- DANU_Demographics %>% left_join(Treatment_exp_Vector) %>%
mutate(diagnosis=ifelse(grepl("Diabetes", diagnosis)&is.na(Exp), NA, diagnosis))  %>% select(-Exp) %>% drop_na()
DANU_Demographics %>% group_by(diagnosis) %>% summarise(n=sum(weight))

OBE2_Comorbidity_Inventories <- fread("DIA Analysis Results 1.1/OBE2 Comorbidity Inventories.txt")

CKD <- unique(OBE2_Comorbidity_Inventories[grepl("N18",diagnosis), c("patid","weight")])
POS <- unique(OBE2_Comorbidity_Inventories[grepl("E28",diagnosis), c("patid","weight")])
PAD <- unique(OBE2_Comorbidity_Inventories[grepl("I70",diagnosis)|grepl("I73",diagnosis), c("patid","weight")])
SLEEPAPNEA <- unique(OBE2_Comorbidity_Inventories[grepl("G47",diagnosis), c("patid","weight")])
HF <- unique(OBE2_Comorbidity_Inventories[grepl("I5",diagnosis), c("patid","weight")])
DISLIPIDEMIA <- unique(OBE2_Comorbidity_Inventories[grepl("E78",diagnosis), c("patid","weight")])
HTN <- unique(OBE2_Comorbidity_Inventories[grepl("I10",diagnosis), c("patid","weight")])
OA <- unique(OBE2_Comorbidity_Inventories[grepl("M15", diagnosis)|grepl("M16", diagnosis)|grepl("M17", diagnosis)|
grepl("M18", diagnosis)| grepl("M19", diagnosis), c("patid","weight")])
NASH <- unique(OBE2_Comorbidity_Inventories[grepl("K75",diagnosis), c("patid","weight")])
PREDIABETES <- unique(OBE2_Comorbidity_Inventories[grepl("R73",diagnosis), c("patid","weight")])
IHD <- unique(OBE2_Comorbidity_Inventories[grepl("I20", diagnosis)|grepl("I21", diagnosis)| grepl("I22", diagnosis)|
grepl("I23", diagnosis)| grepl("I24", diagnosis)| grepl("I25", diagnosis), c("patid","weight")])

Comorb <- Reduce(function (...) { merge(..., all = TRUE) },    list(CKD, POS, PAD, HF, SLEEPAPNEA, DISLIPIDEMIA, HTN, OA, NASH, IHD, PREDIABETES))

Comorb$Comorb <- "Comorb"

DANU_Measures <- fread("DANU Measures 1.1/DANU Measures.txt",  integer64 = "character", stringsAsFactors = F)
DANU_Measures <- DANU_Demographics %>% select(patid) %>% inner_join(DANU_Measures)
DANU_Measures <- DANU_Measures %>% filter(test=="BMI")
DANU_Measures <- DANU_Measures %>% select(patid, value) %>% distinct() %>% group_by(patid ) %>% filter(value==max(value)) %>% slice(1)
DANU_Measures <- DANU_Measures %>% filter(value>=25) %>% mutate(value=ifelse(value>=25&value<27, "25-27", ifelse(value>=27&value<30,"27-30", ">30" )))

DANU_Demographics <- DANU_Demographics %>% left_join(DANU_Measures) %>% 
  mutate(diagnosis=ifelse(diagnosis=="Obesity"&is.na(value), NA, diagnosis)) %>% 
  select(-value) %>% drop_na()

DANU_Demographics %>% group_by(diagnosis) %>% summarise(n=sum(weight))


DANU_Demographics <- DANU_Demographics %>% left_join(DANU_Measures) %>% left_join(Comorb %>% select(-weight)) %>%
mutate(diagnosis=ifelse( (diagnosis=="Obesity"&value=="27-30"&is.na(Comorb)) | (diagnosis=="Obesity" & value=="25-27") , NA, diagnosis)) %>% 
  select(-c(value, Comorb)) %>% drop_na() # %>% group_by(diagnosis) %>% summarise(n=sum(weight))
  
DANU_Demographics %>% group_by(diagnosis) %>% summarise(n=sum(weight))


DANU_Demographics %>% left_join(Comorbs) %>% group_by(diagnosis, group) %>% summarise(n=sum(weight)) %>% 
  mutate(n=ifelse(diagnosis=="Diabetes", n/3862796., ifelse(diagnosis=="Obesity", n/49151140, n/26257291.))) %>%
spread(key=group, value=n)

New_Comorbidity_Groups_Jun1 <- DANU_Demographics %>% left_join(Comorbs)
fwrite(New_Comorbidity_Groups_Jun1, "DIA Analysis Results 1.1/New_Comorbidity_Groups_Jun1.txt")




# ------------------------------------------------
# Max rank and bmi groups - per comorbdiity from pedro ------------

DANU_Ingredients <- fread("DIA Analysis Results 1.1/DANU Ingredients.txt", integer64 = "character", stringsAsFactors = F)
DANU_Ingredients <- DANU_Ingredients %>%  separate(drug_id, c('class', 'molecule'))
DANU_Ingredients <- DANU_Ingredients %>% select(molecule, drug_group)
DANU_Ingredients$molecule <- as.numeric(DANU_Ingredients$molecule)

DIA_Drug_Histories <- fread("DIA Analysis Results 1.1/DIA Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
Treatment_exp_Vector <- fread("DIA Analysis Results 1.1/Treatment_exp_Vector.txt")
DIA_Drug_Histories <- Treatment_exp_Vector %>% left_join(DIA_Drug_Histories)
DIA_Drug_Histories <- gather(DIA_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
DIA_Drug_Histories <- DIA_Drug_Histories %>% select(patient, weight, Drugs) %>% distinct() %>% filter(Drugs!="-")
DIA_Drug_Histories <- separate_rows(DIA_Drug_Histories, Drugs, sep = ",", convert=T)
DIA_Drug_Histories$Drugs <- as.numeric(DIA_Drug_Histories$Drugs)
DIA_Drug_Histories <- DIA_Drug_Histories %>% select(patient, weight, Drugs) %>% distinct() 
DIA_Drug_Histories <- DIA_Drug_Histories %>% left_join(DANU_Ingredients, by=c("Drugs"="molecule")) %>% select(patient, weight, drug_group) %>% distinct()

Ranks <- DIA_Drug_Histories %>% mutate(drug_group=ifelse(drug_group=="GLP1 Injectable", 1, 
                                                ifelse(drug_group=="Insulin", 2,
                                                       ifelse(drug_group=="GLP1 Oral", 3,
                                                              ifelse(drug_group=="SGLT2", 4,
                                                                     ifelse(drug_group=="DPP4"|drug_group=="Antidiabetc",5,6)))))) %>%
  group_by(patient) %>% filter(drug_group==min(drug_group)) %>% slice(1) %>% ungroup()


New_Comorbidity_Groups_Jun1 <- fread("DIA Analysis Results 1.1/New_Comorbidity_Groups_Jun1.txt")
names(New_Comorbidity_Groups_Jun1)[1] <- "patient"
New_Comorbidity_Groups_Jun1 <- New_Comorbidity_Groups_Jun1 %>% select(patient, diagnosis, group)

Ranks %>% left_join(New_Comorbidity_Groups_Jun1) %>% ungroup() %>%
  group_by(drug_group, group   ) %>% summarise(n=sum(weight)) %>%
  spread(key=group, value=n)

DANU_Demographics <- fread("DANU Demographics 1.1/DANU Demographics.txt", integer64 = "character", stringsAsFactors = F)
New_Comorbidity_Groups_Jun1 <- New_Comorbidity_Groups_Jun1 %>% filter(diagnosis=="Obesity")

DANU_Demographics <- DANU_Demographics %>% select(patid, weight) %>% rename("patient"="patid")

New_Comorbidity_Groups_Jun1 %>% left_join(DANU_Demographics)  %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(weight))


DANU_Measures <- fread("DANU Measures 1.1/DANU Measures.txt",  integer64 = "character", stringsAsFactors = F)
names(DANU_Measures)[1] <- "patient"
DANU_Measures <- DANU_Measures %>% inner_join(New_Comorbidity_Groups_Jun1 %>% select(patient))
DANU_Measures <- DANU_Measures %>% filter(test=="BMI")
DANU_Measures <- DANU_Measures %>% select(patient, value, weight) %>% distinct() %>% group_by(patient ) %>% filter(value==max(value)) %>% slice(1)

DANU_Measures <- DANU_Measures %>% filter(value>=25) %>% mutate(value=ifelse(value>=25&value<27, "25-27", ifelse(value>=27&value<30,"27-30", ">30" )))

DANU_Measures %>% select(patient, value, weight) %>%
  group_by(value) %>% summarise(n=sum(weight))

DANU_Measures %>% select(patient, value, weight) %>% left_join(New_Comorbidity_Groups_Jun1) %>% 
  group_by(value, group) %>% summarise(n=sum(weight)) %>%
  spread(key=group, value=n)

New_Comorbidity_Groups_Jun1 <- fread("DIA Analysis Results 1.1/New_Comorbidity_Groups_Jun1.txt")

New_Comorbidity_Groups_Jun1 %>% group_by(diagnosis, group) %>% summarise(n=sum(weight)) %>%
  spread(key=diagnosis, value=n)


New_Comorbidity_Groups_Jun1 %>%select(patid, weight, diagnosis) %>% distinct() %>%
  group_by(diagnosis) %>%
  summarise(n=sum(weight))



# ---------------------
# New Segmentation per BMI groups DIA+OBE --------------------------------
New_Segmentation <- fread("DIA Analysis Results 1.1/New_Segmentation.txt", sep="\t")

DANU_Demographics <- fread("DANU Demographics 1.1/DANU Demographics.txt", integer64 = "character", stringsAsFactors = F)
DANU_Demographics <- DANU_Demographics %>% filter(grepl("Diabetes", diagnosis)) %>%
  select(patid, weight, diagnosis) %>% rename("patient"="patid")

New_Segmentation <- DANU_Demographics %>% left_join(New_Segmentation)

DIA_Comorbidity_Inventories <- fread("DIA Analysis Results 1.1/DIA Comorbidity Inventories.txt")

CKD <- unique(DIA_Comorbidity_Inventories[grepl("N18",diagnosis), c("patid","weight")])
POS <- unique(DIA_Comorbidity_Inventories[grepl("E28",diagnosis), c("patid","weight")])
PAD <- unique(DIA_Comorbidity_Inventories[grepl("I70",diagnosis)|grepl("I73",diagnosis), c("patid","weight")])
SLEEPAPNEA <- unique(DIA_Comorbidity_Inventories[grepl("G47",diagnosis), c("patid","weight")])
HF <- unique(DIA_Comorbidity_Inventories[grepl("I5",diagnosis), c("patid","weight")])
DISLIPIDEMIA <- unique(DIA_Comorbidity_Inventories[grepl("E78",diagnosis), c("patid","weight")])
HTN <- unique(DIA_Comorbidity_Inventories[grepl("I10",diagnosis), c("patid","weight")])
OA <- unique(DIA_Comorbidity_Inventories[grepl("M15", diagnosis)|grepl("M16", diagnosis)|grepl("M17", diagnosis)|
                                     grepl("M18", diagnosis)| grepl("M19", diagnosis), c("patid","weight")])
NASH <- unique(DIA_Comorbidity_Inventories[grepl("K75",diagnosis), c("patid","weight")])
PREDIABETES <- unique(DIA_Comorbidity_Inventories[grepl("R73",diagnosis), c("patid","weight")])
IHD <- unique(DIA_Comorbidity_Inventories[grepl("I20", diagnosis)|grepl("I21", diagnosis)| grepl("I22", diagnosis)|
                                   grepl("I23", diagnosis)| grepl("I24", diagnosis)| grepl("I25", diagnosis), c("patid","weight")])


Comorb <- Reduce(function (...) { merge(..., all = TRUE) },    list(CKD, POS, PAD, HF, SLEEPAPNEA, DISLIPIDEMIA, HTN, OA, NASH, IHD, PREDIABETES)) 


Comorb <-  Comorb %>% select(patid, weight) %>% distinct() %>% mutate(comorb="comorb") %>% rename("patient"="patid")

DANU_Demographics %>% filter(diagnosis=="Diabetes + Obesity") %>% select(patient) %>% left_join(Comorb %>% select(patient, comorb)) %>%
  inner_join(New_Segmentation) %>%
  group_by(group, comorb) %>% summarise(n=sum(weight))

New_Segmentation <- New_Segmentation %>% left_join(Comorb)


DANU_Measures <- fread("DANU Measures 1.1/DANU Measures.txt",  integer64 = "character", stringsAsFactors = F)
DANU_Measures <- DANU_Measures %>% inner_join(New_Segmentation %>% select(patient), by=c("patid"="patient"))
DANU_Measures <- DANU_Measures %>% filter(test=="BMI")
DANU_Measures <- DANU_Measures %>% select(patid, value) %>% distinct() %>% group_by(patid ) %>% filter(value==max(value)) %>% slice(1)
DANU_Measures <- DANU_Measures %>% mutate(value=ifelse(value<25, "<25",
                                                       ifelse(value>=25&value<27, "25-27",
                                                              ifelse(value>=27&value<30, "27-30", ">=30")))) 

New_Segmentation <- New_Segmentation %>% inner_join(DANU_Measures, by=c("patient"="patid"))

data.frame(New_Segmentation %>% inner_join(DANU_Demographics %>% filter(grepl("Obesity", diagnosis)) %>% select(patient)) %>%
  mutate(comorb=ifelse(value=="27-30", comorb, NA)) %>%
  group_by(group, value, comorb) %>% summarise(n=sum(weight)) %>%
    spread(key=value, value=n))


# ----------------
# Comorbidity oer target group per age group ------------------------
New_Comorbidity_Groups_Jun1 <- fread("DIA Analysis Results 1.1/New_Comorbidity_Groups_Jun1.txt")

New_Comorbidity_Groups_Jun1 %>% group_by(diagnosis, group) %>% summarise(n=sum(weight)) %>% 
  mutate(n=ifelse(diagnosis=="Diabetes", n/3862796., ifelse(diagnosis=="Obesity", n/49151140, n/26257291.))) %>%
spread(key=group, value=n)


DANU_Demographics <- fread("DANU Demographics 1.1/DANU Demographics.txt", integer64 = "character", stringsAsFactors = F)
DANU_Demographics <- DANU_Demographics %>% select(patid, weight, age) 

  
New_Comorbidity_Groups_Jun1 %>% left_join(DANU_Demographics) %>% mutate(new_bin = ifelse(age<50, "<50",
                                                                    ifelse(age<65, "<65",
                                                                           ifelse(age<75, "<75", ">=75")))) %>%
  select(patid, diagnosis, weight, new_bin) %>% distinct() %>%
  group_by(diagnosis, new_bin) %>% summarise(n=sum(weight)) %>% arrange(new_bin)



New_Comorbidity_Groups_Jun1 %>% left_join(DANU_Demographics) %>% mutate(new_bin = ifelse(age<50, "<50",
                                                                    ifelse(age<65, "<65",
                                                                           ifelse(age<75, "<75", ">=75")))) %>%
  group_by(diagnosis, new_bin, group) %>% summarise(n=sum(weight)) %>%
  spread(key=group, value=n)
  

# ----------
# Comorbidity per age group per drug usage ever ------------------------
New_Comorbidity_Groups_Jun1 <- fread("DIA Analysis Results 1.1/New_Comorbidity_Groups_Jun1.txt")

DANU_Demographics <- fread("DANU Demographics 1.1/DANU Demographics.txt", integer64 = "character", stringsAsFactors = F)
DANU_Demographics <- DANU_Demographics %>% select(patid, weight, age) 


New_Comorbidity_Groups_Jun1 %>% left_join(DANU_Demographics) %>% mutate(new_bin = ifelse(age<50, "<50",
                                                                    ifelse(age<65, "<65",
                                                                           ifelse(age<75, "<75", ">=75")))) %>%
  filter(grepl("Diabetes", diagnosis)) %>% select(patid, weight, new_bin) %>% distinct() %>%
   summarise(n=sum(weight)) 


New_Comorbidity_Groups_Jun1 %>% left_join(DANU_Demographics) %>% mutate(new_bin = ifelse(age<50, "<50",
                                                                    ifelse(age<65, "<65",
                                                                           ifelse(age<75, "<75", ">=75")))) %>%
  filter(grepl("Diabetes", diagnosis)) %>% select(patid, weight, group, new_bin) %>% distinct() %>%
  group_by(group, new_bin) %>% summarise(n=sum(weight)) %>%
  spread(key=new_bin, value=n)


#   group             `<50`    `<65`    `<75`   `>=75`
# 1 ""             4548792. 5210642. 3370784. 1799245.
# 2 "CKD"           388916. 1353744. 1993043. 2296692.
# 3 "HFpEF"          47094.  180363.  220885.  209387.
# 4 "OSLAP"        1659650. 3129290. 2219065. 1167790.
# 5 "PAD_restrict"  323488. 1138240. 1589346. 1714238 
# 6 "PCOS"         1327774.  121056.   13901.    3769.

DANU_Ingredients <- fread("DIA Analysis Results 1.1/DANU Ingredients.txt", integer64 = "character", stringsAsFactors = F)
DANU_Ingredients <- DANU_Ingredients %>%  separate(drug_id, c('class', 'molecule'))
DANU_Ingredients <- DANU_Ingredients %>% select(molecule, drug_group)
DANU_Ingredients$molecule <- as.numeric(DANU_Ingredients$molecule)

DIA_Drug_Histories <- fread("DIA Analysis Results 1.1/DIA Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
Treatment_exp_Vector <- fread("DIA Analysis Results 1.1/Treatment_exp_Vector.txt")
DIA_Drug_Histories <- Treatment_exp_Vector %>% left_join(DIA_Drug_Histories)
DIA_Drug_Histories <- gather(DIA_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
DIA_Drug_Histories <- DIA_Drug_Histories %>% select(patient, weight, Drugs) %>% distinct() %>% filter(Drugs!="-")
DIA_Drug_Histories <- separate_rows(DIA_Drug_Histories, Drugs, sep = ",", convert=T)
DIA_Drug_Histories$Drugs <- as.numeric(DIA_Drug_Histories$Drugs)
DIA_Drug_Histories <- DIA_Drug_Histories %>% select(patient, weight, Drugs) %>% distinct() 
DIA_Drug_Histories <- DIA_Drug_Histories %>% left_join(DANU_Ingredients, by=c("Drugs"="molecule")) %>% select(patient, weight, drug_group) %>% distinct()



data.frame(
  New_Comorbidity_Groups_Jun1 %>% left_join(DANU_Demographics) %>% mutate(new_bin = ifelse(age<50, "<50",
                                                                    ifelse(age<65, "<65",
                                                                           ifelse(age<75, "<75", ">=75")))) %>%
  filter(grepl("Diabetes", diagnosis)) %>% select(patid, weight, group, new_bin) %>% distinct() %>%
  group_by(group, new_bin) %>% summarise(n=sum(weight)) %>%
  left_join(
New_Comorbidity_Groups_Jun1 %>% left_join(DANU_Demographics) %>% mutate(new_bin = ifelse(age<50, "<50",
                                                                    ifelse(age<65, "<65",
                                                                           ifelse(age<75, "<75", ">=75")))) %>%
  filter(grepl("Diabetes", diagnosis)) %>% select(patid, weight, group, new_bin) %>% distinct() %>%
  inner_join(DIA_Drug_Histories, by=c("patid"="patient", "weight"="weight")) %>% distinct() %>%
  group_by(group, new_bin, drug_group) %>% summarise(n2=sum(weight))
 )
) %>%
  spread(key=drug_group, value=n2) %>%
  arrange(new_bin)

# -------------------
# BMI vs HbA1c all DIA treat-exp -------------

New_Comorbidity_Groups_Jun1 <- fread("DIA Analysis Results 1.1/New_Comorbidity_Groups_Jun1.txt")
New_Comorbidity_Groups_Jun1 <- New_Comorbidity_Groups_Jun1 %>% filter(diagnosis=="Diabetes + Obesity") %>% select(patid, weight) %>% distinct()
sum(New_Comorbidity_Groups_Jun1$weight) # 26257291


DANU_Measures <- fread("DANU Measures 1.1/DANU Measures.txt",  integer64 = "character", stringsAsFactors = F)
DANU_Measures <- New_Comorbidity_Groups_Jun1 %>% inner_join(DANU_Measures)
BMI <- DANU_Measures %>% filter(test=="BMI")
HbA1c <- DANU_Measures %>% filter(test=="HbA1c Level")

BMI <- BMI %>% select(patid, value) %>% distinct() %>% group_by(patid ) %>% filter(value==max(value)) %>% slice(1)
HbA1c <- HbA1c %>% select(patid, value) %>% distinct() %>% group_by(patid ) %>% filter(value==max(value)) %>% slice(1)

rm(DANU_Measures)

names(BMI)[2] <- "BMI"
names(HbA1c)[2] <- "HbA1c"


BMI %>% inner_join(HbA1c) %>%
  inner_join(New_Comorbidity_Groups_Jun1) %>%
    mutate(BMI=ifelse(BMI<27, "<27",
                    ifelse(BMI<30, "<30",
                           ifelse(BMI<35, "<35", ">35")))) %>%
  mutate(HbA1c=ifelse(HbA1c<6.5, "<6.5",
                      ifelse(HbA1c<7.5, "<7.5",
                             ifelse(HbA1c<8.5, "<8.5",
                                    ifelse(HbA1c<9.5,"<9.5", ">9.5"))))) %>%
  group_by(BMI, HbA1c) %>%
  summarise(n=sum(weight)*2.493987) %>%
  spread(key=HbA1c, value=n)

# ------------------
# ASCVD in DIA or OBE  -------------

New_Comorbidity_Groups_Jun1 <- fread("DIA Analysis Results 1.1/New_Comorbidity_Groups_Jun1.txt")
New_Comorbidity_Groups_Jun1 <- New_Comorbidity_Groups_Jun1 %>% mutate(diagnosis=ifelse(grepl("Diabetes", diagnosis), "Diabetes", diagnosis))
New_Comorbidity_Groups_Jun1 %>% group_by(diagnosis) %>% summarise(n=sum(weight))
New_Comorbidity_Groups_Jun1 %>% select(patid,weight,diagnosis) %>% distinct() %>% group_by(diagnosis) %>% summarise(n=sum(weight))
New_Comorbidity_Groups_Jun1 <- New_Comorbidity_Groups_Jun1 %>% select(patid,weight,diagnosis) %>% distinct()

# 1 Diabetes  30120087.
# 2 Obesity   49151140.

DIA_Comorbidity_Inventories <- fread("DIA Analysis Results 1.1/DIA Comorbidity Inventories.txt")
DIA_Comorbidity_Inventories <- DIA_Comorbidity_Inventories %>% select(patid, diagnosis)

DIA_ASCVD <- DIA_Comorbidity_Inventories %>% filter(grepl("G45", diagnosis)|
                                         grepl("I63", diagnosis)|
                                         grepl("H34", diagnosis)|
                                         grepl("I65", diagnosis)|
                                         grepl("I66", diagnosis)|
                                         grepl("I69", diagnosis)|
                                         grepl("I20", diagnosis)|
                                         grepl("I21", diagnosis)|
                                         grepl("I22", diagnosis)|
                                         grepl("I23", diagnosis)|
                                         grepl("I24", diagnosis)|
                                         grepl("I25", diagnosis)|
                                         grepl("I70", diagnosis)|
                                         grepl("I73", diagnosis)) %>% select(patid) %>% distinct()

DIA_ASCVD$DIA_ASCVD <- "DIA_ASCVD"

New_Comorbidity_Groups_Jun1 %>% select(patid,weight,diagnosis) %>% distinct() %>% inner_join(DIA_ASCVD) %>%
  group_by(diagnosis) %>% summarise(n=sum(weight))

OBE2_Comorbidity_Inventories <- fread("DIA Analysis Results 1.1/OBE2 Comorbidity Inventories.txt")
OBE2_Comorbidity_Inventories <- OBE2_Comorbidity_Inventories %>% select(patid, diagnosis)


OBE_ASCVD <- OBE2_Comorbidity_Inventories %>% filter(grepl("G45", diagnosis)|
                                         grepl("I63", diagnosis)|
                                         grepl("H34", diagnosis)|
                                         grepl("I65", diagnosis)|
                                         grepl("I66", diagnosis)|
                                         grepl("I69", diagnosis)|
                                         grepl("I20", diagnosis)|
                                         grepl("I21", diagnosis)|
                                         grepl("I22", diagnosis)|
                                         grepl("I23", diagnosis)|
                                         grepl("I24", diagnosis)|
                                         grepl("I25", diagnosis)|
                                         grepl("I70", diagnosis)|
                                         grepl("I73", diagnosis)) %>% select(patid) %>% distinct()

OBE_ASCVD$OBE_ASCVD <- "OBE_ASCVD"


New_Comorbidity_Groups_Jun1 %>% select(patid,weight,diagnosis) %>% distinct() %>% inner_join(OBE_ASCVD) %>%
  group_by(diagnosis) %>% summarise(n=sum(weight))


DANU_Demographics <- fread("DANU Demographics 1.1/DANU Demographics.txt", integer64 = "character", stringsAsFactors = F)
DANU_Demographics <- DANU_Demographics %>% select(patid, weight, age, gender)

DIA_ASCVD %>% left_join(DANU_Demographics) %>% summarise(n=mean(age))
OBE_ASCVD %>% left_join(DANU_Demographics) %>% summarise(n=mean(age))


DANU_Demographics %>%  filter(grepl("Diabetes", diagnosis))  %>% summarise(n=mean(age)) #61
DANU_Demographics %>% filter(diagnosis=="Obesity")  %>% summarise(n=mean(age)) #51

DANU_Demographics %>% filter(grepl("Diabetes", diagnosis))  %>% summarise(n=sum(weight)) #61
DANU_Demographics %>% filter(diagnosis=="Obesity")  %>% summarise(n=sum(weight)) #51


New_Comorbidity_Groups_Jun1 %>% select(patid,weight,diagnosis) %>% distinct() %>% left_join(OBE_ASCVD) %>%
  left_join(DANU_Demographics) %>% group_by(OBE_ASCVD) %>% filter(age>=65) %>% summarise(n=sum(weight))


New_Comorbidity_Groups_Jun1 %>% select(patid,weight,diagnosis) %>% distinct() %>% left_join(DIA_ASCVD) %>%
  left_join(DANU_Demographics) %>% group_by(DIA_ASCVD) %>% filter(age>=65) %>% summarise(n=sum(weight))

DANU_Demographics %>% filter(grepl("Diabetes", diagnosis)) %>%
  left_join(DIA_ASCVD) %>%
  mutate(DIA_ASCVD=ifelse(is.na(DIA_ASCVD), "DIA NO ASCVD", "DIA + ASCVD")) %>%
  ggplot(aes(age, colour=DIA_ASCVD, fill=DIA_ASCVD)) +
  geom_density(alpha=0.7) +
  theme_minimal() +
  xlab("\n Age (years)") + ylab("Patient density \n")


DANU_Demographics %>% filter(grepl("Diabetes", diagnosis)) %>%
  left_join(DIA_ASCVD) %>%
  mutate(DIA_ASCVD=ifelse(is.na(DIA_ASCVD), "DIA NO ASCVD", "DIA + ASCVD")) %>%
  ggplot(aes(age, colour=DIA_ASCVD, fill=DIA_ASCVD)) +
  geom_density(alpha=0.7) +
  theme_minimal() +
  xlab("\n Age (years)") + ylab("Patient density \n") +
  ggsci::scale_color_jama() +
  ggsci::scale_fill_jama()


DANU_Demographics %>% filter(diagnosis=="Obesity") %>%
  left_join(OBE_ASCVD) %>%
  mutate(OBE_ASCVD=ifelse(is.na(OBE_ASCVD), "OBE NO ASCVD", "OBE + ASCVD")) %>%
  ggplot(aes(age, colour=OBE_ASCVD, fill=OBE_ASCVD)) +
  geom_density(alpha=0.7) +
  theme_minimal() +
  xlab("\n Age (years)") + ylab("Patient density \n") +
  ggsci::scale_color_jama() +
  ggsci::scale_fill_jama()





DIA_ASCVD %>% left_join(DANU_Demographics) %>% group_by(gender) %>% summarise(n=sum(weight))
OBE_ASCVD %>% left_join(DANU_Demographics) %>% group_by(gender) %>% summarise(n=sum(weight))


DANU_Measures <- fread("DANU Measures 1.1/DANU Measures.txt",  integer64 = "character", stringsAsFactors = F)
DANU_Measures <- New_Comorbidity_Groups_Jun1 %>% inner_join(DANU_Measures)
BMI <- DANU_Measures %>% filter(test=="BMI")
HbA1c <- DANU_Measures %>% filter(test=="HbA1c Level")

BMI <- BMI %>% select(patid, value) %>% distinct() %>% group_by(patid ) %>% filter(value==max(value)) %>% slice(1)
HbA1c <- HbA1c %>% select(patid, value) %>% distinct() %>% group_by(patid ) %>% filter(value==max(value)) %>% slice(1)

rm(DANU_Measures)

names(BMI)[2] <- "BMI"
names(HbA1c)[2] <- "HbA1c"


BMI <- BMI %>% mutate(BMI=ifelse(BMI<27, "<27",
                    ifelse(BMI<30, "<30",
                           ifelse(BMI<35, "<35", ">35"))))

HbA1c <- HbA1c %>% mutate(HbA1c=ifelse(HbA1c<6.5, "<6.5",
                      ifelse(HbA1c<7.5, "<7.5",
                             ifelse(HbA1c<8.5, "<8.5",
                                    ifelse(HbA1c<9.5,"<9.5", ">9.5"))))) 




New_Comorbidity_Groups_Jun1 %>% select(patid,weight,diagnosis) %>% distinct() %>%
  filter(diagnosis=="Diabetes") %>%
  inner_join(HbA1c) %>%
   inner_join(DIA_ASCVD) %>%
  group_by(diagnosis, HbA1c) %>% summarise(n=sum(weight))


New_Comorbidity_Groups_Jun1 %>% select(patid,weight,diagnosis) %>% distinct() %>%
  filter(diagnosis=="Obesity") %>%
  inner_join(BMI) %>%
    inner_join(OBE_ASCVD) %>%
  group_by(diagnosis, BMI) %>% summarise(n=sum(weight))


New_Comorbidity_Groups_Jun1 <- fread("DIA Analysis Results 1.1/New_Comorbidity_Groups_Jun1.txt")
New_Comorbidity_Groups_Jun1 <- New_Comorbidity_Groups_Jun1 %>% mutate(diagnosis=ifelse(grepl("Diabetes", diagnosis), "Diabetes", diagnosis))
New_Comorbidity_Groups_Jun1 %>% group_by(diagnosis) %>% summarise(n=sum(weight))
New_Comorbidity_Groups_Jun1 %>% select(patid,weight,diagnosis) %>% distinct() %>% group_by(diagnosis) %>% summarise(n=sum(weight))
New_Comorbidity_Groups_Jun1 <- New_Comorbidity_Groups_Jun1 %>% select(patid,weight,diagnosis, group) %>% distinct()

New_Comorbidity_Groups_Jun1 %>%
     inner_join(DIA_ASCVD) %>%
  filter(diagnosis=="Diabetes") %>% select(patid, weight, group) %>% distinct() %>%
  group_by(group) %>% summarise(n=sum(weight))


New_Comorbidity_Groups_Jun1 %>%
     inner_join(OBE_ASCVD) %>%
  filter(diagnosis=="Obesity") %>% select(patid, weight, group) %>% distinct() %>%
  group_by(group) %>% summarise(n=sum(weight))


# Onset dates for Hendrik -----------------------

DANU_Demographics_Full <- fread("DANU Demographics Full.txt")
NASH_Demographics_All <- fread("NASH Demographics All.txt")

NASH_Demographics_All <- NASH_Demographics_All %>% select(patid, weight, nash, nafld)
NASH_Demographics_All <- NASH_Demographics_All %>% filter(! (is.na(nash)&is.na(nafld)))

DANU_Demographics_Full <- DANU_Demographics_Full %>% select(patid, weight, diabetes_onset, obesity_onset, heart_failure_onset, ckd_onset)
  
  
  
temp <- NASH_Demographics_All %>% select(patid, weight) %>%
  bind_rows(DANU_Demographics_Full %>% select(patid, weight)) %>% distinct() %>%
  left_join(NASH_Demographics_All) %>%
  left_join(DANU_Demographics_Full) %>% rename("nash_onset"="nash") %>% rename("nafld_onset"="nafld")


temp <- temp %>% filter(! (is.na(nash_onset)&is.na(nafld_onset)&is.na(diabetes_onset )&is.na(obesity_onset)&is.na(heart_failure_onset  )&is.na(ckd_onset)))

temp %>% mutate(ckd_onset  =ifelse(is.na(ckd_onset  ),0,1)) %>% group_by(ckd_onset  ) %>% summarise(n=sum(weight))

fwrite(temp, "Disease_onsets_DANU_2022.txt")


# -----------
# ----------------------
# HbA1c benefit based on starting earlier vs later ---------------------

DANU_Ingredients <- fread("DIA Analysis Results 1.1/DANU Ingredients.txt", integer64 = "character", stringsAsFactors = F)
DANU_Ingredients <- DANU_Ingredients %>%  separate(drug_id, c('class', 'molecule'))
DANU_Ingredients <- DANU_Ingredients %>% select(molecule, drug_group)
names(DANU_Ingredients)[1] <- "Drugs"
DANU_Ingredients$Drugs <- as.numeric(DANU_Ingredients$Drugs)

string_Biguanide <- paste0("\\b(",paste0(DANU_Ingredients$molecule[DANU_Ingredients$drug_group == "Biguanide"], collapse = "|"),")\\b")
string_Antidiabetic <- paste0("\\b(",paste0(DANU_Ingredients$molecule[DANU_Ingredients$drug_group == "Antidiabetic"], collapse = "|"),")\\b")
string_DPP4 <- paste0("\\b(",paste0(DANU_Ingredients$molecule[DANU_Ingredients$drug_group == "DPP4"], collapse = "|"),")\\b")
string_SGLT2 <- paste0("\\b(",paste0(DANU_Ingredients$molecule[DANU_Ingredients$drug_group == "SGLT2"], collapse = "|"),")\\b")
string_Insulin <- paste0("\\b(",paste0(DANU_Ingredients$molecule[DANU_Ingredients$drug_group == "Insulin"], collapse = "|"),")\\b")
string_OralGLP1 <- paste0("\\b(",paste0(DANU_Ingredients$molecule[DANU_Ingredients$drug_group == "GLP1 Oral"], collapse = "|"),")\\b")
string_InjectableGLP1 <- paste0("\\b(",paste0(DANU_Ingredients$molecule[DANU_Ingredients$drug_group == "GLP1 Injectable"], collapse = "|"),")\\b")

DIA_Drug_Histories <- read.table("DIA Analysis Results 1.1/DIA Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
DIA_Drug_Histories <- gather(DIA_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
DIA_Drug_Histories$Month <- as.character(DIA_Drug_Histories$Month)
DIA_Drug_Histories$Month <- parse_number(DIA_Drug_Histories$Month)
DIA_Drug_Histories <- DIA_Drug_Histories %>% filter(Drugs != "-")
DIA_Drug_Histories <- separate_rows(DIA_Drug_Histories, Drugs, sep = ",", convert=T)
DIA_Drug_Histories <- DIA_Drug_Histories %>% left_join(DANU_Ingredients)
DIA_Drug_Histories <- DIA_Drug_Histories %>% select(-Drugs) %>% distinct() %>% filter(drug_group=="Biguanide")
DIA_Drug_Histories <- DIA_Drug_Histories %>% group_by(patient, drug_group) %>% filter(Month==min(Month)) %>% ungroup()
DIA_Drug_Histories <- DIA_Drug_Histories %>% select(-c(disease, weight))

Months_lookup <- fread("Months_lookup.txt")
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")
  
DANU_Measures <- fread("DANU Measures 1.1/DANU Measures.txt",  integer64 = "character", stringsAsFactors = F)
DANU_Measures <- DANU_Measures %>% select(patid, weight, value, test, claimed) %>% distinct()
DANU_Measures <- DANU_Measures %>% filter(test=="HbA1c Level")

DANU_Measures$Month_Yr <- format(as.Date(DANU_Measures$claimed), "%Y-%m")
DANU_Measures <- DANU_Measures %>% left_join(Months_lookup, by = c("Month_Yr" = "Month")) %>%  filter(!is.na(Exact_Month)) 
DANU_Measures <- DANU_Measures %>% select(patid, weight, value, Exact_Month)
DANU_Measures <- DANU_Measures %>% group_by(patid, weight, Exact_Month) %>% summarise(value=max(value))
names(DANU_Measures)[1] <- "patient"
DANU_Measures <- DANU_Measures %>% ungroup() %>% select(-weight)

Start_Biguanide <- DIA_Drug_Histories %>% ungroup() %>% left_join(DANU_Measures) %>% ungroup() %>% arrange(patient) %>% drop_na() %>% 
  filter(Exact_Month<=Month & Exact_Month>=Month-6) %>% 
  group_by(patient) %>% filter(Exact_Month==max(Exact_Month)) %>% select(-drug_group)

Start_Biguanide <- Start_Biguanide %>% select(patient, Exact_Month, value) %>% rename("Start_Month"="Exact_Month")  %>% rename("Start_value"="value") 

Start_Biguanide <- Start_Biguanide %>% left_join(DANU_Measures) %>% filter(Exact_Month>Start_Month)  %>% 
  rename("After_Month"="Exact_Month")  %>% rename("After_value"="value") 


Start_Biguanide %>% filter(After_Month-Start_Month>12 & After_Month-Start_Month<24) %>% ungroup() %>% summarise(n=mean(Start_value ))


Start_Biguanide %>% filter(After_Month-Start_Month>12 & After_Month-Start_Month<24) %>% ungroup() %>% 
  group_by(patient) %>% filter(After_value==max(After_value)) %>% slice(1) %>% ungroup() %>%
  filter(Start_value>4&Start_value<16) %>% filter(After_value>4&After_value<16) %>%
  ggplot(aes(Start_value , After_value)) +
  geom_jitter(size=1) +
  geom_smooth(colour="firebrick", fill="firebrick") +
  theme_minimal() +
  xlab("\n HbA1c % right before \n 1st Biguanide Initiation") + ylab("HbA1c % 12 to 24 months \n AFTER 1st Biguanide Initiation \n")




Start_Biguanide %>% mutate(Elapsed=After_Month-Start_Month) %>% ungroup() %>% 
  #filter(Elapsed<=12) %>%
   mutate(Start_value=ifelse(Start_value<6.5, "<6.5",
                      ifelse(Start_value <7.5, "<7.5",
                             ifelse(Start_value <8.5, "<8.5",
                                    ifelse(Start_value <9.5,"<9.5", ">9.5"))))) %>% 
  ggplot(aes(Elapsed , After_value, colour=Start_value, fill=Start_value )) +
  #geom_jitter(size=1) +
  geom_smooth() +
  theme_minimal() +
  xlab("\n No. Months Elapsed since 1st Biguanide Initiation") + ylab("HbA1c % \n") +
  scale_fill_brewer("Blues") +
  scale_colour_brewer("Blues") 



# ---------------------------------------
# BMI vs HbA1c all DIA treat-exp GLP1 usage  -------------

New_Comorbidity_Groups_Jun1 <- fread("DIA Analysis Results 1.1/New_Comorbidity_Groups_Jun1.txt")
New_Comorbidity_Groups_Jun1 <- New_Comorbidity_Groups_Jun1 %>% filter(diagnosis=="Diabetes + Obesity") %>% select(patid, weight) %>% distinct()
sum(New_Comorbidity_Groups_Jun1$weight) # 26257291


DANU_Measures <- fread("DANU Measures 1.1/DANU Measures.txt",  integer64 = "character", stringsAsFactors = F)
DANU_Measures <- New_Comorbidity_Groups_Jun1 %>% inner_join(DANU_Measures)
BMI <- DANU_Measures %>% filter(test=="BMI")
HbA1c <- DANU_Measures %>% filter(test=="HbA1c Level")

BMI <- BMI %>% select(patid, value) %>% distinct() %>% group_by(patid ) %>% filter(value==max(value)) %>% slice(1)
HbA1c <- HbA1c %>% select(patid, value) %>% distinct() %>% group_by(patid ) %>% filter(value==max(value)) %>% slice(1)

rm(DANU_Measures)

names(BMI)[2] <- "BMI"
names(HbA1c)[2] <- "HbA1c"


BMI %>% inner_join(HbA1c) %>%
  inner_join(New_Comorbidity_Groups_Jun1) %>%
    mutate(BMI=ifelse(BMI<27, "<27",
                    ifelse(BMI<30, "<30",
                           ifelse(BMI<35, "<35", ">35")))) %>%
  mutate(HbA1c=ifelse(HbA1c<6.5, "<6.5",
                      ifelse(HbA1c<7.5, "<7.5",
                             ifelse(HbA1c<8.5, "<8.5",
                                    ifelse(HbA1c<9.5,"<9.5", ">9.5"))))) %>%
  group_by(BMI, HbA1c) %>%
  summarise(n=sum(weight)*2.493987) %>%
  spread(key=HbA1c, value=n)




DANU_Ingredients <- fread("DIA Analysis Results 1.1/DANU Ingredients.txt", integer64 = "character", stringsAsFactors = F)
DANU_Ingredients <- DANU_Ingredients %>%  separate(drug_id, c('class', 'molecule'))
string_OralGLP1        <- paste0("\\b(",paste0(DANU_Ingredients$molecule[DANU_Ingredients$drug_group == "GLP1 Oral"], collapse = "|"),")\\b")
string_InjectableGLP1  <- paste0("\\b(",paste0(DANU_Ingredients$molecule[DANU_Ingredients$drug_group == "GLP1 Injectable"], collapse = "|"),")\\b")

DIA_Drug_Histories <- fread("DIA Analysis Results 1.1/DIA Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
DIA_Drug_Histories <- DIA_Drug_Histories %>% select(-c(disease)) 
DIA_Drug_Histories <- gather(DIA_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
DIA_Drug_Histories <- DIA_Drug_Histories %>% select(patient, Treat, Month, weight) %>% distinct() %>% filter(Treat!="-")
DIA_Drug_Histories$Month <- as.character(DIA_Drug_Histories$Month)
DIA_Drug_Histories$Month <- parse_number(DIA_Drug_Histories$Month)


Ever_GLP1 <- DIA_Drug_Histories %>% filter(grepl(string_OralGLP1, Treat)|grepl(string_InjectableGLP1, Treat)) %>% 
  select(patient, weight) %>% distinct() %>% mutate(group="Ever_GLP1")

Ever_GLP1_Oral <- DIA_Drug_Histories %>% filter(grepl(string_OralGLP1, Treat)) %>% 
  select(patient, weight) %>% distinct() %>% mutate(group="Ever_GLP1_Oral")

L12m_GLP1 <- DIA_Drug_Histories %>% filter(Month>=49) %>% filter(grepl(string_OralGLP1, Treat)|grepl(string_InjectableGLP1, Treat)) %>% 
  select(patient, weight) %>% distinct() %>% mutate(group="L12m_GLP1")

L12m_GLP1_Oral <- DIA_Drug_Histories %>% filter(Month>=49) %>% filter(grepl(string_OralGLP1, Treat)) %>% 
  select(patient, weight) %>% distinct() %>% mutate(group="L12m_GLP1_Oral")



BMI %>% inner_join(HbA1c) %>%
  inner_join(New_Comorbidity_Groups_Jun1) %>%
  inner_join(L12m_GLP1_Oral, by=c("patid"="patient", "weight"="weight")) %>%
    mutate(BMI=ifelse(BMI<27, "<27",
                    ifelse(BMI<30, "<30",
                           ifelse(BMI<35, "<35", ">35")))) %>%
  mutate(HbA1c=ifelse(HbA1c<6.5, "<6.5",
                      ifelse(HbA1c<7.5, "<7.5",
                             ifelse(HbA1c<8.5, "<8.5",
                                    ifelse(HbA1c<9.5,"<9.5", ">9.5"))))) %>%
  group_by(BMI, HbA1c) %>%
  summarise(n=sum(weight)*2.493987) %>%
  spread(key=HbA1c, value=n)

# -----------------
# Brands GLP1 usage in Obesity ----------------------------

New_Comorbidity_Groups_Jun1 <- fread("DIA Analysis Results 1.1/New_Comorbidity_Groups_Jun1.txt")
New_Comorbidity_Groups_Jun1 <- New_Comorbidity_Groups_Jun1 %>% filter(diagnosis=="Obesity") %>% select(patid, weight) %>% distinct()


DANU_Ingredients <- fread("OBE2 Analysis Results 1.1/DANU Ingredients.txt", integer64 = "character", stringsAsFactors = F)
DANU_Ingredients <- DANU_Ingredients %>%  separate(drug_id, c('class', 'molecule'))
DANU_Ingredients <- DANU_Ingredients %>% select(molecule, drug_group)
names(DANU_Ingredients)[1] <- "Drugs"
DANU_Ingredients$Drugs <- as.numeric(DANU_Ingredients$Drugs)

string_Surgery <- paste0("\\b(",paste0(DANU_Ingredients$Drugs[DANU_Ingredients$drug_group == "Surgery"], collapse = "|"),")\\b")


OBE2_Drug_Histories <- fread("OBE2 Analysis Results 1.1/OBE2 Drug Histories.txt")
OBE2_Drug_Histories <- gather(OBE2_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
OBE2_Drug_Histories <- OBE2_Drug_Histories %>% filter(Drugs != "-") %>% select(patient, weight, Drugs) %>% distinct()
OBE2_Drug_Histories <- OBE2_Drug_Histories %>% filter(grepl(string_Surgery, Drugs)) %>% select(patient, weight) %>% distinct()

DANU_Measures <- fread("DANU Measures 1.1/DANU Measures.txt",  integer64 = "character", stringsAsFactors = F)
DANU_Measures <- DANU_Measures %>% inner_join(OBE2_Drug_Histories, by=c("patid"="patient"))
DANU_Measures <- DANU_Measures %>% select(patid, weight.x, value, test) %>% distinct()  %>% filter(test=="BMI")
DANU_Measures <- DANU_Measures %>% group_by(patid) %>% filter(value==max(value)) %>% slice(1) %>% select(-test)

DANU_Measures %>% ungroup() %>% summarise(n=weighted.mean(value, weight.x)) # 41.8

DANU_Measures %>%
  filter(value>27&value<70) %>%
  ggplot(aes(value)) +
  geom_histogram(colour="deepskyblue4", fill="deepskyblue4", alpha=0.7) +
  theme_minimal() +
  xlab("\n BMI") +  ylab("Patient count \n")


OBE2_Doses <- fread("OBE2 Analysis Results 1.1/OBE2 Doses.txt")
OBE2_Doses <- OBE2_Doses %>% filter(grepl("GLP1",drug_group)&paid=="P") %>% select(pat_id, weight, drug_id, from_dt, generic_name)

DANU_Treatments <- fread("OBE2 Analysis Results 1.1/DANU Treatments.txt")
DANU_Treatments <- DANU_Treatments %>% select(drug_id, generic_name, brand_name)

OBE2_Doses <- OBE2_Doses %>% mutate(drug_id = str_sub(drug_id, 1L, 8L))
OBE2_Doses <- OBE2_Doses %>% left_join(DANU_Treatments) %>% filter(!is.na(brand_name))
OBE2_Doses$from_dt <- as.character(OBE2_Doses$from_dt)

Months_lookup <- fread("Months_lookup.txt")
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")
  
OBE2_Doses$from_dt <- format(as.Date(OBE2_Doses$from_dt), "%Y-%m")
OBE2_Doses <- OBE2_Doses %>% left_join(Months_lookup, by = c("from_dt" = "Month")) %>%  filter(!is.na(Exact_Month)) %>% select(-c(from_dt, drug_id, generic_name))

New_Comorbidity_Groups_Jun1 <- fread("DIA Analysis Results 1.1/New_Comorbidity_Groups_Jun1.txt")

data.frame(
  OBE2_Doses %>% inner_join(New_Comorbidity_Groups_Jun1 %>% select(patid) %>% distinct(), by=c("pat_id"="patid")) %>%
  distinct() %>% ungroup() %>% group_by(Exact_Month, brand_name) %>% summarise(n=sum(weight))
  ) %>%
  spread(key=brand_name, value=n)


# ----------------------------
# Total Durations on each class patients starting month 37 to 49 ------------------------------------------------------------
DANU_Ingredients <- fread("DIA Analysis Results 1.1/DANU Ingredients.txt", integer64 = "character", stringsAsFactors = F)
DANU_Ingredients <- DANU_Ingredients %>%  separate(drug_id, c('class', 'molecule'))
DANU_Ingredients <- DANU_Ingredients %>% select(molecule, drug_group)
names(DANU_Ingredients)[1] <- "Drugs"
DANU_Ingredients$Drugs <- as.numeric(DANU_Ingredients$Drugs)


DIA_Drug_Histories <- read.table("DIA Analysis Results 1.1/DIA Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
DIA_Drug_Histories <- gather(DIA_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
DIA_Drug_Histories$Month <- as.character(DIA_Drug_Histories$Month)
DIA_Drug_Histories$Month <- parse_number(DIA_Drug_Histories$Month)
#DIA_Drug_Histories <- DIA_Drug_Histories %>% filter(Month>=37)
DIA_Drug_Histories <- DIA_Drug_Histories %>% filter(Drugs != "-") %>% select(-c(disease))
DIA_Drug_Histories <- separate_rows(DIA_Drug_Histories, Drugs, sep = ",", convert=T)
DIA_Drug_Histories <- DIA_Drug_Histories %>% left_join(DANU_Ingredients)
DIA_Drug_Histories <- DIA_Drug_Histories %>% select(-Drugs) %>% distinct()

DIA_Drug_Histories <- DIA_Drug_Histories %>% group_by(patient, weight, drug_group) %>% filter(Month==min(Month)) %>% 
  filter(Month>=37&Month<=49) %>% select(patient, weight, drug_group) %>% distinct() %>% ungroup() %>% 
  left_join(DIA_Drug_Histories)

DIA_Drug_Histories <- DIA_Drug_Histories %>% group_by(patient, weight, drug_group) %>% count() 

DIA_Drug_Histories %>% ungroup() %>% group_by(drug_group) %>% summarise(mean=weighted.mean(n, as.numeric(weight)))

# 1 Antidiabetic    11.0 
# 2 Biguanide       10.6 
# 3 DPP4            10.0 
# 4 GLP1 Injectable 10.7 
# 5 GLP1 Oral        8.01
# 6 Insulin          7.35
# 7 SGLT2           11.0 


DIA_Drug_Histories <- DIA_Drug_Histories %>% ungroup() %>% group_by(drug_group, n) %>% summarise(Total=sum(as.numeric(weight))) %>% arrange(drug_group, n)



temp <- data.frame(DIA_Drug_Histories %>% group_by(drug_group) %>% mutate(Total_cum=cumsum(Total)) %>% mutate(grand_Total=sum(Total)) %>%
             mutate(Remain=grand_Total-lag(Total_cum)) %>% mutate(Remain=ifelse(is.na(Remain), grand_Total, Remain)) %>%
             mutate(Remain=Remain/grand_Total) %>%
             select(drug_group, n, Remain)) 


temp %>% filter(n==12)

temp %>%
  mutate(drug_group=factor(drug_group, levels=c("Biguanide", "Antidiabetic", "DPP4", "SGLT2", "Insulin", "GLP1 Oral", "GLP1 Injectable"))) %>%
  ggplot(aes(n, Remain, colour=drug_group)) +
  geom_smooth(se=F, size=2, alpha=.7) +
  ylim(0,1) +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent, breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)) +
  #scale_x_continuous(breaks = c(-60, -50, -40, -30, -20, -10, 0)) +
  scale_colour_manual(values=c("#c49a7c","#7d95be","#0d2b4e","#ff9933","#7cc49a", "#e87496","#a52a2a")) +
  xlab("\n Number of Months Elapsed Since Therapy Initiation ") + ylab("Proportion of Patients Still ON Drugs \n")



# ------------------------------
# No. months ON Rybelsus from possible treatment-months last year -------------------------

DANU_Ingredients <- fread("DIA Analysis Results 1.1/DANU Ingredients.txt", integer64 = "character", stringsAsFactors = F)
DANU_Ingredients <- DANU_Ingredients %>%  separate(drug_id, c('class', 'molecule'))
DANU_Ingredients <- DANU_Ingredients %>% select(molecule, drug_group)
names(DANU_Ingredients)[1] <- "Drugs"
DANU_Ingredients$Drugs <- as.numeric(DANU_Ingredients$Drugs)


DIA_Drug_Histories <- read.table("DIA Analysis Results 1.1/DIA Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
DIA_Drug_Histories <- gather(DIA_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
DIA_Drug_Histories$Month <- as.character(DIA_Drug_Histories$Month)
DIA_Drug_Histories$Month <- parse_number(DIA_Drug_Histories$Month)
DIA_Drug_Histories <- DIA_Drug_Histories %>% filter(Drugs != "-") %>% select(-c(disease))
DIA_Drug_Histories <- separate_rows(DIA_Drug_Histories, Drugs, sep = ",", convert=T)
DIA_Drug_Histories <- DIA_Drug_Histories %>% left_join(DANU_Ingredients)
DIA_Drug_Histories <- DIA_Drug_Histories %>% select(-Drugs) %>% distinct()
DIA_Drug_Histories <- DIA_Drug_Histories %>% filter(drug_group=="GLP1 Oral") %>% select(-drug_group)

DIA_Drug_Histories <- DIA_Drug_Histories %>% group_by(patient, weight) %>% filter(Month==min(Month)) %>% filter(Month>=49) %>%
  rename("First"="Month") %>%
  inner_join(DIA_Drug_Histories)

DIA_Drug_Histories$Total_Duration <- 60 - DIA_Drug_Histories$First + 1

DIA_Drug_Histories %>% ungroup() %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(as.numeric(weight))) # 306,878 (x12 3,682,536)

Total_Duration <- DIA_Drug_Histories %>% select(patient, weight, Total_Duration) %>% distinct()
GLP1_Duration <- DIA_Drug_Histories %>% group_by(patient, weight) %>% count()

Total_Duration %>% ungroup() %>% summarise(n=sum(as.numeric(weight)*Total_Duration))  # 1,983,343
GLP1_Duration %>% ungroup() %>% summarise(n=sum(as.numeric(weight)*n))  # 1,186,574  0.5982697  | 












DANU_Ingredients <- fread("DIA Analysis Results 1.1/DANU Ingredients.txt", integer64 = "character", stringsAsFactors = F)
DANU_Ingredients <- DANU_Ingredients %>%  separate(drug_id, c('class', 'molecule'))
DANU_Ingredients <- DANU_Ingredients %>% select(molecule, drug_group)
names(DANU_Ingredients)[1] <- "Drugs"
DANU_Ingredients$Drugs <- as.numeric(DANU_Ingredients$Drugs)


OBE2_Drug_Histories <- read.table("OBE2 Analysis Results 1.1/OBE2 Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
OBE2_Drug_Histories <- gather(OBE2_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
OBE2_Drug_Histories$Month <- as.character(OBE2_Drug_Histories$Month)
OBE2_Drug_Histories$Month <- parse_number(OBE2_Drug_Histories$Month)
OBE2_Drug_Histories <- OBE2_Drug_Histories %>% filter(Drugs != "-") %>% select(-c(disease))
OBE2_Drug_Histories <- separate_rows(OBE2_Drug_Histories, Drugs, sep = ",", convert=T)
OBE2_Drug_Histories <- OBE2_Drug_Histories %>% left_join(DANU_Ingredients)
OBE2_Drug_Histories <- OBE2_Drug_Histories %>% select(-Drugs) %>% distinct()
OBE2_Drug_Histories <- OBE2_Drug_Histories %>% filter(drug_group=="GLP1 Oral") %>% select(-drug_group)

OBE2_Drug_Histories <- OBE2_Drug_Histories %>% group_by(patient, weight) %>% filter(Month==min(Month)) %>% filter(Month>=49) %>%
  rename("First"="Month") %>%
  inner_join(OBE2_Drug_Histories)

OBE2_Drug_Histories$Total_Duration <- 60 - OBE2_Drug_Histories$First + 1

OBE2_Drug_Histories %>% ungroup() %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(as.numeric(weight))) # 23,425 (x12 281,100)

Total_Duration <- OBE2_Drug_Histories %>% select(patient, weight, Total_Duration) %>% distinct()
GLP1_Duration <- OBE2_Drug_Histories %>% group_by(patient, weight) %>% count()

Total_Duration %>% ungroup() %>% summarise(n=sum(as.numeric(weight)*Total_Duration))  # 140,795
GLP1_Duration %>% ungroup() %>% summarise(n=sum(as.numeric(weight)*n))  # 63,179  0.5982697  | 


# ---------------------
# Patients exclusively on a single class --------------------------------

DANU_Ingredients <- fread("DIA Analysis Results 1.1/DANU Ingredients.txt", integer64 = "character", stringsAsFactors = F)
DANU_Ingredients <- DANU_Ingredients %>%  separate(drug_id, c('class', 'molecule'))

string_Biguanide       <- paste0("\\b(",paste0(DANU_Ingredients$molecule[DANU_Ingredients$drug_group == "Biguanide"], collapse = "|"),")\\b")
string_Antidiabetic    <- paste0("\\b(",paste0(DANU_Ingredients$molecule[DANU_Ingredients$drug_group == "Antidiabetic"], collapse = "|"),")\\b")
string_DPP4            <- paste0("\\b(",paste0(DANU_Ingredients$molecule[DANU_Ingredients$drug_group == "DPP4"], collapse = "|"),")\\b")
string_SGLT2           <- paste0("\\b(",paste0(DANU_Ingredients$molecule[DANU_Ingredients$drug_group == "SGLT2"], collapse = "|"),")\\b")
string_Insulin         <- paste0("\\b(",paste0(DANU_Ingredients$molecule[DANU_Ingredients$drug_group == "Insulin"], collapse = "|"),")\\b")
string_OralGLP1        <- paste0("\\b(",paste0(DANU_Ingredients$molecule[DANU_Ingredients$drug_group == "GLP1 Oral"], collapse = "|"),")\\b")
string_InjectableGLP1  <- paste0("\\b(",paste0(DANU_Ingredients$molecule[DANU_Ingredients$drug_group == "GLP1 Injectable"], collapse = "|"),")\\b")


DIA_Flows_Aux_Long <- fread("DIA Analysis Results 1.1/DIA_Flows_Aux_Long.txt", integer64 = "character", stringsAsFactors = F)
Treatment_exp_Vector <- fread("DIA Analysis Results 1.1/Treatment_exp_Vector.txt")
DIA_Flows_Aux_Long <- Treatment_exp_Vector %>% inner_join(DIA_Flows_Aux_Long)
DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% select(patient, weight, p1, p2, d1, d2) 

DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% mutate(BiguanideExp = ifelse(grepl(string_Biguanide,d1)|grepl(string_Biguanide,d2),1,0))
DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% group_by(patient) %>% mutate(BiguanideExp = cumsum(BiguanideExp))
DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% group_by(patient) %>% mutate(BiguanideExp = ifelse(BiguanideExp==0,0,1))

DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% mutate(AntidiabeticExp = ifelse(grepl(string_Antidiabetic,d1)|grepl(string_Antidiabetic,d2),1,0))
DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% group_by(patient) %>% mutate(AntidiabeticExp = cumsum(AntidiabeticExp))
DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% group_by(patient) %>% mutate(AntidiabeticExp = ifelse(AntidiabeticExp==0,0,1))
 
DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% mutate(DPP4Exp = ifelse(grepl(string_DPP4,d1)|grepl(string_DPP4,d2),1,0))
DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% group_by(patient) %>% mutate(DPP4Exp = cumsum(DPP4Exp))
DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% group_by(patient) %>% mutate(DPP4Exp = ifelse(DPP4Exp==0,0,1))

DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% mutate(SGLT2Exp = ifelse(grepl(string_SGLT2,d1)|grepl(string_SGLT2,d2),1,0))
DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% group_by(patient) %>% mutate(SGLT2Exp = cumsum(SGLT2Exp))
DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% group_by(patient) %>% mutate(SGLT2Exp = ifelse(SGLT2Exp==0,0,1))
 
DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% mutate(OralExp = ifelse(grepl(string_OralGLP1,d1)|grepl(string_OralGLP1,d2),1,0))
DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% group_by(patient) %>% mutate(OralExp = ifelse(OralExp==0,0,1))
 
DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% mutate(InjExp = ifelse(grepl(string_InjectableGLP1,d1)|grepl(string_InjectableGLP1,d2),1,0))
DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% group_by(patient) %>% mutate(InjExp = cumsum(InjExp))
DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% group_by(patient) %>% mutate(InjExp = ifelse(InjExp==0,0,1))

DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% mutate(InsulinExp = ifelse(grepl(string_Insulin,d1)|grepl(string_Insulin,d2),1,0))
DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% group_by(patient) %>% mutate(InsulinExp = cumsum(InsulinExp))
DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% group_by(patient) %>% mutate(InsulinExp = ifelse(InsulinExp==0,0,1))


# -----------------
# Physycians prescribing share -------------------------------------------

DIA_US_Doses <- fread("DIA Doses 1.1/DIA Doses.txt", colClasses = "character")
DIA_US_Doses <- DIA_US_Doses %>% filter(status != "G" & paid=="P")
DIA_US_Doses <- DIA_US_Doses %>% select(drug_group, pat_id, weight, from_dt, specialty, prov)

DIA_Specialty_codes <- fread("DIA Analysis Results 1.1/DANU Specialty Codes.txt", colClasses = "character")
DIA_Specialty_codes <- DIA_Specialty_codes %>% select(code, specialty)
names(DIA_Specialty_codes)[2] <- "TYPE" ;  names(DIA_Specialty_codes)[1] <- "specialty"
unique(DIA_Specialty_codes$TYPE)
DIA_Specialty_codes <- DIA_Specialty_codes %>% filter(TYPE!="Facility"&TYPE!="Other Provider"&TYPE!="Unknown")

DIA_US_Doses <- DIA_US_Doses %>% inner_join(DIA_Specialty_codes) %>% select(-specialty)
DIA_US_Doses$from_dt <- as.Date(DIA_US_Doses$from_dt)

DIA_US_Doses <- DIA_US_Doses %>% mutate(Year=ifelse(from_dt>= "2021-07-01", 5,
                                    ifelse(from_dt>= "2020-07-01", 4,
                                           ifelse(from_dt>= "2019-07-01", 3,
                                                  ifelse(from_dt>= "2018-07-01", 2 , 1)))))


# Volume 
temp <- DIA_US_Doses %>% group_by(Year, prov) %>% summarise(total_scripts=sum(as.numeric(weight))) %>%
  left_join(
    DIA_US_Doses %>% filter(grepl("GLP1", drug_group)) %>% group_by(Year, prov) %>% summarise(glp_scripts=sum(as.numeric(weight)))
  ) %>%
  left_join(
    DIA_US_Doses %>% filter(grepl("Oral", drug_group)) %>% group_by(Year, prov) %>% summarise(glp_oral_scripts=sum(as.numeric(weight)))
  ) %>%
    left_join(
    DIA_US_Doses %>% filter(grepl("Injectable", drug_group)) %>% group_by(Year, prov) %>% summarise(glp_injectable_scripts=sum(as.numeric(weight)))
  )


temp[is.na(temp)] <- 0


temp %>% group_by(Year) %>% mutate(total_scripts=sum(total_scripts)) %>% 
  mutate(glp_scripts =sum(glp_scripts )) %>%
  mutate(glp_oral_scripts  =sum(glp_oral_scripts  )) %>%
  mutate(glp_injectable_scripts =sum(glp_injectable_scripts )) %>%
  select(-prov) %>%
  distinct()

#    Year total_scripts glp_scripts glp_oral_scripts glp_injectable_scripts
# 1     1    232195224.   11979447.               0               11979447.
# 2     2    136823356.    9724770.               0                9724770.
# 3     3    139343483.   12181496.          127416.              12054080.
# 4     4    143945545.   15052949.          702669.              14350280.
# 5     5    138478273.   18163555.         1214169.              16949386.

 
# temp$glp_scripts <- temp$glp_scripts / temp$total_scripts
# temp$glp_oral_scripts  <- temp$glp_oral_scripts  / temp$total_scripts
# temp$glp_injectable_scripts  <- temp$glp_injectable_scripts  / temp$total_scripts



# Unique physicians 
temp <- DIA_US_Doses %>% select(Year, prov) %>% distinct() %>%
  left_join(
    DIA_US_Doses %>% filter(grepl("GLP1", drug_group)) %>% select(Year, prov) %>% distinct() %>% mutate(glp_scripts=1)
  ) %>%
  left_join(
    DIA_US_Doses %>% filter(grepl("Oral", drug_group)) %>% select(Year, prov) %>% distinct() %>% mutate(glp_oral_scripts=1)
  ) %>%
    left_join(
    DIA_US_Doses %>% filter(grepl("Injectable", drug_group)) %>% select(Year, prov) %>% distinct() %>% mutate(glp_injectable_scripts=1)
  )


temp[is.na(temp)] <- 0


temp %>% group_by(Year) %>% count() %>%
 left_join(temp %>% filter(glp_scripts ==1) %>% group_by(Year) %>% count(), by=c("Year"="Year")) %>%
 left_join(temp %>% filter(glp_oral_scripts ==1) %>% group_by(Year) %>% count(), by=c("Year"="Year")) %>%
 left_join(temp %>% filter(glp_injectable_scripts ==1) %>% group_by(Year) %>% count(), by=c("Year"="Year"))
 

length(unique(temp$prov))

temp %>% filter(Year==4) %>% select(prov) %>% mutate(Year4=1)  %>%
  full_join(temp %>% filter(Year==5) %>% select(prov) %>% mutate(Year5=1) ) %>%
  group_by(Year4, Year5) %>% count()




# Volume  2
temp <- DIA_US_Doses %>% group_by(Year, prov) %>% summarise(total_scripts=sum(as.numeric(weight))) %>%
  left_join(
    DIA_US_Doses %>% filter(grepl("GLP1", drug_group)) %>% group_by(Year, prov) %>% summarise(glp_scripts=sum(as.numeric(weight)))
  ) %>%
  left_join(
    DIA_US_Doses %>% filter(grepl("Oral", drug_group)) %>% group_by(Year, prov) %>% summarise(glp_oral_scripts=sum(as.numeric(weight)))
  ) %>%
    left_join(
    DIA_US_Doses %>% filter(grepl("Injectable", drug_group)) %>% group_by(Year, prov) %>% summarise(glp_injectable_scripts=sum(as.numeric(weight)))
  )


temp[is.na(temp)] <- 0
temp <- temp %>% filter(Year==4|Year==5)

oral <- temp %>% select(Year, prov, glp_oral_scripts)  %>% spread(key=Year, value=glp_oral_scripts)
oral[is.na(oral)] <- 0

sum(oral$`4`) # 702669.2
sum(oral$`5`) # 1214169
oral$diff <- oral$`5` - oral$`4`
sum(oral$diff)
length(unique(oral$prov))
oral %>% filter(`5`!=0&`4`==0) %>% summarise(n=sum(diff))
oral %>% filter(`5`==0&`4`!=0) %>% summarise(n=sum(diff))
oral %>% filter(`5`>`4`&`4`!=0) %>% summarise(n=sum(diff))
oral %>% filter(`5`<`4`&`4`!=0) %>% summarise(n=sum(diff))

oral %>% mutate(type=ifelse(`5`!=0&`4`==0, "`5`!=0&`4`==0",
                            ifelse(`5`==0&`4`!=0, "`5`==0&`4`!=0",
                                   ifelse(`5`>`4`&`4`!=0, "`5`>`4`&`4`!=0", 
                                          ifelse(`5`<`4`&`4`!=0, "`5`<`4`&`4`!=0", NA))))) %>%
  filter(diff!=0) %>% group_by(type) %>% summarise(n=sum(diff))


injectable <- temp %>% select(Year, prov, glp_injectable_scripts)  %>% spread(key=Year, value=glp_injectable_scripts)
injectable[is.na(injectable)] <- 0




sum(injectable$`4`) # 14350280
sum(injectable$`5`) # 16949386
injectable$diff <- injectable$`5` - injectable$`4` # 2599106
sum(injectable$diff)
length(unique(injectable$prov))
injectable %>% filter(`5`!=0&`4`==0) %>% summarise(n=sum(diff))
injectable %>% filter(`5`==0&`4`!=0) %>% summarise(n=sum(diff))
injectable %>% filter(`5`>`4`&`4`!=0) %>% summarise(n=sum(diff))
injectable %>% filter(`5`<`4`&`4`!=0) %>% summarise(n=sum(diff))

injectable %>% mutate(type=ifelse(`5`!=0&`4`==0, "`5`!=0&`4`==0",
                            ifelse(`5`==0&`4`!=0, "`5`==0&`4`!=0",
                                   ifelse(`5`>`4`&`4`!=0, "`5`>`4`&`4`!=0", 
                                          ifelse(`5`<`4`&`4`!=0, "`5`<`4`&`4`!=0", NA))))) %>%
  filter(diff!=0) %>% group_by(type) %>% summarise(n=sum(diff))


# Brand share Inj
DIA_US_Doses <- fread("DIA Doses 1.1/DIA Doses.txt", colClasses = "character")
DIA_US_Doses <- DIA_US_Doses %>% filter(status != "G" & paid=="P")
DIA_US_Doses <- DIA_US_Doses %>% filter(drug_group=="GLP1 Injectable") %>%
  select(generic_name, pat_id, weight, from_dt, specialty, prov)

DIA_Specialty_codes <- fread("DIA Analysis Results 1.1/DANU Specialty Codes.txt", colClasses = "character")
DIA_Specialty_codes <- DIA_Specialty_codes %>% select(code, specialty)
names(DIA_Specialty_codes)[2] <- "TYPE" ;  names(DIA_Specialty_codes)[1] <- "specialty"
unique(DIA_Specialty_codes$TYPE)
DIA_Specialty_codes <- DIA_Specialty_codes %>% filter(TYPE!="Facility"&TYPE!="Other Provider"&TYPE!="Unknown")

DIA_US_Doses <- DIA_US_Doses %>% inner_join(DIA_Specialty_codes) %>% select(-specialty)
DIA_US_Doses$from_dt <- as.Date(DIA_US_Doses$from_dt)
DIA_US_Doses <- DIA_US_Doses %>% filter(from_dt>= "2021-07-01") %>% select(-from_dt)
DIA_US_Doses <- DIA_US_Doses %>% group_by(prov, generic_name) %>% summarise(n=sum(as.numeric(weight)))
DIA_US_Doses <- DIA_US_Doses %>% spread(key=generic_name, value=n)
DIA_US_Doses[is.na(DIA_US_Doses)] <- 0

DIA_US_Doses$total <- DIA_US_Doses$Dulaglutide + DIA_US_Doses$Exenatide + DIA_US_Doses$Liraglutide + 
  DIA_US_Doses$Lixisenatide + DIA_US_Doses$`Semaglutide Injectable` + DIA_US_Doses$Tirzepatide

names(DIA_US_Doses)

DIA_US_Doses %>% mutate(group=ifelse(`Semaglutide Injectable`==total, "100_sema",
                                     ifelse(Dulaglutide==total, "100_dula", 
                                            ifelse(`Semaglutide Injectable`>0.5*total, "50_sema",
                                                   ifelse(Dulaglutide>0.5*total, "50_dula",
                                                          ifelse(Exenatide==total|Liraglutide==total|Lixisenatide==total|Tirzepatide==total, "100_other", "portfolio")))))) %>%
  group_by(group) %>% count()





# Brand share Oral
DIA_US_Doses <- fread("DIA Doses 1.1/DIA Doses.txt", colClasses = "character")
DIA_US_Doses <- DIA_US_Doses %>% filter(status != "G" & paid=="P")
DIA_US_Doses <- DIA_US_Doses %>% filter(drug_group=="GLP1 Oral") %>% select(prov) %>% distinct() %>%
  left_join(DIA_US_Doses) %>% filter(grepl("GLP", drug_group)) %>% select(generic_name, pat_id, weight, from_dt, specialty, prov)

DIA_Specialty_codes <- fread("DIA Analysis Results 1.1/DANU Specialty Codes.txt", colClasses = "character")
DIA_Specialty_codes <- DIA_Specialty_codes %>% select(code, specialty)
names(DIA_Specialty_codes)[2] <- "TYPE" ;  names(DIA_Specialty_codes)[1] <- "specialty"
unique(DIA_Specialty_codes$TYPE)
DIA_Specialty_codes <- DIA_Specialty_codes %>% filter(TYPE!="Facility"&TYPE!="Other Provider"&TYPE!="Unknown")

DIA_US_Doses <- DIA_US_Doses %>% inner_join(DIA_Specialty_codes) %>% select(-specialty)
DIA_US_Doses$from_dt <- as.Date(DIA_US_Doses$from_dt)
DIA_US_Doses <- DIA_US_Doses %>% filter(from_dt>= "2021-07-01") %>% select(-from_dt)
DIA_US_Doses <- DIA_US_Doses %>% group_by(prov, generic_name) %>% summarise(n=sum(as.numeric(weight)))
DIA_US_Doses <- DIA_US_Doses %>% spread(key=generic_name, value=n)
DIA_US_Doses[is.na(DIA_US_Doses)] <- 0

names(DIA_US_Doses)

DIA_US_Doses$total <- DIA_US_Doses$Dulaglutide + DIA_US_Doses$Exenatide + DIA_US_Doses$Liraglutide + 
  DIA_US_Doses$Lixisenatide + DIA_US_Doses$`Semaglutide Injectable` + DIA_US_Doses$Tirzepatide + DIA_US_Doses$`Semaglutide Oral`


DIA_US_Doses %>% mutate(group=ifelse(`Semaglutide Oral`==total, "100_sema_oral",
                                     ifelse(`Semaglutide Oral`+`Semaglutide Injectable`==total, "100_sema", 
                                            ifelse(`Semaglutide Oral`>0.5*total, "50_sema_oral",
                                                   ifelse(`Semaglutide Oral`+`Semaglutide Injectable`>0.5*total, "50_sema", "other"))))) %>%
  group_by(group) %>% count()









# ------------------------------------------------------------------
# Proportion of PCOS with N92 and OBE prediabetic -------------------
Mkt_Clarity_PAD_PCOS_SLAP_CKD_HFpEF_ce18_pts <- fread("DIA Analysis Results 1.1/Mkt_Clarity_PAD_PCOS_SLAP_CKD_HFpEF_ce18_pts.txt")
Mkt_Clarity_PAD_PCOS_SLAP_CKD_HFpEF_ce18_pts <- Mkt_Clarity_PAD_PCOS_SLAP_CKD_HFpEF_ce18_pts %>% filter(PCOS==1) %>% select(ptid) %>% rename("patient"="ptid")

DANU_Demographics <- fread("DANU Demographics 1.1/DANU Demographics.txt")
DANU_Demographics <- DANU_Demographics %>% filter(diagnosis=="Diabetes + Obesity") %>% select(patid, weight) %>% rename("patient"="patid")

DANU_Demographics <- DANU_Demographics %>% inner_join(Mkt_Clarity_PAD_PCOS_SLAP_CKD_HFpEF_ce18_pts)
sum(DANU_Demographics$weight)

OBE2_Comorbidity_Inventories <- fread("DIA Analysis Results 1.1/DIA Comorbidity Inventories.txt")
names(OBE2_Comorbidity_Inventories)[1] <- "patient"
OBE2_Comorbidity_Inventories <- OBE2_Comorbidity_Inventories %>% inner_join(DANU_Demographics %>% select(patient))

OBE2_Comorbidity_Inventories %>% filter(grepl("N92", diagnosis)|diagnosis=="N92") %>%
  select(patient, weight) %>% distinct() %>%
  summarise(n=sum(weight))  # 0.6640017 OBE  |  0.53439 T2D+OBE





Mkt_Clarity_PAD_PCOS_SLAP_CKD_HFpEF_ce18_pts <- fread("DIA Analysis Results 1.1/Mkt_Clarity_PAD_PCOS_SLAP_CKD_HFpEF_ce18_pts.txt")
Mkt_Clarity_PAD_PCOS_SLAP_CKD_HFpEF_ce18_pts <- Mkt_Clarity_PAD_PCOS_SLAP_CKD_HFpEF_ce18_pts %>% filter(PCOS==1) %>% select(ptid) %>% rename("patient"="ptid")

DANU_Demographics <- fread("DANU Demographics 1.1/DANU Demographics.txt")
DANU_Demographics <- DANU_Demographics %>% filter(diagnosis=="Obesity") %>% select(patid, weight) %>% rename("patient"="patid")

DANU_Demographics <- DANU_Demographics %>% inner_join(Mkt_Clarity_PAD_PCOS_SLAP_CKD_HFpEF_ce18_pts)
sum(DANU_Demographics$weight)


OBE2_Comorbidity_Inventories <- fread("DIA Analysis Results 1.1/OBE2 Comorbidity Inventories.txt")
names(OBE2_Comorbidity_Inventories)[1] <- "patient"
OBE2_Comorbidity_Inventories <- OBE2_Comorbidity_Inventories %>% inner_join(DANU_Demographics %>% select(patient))

OBE2_Comorbidity_Inventories %>% filter(diagnosis=="R73") %>%
  select(patient, weight) %>% distinct() %>%
  summarise(n=sum(weight))  # 0.2152717 OBE 


  
DANU_Measures <- fread("DANU Measures 1.1/DANU Measures.txt",  integer64 = "character", stringsAsFactors = F)
DANU_Measures <- DANU_Measures %>% inner_join(DANU_Demographics %>% select(patient), by=c("patid"="patient"))
DANU_Measures <- DANU_Measures %>% filter(test=="HbA1c Level")
DANU_Measures <- DANU_Measures %>% select(patid, value, weight, claimed) %>% distinct() %>% group_by(patid ) %>%  filter(value==max(value)) %>% slice(1)
sum(DANU_Measures$weight)

DANU_Measures %>% filter(value>=5.7 ) %>% ungroup() %>% summarise(n=sum(weight)) # 0.100944


# --------------------
# No. months ON Rybelsus from possible treatment-months last year ~Age  -------------------------

DANU_Ingredients <- fread("DIA Analysis Results 1.1/DANU Ingredients.txt", integer64 = "character", stringsAsFactors = F)
DANU_Ingredients <- DANU_Ingredients %>%  separate(drug_id, c('class', 'molecule'))
DANU_Ingredients <- DANU_Ingredients %>% select(molecule, drug_group)
names(DANU_Ingredients)[1] <- "Drugs"
DANU_Ingredients$Drugs <- as.numeric(DANU_Ingredients$Drugs)


DIA_Drug_Histories <- read.table("DIA Analysis Results 1.1/DIA Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
DIA_Drug_Histories <- gather(DIA_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
DIA_Drug_Histories$Month <- as.character(DIA_Drug_Histories$Month)
DIA_Drug_Histories$Month <- parse_number(DIA_Drug_Histories$Month)
DIA_Drug_Histories <- DIA_Drug_Histories %>% filter(Drugs != "-") %>% select(-c(disease))
DIA_Drug_Histories <- separate_rows(DIA_Drug_Histories, Drugs, sep = ",", convert=T)
DIA_Drug_Histories <- DIA_Drug_Histories %>% left_join(DANU_Ingredients)
DIA_Drug_Histories <- DIA_Drug_Histories %>% select(-Drugs) %>% distinct()
DIA_Drug_Histories <- DIA_Drug_Histories %>% filter(drug_group=="GLP1 Oral") %>% select(-drug_group)

DIA_Drug_Histories <- DIA_Drug_Histories %>% group_by(patient, weight) %>% filter(Month==min(Month)) %>% filter(Month>=49) %>%
  rename("First"="Month") %>%
  inner_join(DIA_Drug_Histories)

DIA_Drug_Histories$Total_Duration <- 60 - DIA_Drug_Histories$First + 1

DIA_Drug_Histories %>% ungroup() %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(as.numeric(weight))) # 306,878 (x12 3,682,536)

Total_Duration <- DIA_Drug_Histories %>% select(patient, weight, Total_Duration) %>% distinct()
GLP1_Duration <- DIA_Drug_Histories %>% group_by(patient, weight) %>% count()

Total_Duration %>% ungroup() %>% summarise(n=sum(as.numeric(weight)*Total_Duration))  # 1,983,343
GLP1_Duration %>% ungroup() %>% summarise(n=sum(as.numeric(weight)*n))  # 1,186,574  0.5982697  | 


DANU_Demographics <- fread("DANU Demographics 1.1/DANU Demographics.txt")


Total_Duration %>% left_join(GLP1_Duration) %>% mutate(perc=n/Total_Duration) %>%
  ungroup() %>% left_join(DANU_Demographics %>% select(patid, age), by=c("patient"="patid")) %>%
  ggplot(aes(age, perc)) +
  geom_jitter(alpha=0.5) +
  geom_smooth(colour="firebrick", fill="firebrick") +
  ylim(0,1) +
  theme_minimal() +
  xlab("\n Age (years)") + ylab("DIABETES \n Proportion of treated months \n Actually ON Oral GLP1 \n")









DANU_Ingredients <- fread("DIA Analysis Results 1.1/DANU Ingredients.txt", integer64 = "character", stringsAsFactors = F)
DANU_Ingredients <- DANU_Ingredients %>%  separate(drug_id, c('class', 'molecule'))
DANU_Ingredients <- DANU_Ingredients %>% select(molecule, drug_group)
names(DANU_Ingredients)[1] <- "Drugs"
DANU_Ingredients$Drugs <- as.numeric(DANU_Ingredients$Drugs)


OBE2_Drug_Histories <- read.table("OBE2 Analysis Results 1.1/OBE2 Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
OBE2_Drug_Histories <- gather(OBE2_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
OBE2_Drug_Histories$Month <- as.character(OBE2_Drug_Histories$Month)
OBE2_Drug_Histories$Month <- parse_number(OBE2_Drug_Histories$Month)
OBE2_Drug_Histories <- OBE2_Drug_Histories %>% filter(Drugs != "-") %>% select(-c(disease))
OBE2_Drug_Histories <- separate_rows(OBE2_Drug_Histories, Drugs, sep = ",", convert=T)
OBE2_Drug_Histories <- OBE2_Drug_Histories %>% left_join(DANU_Ingredients)
OBE2_Drug_Histories <- OBE2_Drug_Histories %>% select(-Drugs) %>% distinct()
OBE2_Drug_Histories <- OBE2_Drug_Histories %>% filter(drug_group=="GLP1 Oral") %>% select(-drug_group)

OBE2_Drug_Histories <- OBE2_Drug_Histories %>% group_by(patient, weight) %>% filter(Month==min(Month)) %>% filter(Month>=49) %>%
  rename("First"="Month") %>%
  inner_join(OBE2_Drug_Histories)

OBE2_Drug_Histories$Total_Duration <- 60 - OBE2_Drug_Histories$First + 1

OBE2_Drug_Histories %>% ungroup() %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(as.numeric(weight))) # 23,425 (x12 281,100)

Total_Duration <- OBE2_Drug_Histories %>% select(patient, weight, Total_Duration) %>% distinct()
GLP1_Duration <- OBE2_Drug_Histories %>% group_by(patient, weight) %>% count()

Total_Duration %>% ungroup() %>% summarise(n=sum(as.numeric(weight)*Total_Duration))  # 140,795
GLP1_Duration %>% ungroup() %>% summarise(n=sum(as.numeric(weight)*n))  # 63,179  0.5982697  | 



DANU_Demographics <- fread("DANU Demographics 1.1/DANU Demographics.txt")


Total_Duration %>% left_join(GLP1_Duration) %>% mutate(perc=n/Total_Duration) %>%
  ungroup() %>% left_join(DANU_Demographics %>% select(patid, age), by=c("patient"="patid")) %>%
  ggplot(aes(age, perc)) +
  geom_jitter(alpha=0.5) +
  geom_smooth(colour="deepskyblue4", fill="deepskyblue4", method="lm") +
  ylim(0,1) +
  theme_minimal() +
  xlab("\n Age (years)") + ylab("OBESITY \n Proportion of treated months \n Actually ON Oral GLP1 \n")


# ----------------
# New Venn OBE + DIA ---------------------------------

New_Comorbidity_Groups_Jun1 <- fread("DIA Analysis Results 1.1/New_Comorbidity_Groups_Jun1.txt")
New_Comorbidity_Groups_Jun1 <- New_Comorbidity_Groups_Jun1 %>% mutate(diagnosis=ifelse(grepl("Diabetes", diagnosis), "Diabetes", diagnosis))
New_Comorbidity_Groups_Jun1 %>% group_by(diagnosis) %>% summarise(n=sum(weight))
New_Comorbidity_Groups_Jun1 %>% select(patid,weight,diagnosis) %>% distinct() %>% group_by(diagnosis) %>% summarise(n=sum(weight))
New_Comorbidity_Groups_Jun1 <- New_Comorbidity_Groups_Jun1 %>% select(patid,weight,diagnosis) %>% distinct()

# 1 Diabetes  30120087.
# 2 Obesity   49151140.

DIA_Comorbidity_Inventories <- fread("DIA Analysis Results 1.1/DIA Comorbidity Inventories.txt")
DIA_Comorbidity_Inventories <- DIA_Comorbidity_Inventories %>% select(patid, diagnosis)

DIA_ASCVD <- DIA_Comorbidity_Inventories %>% filter(grepl("G45", diagnosis)|
                                         grepl("I63", diagnosis)|
                                         grepl("H34", diagnosis)|
                                         grepl("I65", diagnosis)|
                                         grepl("I66", diagnosis)|
                                         grepl("I69", diagnosis)|
                                         grepl("I20", diagnosis)|
                                         grepl("I21", diagnosis)|
                                         grepl("I22", diagnosis)|
                                         grepl("I23", diagnosis)|
                                         grepl("I24", diagnosis)|
                                         grepl("I25", diagnosis)|
                                         grepl("I70", diagnosis)|
                                         grepl("I73", diagnosis)) %>% select(patid) %>% distinct()

DIA_ASCVD$DIA_ASCVD <- "DIA_ASCVD"

New_Comorbidity_Groups_Jun1 %>% select(patid,weight,diagnosis) %>% distinct() %>% inner_join(DIA_ASCVD) %>%
  group_by(diagnosis) %>% summarise(n=sum(weight))

OBE2_Comorbidity_Inventories <- fread("DIA Analysis Results 1.1/OBE2 Comorbidity Inventories.txt")
OBE2_Comorbidity_Inventories <- OBE2_Comorbidity_Inventories %>% select(patid, diagnosis)


OBE_ASCVD <- OBE2_Comorbidity_Inventories %>% filter(grepl("G45", diagnosis)|
                                         grepl("I63", diagnosis)|
                                         grepl("H34", diagnosis)|
                                         grepl("I65", diagnosis)|
                                         grepl("I66", diagnosis)|
                                         grepl("I69", diagnosis)|
                                         grepl("I20", diagnosis)|
                                         grepl("I21", diagnosis)|
                                         grepl("I22", diagnosis)|
                                         grepl("I23", diagnosis)|
                                         grepl("I24", diagnosis)|
                                         grepl("I25", diagnosis)|
                                         grepl("I70", diagnosis)|
                                         grepl("I73", diagnosis)) %>% select(patid) %>% distinct()

OBE_ASCVD$OBE_ASCVD <- "OBE_ASCVD"


New_Comorbidity_Groups_Jun1 %>% select(patid,weight,diagnosis) %>% distinct() %>% inner_join(OBE_ASCVD) %>%
  group_by(diagnosis) %>% summarise(n=sum(weight))


DANU_Demographics <- fread("DANU Demographics 1.1/DANU Demographics.txt", integer64 = "character", stringsAsFactors = F)
DANU_Demographics <- DANU_Demographics %>% select(patid, weight, age, gender)

DIA_ASCVD %>% left_join(DANU_Demographics) %>% summarise(n=mean(age))
OBE_ASCVD %>% left_join(DANU_Demographics) %>% summarise(n=mean(age))


DANU_Demographics %>%  filter(grepl("Diabetes", diagnosis))  %>% summarise(n=mean(age)) #61
DANU_Demographics %>% filter(diagnosis=="Obesity")  %>% summarise(n=mean(age)) #51

DANU_Demographics %>% filter(grepl("Diabetes", diagnosis))  %>% summarise(n=sum(weight)) #61
DANU_Demographics %>% filter(diagnosis=="Obesity")  %>% summarise(n=sum(weight)) #51


New_Comorbidity_Groups_Jun1 %>% select(patid,weight,diagnosis) %>% distinct() %>% left_join(OBE_ASCVD) %>%
  left_join(DANU_Demographics) %>% group_by(OBE_ASCVD) %>% filter(age>=65) %>% summarise(n=sum(weight))


New_Comorbidity_Groups_Jun1 %>% select(patid,weight,diagnosis) %>% distinct() %>% left_join(DIA_ASCVD) %>%
  left_join(DANU_Demographics) %>% group_by(DIA_ASCVD) %>% filter(age>=65) %>% summarise(n=sum(weight))

DANU_Demographics %>% filter(grepl("Diabetes", diagnosis)) %>%
  left_join(DIA_ASCVD) %>%
  mutate(DIA_ASCVD=ifelse(is.na(DIA_ASCVD), "DIA NO ASCVD", "DIA + ASCVD")) %>%
  ggplot(aes(age, colour=DIA_ASCVD, fill=DIA_ASCVD)) +
  geom_density(alpha=0.7) +
  theme_minimal() +
  xlab("\n Age (years)") + ylab("Patient density \n")


DANU_Demographics %>% filter(grepl("Diabetes", diagnosis)) %>%
  left_join(DIA_ASCVD) %>%
  mutate(DIA_ASCVD=ifelse(is.na(DIA_ASCVD), "DIA NO ASCVD", "DIA + ASCVD")) %>%
  ggplot(aes(age, colour=DIA_ASCVD, fill=DIA_ASCVD)) +
  geom_density(alpha=0.7) +
  theme_minimal() +
  xlab("\n Age (years)") + ylab("Patient density \n") +
  ggsci::scale_color_jama() +
  ggsci::scale_fill_jama()


DANU_Demographics %>% filter(diagnosis=="Obesity") %>%
  left_join(OBE_ASCVD) %>%
  mutate(OBE_ASCVD=ifelse(is.na(OBE_ASCVD), "OBE NO ASCVD", "OBE + ASCVD")) %>%
  ggplot(aes(age, colour=OBE_ASCVD, fill=OBE_ASCVD)) +
  geom_density(alpha=0.7) +
  theme_minimal() +
  xlab("\n Age (years)") + ylab("Patient density \n") +
  ggsci::scale_color_jama() +
  ggsci::scale_fill_jama()





DIA_ASCVD %>% left_join(DANU_Demographics) %>% group_by(gender) %>% summarise(n=sum(weight))
OBE_ASCVD %>% left_join(DANU_Demographics) %>% group_by(gender) %>% summarise(n=sum(weight))


DANU_Measures <- fread("DANU Measures 1.1/DANU Measures.txt",  integer64 = "character", stringsAsFactors = F)
DANU_Measures <- New_Comorbidity_Groups_Jun1 %>% inner_join(DANU_Measures)
BMI <- DANU_Measures %>% filter(test=="BMI")
HbA1c <- DANU_Measures %>% filter(test=="HbA1c Level")

BMI <- BMI %>% select(patid, value) %>% distinct() %>% group_by(patid ) %>% filter(value==max(value)) %>% slice(1)
HbA1c <- HbA1c %>% select(patid, value) %>% distinct() %>% group_by(patid ) %>% filter(value==max(value)) %>% slice(1)

rm(DANU_Measures)

names(BMI)[2] <- "BMI"
names(HbA1c)[2] <- "HbA1c"


BMI <- BMI %>% mutate(BMI=ifelse(BMI<27, "<27",
                    ifelse(BMI<30, "<30",
                           ifelse(BMI<35, "<35", ">35"))))

HbA1c <- HbA1c %>% mutate(HbA1c=ifelse(HbA1c<6.5, "<6.5",
                      ifelse(HbA1c<7.5, "<7.5",
                             ifelse(HbA1c<8.5, "<8.5",
                                    ifelse(HbA1c<9.5,"<9.5", ">9.5"))))) 



New_Comorbidity_Groups_Jun1 %>% select(patid,weight,diagnosis) %>% distinct() %>% inner_join(DIA_ASCVD) %>% summarise(n=sum(weight))

New_Comorbidity_Groups_Jun1 %>% select(patid,weight,diagnosis) %>% distinct() %>%
  filter(diagnosis=="Diabetes") %>%
  inner_join(HbA1c) %>%
   inner_join(DIA_ASCVD) %>%
  group_by(diagnosis, HbA1c) %>% summarise(n=sum(weight))


New_Comorbidity_Groups_Jun1 %>% select(patid,weight,diagnosis) %>% distinct() %>%
  filter(diagnosis=="Obesity") %>%
  inner_join(BMI) %>%
    inner_join(OBE_ASCVD) %>%
  group_by(diagnosis, BMI) %>% summarise(n=sum(weight))


New_Comorbidity_Groups_Jun1 <- fread("DIA Analysis Results 1.1/New_Comorbidity_Groups_Jun1.txt")
New_Comorbidity_Groups_Jun1 <- New_Comorbidity_Groups_Jun1 %>% mutate(diagnosis=ifelse(grepl("Diabetes", diagnosis), "Diabetes", diagnosis))
New_Comorbidity_Groups_Jun1 %>% group_by(diagnosis) %>% summarise(n=sum(weight))
New_Comorbidity_Groups_Jun1 %>% select(patid,weight,diagnosis) %>% distinct() %>% group_by(diagnosis) %>% summarise(n=sum(weight))
New_Comorbidity_Groups_Jun1 <- New_Comorbidity_Groups_Jun1 %>% select(patid,weight,diagnosis, group) %>% distinct()

New_Comorbidity_Groups_Jun1 %>%
     inner_join(DIA_ASCVD) %>%
  filter(diagnosis=="Diabetes") %>% select(patid, weight, group) %>% distinct() %>%
  group_by(group) %>% summarise(n=sum(weight))


New_Comorbidity_Groups_Jun1 %>%
     inner_join(OBE_ASCVD) %>%
  filter(diagnosis=="Obesity") %>% select(patid, weight, group) %>% distinct() %>%
  group_by(group) %>% summarise(n=sum(weight))



New_Comorbidity_Groups_Jun1 <- fread("DIA Analysis Results 1.1/New_Comorbidity_Groups_Jun1.txt")
New_Comorbidity_Groups_Jun1 <- New_Comorbidity_Groups_Jun1 %>% select(patid,weight,diagnosis, group) %>% distinct()

length(unique(New_Comorbidity_Groups_Jun1$patid))

data.frame(New_Comorbidity_Groups_Jun1 %>% 
  mutate(Diabetes=ifelse(grepl("Diabetes", diagnosis), 1,0)) %>%
  mutate(Obesity=ifelse(grepl("Obesity", diagnosis)|diagnosis=="Obesity", 1,0)) %>%
  select(-diagnosis) %>%
  left_join(DIA_ASCVD %>% full_join(OBE_ASCVD) %>% mutate(ASCVD = "ASCVD") %>% select(-c(DIA_ASCVD,OBE_ASCVD)) %>% distinct()) %>% 
  left_join(New_Comorbidity_Groups_Jun1 %>% filter(group=="OSLAP") %>% select(patid) %>% distinct() %>% mutate(OSLAP="OSLAP"))  %>%
  left_join(New_Comorbidity_Groups_Jun1 %>% filter(group!=""&group!="OSLAP") %>% select(patid) %>% distinct()  %>% mutate(Comorb="Comorb"))  %>%
  select(-group) %>% distinct() %>%
  mutate(No_5_Comorb=ifelse(is.na(Comorb),1,0)) %>%
  group_by(Diabetes, Obesity, ASCVD, OSLAP, Comorb, No_5_Comorb) %>% summarise(n=sum(weight)))

# -----------------------
# Incremental yield by each added comorbidity ---------------------------------

New_Comorbidity_Groups_Jun1 <- fread("DIA Analysis Results 1.1/New_Comorbidity_Groups_Jun1.txt")
New_Comorbidity_Groups_Jun1 <- New_Comorbidity_Groups_Jun1 %>% mutate(diagnosis=ifelse(grepl("Diabetes", diagnosis), "Diabetes", diagnosis))
New_Comorbidity_Groups_Jun1 %>% group_by(diagnosis) %>% summarise(n=sum(weight))
New_Comorbidity_Groups_Jun1 %>% select(patid,weight,diagnosis) %>% distinct() %>% group_by(diagnosis) %>% summarise(n=sum(weight))
New_Comorbidity_Groups_Jun1 <- New_Comorbidity_Groups_Jun1 %>% select(patid,weight,diagnosis) %>% distinct()

# 1 Diabetes  30120087.
# 2 Obesity   49151140.

DIA_Comorbidity_Inventories <- fread("DIA Analysis Results 1.1/DIA Comorbidity Inventories.txt")
DIA_Comorbidity_Inventories <- DIA_Comorbidity_Inventories %>% select(patid, diagnosis)

DIA_ASCVD <- DIA_Comorbidity_Inventories %>% filter(grepl("G45", diagnosis)|
                                         grepl("I63", diagnosis)|
                                         grepl("H34", diagnosis)|
                                         grepl("I65", diagnosis)|
                                         grepl("I66", diagnosis)|
                                         grepl("I69", diagnosis)|
                                         grepl("I20", diagnosis)|
                                         grepl("I21", diagnosis)|
                                         grepl("I22", diagnosis)|
                                         grepl("I23", diagnosis)|
                                         grepl("I24", diagnosis)|
                                         grepl("I25", diagnosis)|
                                         grepl("I70", diagnosis)|
                                         grepl("I73", diagnosis)) %>% select(patid) %>% distinct()

DIA_ASCVD$DIA_ASCVD <- "DIA_ASCVD"

New_Comorbidity_Groups_Jun1 %>% select(patid,weight,diagnosis) %>% distinct() %>% inner_join(DIA_ASCVD) %>%
  group_by(diagnosis) %>% summarise(n=sum(weight))

OBE2_Comorbidity_Inventories <- fread("DIA Analysis Results 1.1/OBE2 Comorbidity Inventories.txt")
OBE2_Comorbidity_Inventories <- OBE2_Comorbidity_Inventories %>% select(patid, diagnosis)


OBE_ASCVD <- OBE2_Comorbidity_Inventories %>% filter(grepl("G45", diagnosis)|
                                         grepl("I63", diagnosis)|
                                         grepl("H34", diagnosis)|
                                         grepl("I65", diagnosis)|
                                         grepl("I66", diagnosis)|
                                         grepl("I69", diagnosis)|
                                         grepl("I20", diagnosis)|
                                         grepl("I21", diagnosis)|
                                         grepl("I22", diagnosis)|
                                         grepl("I23", diagnosis)|
                                         grepl("I24", diagnosis)|
                                         grepl("I25", diagnosis)|
                                         grepl("I70", diagnosis)|
                                         grepl("I73", diagnosis)) %>% select(patid) %>% distinct()

OBE_ASCVD$OBE_ASCVD <- "OBE_ASCVD"


New_Comorbidity_Groups_Jun1 <- fread("DIA Analysis Results 1.1/New_Comorbidity_Groups_Jun1.txt")
New_Comorbidity_Groups_Jun1 <- New_Comorbidity_Groups_Jun1 %>% select(patid,weight,diagnosis, group) %>% distinct()

length(unique(New_Comorbidity_Groups_Jun1$patid))

data.frame(New_Comorbidity_Groups_Jun1 %>% 
  mutate(Diabetes=ifelse(grepl("Diabetes", diagnosis), 1,0)) %>%
  mutate(Obesity=ifelse(grepl("Obesity", diagnosis)|diagnosis=="Obesity", 1,0)) %>%
  select(-diagnosis) %>%
  left_join(DIA_ASCVD %>% full_join(OBE_ASCVD) %>% mutate(ASCVD = "ASCVD") %>% select(-c(DIA_ASCVD,OBE_ASCVD)) %>% distinct()) %>% 
  left_join(New_Comorbidity_Groups_Jun1 %>% filter(group=="OSLAP") %>% select(patid) %>% distinct() %>% mutate(OSLAP="OSLAP"))  %>%
  left_join(New_Comorbidity_Groups_Jun1 %>% filter(group!=""&group!="OSLAP") %>% select(patid) %>% distinct()  %>% mutate(Comorb="Comorb"))  %>%
  select(-group) %>% distinct() %>%
  mutate(No_5_Comorb=ifelse(is.na(Comorb),1,0)) %>%
  group_by(Diabetes, Obesity, ASCVD, OSLAP, Comorb, No_5_Comorb) %>% summarise(n=sum(weight)))


New_Comorbidity_Groups_Jun1 %>% 
  mutate(weight=ifelse(diagnosis=="Obesity", weight*1.94298648, weight)) %>%
    left_join(DIA_ASCVD %>% full_join(OBE_ASCVD) %>% mutate(ASCVD = "ASCVD") %>% select(-c(DIA_ASCVD,OBE_ASCVD)) %>% distinct()) %>% 
  #filter(ASCVD=="ASCVD") %>% select(patid, weight) %>% distinct() %>% summarise(n=sum(weight)) # 31737184
 # filter(ASCVD=="ASCVD"|group=="OSLAP") %>% select(patid, weight) %>% distinct() %>% summarise(n=sum(weight)) # 46099400
   #filter(ASCVD=="ASCVD"|group=="OSLAP"|group=="CKD") %>% select(patid, weight) %>% distinct() %>% summarise(n=sum(weight)) # 49481558
  #filter(ASCVD=="ASCVD"|group=="OSLAP"|group=="CKD"|group=="PAD_restrict") %>% select(patid, weight) %>% distinct() %>% summarise(n=sum(weight)) # 49629518
   # filter(ASCVD=="ASCVD"|group=="OSLAP"|group=="CKD"|group=="PAD_restrict"|group=="PCOS") %>% select(patid, weight) %>% distinct() %>% summarise(n=sum(weight)) # 51688167
 #filter(ASCVD=="ASCVD"|group=="OSLAP"|group=="CKD"|group=="PAD_restrict"|group=="PCOS"|group=="HFpEF") %>% select(patid, weight) %>% distinct() %>% summarise(n=sum(weight)) # 51823085

  
# -----------------------

# DANU Pediatrics population -----------------------------------------

DANUP_Weights <- fread("DANUP Pediatric Analysis Results 1.1/DANUP Weights.txt", integer64 = "character", stringsAsFactors = F)
DANUP_Weights %>% group_by(age) %>% summarise(n=sum(insured_population)) %>% transpose()


DANUP_Demographics <- fread("DANUP Pediatric Analysis Results 1.1/DANUP Demographics.txt", integer64 = "character", stringsAsFactors = F)
unique(DANUP_Demographics$diagnosis)
DANUP_Demographics %>% filter(diagnosis=="Obesity") %>% group_by(age) %>% summarise(n=sum(weight)) %>% transpose()

DANUP_Demographics %>% filter(diagnosis=="Obesity") %>% group_by(age, obesity_condition) %>% summarise(n=sum(weight)) %>%
  spread(key=age, value=n)

DANUP_Measures <- fread("DANUP Pediatric Analysis Results 1.1/DANUP Measures.txt", integer64 = "character", stringsAsFactors = F)
DANUP_Measures <- DANUP_Measures %>% filter(test=="BMI") %>% group_by(patid) %>% filter(value==max(value)) %>% slice(1) %>% select(patid, weight, value) %>% distinct()

DANUP_Measures %>% ungroup %>% inner_join(
  DANUP_Demographics %>%  filter(grepl("Obesi", diagnosis))  %>% select(patid, weight, age)
  ) %>% group_by(age) %>% summarise(n=sum(weight)) %>% transpose()

DANUP_Measures %>% mutate(value=ifelse(value>=25&value<27, "25-27",
                                       ifelse(value>=27&value<30, "27-30",
                                              ifelse(value>=30&value<35,"30-35", 
                                                    ifelse(value<25, "-25", "+35"))))) %>%
  inner_join(
  DANUP_Demographics %>%  filter(grepl("Obesi", diagnosis))  %>% select(patid, weight, age)
  ) %>% group_by(age, value) %>% summarise(n=sum(weight)) %>% spread(key=age, value=n)






DANU_Ingredients <- fread("DANUP Pediatric Analysis Results 1.1/DANU Ingredients.txt", integer64 = "character", stringsAsFactors = F)
DANU_Ingredients <- DANU_Ingredients %>%  separate(drug_id, c('class', 'molecule'))
DANU_Ingredients  <- DANU_Ingredients %>% select(molecule, drug_group)
names(DANU_Ingredients)[1] <- "Drugs"
DANU_Ingredients$Drugs <- as.numeric(DANU_Ingredients$Drugs)
string_OralGLP1        <- paste0("\\b(",paste0(DANU_Ingredients$Drugs[DANU_Ingredients$drug_group == "GLP1 Oral"], collapse = "|"),")\\b")
string_InjectableGLP1  <- paste0("\\b(",paste0(DANU_Ingredients$Drugs[DANU_Ingredients$drug_group == "GLP1 Injectable"], collapse = "|"),")\\b")
string_GLP1  <- paste0("\\b(",paste0(DANU_Ingredients$Drugs[DANU_Ingredients$drug_group == "GLP1 Injectable"|DANU_Ingredients$drug_group == "GLP1 Oral"], collapse = "|"),")\\b")

DIAP_Drug_Histories <- fread("DANUP Pediatric Analysis Results 1.1/DIAP Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
DIAP_Drug_Histories <- DIAP_Drug_Histories %>% select(4:63)
DIAP_Drug_Histories[DIAP_Drug_Histories != "-"] <- 1  # on drug 
DIAP_Drug_Histories[DIAP_Drug_Histories == "-"] <- 0  # no drug
DIAP_Drug_Histories[] <- lapply(DIAP_Drug_Histories,as.numeric)
DIAP_Drug_Histories$SUM <- rowSums(DIAP_Drug_Histories)
DIAP_Drug_Histories_LONG <- fread("DANUP Pediatric Analysis Results 1.1/DIAP Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
Pats_vec <- DIAP_Drug_Histories_LONG %>% select(patient, weight)
DIAP_Drug_Histories <- Pats_vec %>% bind_cols(DIAP_Drug_Histories)
DIAP_Drug_Histories <- DIAP_Drug_Histories %>% filter(SUM != 0)
DIAP_Treatment_exp_Vector <- DIAP_Drug_Histories %>% select(patient, weight)
DIAP_Drug_Histories <- fread("DANUP Pediatric Analysis Results 1.1/DIAP Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
DIAP_Drug_Histories <- DIAP_Treatment_exp_Vector %>% left_join(DIAP_Drug_Histories)
sum(DIAP_Drug_Histories$weight)

OBE2P_Drug_Histories <- fread("DANUP Pediatric Analysis Results 1.1/OBE2P Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
OBE2P_Drug_Histories <- OBE2P_Drug_Histories %>% select(4:63)
OBE2P_Drug_Histories[OBE2P_Drug_Histories != "-"] <- 1  # on drug 
OBE2P_Drug_Histories[OBE2P_Drug_Histories == "-"] <- 0  # no drug
OBE2P_Drug_Histories[] <- lapply(OBE2P_Drug_Histories,as.numeric)
OBE2P_Drug_Histories$SUM <- rowSums(OBE2P_Drug_Histories)
OBE2P_Drug_Histories_LONG <- fread("DANUP Pediatric Analysis Results 1.1/OBE2P Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
Pats_vec <- OBE2P_Drug_Histories_LONG %>% select(patient, weight)
OBE2P_Drug_Histories <- Pats_vec %>% bind_cols(OBE2P_Drug_Histories)
OBE2P_Drug_Histories <- OBE2P_Drug_Histories %>% filter(SUM != 0)
OBE2P_Treatment_exp_Vector <- OBE2P_Drug_Histories %>% select(patient, weight)
OBE2P_Drug_Histories <- fread("DANUP Pediatric Analysis Results 1.1/OBE2P Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
OBE2P_Drug_Histories <- OBE2P_Treatment_exp_Vector %>% left_join(OBE2P_Drug_Histories)
sum(OBE2P_Drug_Histories$weight)

Drug_Histories <-  OBE2P_Drug_Histories %>% select(-disease) %>%
  bind_rows(DIAP_Drug_Histories %>% select(-disease))

Drug_Histories <- gather(Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
Drug_Histories <- Drug_Histories %>% filter(Drugs!="-")
Drug_Histories$Month <- as.character(Drug_Histories$Month)
Drug_Histories$Month <- parse_number(Drug_Histories$Month)

DANUP_Demographics %>%  filter(diagnosis=="Obesity")  %>% select(patid, weight, age) %>%
   inner_join(Drug_Histories %>% select(patient), by=c("patid"="patient")) %>% distinct() %>%
   group_by(age) %>% summarise(n=sum(weight)) %>% transpose()

DANUP_Demographics %>%   filter(diagnosis=="Obesity") %>% select(patid, weight, age) %>%
  inner_join(Drug_Histories %>% filter(Month>=49) %>% select(patient), by=c("patid"="patient")) %>% distinct() %>%
  group_by(age) %>% summarise(n=sum(weight)) %>% transpose()

DANUP_Demographics %>%   filter(diagnosis=="Obesity") %>% select(patid, weight, age) %>%
  inner_join(Drug_Histories %>% filter(grepl(string_GLP1, Drugs)) %>% select(patient), by=c("patid"="patient")) %>% distinct() %>%
  group_by(age) %>% summarise(n=sum(weight)) %>% transpose()
 

DANUP_Demographics %>% filter(diagnosis=="Obesity")  %>% select(patid, weight, age) %>%
  inner_join(Drug_Histories %>% filter(grepl(string_GLP1, Drugs) & Month>=49) %>% select(patient), by=c("patid"="patient")) %>% distinct() %>%
  group_by(age) %>% summarise(n=sum(weight)) %>% transpose()


DANUP_Demographics %>% filter(diagnosis=="Obesity") %>% select(patid, weight, age) %>%
  inner_join(Drug_Histories %>% filter(grepl(string_InjectableGLP1, Drugs) & Month>=49) %>% select(patient), by=c("patid"="patient")) %>% distinct() %>%
  group_by(age) %>% summarise(n=sum(weight)) %>% transpose()


DANUP_Demographics %>%  filter(diagnosis=="Obesity") %>% select(patid, weight, age) %>%
  inner_join(Drug_Histories %>% filter(grepl(string_OralGLP1, Drugs) & Month>=49) %>% select(patient), by=c("patid"="patient")) %>% distinct() %>%
  group_by(age) %>% summarise(n=sum(weight)) %>% transpose()


Drug_Histories %>% select(Month) %>% distinct() %>%
  left_join(
data.frame(DANUP_Demographics %>% filter(diagnosis=="Obesity")  %>% select(patid, weight) %>%
  inner_join(Drug_Histories %>% filter(grepl(string_OralGLP1, Drugs)) %>% select(patient, Month) %>% distinct(), by=c("patid"="patient")) %>% distinct() %>%
  group_by(Month) %>% summarise(n=sum(weight)))
)

# -----------------------------------

# LCM Accenture market size  HF --------------------------

New_Comorbidity_Groups_Jun1 <- fread("Mkt_Comorbidity_Groups_OBE_adjusted_Jun22.txt", sep="\t")
New_Comorbidity_Groups_Jun1 %>% group_by(diagnosis) %>% summarise(n=sum(weight))

# 1 Diabetes            3862796.
# 2 Diabetes + Obesity 26257291.
# 3 Obesity            95506738

DANU_Measures <- fread("DANU Measures 1.1/DANU Measures.txt",  integer64 = "character", stringsAsFactors = F)
DANU_Measures <- DANU_Measures %>% select(-weight) %>% inner_join(New_Comorbidity_Groups_Jun1 %>% select(patid, weight, diagnosis))
DANU_Measures <- DANU_Measures %>% filter(test=="BMI")
DANU_Measures <- DANU_Measures %>% select(patid, value, weight, diagnosis) %>% distinct() %>% group_by(patid ) %>%  filter(value==max(value)) %>% slice(1) %>% ungroup()

DANU_Measures %>% group_by(diagnosis) %>% summarise(n=sum(weight))

# 1 Diabetes             934859.
# 2 Diabetes + Obesity 17145906.
# 3 Obesity            95506738.

DANU_Measures %>% filter(value>=30) %>% group_by(diagnosis) %>% summarise(n=sum(weight))

# 1 Diabetes              11742.
# 2 Diabetes + Obesity 12950513.
# 3 Obesity            73280987

# 12950513./17145906.

DANU_Measures <- DANU_Measures %>% filter(value>=30) %>% select(patid, weight, diagnosis)

DANU_Measures %>% inner_join(New_Comorbidity_Groups_Jun1 %>% filter(HFpEF==1) %>% select(patid)) %>% group_by(diagnosis) %>% summarise(n=sum(weight))

# 281665/12950513.

# From Heart Failure Data

DANU_Measures_Full <- fread("DANU Measures Full.txt")
DANU_Measures_Full <- DANU_Measures_Full %>% filter(test=="BMI")

Predicted_Stages_gbm_All <- fread("Predicted_Stages_gbm_All.txt")
First_Diastolic_All_L5y <- fread("First_Diastolic_All_L5y.txt")

DANU_Measures_Full <- DANU_Measures_Full %>% inner_join(First_Diastolic_All_L5y %>% select(patient) , by=c("patid"="patient"))

HF_Demographics <- fread("HF Demographics.txt")
HF_Demographics %>% select(patid, weight) %>% inner_join(First_Diastolic_All_L5y , by=c("patid"="patient")) %>% summarise(n=sum(weight))

DANU_Measures_Full <- DANU_Measures_Full %>% group_by(patid) %>%filter(value==max(value)) %>% slice(1) %>% filter(value>=30) %>% select(patid, weight)
DANU_Measures_Full <- DANU_Measures_Full %>% inner_join(HF_Demographics %>% filter(diagnosis =="Obesity" | diagnosis=="Diabetes + Obesity")) %>% select(patid, weight, diagnosis)

DANU_Measures_Full %>% group_by(diagnosis) %>% summarise(n=sum(weight))

# 1 Diabetes + Obesity 739714.
# 2 Obesity            375898.

DANU_Measures_Full %>% left_join(Predicted_Stages_gbm_All, by=c("patid"="patient")) %>% filter(Predicted.Stage!=1) %>%
  group_by(diagnosis) %>% summarise(n=sum(weight))

# 1 Diabetes + Obesity 687285.
# 2 Obesity            322572.

Target <- DANU_Measures_Full %>% left_join(Predicted_Stages_gbm_All, by=c("patid"="patient")) %>% filter(Predicted.Stage!=1) %>% select(-Predicted.Stage)


HF_Drug_Histories <- fread("HF Drug Histories.txt", colClasses = "character")
HF_Drug_Histories <- gather(HF_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
HF_Drug_Histories$Month <- as.character(HF_Drug_Histories$Month)
HF_Drug_Histories$Month <- parse_number(HF_Drug_Histories$Month)

HF_Drug_Histories <- HF_Drug_Histories %>% filter(Month>=49)
HF_Drug_Histories <- HF_Drug_Histories %>% filter(Drugs!="-") %>% select(patient) %>% distinct()

Target %>% inner_join(HF_Drug_Histories, by=c("patid"="patient")) %>% group_by(diagnosis) %>% summarise(n=sum(weight))
Target <- Target %>% inner_join(HF_Drug_Histories, by=c("patid"="patient"))
Target %>% group_by(diagnosis) %>% summarise(n=sum(weight))


# 1 Diabetes + Obesity 687285.
# 2 Obesity            322572.


# From DANU Diabetes Data


DANU_Ingredients <- fread("DANU Ingredients.txt", integer64 = "character", stringsAsFactors = F)
DANU_Ingredients <- DANU_Ingredients %>%  separate(drug_id, c('class', 'molecule'))
DANU_Ingredients  <- DANU_Ingredients %>% select(molecule, drug_group)
names(DANU_Ingredients)[1] <- "Drugs"
DANU_Ingredients$Drugs <- as.numeric(DANU_Ingredients$Drugs)
string_OralGLP1        <- paste0("\\b(",paste0(DANU_Ingredients$Drugs[DANU_Ingredients$drug_group == "GLP1 Oral"], collapse = "|"),")\\b")
string_InjectableGLP1  <- paste0("\\b(",paste0(DANU_Ingredients$Drugs[DANU_Ingredients$drug_group == "GLP1 Injectable"], collapse = "|"),")\\b")
string_GLP1  <- paste0("\\b(",paste0(DANU_Ingredients$Drugs[DANU_Ingredients$drug_group == "GLP1 Injectable"|DANU_Ingredients$drug_group == "GLP1 Oral"], collapse = "|"),")\\b")

DIA_Drug_Histories <- fread("DIA Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
OBE_Drug_Histories <- fread("OBE Drug Histories.txt", integer64 = "character", stringsAsFactors = F)

Drug_Histories <-  DIA_Drug_Histories %>% select(-disease) %>%
  bind_rows(OBE_Drug_Histories %>% select(-disease))

Drug_Histories <- gather(Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
Drug_Histories <- Drug_Histories %>% filter(Drugs!="-")
Drug_Histories$Month <- as.character(Drug_Histories$Month)
Drug_Histories$Month <- parse_number(Drug_Histories$Month)

Drug_Histories <- Drug_Histories %>% filter(Month>=49)
Drug_Histories <- Drug_Histories %>% select(patient, Drugs) %>% distinct()

Drug_Histories %>% filter(grepl(string_GLP1, Drugs)) %>% select(patient) %>% distinct() %>%
  inner_join(Target, by=c("patient"="patid")) %>% group_by(diagnosis) %>% summarise(n=sum(weight))

# 1 Diabetes + Obesity 89036.
# 2 Obesity              956.

Drug_Histories %>% filter(grepl(string_OralGLP1, Drugs)) %>% select(patient) %>% distinct() %>%
  inner_join(Target, by=c("patient"="patid")) %>% group_by(diagnosis) %>% summarise(n=sum(weight))

# Diabetes + Obesity 2745.


# ---------------------------------------

# LCM Accenture market size  CKD --------------------------

New_Comorbidity_Groups_Jun1 <- fread("Mkt_Comorbidity_Groups_OBE_adjusted_Jun22.txt", sep="\t")
New_Comorbidity_Groups_Jun1 %>% group_by(diagnosis) %>% summarise(n=sum(weight))

# 1 Diabetes            3862796.
# 2 Diabetes + Obesity 26257291.
# 3 Obesity            95506738

DANU_Measures <- fread("DANU Measures 1.1/DANU Measures.txt",  integer64 = "character", stringsAsFactors = F)
DANU_Measures <- DANU_Measures %>% select(-weight) %>% inner_join(New_Comorbidity_Groups_Jun1 %>% select(patid, weight, diagnosis))
DANU_Measures <- DANU_Measures %>% filter(test=="BMI")
DANU_Measures <- DANU_Measures %>% select(patid, value, weight, diagnosis,) %>% distinct() %>% group_by(patid ) %>%  filter(value==max(value)) %>% slice(1) %>% ungroup()

DANU_Measures %>% group_by(diagnosis) %>% summarise(n=sum(weight))

# 1 Diabetes             934859.
# 2 Diabetes + Obesity 17145906.
# 3 Obesity            95506738.

DANU_Measures %>% filter(value>=27) %>% group_by(diagnosis) %>% summarise(n=sum(weight))

# 1 Diabetes              17908.
# 2 Diabetes + Obesity 15585321.
# 3 Obesity            95506738.

DANU_Measures <- DANU_Measures %>% filter(value>=27) %>% select(patid, weight, diagnosis)

DANU_Measures %>% inner_join(New_Comorbidity_Groups_Jun1 %>% filter(CKD==1) %>% select(patid)) %>% group_by(diagnosis) %>% summarise(n=sum(weight))

# 1 Diabetes              3966.
# 2 Diabetes + Obesity 3218935.
# 3 Obesity            4561006.

# From CKD Data

DANU_Measures <- fread("DANU Measures.txt")
DANU_Measures <- DANU_Measures %>% filter(test=="BMI")

CKD_Stages_Complete_FilledIn <- fread("CKD_Stages_Complete_FilledIn.txt")


DANU_Measures <- DANU_Measures %>% inner_join(CKD_Stages_Complete_FilledIn %>% select(patient) , by=c("patid"="patient"))

CKD_Demographics <- fread("CKD Demographics.txt")
CKD_Demographics %>% select(patid, weight) %>% inner_join(CKD_Stages_Complete_FilledIn , by=c("patid"="patient")) %>% summarise(n=sum(weight))

CKD_Pts_Cmbdt_flags_Cmbdt_Hierchy_grps <- fread("CKD_Pts_Cmbdt_flags_&_Cmbdt_Hierchy_grps.csv")
CKD_Pts_Cmbdt_flags_Cmbdt_Hierchy_grps <- CKD_Pts_Cmbdt_flags_Cmbdt_Hierchy_grps %>% select(patid)

CKD_Demographics %>% select(patid, weight) %>% inner_join(CKD_Pts_Cmbdt_flags_Cmbdt_Hierchy_grps) %>%
  inner_join(CKD_Stages_Complete_FilledIn , by=c("patid"="patient")) %>% summarise(n=sum(weight))


DANU_Measures <- DANU_Measures %>% group_by(patid) %>%filter(value==max(value)) %>% slice(1) %>% filter(value>=27) %>% select(patid, weight)
DANU_Measures <- DANU_Measures %>% inner_join(CKD_Demographics %>% filter(diagnosis =="Obesity" | diagnosis=="Diabetes + Obesity")) %>% select(patid, weight, diagnosis)


DANU_Measures_ACR <- fread("DANU Measures.txt")
DANU_Measures_ACR <- DANU_Measures_ACR %>% filter(test=="ACR Ratio") %>% select(patid, weight, value) %>% distinct()
names(DANU_Measures_ACR)[1] <- "patient"
DANU_Measures_ACR <- DANU_Measures_ACR %>% group_by(patient) %>% filter(value==max(value)) %>% slice(1)
DANU_Measures_ACR <- DANU_Measures_ACR %>% ungroup() %>% mutate(value=ifelse(value>300, "Yes", "No"))
  
  
DANU_Measures <- DANU_Measures %>% inner_join(DANU_Measures_ACR %>% select(-weight), by=c("patid"="patient")) %>%
  inner_join(CKD_Demographics %>% filter(diagnosis =="Obesity" | diagnosis=="Diabetes + Obesity")) %>% select(patid, weight, diagnosis, value)

DANU_Measures %>% group_by(diagnosis) %>% summarise(n=sum(weight))

# 1 Diabetes + Obesity 1296058.
# 2 Obesity             319157.



DANU_Measures %>% left_join(CKD_Stages_Complete_FilledIn, by=c("patid"="patient")) %>% filter(Stage=="Stage3"|
                                                                                                Stage=="Stage4"|
                                                                                                (Stage=="Stage1"&value=="Yes")|
                                                                                                (Stage=="Stage2"&value=="Yes")) %>%
  group_by(diagnosis) %>% summarise(n=sum(weight))

# 1 Diabetes + Obesity 833673.
# 2 Obesity            231098.

Target <- DANU_Measures %>% left_join(CKD_Stages_Complete_FilledIn, by=c("patid"="patient")) %>% filter(Stage=="Stage3"|
                                                                                                Stage=="Stage4"|
                                                                                                (Stage=="Stage1"&value=="Yes")|
                                                                                                (Stage=="Stage2"&value=="Yes"))

CKD_Drug_Histories <- fread("CKD Drug Histories.txt", colClasses = "character")
CKD_Drug_Histories <- gather(CKD_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
CKD_Drug_Histories$Month <- as.character(CKD_Drug_Histories$Month)
CKD_Drug_Histories$Month <- parse_number(CKD_Drug_Histories$Month)

CKD_Drug_Histories <- CKD_Drug_Histories %>% filter(Month>=49)
CKD_Drug_Histories <- CKD_Drug_Histories %>% filter(Drugs!="-") %>% select(patient) %>% distinct()

Target %>% inner_join(CKD_Drug_Histories, by=c("patid"="patient")) %>% group_by(diagnosis) %>% summarise(n=sum(weight))
Target <- Target %>% inner_join(CKD_Drug_Histories, by=c("patid"="patient"))
Target %>% group_by(diagnosis) %>% summarise(n=sum(weight))


# 1 Diabetes + Obesity 833673.
# 2 Obesity            231098.


# From DANU Diabetes Data


DANU_Ingredients <- fread("DANU Ingredients.txt", integer64 = "character", stringsAsFactors = F)
DANU_Ingredients <- DANU_Ingredients %>%  separate(drug_id, c('class', 'molecule'))
DANU_Ingredients  <- DANU_Ingredients %>% select(molecule, drug_group)
names(DANU_Ingredients)[1] <- "Drugs"
DANU_Ingredients$Drugs <- as.numeric(DANU_Ingredients$Drugs)
string_OralGLP1        <- paste0("\\b(",paste0(DANU_Ingredients$Drugs[DANU_Ingredients$drug_group == "GLP1 Oral"], collapse = "|"),")\\b")
string_InjectableGLP1  <- paste0("\\b(",paste0(DANU_Ingredients$Drugs[DANU_Ingredients$drug_group == "GLP1 Injectable"], collapse = "|"),")\\b")
string_GLP1  <- paste0("\\b(",paste0(DANU_Ingredients$Drugs[DANU_Ingredients$drug_group == "GLP1 Injectable"|DANU_Ingredients$drug_group == "GLP1 Oral"], collapse = "|"),")\\b")

DIA_Drug_Histories <- fread("DIA Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
OBE_Drug_Histories <- fread("OBE Drug Histories.txt", integer64 = "character", stringsAsFactors = F)

Drug_Histories <-  DIA_Drug_Histories %>% select(-disease) %>%
  bind_rows(OBE_Drug_Histories %>% select(-disease))

Drug_Histories <- gather(Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
Drug_Histories <- Drug_Histories %>% filter(Drugs!="-")
Drug_Histories$Month <- as.character(Drug_Histories$Month)
Drug_Histories$Month <- parse_number(Drug_Histories$Month)

Drug_Histories <- Drug_Histories %>% filter(Month>=49)
Drug_Histories <- Drug_Histories %>% select(patient, Drugs) %>% distinct()

Drug_Histories %>% filter(grepl(string_GLP1, Drugs)) %>% select(patient) %>% distinct() %>%
  inner_join(Target, by=c("patient"="patid")) %>% group_by(diagnosis) %>% summarise(n=sum(weight))

 # Diabetes + Obesity 139557.

Drug_Histories %>% filter(grepl(string_OralGLP1, Drugs)) %>% select(patient) %>% distinct() %>%
  inner_join(Target, by=c("patient"="patid")) %>% group_by(diagnosis) %>% summarise(n=sum(weight))

# Diabetes + Obesity 4373.


# --------------------

# LCM Accenture market size  OSLAP --------------------------

New_Comorbidity_Groups_Jun1 <- fread("Mkt_Comorbidity_Groups_OBE_adjusted_Jun22.txt", sep="\t")
New_Comorbidity_Groups_Jun1 %>% group_by(diagnosis) %>% summarise(n=sum(weight))

# 1 Diabetes            3862796.
# 2 Diabetes + Obesity 26257291.
# 3 Obesity            95506738

DANU_Measures <- fread("DANU Measures 1.1/DANU Measures.txt",  integer64 = "character", stringsAsFactors = F)
DANU_Measures <- DANU_Measures %>% select(-weight) %>% inner_join(New_Comorbidity_Groups_Jun1 %>% select(patid, weight, diagnosis))
DANU_Measures <- DANU_Measures %>% filter(test=="BMI")
DANU_Measures <- DANU_Measures %>% select(patid, value, weight, diagnosis,) %>% distinct() %>% group_by(patid ) %>%  filter(value==max(value)) %>% slice(1) %>% ungroup()

DANU_Measures %>% group_by(diagnosis) %>% summarise(n=sum(weight))

# 1 Diabetes             934859.
# 2 Diabetes + Obesity 17145906.
# 3 Obesity            95506738.

DANU_Measures %>% filter(value>=27) %>% group_by(diagnosis) %>% summarise(n=sum(weight))

# 1 Diabetes              17908.
# 2 Diabetes + Obesity 15585321.
# 3 Obesity            95506738.

DANU_Measures <- DANU_Measures %>% filter(value>=27) %>% select(patid, weight, diagnosis)

DANU_Measures %>% inner_join(New_Comorbidity_Groups_Jun1 %>% filter(OSLAP==1) %>% select(patid)) %>% group_by(diagnosis) %>% summarise(n=sum(weight))

# 1 Diabetes               1625.
# 2 Diabetes + Obesity  4844037.
# 3 Obesity            11818708.



# From DANU Diabetes Data


New_Comorbidity_Groups_Jun1 %>% filter(OSLAP==1) %>% select(patid, weight, diagnosis) %>%
  group_by(diagnosis) %>% summarise(n=sum(weight))



DANU_Ingredients <- fread("DANU Ingredients.txt", integer64 = "character", stringsAsFactors = F)
DANU_Ingredients <- DANU_Ingredients %>%  separate(drug_id, c('class', 'molecule'))
DANU_Ingredients  <- DANU_Ingredients %>% select(molecule, drug_group)
names(DANU_Ingredients)[1] <- "Drugs"
DANU_Ingredients$Drugs <- as.numeric(DANU_Ingredients$Drugs)
string_OralGLP1        <- paste0("\\b(",paste0(DANU_Ingredients$Drugs[DANU_Ingredients$drug_group == "GLP1 Oral"], collapse = "|"),")\\b")
string_InjectableGLP1  <- paste0("\\b(",paste0(DANU_Ingredients$Drugs[DANU_Ingredients$drug_group == "GLP1 Injectable"], collapse = "|"),")\\b")
string_GLP1  <- paste0("\\b(",paste0(DANU_Ingredients$Drugs[DANU_Ingredients$drug_group == "GLP1 Injectable"|DANU_Ingredients$drug_group == "GLP1 Oral"], collapse = "|"),")\\b")

DIA_Drug_Histories <- fread("DIA Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
OBE_Drug_Histories <- fread("OBE Drug Histories.txt", integer64 = "character", stringsAsFactors = F)

Drug_Histories <-  DIA_Drug_Histories %>% select(-disease) %>%
  bind_rows(OBE_Drug_Histories %>% select(-disease))

Drug_Histories %>% select(patient) %>% distinct() %>% 
  inner_join(New_Comorbidity_Groups_Jun1 %>% filter(OSLAP==1) %>% select(patid, weight, diagnosis), by=c("patient"="patid")) %>%
  group_by(diagnosis) %>% summarise(n=sum(weight))

# 1 Diabetes             65756.
# 2 Diabetes + Obesity 1538270.
# 3 Obesity            2306604.

Drug_Histories <- gather(Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
Drug_Histories <- Drug_Histories %>% filter(Drugs!="-")
Drug_Histories$Month <- as.character(Drug_Histories$Month)
Drug_Histories$Month <- parse_number(Drug_Histories$Month)
Drug_Histories <- Drug_Histories %>% filter(Month>=49)
Drug_Histories <- Drug_Histories %>% select(patient, Drugs) %>% distinct()


Drug_Histories %>% filter(grepl(string_GLP1, Drugs)) %>% select(patient) %>% distinct() %>%
    inner_join(New_Comorbidity_Groups_Jun1 %>% filter(OSLAP==1) %>% select(patid, weight, diagnosis), by=c("patient"="patid")) %>%
  group_by(diagnosis) %>% summarise(n=sum(weight))

# 1 Diabetes             7873.
# 2 Diabetes + Obesity 372915.
# 3 Obesity             13805.

Drug_Histories %>% filter(grepl(string_OralGLP1, Drugs)) %>% select(patient) %>% distinct() %>%
    inner_join(New_Comorbidity_Groups_Jun1 %>% filter(OSLAP==1) %>% select(patid, weight, diagnosis), by=c("patient"="patid")) %>%
  group_by(diagnosis) %>% summarise(n=sum(weight))

# 1 Diabetes             591.
# 2 Diabetes + Obesity 15545.
# 3 Obesity             1211.


#  --------------------
# Stocks and Flows - DIA vs DIA+OBE ---------------------------
DANU_Demographics <- fread("DANU Demographics 1.1/DANU Demographics.txt")
names(DANU_Demographics)[1] <- "patient"
Treatment_exp_Vector <- fread("DIA Analysis Results 1.1/Treatment_exp_Vector.txt")
DANU_Demographics <- Treatment_exp_Vector %>% inner_join(DANU_Demographics)
#DANU_Demographics <- DANU_Demographics %>% filter(grepl("Dia", diagnosis))
DANU_Demographics <- DANU_Demographics %>% select(patient, weight, diabetes_onset, obesity_onset)
DANU_Demographics <- gather(DANU_Demographics, onset, date, diabetes_onset:obesity_onset, factor_key=TRUE)

Months_lookup <- fread("Months_lookup.txt")
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")

DANU_Demographics$date <- format(as.Date(DANU_Demographics$date), "%Y-%m")
unique(DANU_Demographics$date)

DANU_Demographics <- DANU_Demographics %>% left_join(Months_lookup, by = c("date" = "Month")) %>% 
  mutate(Exact_Month=ifelse(is.na(Exact_Month)&!is.na(date),0, Exact_Month)) 

DANU_Demographics <- DANU_Demographics %>% select(-date)

Index <- data.frame(c(0:60)) 
names(Index)[1] <- "Exact_Month"
Index$ID <- 1

DANU_Demographics <- DANU_Demographics %>% spread(key=onset, value=Exact_Month)
DANU_Demographics$ID <- 1

DANU_Demographics <- Index %>% left_join(DANU_Demographics) %>% arrange(patient) %>%
  mutate(diabetes=ifelse(diabetes_onset==Exact_Month, "diabetes", "no")) %>%
  mutate(obesity=ifelse(obesity_onset==Exact_Month, "obesity", "no"))

DANU_Demographics <- data.frame(DANU_Demographics %>% mutate(obesity=ifelse(is.na(obesity),"0",
                                            ifelse(obesity!="no","2", "1"))) %>%
  group_by(patient) %>%
  mutate(obesity=ifelse(cumsum(obesity=="2")>=1,"2", "0")) %>%
    mutate(diabetes=ifelse(cumsum(diabetes=="diabetes")>=1,"2", "0")))


data.frame(DANU_Demographics %>% select(-c(diabetes_onset, obesity_onset)) %>%
  group_by(patient) %>%
  mutate(flow=ifelse( diabetes=="2"&obesity=="2"&lag(diabetes)=="0"&lag(obesity)=="0", "naive_to_comorb",
                      ifelse( diabetes=="2"&obesity=="2"&lag(diabetes)=="2"&lag(obesity)=="0", "dia_to_comorb",
                              ifelse( diabetes=="2"&obesity=="2"&lag(diabetes)=="0"&lag(obesity)=="2", "obe_to_comorb",
                                      ifelse( diabetes=="2"&lag(diabetes)=="0", "naive_to_dia",
                                             ifelse(obesity=="2"&lag(obesity)=="0", "naive_to_obe" , NA )))))) %>%
  ungroup() %>% group_by(Exact_Month, flow) %>% summarise(n=sum(weight)) %>%
  drop_na() %>% spread(key=flow, value=n))
  


data.frame(DANU_Demographics %>% group_by(Exact_Month, diabetes, obesity) %>% summarise(n=sum(weight))) %>%
  mutate(group=ifelse(diabetes=="0"&obesity=="0", "naive",
                      ifelse(diabetes=="2"&obesity=="2", "dia_obe",
                             ifelse(diabetes=="2"&obesity=="0", "dia", "obe")))) %>%
  select(-c(diabetes, obesity)) %>% 
  spread(key=group, value=n) %>%
  mutate(total=dia+dia_obe+obe+naive)

# -----------------

# DANU Pediatrics population NEW WEIGHTS -----------------------------------------

DANUP_Weights <- fread("DANUP Pediatric Analysis Results 1.1/DANUP Weights.txt", integer64 = "character", stringsAsFactors = F)
DANUP_Weights %>% group_by(age) %>% summarise(n=sum(insured_population)) %>% transpose()

OBE2P_Demographics_All <- fread("DANUP Pediatric Analysis Results 1.1/OBE2P Demographics All.txt", integer64 = "character", stringsAsFactors = F)
OBE2P_Demographics_All <- OBE2P_Demographics_All %>% select(patid, weight, age, diagnosis, obesity_condition) %>% filter(diagnosis!="-")

unique(OBE2P_Demographics_All$obesity_condition)

OBE2P_Demographics_All <- OBE2P_Demographics_All %>% 
  mutate(obesity_condition=ifelse(obesity_condition=="Treatment For Diabetes Or Obesity"|
                                    obesity_condition=="Nonspecific Treatment For Obesity"|
                                    obesity_condition=="Treatment For Obesity", "General Obesity", obesity_condition)) 


OBE2P_Demographics_All <- OBE2P_Demographics_All %>% mutate(weight=ifelse(obesity_condition=="Overweight", weight*2,
                                                ifelse(obesity_condition=="General Obesity"|obesity_condition=="Moderate Obesity",weight*1.2,weight)))

OBE2P_Demographics_All %>% group_by(age) %>% summarise(n=sum(weight)) %>% transpose()


OBE2P_Demographics_All %>% filter(age>=10&grepl("Obesi", diagnosis)&obesity_condition!="Overweight") %>% summarise(n=sum(weight)) %>% transpose()

OBE2P_Events <- fread("DANUP Pediatric Analysis Results 1.1/OBE2P Events.txt", integer64 = "character", stringsAsFactors = F)
DANUP_Diagnosis_Codes <- fread("DANUP Pediatric Analysis Results 1.1/DANUP Diagnosis Codes.txt", integer64 = "character", stringsAsFactors = F)

Dx_OBE <- OBE2P_Events %>% inner_join(DANUP_Diagnosis_Codes %>% filter(diagnosis=="Obesity"&type!="Status") %>% select(code)) %>% select(patid) %>% distinct()

OBE2P_Demographics_All %>% inner_join(Dx_OBE) %>% filter(age>=10&grepl("Obesi", diagnosis)&obesity_condition!="Overweight") %>% summarise(n=sum(weight)) %>% transpose()



OBE2P_Demographics_All  %>%
  group_by(age, obesity_condition) %>% summarise(n=sum(weight)) %>%
  spread(key=age, value=n)

DANUP_Measures <- fread("DANUP Pediatric Analysis Results 1.1/DANUP Measures.txt", integer64 = "character", stringsAsFactors = F)
DANUP_Measures <- DANUP_Measures %>% filter(test=="BMI") %>% group_by(patid) %>% filter(value==max(value)) %>% slice(1) %>% select(patid, value) %>% distinct()

DANUP_Measures %>% ungroup %>% inner_join(
  OBE2P_Demographics_All %>%  filter(grepl("Obesi", diagnosis))  %>% select(patid, weight, age)
  ) %>% group_by(age) %>% summarise(n=sum(weight)) %>% transpose()

DANUP_Measures %>% mutate(value=ifelse(value>=25&value<27, "25-27",
                                       ifelse(value>=27&value<30, "27-30",
                                              ifelse(value>=30&value<35,"30-35", 
                                                    ifelse(value<25, "-25", "+35"))))) %>%
  inner_join(
  OBE2P_Demographics_All %>%  filter(grepl("Obesi", diagnosis))  %>% select(patid, weight, age)
  ) %>% group_by(age, value) %>% summarise(n=sum(weight)) %>% spread(key=age, value=n)






DANU_Ingredients <- fread("DANUP Pediatric Analysis Results 1.1/DANU Ingredients.txt", integer64 = "character", stringsAsFactors = F)
DANU_Ingredients <- DANU_Ingredients %>%  separate(drug_id, c('class', 'molecule'))
DANU_Ingredients  <- DANU_Ingredients %>% select(molecule, drug_group)
names(DANU_Ingredients)[1] <- "Drugs"
DANU_Ingredients$Drugs <- as.numeric(DANU_Ingredients$Drugs)
string_OralGLP1        <- paste0("\\b(",paste0(DANU_Ingredients$Drugs[DANU_Ingredients$drug_group == "GLP1 Oral"], collapse = "|"),")\\b")
string_InjectableGLP1  <- paste0("\\b(",paste0(DANU_Ingredients$Drugs[DANU_Ingredients$drug_group == "GLP1 Injectable"], collapse = "|"),")\\b")
string_GLP1  <- paste0("\\b(",paste0(DANU_Ingredients$Drugs[DANU_Ingredients$drug_group == "GLP1 Injectable"|DANU_Ingredients$drug_group == "GLP1 Oral"], collapse = "|"),")\\b")

DIAP_Drug_Histories <- fread("DANUP Pediatric Analysis Results 1.1/DIAP Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
DIAP_Drug_Histories <- DIAP_Drug_Histories %>% select(4:63)
DIAP_Drug_Histories[DIAP_Drug_Histories != "-"] <- 1  # on drug 
DIAP_Drug_Histories[DIAP_Drug_Histories == "-"] <- 0  # no drug
DIAP_Drug_Histories[] <- lapply(DIAP_Drug_Histories,as.numeric)
DIAP_Drug_Histories$SUM <- rowSums(DIAP_Drug_Histories)
DIAP_Drug_Histories_LONG <- fread("DANUP Pediatric Analysis Results 1.1/DIAP Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
Pats_vec <- DIAP_Drug_Histories_LONG %>% select(patient, weight)
DIAP_Drug_Histories <- Pats_vec %>% bind_cols(DIAP_Drug_Histories)
DIAP_Drug_Histories <- DIAP_Drug_Histories %>% filter(SUM != 0)
DIAP_Treatment_exp_Vector <- DIAP_Drug_Histories %>% select(patient, weight)
DIAP_Drug_Histories <- fread("DANUP Pediatric Analysis Results 1.1/DIAP Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
DIAP_Drug_Histories <- DIAP_Treatment_exp_Vector %>% left_join(DIAP_Drug_Histories)
sum(DIAP_Drug_Histories$weight)

OBE2P_Drug_Histories <- fread("DANUP Pediatric Analysis Results 1.1/OBE2P Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
OBE2P_Drug_Histories <- OBE2P_Drug_Histories %>% select(4:63)
OBE2P_Drug_Histories[OBE2P_Drug_Histories != "-"] <- 1  # on drug 
OBE2P_Drug_Histories[OBE2P_Drug_Histories == "-"] <- 0  # no drug
OBE2P_Drug_Histories[] <- lapply(OBE2P_Drug_Histories,as.numeric)
OBE2P_Drug_Histories$SUM <- rowSums(OBE2P_Drug_Histories)
OBE2P_Drug_Histories_LONG <- fread("DANUP Pediatric Analysis Results 1.1/OBE2P Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
Pats_vec <- OBE2P_Drug_Histories_LONG %>% select(patient, weight)
OBE2P_Drug_Histories <- Pats_vec %>% bind_cols(OBE2P_Drug_Histories)
OBE2P_Drug_Histories <- OBE2P_Drug_Histories %>% filter(SUM != 0)
OBE2P_Treatment_exp_Vector <- OBE2P_Drug_Histories %>% select(patient, weight)
OBE2P_Drug_Histories <- fread("DANUP Pediatric Analysis Results 1.1/OBE2P Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
OBE2P_Drug_Histories <- OBE2P_Treatment_exp_Vector %>% left_join(OBE2P_Drug_Histories)
sum(OBE2P_Drug_Histories$weight)

Drug_Histories <-  OBE2P_Drug_Histories %>% select(-disease) %>%
  bind_rows(DIAP_Drug_Histories %>% select(-disease))

Drug_Histories <- gather(Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
Drug_Histories <- Drug_Histories %>% filter(Drugs!="-")
Drug_Histories$Month <- as.character(Drug_Histories$Month)
Drug_Histories$Month <- parse_number(Drug_Histories$Month)

Drug_Histories <- Drug_Histories %>% select(-weight)




OBE2P_Demographics_All %>%   filter(grepl("Obesi", diagnosis))   %>% select(patid, weight, age) %>%
   inner_join(Drug_Histories %>% select(patient), by=c("patid"="patient")) %>% distinct() %>%
   group_by(age) %>% summarise(n=sum(weight)) %>% transpose()

OBE2P_Demographics_All %>% filter(age>=10&grepl("Obesi", diagnosis)&obesity_condition!="Overweight")  %>% select(patid, weight, age) %>%
   inner_join(Drug_Histories %>%  filter(Month>=49) %>% select(patient), by=c("patid"="patient")) %>% distinct() %>%
    summarise(n=sum(weight)) %>% transpose()


OBE2P_Demographics_All %>%    filter(grepl("Obesi", diagnosis))  %>% select(patid, weight, age) %>%
  inner_join(Drug_Histories %>% filter(Month>=49) %>% select(patient), by=c("patid"="patient")) %>% distinct() %>%
  group_by(age) %>% summarise(n=sum(weight)) %>% transpose()

OBE2P_Demographics_All %>%    filter(grepl("Obesi", diagnosis))  %>% select(patid, weight, age) %>%
  inner_join(Drug_Histories %>% filter(grepl(string_GLP1, Drugs)) %>% select(patient), by=c("patid"="patient")) %>% distinct() %>%
  group_by(age) %>% summarise(n=sum(weight)) %>% transpose()
 

OBE2P_Demographics_All %>% filter(age>=10&grepl("Obesi", diagnosis)&obesity_condition!="Overweight")  %>% select(patid, weight, age) %>%
   inner_join(Drug_Histories %>%  filter(grepl(string_OralGLP1, Drugs) & Month>=49) %>% select(patient), by=c("patid"="patient")) %>% distinct() %>%
    summarise(n=sum(weight)) %>% transpose()


OBE2P_Demographics_All %>%  filter(grepl("Obesi", diagnosis))  %>% select(patid, weight, age, obesity_condition) %>%
  inner_join(Drug_Histories %>% filter(grepl(string_GLP1, Drugs) & Month>=49) %>% select(patient), by=c("patid"="patient")) %>% distinct() %>%
  filter(age>10) %>% group_by(obesity_condition) %>% summarise(n=sum(weight)) %>% transpose()


OBE2P_Demographics_All %>%  filter(grepl("Obesi", diagnosis))  %>% select(patid, weight, age) %>%
  inner_join(Drug_Histories %>% filter(grepl(string_InjectableGLP1, Drugs) & Month>=49) %>% select(patient), by=c("patid"="patient")) %>% distinct() %>%
  group_by(age) %>% summarise(n=sum(weight)) %>% transpose()


OBE2P_Demographics_All %>%   filter(grepl("Obesi", diagnosis))  %>% select(patid, weight, age) %>%
  inner_join(Drug_Histories %>% filter(grepl(string_OralGLP1, Drugs) & Month>=49) %>% select(patient), by=c("patid"="patient")) %>% distinct() %>%
  group_by(age) %>% summarise(n=sum(weight)) %>% transpose()


Drug_Histories %>% select(Month) %>% distinct() %>%
  left_join(
data.frame(OBE2P_Demographics_All %>%  filter(grepl("Obesi", diagnosis))   %>% select(patid, weight) %>%
  inner_join(Drug_Histories %>% filter(grepl(string_GLP1, Drugs)) %>% select(patient, Month) %>% distinct(), by=c("patid"="patient")) %>% distinct() %>%
  group_by(Month) %>% summarise(n=sum(weight)))
)



# ----------------------------
# T2D rank vs BMI bucket ---------------------------------------

DANU_Ingredients <- fread("DIA Analysis Results 1.1/DANU Ingredients.txt", integer64 = "character", stringsAsFactors = F)
DANU_Ingredients <- DANU_Ingredients %>%  separate(drug_id, c('class', 'molecule'))
DANU_Ingredients <- DANU_Ingredients %>% select(molecule, drug_group)
DANU_Ingredients$molecule <- as.numeric(DANU_Ingredients$molecule)

DIA_Drug_Histories <- fread("DIA Analysis Results 1.1/DIA Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
Treatment_exp_Vector <- fread("DIA Analysis Results 1.1/Treatment_exp_Vector.txt")
DIA_Drug_Histories <- Treatment_exp_Vector %>% left_join(DIA_Drug_Histories)
DIA_Drug_Histories <- gather(DIA_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
DIA_Drug_Histories <- DIA_Drug_Histories %>% select(patient, weight, Drugs) %>% distinct() %>% filter(Drugs!="-")
DIA_Drug_Histories <- separate_rows(DIA_Drug_Histories, Drugs, sep = ",", convert=T)
DIA_Drug_Histories$Drugs <- as.numeric(DIA_Drug_Histories$Drugs)
DIA_Drug_Histories <- DIA_Drug_Histories %>% select(patient, weight, Drugs) %>% distinct() 
DIA_Drug_Histories <- DIA_Drug_Histories %>% left_join(DANU_Ingredients, by=c("Drugs"="molecule")) %>% select(patient, weight, drug_group) %>% distinct()

Ranks <- DIA_Drug_Histories %>% mutate(drug_group=ifelse(drug_group=="GLP1 Injectable", 1, 
                                                ifelse(drug_group=="Insulin", 2,
                                                       ifelse(drug_group=="GLP1 Oral", 3,
                                                              ifelse(drug_group=="SGLT2", 4,
                                                                     ifelse(drug_group=="DPP4"|drug_group=="Antidiabetc",5,6)))))) %>%
  group_by(patient) %>% filter(drug_group==min(drug_group)) %>% slice(1) %>% ungroup()

sum(Ranks$weight)



DIA_Drug_Histories <- fread("DIA Analysis Results 1.1/DIA Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
Treatment_exp_Vector <- fread("DIA Analysis Results 1.1/Treatment_exp_Vector.txt")
DIA_Drug_Histories <- Treatment_exp_Vector %>% left_join(DIA_Drug_Histories) %>% select(-c(disease))
DIA_Drug_Histories %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(weight))
DIA_Drug_Histories <- gather(DIA_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
DIA_Drug_Histories <- DIA_Drug_Histories %>% filter(Drugs!="-")
DIA_Drug_Histories$Month <- as.character(DIA_Drug_Histories$Month)
DIA_Drug_Histories$Month <- parse_number(DIA_Drug_Histories$Month)
  
DANU_Ingredients       <- fread("DIA Analysis Results 1.1/DANU Ingredients.txt", integer64 = "character", stringsAsFactors = F)
DANU_Ingredients       <- DANU_Ingredients %>%  separate(drug_id, c('class', 'molecule'))
string_Injectables       <- paste0("\\b(",paste0(DANU_Ingredients$molecule[DANU_Ingredients$drug_group == "GLP1 Injectable"], collapse = "|"),")\\b")

DIA_Drug_Histories <- DIA_Drug_Histories %>% mutate(Inj = ifelse( grepl(string_Injectables, Drugs),"I", "O"))

DIA_Drug_Histories_2 <- DIA_Drug_Histories %>% select(patient, weight, Month, Inj) %>%
  filter(Inj=="I") %>% select(patient) %>% distinct() %>%
  left_join(DIA_Drug_Histories %>% select(patient, weight, Month, Inj))

DIA_Drug_Histories_2 <- DIA_Drug_Histories_2 %>% spread(key=Month, value=Inj)
DIA_Drug_Histories_2[is.na(DIA_Drug_Histories_2)] <- "-"
DIA_Drug_Histories_2 <- gather(DIA_Drug_Histories_2, Month, Drugs, `1`:`60`, factor_key=TRUE)
DIA_Drug_Histories_2 <- DIA_Drug_Histories_2 %>% arrange(patient, Month)

DIA_Drug_Histories_3 <- DIA_Drug_Histories_2 %>% group_by(patient, weight) %>% 
  slice(if(any(Drugs=="I")) which.max(Drugs=="I"):which.max(Month=="60") else NA) 

DIA_Drug_Histories_3 <- DIA_Drug_Histories_3 %>% group_by(patient) %>% mutate(grp = rle(Drugs)$lengths %>% {rep(seq(length(.)), .)})
DIA_Drug_Histories_3 <- DIA_Drug_Histories_3 %>% arrange(patient, Month)
DIA_Drug_Histories_3 %>% ungroup() %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(weight)) # 6444437 Inj exp

GLP1_Injectable_Pats <- DIA_Drug_Histories_3  %>% ungroup() %>% select(patient, weight) %>% distinct()

DIA_Drug_Histories_3 %>% ungroup() %>% filter(grp!=1) %>%
   filter(!grepl("I", Drugs)) %>% select(patient, weight, Drugs, grp) %>%
  group_by(patient, weight, grp) %>% count() %>% ungroup() %>%
  filter(n>=6) %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(weight)) 

Stopped_Inj_GLP1 <- DIA_Drug_Histories_3 %>% ungroup() %>% filter(grp!=1) %>%
   filter(!grepl("I", Drugs)) %>% select(patient, weight, Drugs, grp) %>%
  group_by(patient, weight, grp) %>% count() %>% ungroup() %>%
  filter(n>=6) %>% select(patient, weight) %>% distinct()

GLP1_Injectable_Pats <- GLP1_Injectable_Pats %>% anti_join(Stopped_Inj_GLP1)



DIA_Drug_Histories <- fread("DIA Analysis Results 1.1/DIA Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
Treatment_exp_Vector <- fread("DIA Analysis Results 1.1/Treatment_exp_Vector.txt")
DIA_Drug_Histories <- Treatment_exp_Vector %>% left_join(DIA_Drug_Histories) %>% select(-c(disease))
DIA_Drug_Histories %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(weight))
DIA_Drug_Histories <- gather(DIA_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
DIA_Drug_Histories <- DIA_Drug_Histories %>% filter(Drugs!="-")
DIA_Drug_Histories$Month <- as.character(DIA_Drug_Histories$Month)
DIA_Drug_Histories$Month <- parse_number(DIA_Drug_Histories$Month)
  
DANU_Ingredients       <- fread("DIA Analysis Results 1.1/DANU Ingredients.txt", integer64 = "character", stringsAsFactors = F)
DANU_Ingredients       <- DANU_Ingredients %>%  separate(drug_id, c('class', 'molecule'))
string_Injectables       <- paste0("\\b(",paste0(DANU_Ingredients$molecule[DANU_Ingredients$drug_group == "Insulin"], collapse = "|"),")\\b")

DIA_Drug_Histories <- DIA_Drug_Histories %>% mutate(Inj = ifelse( grepl(string_Injectables, Drugs),"I", "O"))

DIA_Drug_Histories_2 <- DIA_Drug_Histories %>% select(patient, weight, Month, Inj) %>%
  filter(Inj=="I") %>% select(patient) %>% distinct() %>%
  left_join(DIA_Drug_Histories %>% select(patient, weight, Month, Inj))

DIA_Drug_Histories_2 <- DIA_Drug_Histories_2 %>% spread(key=Month, value=Inj)
DIA_Drug_Histories_2[is.na(DIA_Drug_Histories_2)] <- "-"
DIA_Drug_Histories_2 <- gather(DIA_Drug_Histories_2, Month, Drugs, `1`:`60`, factor_key=TRUE)
DIA_Drug_Histories_2 <- DIA_Drug_Histories_2 %>% arrange(patient, Month)

DIA_Drug_Histories_3 <- DIA_Drug_Histories_2 %>% group_by(patient, weight) %>% 
  slice(if(any(Drugs=="I")) which.max(Drugs=="I"):which.max(Month=="60") else NA) 

DIA_Drug_Histories_3 <- DIA_Drug_Histories_3 %>% group_by(patient) %>% mutate(grp = rle(Drugs)$lengths %>% {rep(seq(length(.)), .)})
DIA_Drug_Histories_3 <- DIA_Drug_Histories_3 %>% arrange(patient, Month)
DIA_Drug_Histories_3 %>% ungroup() %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(weight)) # 11602149 Insulin exp

Insulin_Pats <- DIA_Drug_Histories_3  %>% ungroup() %>% select(patient, weight) %>% distinct()

DIA_Drug_Histories_3 %>% ungroup() %>% filter(grp!=1) %>%
   filter(!grepl("I", Drugs)) %>% select(patient, weight, Drugs, grp) %>%
  group_by(patient, weight, grp) %>% count() %>% ungroup() %>%
  filter(n>=6) %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(weight)) 

Stopped_Insulin <- DIA_Drug_Histories_3 %>% ungroup() %>% filter(grp!=1) %>%
   filter(!grepl("I", Drugs)) %>% select(patient, weight, Drugs, grp) %>%
  group_by(patient, weight, grp) %>% count() %>% ungroup() %>%
  filter(n>=6) %>% select(patient, weight) %>% distinct()

Insulin_Pats <- Insulin_Pats %>% anti_join(Stopped_Insulin)


Continuous <- GLP1_Injectable_Pats %>% bind_rows(Insulin_Pats) %>% distinct()
Continuous$Continuous <- "Continuous"

Stopped_Inj_GLP1$Stopped_Inj_GLP1 <- "Stopped_Inj_GLP1"

Stopped_Insulin <- Stopped_Insulin %>% anti_join(Stopped_Inj_GLP1)

Stopped_Insulin$Stopped_Insulin <- "Stopped_Insulin"

temp <- Ranks %>% left_join(Continuous) %>% left_join(Stopped_Inj_GLP1) %>% left_join(Stopped_Insulin) 
temp[is.na(temp)] <- "-"

Groups <- temp %>% mutate(group=ifelse(Continuous=="Continuous", "Continuous",
                      ifelse(Stopped_Inj_GLP1=="Stopped_Inj_GLP1", "Interm_GLP1",
                             ifelse(Stopped_Insulin=="Stopped_Insulin", "Interm_Insulin",
                                    ifelse(drug_group==3|drug_group==4|drug_group==5, "AdvancedOral", "Biguanide"))))) %>%
 select(patient, weight, group)

sum(Groups$weight)

Groups %>% group_by(group) %>% summarise(n=sum(weight))

DANU_Measures <- fread("DANU Measures 1.1/DANU Measures.txt",  integer64 = "character", stringsAsFactors = F)
DANU_Measures <- DANU_Measures %>% filter(test=="BMI")
DANU_Measures <- Groups %>% select(patient) %>% inner_join(DANU_Measures, by=c("patient"="patid"))
DANU_Measures <- DANU_Measures %>% select(patient, value) %>% distinct() %>% group_by(patient ) %>% filter(value==max(value)) %>% slice(1)
DANU_Measures <- DANU_Measures %>% mutate(value=ifelse(value<27, "<27", ifelse(value>=27&value<30,"27-30", ifelse(value>=30&value<35, "30-35", ">35"))))
DANU_Measures <- DANU_Measures %>% select(patient, value) 

DIA_Comorbidity_Inventories <- fread("DIA Analysis Results 1.1/DIA Comorbidity Inventories.txt")

CKD <- unique(DIA_Comorbidity_Inventories[grepl("N18",diagnosis), c("patid","weight")])
POS <- unique(DIA_Comorbidity_Inventories[grepl("E28",diagnosis), c("patid","weight")])
PAD <- unique(DIA_Comorbidity_Inventories[grepl("I70",diagnosis)|grepl("I73",diagnosis), c("patid","weight")])
SLEEPAPNEA <- unique(DIA_Comorbidity_Inventories[grepl("G47",diagnosis), c("patid","weight")])
HF <- unique(DIA_Comorbidity_Inventories[grepl("I5",diagnosis), c("patid","weight")])
DISLIPIDEMIA <- unique(DIA_Comorbidity_Inventories[grepl("E78",diagnosis), c("patid","weight")])
HTN <- unique(DIA_Comorbidity_Inventories[grepl("I10",diagnosis), c("patid","weight")])
OA <- unique(DIA_Comorbidity_Inventories[grepl("M15", diagnosis)|grepl("M16", diagnosis)|grepl("M17", diagnosis)|
grepl("M18", diagnosis)| grepl("M19", diagnosis), c("patid","weight")])
NASH <- unique(DIA_Comorbidity_Inventories[grepl("K75",diagnosis), c("patid","weight")])
PREDIABETES <- unique(DIA_Comorbidity_Inventories[grepl("R73",diagnosis), c("patid","weight")])
IHD <- unique(DIA_Comorbidity_Inventories[grepl("I20", diagnosis)|grepl("I21", diagnosis)| grepl("I22", diagnosis)|
grepl("I23", diagnosis)| grepl("I24", diagnosis)| grepl("I25", diagnosis), c("patid","weight")])

Comorb <- Reduce(function (...) { merge(..., all = TRUE) },    list(CKD, POS, PAD, HF, SLEEPAPNEA, DISLIPIDEMIA, HTN, OA, NASH, IHD, PREDIABETES))

Comorb$Comorb <- "Comorb"
Comorb <- Comorb %>% select(-weight)
DANU_Measures <- DANU_Measures %>% left_join(Comorb, by=c("patient"="patid")) %>% mutate(Comorb=ifelse(value=="27-30", Comorb, NA))

DANU_Measures %>% inner_join(Groups) %>%
  group_by(group, value, Comorb) %>% summarise(n=sum(weight)) %>%
  spread(key=value, value=n)

# ----------------
# T2D rank vs BMI bucket v2 ---------------------------------------

DANU_Ingredients <- fread("DIA Analysis Results 1.1/DANU Ingredients.txt", integer64 = "character", stringsAsFactors = F)
DANU_Ingredients <- DANU_Ingredients %>%  separate(drug_id, c('class', 'molecule'))
DANU_Ingredients <- DANU_Ingredients %>% select(molecule, drug_group)
DANU_Ingredients$molecule <- as.numeric(DANU_Ingredients$molecule)

DIA_Drug_Histories <- fread("DIA Analysis Results 1.1/DIA Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
Treatment_exp_Vector <- fread("DIA Analysis Results 1.1/Treatment_exp_Vector.txt")
DIA_Drug_Histories <- Treatment_exp_Vector %>% left_join(DIA_Drug_Histories)
DIA_Drug_Histories <- gather(DIA_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
DIA_Drug_Histories <- DIA_Drug_Histories %>% select(patient, weight, Drugs) %>% distinct() %>% filter(Drugs!="-")
DIA_Drug_Histories <- separate_rows(DIA_Drug_Histories, Drugs, sep = ",", convert=T)
DIA_Drug_Histories$Drugs <- as.numeric(DIA_Drug_Histories$Drugs)
DIA_Drug_Histories <- DIA_Drug_Histories %>% select(patient, weight, Drugs) %>% distinct() 
DIA_Drug_Histories <- DIA_Drug_Histories %>% left_join(DANU_Ingredients, by=c("Drugs"="molecule")) %>% select(patient, weight, drug_group) %>% distinct()

Ranks <- DIA_Drug_Histories %>% mutate(drug_group=ifelse(drug_group=="GLP1 Injectable", 1, 
                                                ifelse(drug_group=="Insulin", 2,
                                                       ifelse(drug_group=="GLP1 Oral", 3,
                                                              ifelse(drug_group=="SGLT2", 4,
                                                                     ifelse(drug_group=="DPP4"|drug_group=="Antidiabetc",5,6)))))) %>%
  group_by(patient) %>% filter(drug_group==min(drug_group)) %>% slice(1) %>% ungroup()

sum(Ranks$weight)



DIA_Drug_Histories <- fread("DIA Analysis Results 1.1/DIA Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
Treatment_exp_Vector <- fread("DIA Analysis Results 1.1/Treatment_exp_Vector.txt")
DIA_Drug_Histories <- Treatment_exp_Vector %>% left_join(DIA_Drug_Histories) %>% select(-c(disease))
DIA_Drug_Histories %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(weight))
DIA_Drug_Histories <- gather(DIA_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
DIA_Drug_Histories <- DIA_Drug_Histories %>% filter(Drugs!="-")
DIA_Drug_Histories$Month <- as.character(DIA_Drug_Histories$Month)
DIA_Drug_Histories$Month <- parse_number(DIA_Drug_Histories$Month)
  
DANU_Ingredients       <- fread("DIA Analysis Results 1.1/DANU Ingredients.txt", integer64 = "character", stringsAsFactors = F)
DANU_Ingredients       <- DANU_Ingredients %>%  separate(drug_id, c('class', 'molecule'))
string_Injectables       <- paste0("\\b(",paste0(DANU_Ingredients$molecule[DANU_Ingredients$drug_group == "GLP1 Injectable"], collapse = "|"),")\\b")

DIA_Drug_Histories <- DIA_Drug_Histories %>% mutate(Inj = ifelse( grepl(string_Injectables, Drugs),"I", "O"))

DIA_Drug_Histories_2 <- DIA_Drug_Histories %>% select(patient, weight, Month, Inj) %>%
  filter(Inj=="I") %>% select(patient) %>% distinct() %>%
  left_join(DIA_Drug_Histories %>% select(patient, weight, Month, Inj))

DIA_Drug_Histories_2 <- DIA_Drug_Histories_2 %>% spread(key=Month, value=Inj)
DIA_Drug_Histories_2[is.na(DIA_Drug_Histories_2)] <- "-"
DIA_Drug_Histories_2 <- gather(DIA_Drug_Histories_2, Month, Drugs, `1`:`60`, factor_key=TRUE)
DIA_Drug_Histories_2 <- DIA_Drug_Histories_2 %>% arrange(patient, Month)

DIA_Drug_Histories_3 <- DIA_Drug_Histories_2 %>% group_by(patient, weight) %>% 
  slice(if(any(Drugs=="I")) which.max(Drugs=="I"):which.max(Month=="60") else NA) 

DIA_Drug_Histories_3 <- DIA_Drug_Histories_3 %>% group_by(patient) %>% mutate(grp = rle(Drugs)$lengths %>% {rep(seq(length(.)), .)})
DIA_Drug_Histories_3 <- DIA_Drug_Histories_3 %>% arrange(patient, Month)
DIA_Drug_Histories_3 %>% ungroup() %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(weight)) # 6444437 Inj exp

GLP1_Injectable_Pats <- DIA_Drug_Histories_3  %>% ungroup() %>% select(patient, weight) %>% distinct()

DIA_Drug_Histories_3 %>% ungroup() %>% filter(grp!=1) %>%
   filter(!grepl("I", Drugs)) %>% select(patient, weight, Drugs, grp) %>%
  group_by(patient, weight, grp) %>% count() %>% ungroup() %>%
  filter(n>=6) %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(weight)) 

Stopped_Inj_GLP1 <- DIA_Drug_Histories_3 %>% ungroup() %>% filter(grp!=1) %>%
   filter(!grepl("I", Drugs)) %>% select(patient, weight, Drugs, grp) %>%
  group_by(patient, weight, grp) %>% count() %>% ungroup() %>%
  filter(n>=6) %>% select(patient, weight) %>% distinct()

GLP1_Injectable_Pats <- GLP1_Injectable_Pats %>% anti_join(Stopped_Inj_GLP1)

sum(GLP1_Injectable_Pats$weight)
sum(Stopped_Inj_GLP1$weight)


DIA_Drug_Histories <- fread("DIA Analysis Results 1.1/DIA Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
Treatment_exp_Vector <- fread("DIA Analysis Results 1.1/Treatment_exp_Vector.txt")
DIA_Drug_Histories <- Treatment_exp_Vector %>% left_join(DIA_Drug_Histories) %>% select(-c(disease))
DIA_Drug_Histories %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(weight))
DIA_Drug_Histories <- gather(DIA_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
DIA_Drug_Histories <- DIA_Drug_Histories %>% filter(Drugs!="-")
DIA_Drug_Histories$Month <- as.character(DIA_Drug_Histories$Month)
DIA_Drug_Histories$Month <- parse_number(DIA_Drug_Histories$Month)
  
DANU_Ingredients       <- fread("DIA Analysis Results 1.1/DANU Ingredients.txt", integer64 = "character", stringsAsFactors = F)
DANU_Ingredients       <- DANU_Ingredients %>%  separate(drug_id, c('class', 'molecule'))
string_Injectables       <- paste0("\\b(",paste0(DANU_Ingredients$molecule[DANU_Ingredients$drug_group == "Insulin"], collapse = "|"),")\\b")

DIA_Drug_Histories <- DIA_Drug_Histories %>% mutate(Inj = ifelse( grepl(string_Injectables, Drugs),"I", "O"))

DIA_Drug_Histories_2 <- DIA_Drug_Histories %>% select(patient, weight, Month, Inj) %>%
  filter(Inj=="I") %>% select(patient) %>% distinct() %>%
  left_join(DIA_Drug_Histories %>% select(patient, weight, Month, Inj))

DIA_Drug_Histories_2 <- DIA_Drug_Histories_2 %>% spread(key=Month, value=Inj)
DIA_Drug_Histories_2[is.na(DIA_Drug_Histories_2)] <- "-"
DIA_Drug_Histories_2 <- gather(DIA_Drug_Histories_2, Month, Drugs, `1`:`60`, factor_key=TRUE)
DIA_Drug_Histories_2 <- DIA_Drug_Histories_2 %>% arrange(patient, Month)

DIA_Drug_Histories_3 <- DIA_Drug_Histories_2 %>% group_by(patient, weight) %>% 
  slice(if(any(Drugs=="I")) which.max(Drugs=="I"):which.max(Month=="60") else NA) 

DIA_Drug_Histories_3 <- DIA_Drug_Histories_3 %>% group_by(patient) %>% mutate(grp = rle(Drugs)$lengths %>% {rep(seq(length(.)), .)})
DIA_Drug_Histories_3 <- DIA_Drug_Histories_3 %>% arrange(patient, Month)
DIA_Drug_Histories_3 %>% ungroup() %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(weight)) # 11602149 Insulin exp

Insulin_Pats <- DIA_Drug_Histories_3  %>% ungroup() %>% select(patient, weight) %>% distinct()

DIA_Drug_Histories_3 %>% ungroup() %>% filter(grp!=1) %>%
   filter(!grepl("I", Drugs)) %>% select(patient, weight, Drugs, grp) %>%
  group_by(patient, weight, grp) %>% count() %>% ungroup() %>%
  filter(n>=6) %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(weight)) 

Stopped_Insulin <- DIA_Drug_Histories_3 %>% ungroup() %>% filter(grp!=1) %>%
   filter(!grepl("I", Drugs)) %>% select(patient, weight, Drugs, grp) %>%
  group_by(patient, weight, grp) %>% count() %>% ungroup() %>%
  filter(n>=6) %>% select(patient, weight) %>% distinct()

Insulin_Pats <- Insulin_Pats %>% anti_join(Stopped_Insulin)



Continuous <- GLP1_Injectable_Pats %>% bind_rows(Insulin_Pats) %>% distinct()
Continuous$Continuous <- "Continuous"

GLP1_Injectable_Pats$GLP1_Injectable_Pats <- "GLP1_Injectable_Pats"
Insulin_Pats$Insulin_Pats <- "Insulin_Pats"


Stopped_Inj_GLP1$Stopped_Inj_GLP1 <- "Stopped_Inj_GLP1"

Stopped_Insulin <- Stopped_Insulin %>% anti_join(Stopped_Inj_GLP1)

Stopped_Insulin$Stopped_Insulin <- "Stopped_Insulin"

temp <- Ranks %>% left_join(GLP1_Injectable_Pats) %>% left_join(Insulin_Pats) %>%  left_join(Stopped_Inj_GLP1) %>% left_join(Stopped_Insulin) 
temp[is.na(temp)] <- "-"

Groups <- temp %>% mutate(group=ifelse(GLP1_Injectable_Pats=="GLP1_Injectable_Pats", "GLP1_Injectable_Pats",
                      ifelse(Stopped_Inj_GLP1=="Stopped_Inj_GLP1", "Stopped_Inj_GLP1",
                             ifelse(Insulin_Pats=="Insulin_Pats", "Insulin_Pats",
                                    ifelse(Stopped_Insulin=="Stopped_Insulin" , "Stopped_Insulin", 
                                    ifelse(drug_group==3|drug_group==4|drug_group==5, "AdvancedOral", "Biguanide")))))) %>%
 select(patient, weight, group)

sum(Groups$weight)

Groups %>% group_by(group) %>% summarise(n=sum(weight))

DANU_Measures <- fread("DANU Measures 1.1/DANU Measures.txt",  integer64 = "character", stringsAsFactors = F)
DANU_Measures <- DANU_Measures %>% filter(test=="BMI")
DANU_Measures <- Groups %>% select(patient) %>% inner_join(DANU_Measures, by=c("patient"="patid"))
DANU_Measures <- DANU_Measures %>% select(patient, value) %>% distinct() %>% group_by(patient ) %>% filter(value==max(value)) %>% slice(1)
DANU_Measures <- DANU_Measures %>% mutate(value=ifelse(value<27, "<27", ifelse(value>=27&value<30,"27-30", ifelse(value>=30&value<35, "30-35", ">35"))))
DANU_Measures <- DANU_Measures %>% select(patient, value) 

DIA_Comorbidity_Inventories <- fread("DIA Analysis Results 1.1/DIA Comorbidity Inventories.txt")

CKD <- unique(DIA_Comorbidity_Inventories[grepl("N18",diagnosis), c("patid","weight")])
POS <- unique(DIA_Comorbidity_Inventories[grepl("E28",diagnosis), c("patid","weight")])
PAD <- unique(DIA_Comorbidity_Inventories[grepl("I70",diagnosis)|grepl("I73",diagnosis), c("patid","weight")])
SLEEPAPNEA <- unique(DIA_Comorbidity_Inventories[grepl("G47",diagnosis), c("patid","weight")])
HF <- unique(DIA_Comorbidity_Inventories[grepl("I5",diagnosis), c("patid","weight")])
DISLIPIDEMIA <- unique(DIA_Comorbidity_Inventories[grepl("E78",diagnosis), c("patid","weight")])
HTN <- unique(DIA_Comorbidity_Inventories[grepl("I10",diagnosis), c("patid","weight")])
OA <- unique(DIA_Comorbidity_Inventories[grepl("M15", diagnosis)|grepl("M16", diagnosis)|grepl("M17", diagnosis)|
grepl("M18", diagnosis)| grepl("M19", diagnosis), c("patid","weight")])
NASH <- unique(DIA_Comorbidity_Inventories[grepl("K75",diagnosis), c("patid","weight")])
PREDIABETES <- unique(DIA_Comorbidity_Inventories[grepl("R73",diagnosis), c("patid","weight")])
IHD <- unique(DIA_Comorbidity_Inventories[grepl("I20", diagnosis)|grepl("I21", diagnosis)| grepl("I22", diagnosis)|
grepl("I23", diagnosis)| grepl("I24", diagnosis)| grepl("I25", diagnosis), c("patid","weight")])

Comorb <- Reduce(function (...) { merge(..., all = TRUE) },    list(CKD, POS, PAD, HF, SLEEPAPNEA, DISLIPIDEMIA, HTN, OA, NASH, IHD, PREDIABETES))

Comorb$Comorb <- "Comorb"
Comorb <- Comorb %>% select(-weight)
DANU_Measures <- DANU_Measures %>% left_join(Comorb, by=c("patient"="patid")) %>% mutate(Comorb=ifelse(value=="27-30", Comorb, NA))




DIA_Drug_Histories <- fread("DIA Analysis Results 1.1/DIA Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
Treatment_exp_Vector <- fread("DIA Analysis Results 1.1/Treatment_exp_Vector.txt")
DIA_Drug_Histories <- Treatment_exp_Vector %>% left_join(DIA_Drug_Histories) %>% select(-c(disease))
DIA_Drug_Histories <- gather(DIA_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
DIA_Drug_Histories <- DIA_Drug_Histories %>% filter(Drugs!="-")
DIA_Drug_Histories$Month <- as.character(DIA_Drug_Histories$Month)
DIA_Drug_Histories$Month <- parse_number(DIA_Drug_Histories$Month)
DIA_Drug_Histories <- DIA_Drug_Histories %>% group_by(patient) %>% filter(Month==min(Month))
New_Rx <- DIA_Drug_Histories %>% filter(Month>=49) %>% select(patient) %>% distinct()
New_Rx$New <- "New"
New_Rx <- New_Rx %>% ungroup()

Treatment_exp_Vector <- Treatment_exp_Vector %>% left_join(New_Rx) %>% mutate(New=ifelse(is.na(New), "Old", New))
Treatment_exp_Vector <- Treatment_exp_Vector %>% inner_join(Groups %>% filter(group=="Biguanide")) %>% select(patient, New)


Groups <- Groups %>% left_join(Treatment_exp_Vector) %>% mutate(group=ifelse(group=="Biguanide"&New=="New", "New", group)) %>% select(-New) %>% drop_na()

Groups %>% group_by(group) %>% summarise(n=sum(weight))

DANU_Measures %>% inner_join(Groups) %>% 
  group_by(group, value) %>% summarise(n=sum(weight)) %>%
  spread(key=value, value=n)

# ------------------------

# T2D rank vs BMI bucket v3 ---------------------------------------

DANU_Ingredients <- fread("DIA Analysis Results 1.1/DANU Ingredients.txt", integer64 = "character", stringsAsFactors = F)
DANU_Ingredients <- DANU_Ingredients %>%  separate(drug_id, c('class', 'molecule'))
DANU_Ingredients <- DANU_Ingredients %>% select(molecule, drug_group)
DANU_Ingredients$molecule <- as.numeric(DANU_Ingredients$molecule)

DIA_Drug_Histories <- fread("DIA Analysis Results 1.1/DIA Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
Treatment_exp_Vector <- fread("DIA Analysis Results 1.1/Treatment_exp_Vector.txt")
DIA_Drug_Histories <- Treatment_exp_Vector %>% left_join(DIA_Drug_Histories)
DIA_Drug_Histories <- gather(DIA_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
DIA_Drug_Histories <- DIA_Drug_Histories %>% select(patient, weight, Drugs) %>% distinct() %>% filter(Drugs!="-")
DIA_Drug_Histories <- separate_rows(DIA_Drug_Histories, Drugs, sep = ",", convert=T)
DIA_Drug_Histories$Drugs <- as.numeric(DIA_Drug_Histories$Drugs)
DIA_Drug_Histories <- DIA_Drug_Histories %>% select(patient, weight, Drugs) %>% distinct() 
DIA_Drug_Histories <- DIA_Drug_Histories %>% left_join(DANU_Ingredients, by=c("Drugs"="molecule")) %>% select(patient, weight, drug_group) %>% distinct()

Ranks <- DIA_Drug_Histories %>% mutate(drug_group=ifelse(drug_group=="GLP1 Injectable", 1, 
                                                ifelse(drug_group=="Insulin", 2,
                                                       ifelse(drug_group=="GLP1 Oral", 3,
                                                              ifelse(drug_group=="SGLT2", 4,
                                                                     ifelse(drug_group=="DPP4"|drug_group=="Antidiabetc",5,6)))))) %>%
  group_by(patient) %>% filter(drug_group==min(drug_group)) %>% slice(1) %>% ungroup()

sum(Ranks$weight)



DIA_Drug_Histories <- fread("DIA Analysis Results 1.1/DIA Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
Treatment_exp_Vector <- fread("DIA Analysis Results 1.1/Treatment_exp_Vector.txt")
DIA_Drug_Histories <- Treatment_exp_Vector %>% left_join(DIA_Drug_Histories) %>% select(-c(disease))
DIA_Drug_Histories %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(weight))
DIA_Drug_Histories <- gather(DIA_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
DIA_Drug_Histories <- DIA_Drug_Histories %>% filter(Drugs!="-")
DIA_Drug_Histories$Month <- as.character(DIA_Drug_Histories$Month)
DIA_Drug_Histories$Month <- parse_number(DIA_Drug_Histories$Month)
  
DANU_Ingredients       <- fread("DIA Analysis Results 1.1/DANU Ingredients.txt", integer64 = "character", stringsAsFactors = F)
DANU_Ingredients       <- DANU_Ingredients %>%  separate(drug_id, c('class', 'molecule'))
string_Injectables       <- paste0("\\b(",paste0(DANU_Ingredients$molecule[DANU_Ingredients$drug_group == "GLP1 Injectable"], collapse = "|"),")\\b")

DIA_Drug_Histories <- DIA_Drug_Histories %>% mutate(Inj = ifelse( grepl(string_Injectables, Drugs),"I", "O"))

DIA_Drug_Histories_2 <- DIA_Drug_Histories %>% select(patient, weight, Month, Inj) %>%
  filter(Inj=="I") %>% select(patient) %>% distinct() %>%
  left_join(DIA_Drug_Histories %>% select(patient, weight, Month, Inj))

DIA_Drug_Histories_2 <- DIA_Drug_Histories_2 %>% spread(key=Month, value=Inj)
DIA_Drug_Histories_2[is.na(DIA_Drug_Histories_2)] <- "-"
DIA_Drug_Histories_2 <- gather(DIA_Drug_Histories_2, Month, Drugs, `1`:`60`, factor_key=TRUE)
DIA_Drug_Histories_2 <- DIA_Drug_Histories_2 %>% arrange(patient, Month)

DIA_Drug_Histories_3 <- DIA_Drug_Histories_2 %>% group_by(patient, weight) %>% 
  slice(if(any(Drugs=="I")) which.max(Drugs=="I"):which.max(Month=="60") else NA) 

DIA_Drug_Histories_3 <- DIA_Drug_Histories_3 %>% group_by(patient) %>% mutate(grp = rle(Drugs)$lengths %>% {rep(seq(length(.)), .)})
DIA_Drug_Histories_3 <- DIA_Drug_Histories_3 %>% arrange(patient, Month)
DIA_Drug_Histories_3 %>% ungroup() %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(weight)) # 6444437 Inj exp

GLP1_Injectable_Pats <- DIA_Drug_Histories_3  %>% ungroup() %>% select(patient, weight) %>% distinct()

DIA_Drug_Histories_3 %>% ungroup() %>% filter(grp!=1) %>%
   filter(!grepl("I", Drugs)) %>% select(patient, weight, Drugs, grp) %>%
  group_by(patient, weight, grp) %>% count() %>% ungroup() %>%
  filter(n>=6) %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(weight)) 

Stopped_Inj_GLP1 <- DIA_Drug_Histories_3 %>% ungroup() %>% filter(grp!=1) %>%
   filter(!grepl("I", Drugs)) %>% select(patient, weight, Drugs, grp) %>%
  group_by(patient, weight, grp) %>% count() %>% ungroup() %>%
  filter(n>=6) %>% select(patient, weight) %>% distinct()

GLP1_Injectable_Pats <- GLP1_Injectable_Pats %>% anti_join(Stopped_Inj_GLP1)

sum(GLP1_Injectable_Pats$weight)
sum(Stopped_Inj_GLP1$weight)


DIA_Drug_Histories <- fread("DIA Analysis Results 1.1/DIA Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
Treatment_exp_Vector <- fread("DIA Analysis Results 1.1/Treatment_exp_Vector.txt")
DIA_Drug_Histories <- Treatment_exp_Vector %>% left_join(DIA_Drug_Histories) %>% select(-c(disease))
DIA_Drug_Histories %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(weight))
DIA_Drug_Histories <- gather(DIA_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
DIA_Drug_Histories <- DIA_Drug_Histories %>% filter(Drugs!="-")
DIA_Drug_Histories$Month <- as.character(DIA_Drug_Histories$Month)
DIA_Drug_Histories$Month <- parse_number(DIA_Drug_Histories$Month)
  
DANU_Ingredients       <- fread("DIA Analysis Results 1.1/DANU Ingredients.txt", integer64 = "character", stringsAsFactors = F)
DANU_Ingredients       <- DANU_Ingredients %>%  separate(drug_id, c('class', 'molecule'))
string_Injectables       <- paste0("\\b(",paste0(DANU_Ingredients$molecule[DANU_Ingredients$drug_group == "Insulin"], collapse = "|"),")\\b")

DIA_Drug_Histories <- DIA_Drug_Histories %>% mutate(Inj = ifelse( grepl(string_Injectables, Drugs),"I", "O"))

DIA_Drug_Histories_2 <- DIA_Drug_Histories %>% select(patient, weight, Month, Inj) %>%
  filter(Inj=="I") %>% select(patient) %>% distinct() %>%
  left_join(DIA_Drug_Histories %>% select(patient, weight, Month, Inj))

DIA_Drug_Histories_2 <- DIA_Drug_Histories_2 %>% spread(key=Month, value=Inj)
DIA_Drug_Histories_2[is.na(DIA_Drug_Histories_2)] <- "-"
DIA_Drug_Histories_2 <- gather(DIA_Drug_Histories_2, Month, Drugs, `1`:`60`, factor_key=TRUE)
DIA_Drug_Histories_2 <- DIA_Drug_Histories_2 %>% arrange(patient, Month)

DIA_Drug_Histories_3 <- DIA_Drug_Histories_2 %>% group_by(patient, weight) %>% 
  slice(if(any(Drugs=="I")) which.max(Drugs=="I"):which.max(Month=="60") else NA) 

DIA_Drug_Histories_3 <- DIA_Drug_Histories_3 %>% group_by(patient) %>% mutate(grp = rle(Drugs)$lengths %>% {rep(seq(length(.)), .)})
DIA_Drug_Histories_3 <- DIA_Drug_Histories_3 %>% arrange(patient, Month)
DIA_Drug_Histories_3 %>% ungroup() %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(weight)) # 11602149 Insulin exp

Insulin_Pats <- DIA_Drug_Histories_3  %>% ungroup() %>% select(patient, weight) %>% distinct()

DIA_Drug_Histories_3 %>% ungroup() %>% filter(grp!=1) %>%
   filter(!grepl("I", Drugs)) %>% select(patient, weight, Drugs, grp) %>%
  group_by(patient, weight, grp) %>% count() %>% ungroup() %>%
  filter(n>=6) %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(weight)) 

Stopped_Insulin <- DIA_Drug_Histories_3 %>% ungroup() %>% filter(grp!=1) %>%
   filter(!grepl("I", Drugs)) %>% select(patient, weight, Drugs, grp) %>%
  group_by(patient, weight, grp) %>% count() %>% ungroup() %>%
  filter(n>=6) %>% select(patient, weight) %>% distinct()

Insulin_Pats <- Insulin_Pats %>% anti_join(Stopped_Insulin)


GLP1_Injectable_Pats$GLP1_Injectable_Pats <- "GLP1_Injectable_Pats"
Insulin_Pats$Insulin_Pats <- "Insulin_Pats"
Stopped_Inj_GLP1$Stopped_Inj_GLP1 <- "Stopped_Inj_GLP1"
Stopped_Insulin$Stopped_Insulin <- "Stopped_Insulin"


Stopped_Insulin <- Stopped_Insulin %>% anti_join(Stopped_Inj_GLP1) %>% anti_join(GLP1_Injectable_Pats)
Insulin_Pats <- Insulin_Pats %>% anti_join(Stopped_Inj_GLP1) %>% anti_join(GLP1_Injectable_Pats)


temp <- Ranks %>% left_join(GLP1_Injectable_Pats) %>% left_join(Insulin_Pats) %>%  left_join(Stopped_Inj_GLP1) %>% left_join(Stopped_Insulin) 
temp[is.na(temp)] <- "-"

Groups <- temp %>% mutate(group=ifelse(GLP1_Injectable_Pats=="GLP1_Injectable_Pats", "GLP1_Injectable_Pats",
                      ifelse(Stopped_Inj_GLP1=="Stopped_Inj_GLP1", "Stopped_Inj_GLP1",
                             ifelse(Insulin_Pats=="Insulin_Pats", "Insulin_Pats",
                                    ifelse(Stopped_Insulin=="Stopped_Insulin" , "Stopped_Insulin", 
                                    ifelse(drug_group==3|drug_group==4|drug_group==5, "AdvancedOral", "Biguanide")))))) %>%
 select(patient, weight, group)

sum(Groups$weight)

Groups %>% group_by(group) %>% summarise(n=sum(weight))

DANU_Measures <- fread("DANU Measures 1.1/DANU Measures.txt",  integer64 = "character", stringsAsFactors = F)
DANU_Measures <- DANU_Measures %>% filter(test=="BMI"|test=="HbA1c Level")
DANU_Measures <- Groups %>% select(patient) %>% inner_join(DANU_Measures, by=c("patient"="patid"))
DANU_Measures <- DANU_Measures %>% select(patient,test, value) %>% distinct() %>% group_by(patient, test ) %>% filter(value==max(value)) %>% slice(1)
DANU_Measures <- DANU_Measures %>% mutate(value=ifelse(test=="BMI"&value>=30, "BMI>30",
                                                       ifelse(test=="BMI", "BMI<30",
                                                              ifelse(test=="HbA1c Level"&value<7.5, "HbA1c<7.5",
                                                                     ifelse(test=="HbA1c Level"&value<9, "HbA1c<9", "HbA1c>9")))))
DANU_Measures <- DANU_Measures %>% select(patient, value, test, value) %>% spread(key=test, value=value) %>% drop_na()





DIA_Drug_Histories <- fread("DIA Analysis Results 1.1/DIA Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
Treatment_exp_Vector <- fread("DIA Analysis Results 1.1/Treatment_exp_Vector.txt")
DIA_Drug_Histories <- Treatment_exp_Vector %>% left_join(DIA_Drug_Histories) %>% select(-c(disease))
DIA_Drug_Histories <- gather(DIA_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
DIA_Drug_Histories <- DIA_Drug_Histories %>% filter(Drugs!="-")
DIA_Drug_Histories$Month <- as.character(DIA_Drug_Histories$Month)
DIA_Drug_Histories$Month <- parse_number(DIA_Drug_Histories$Month)
DIA_Drug_Histories <- DIA_Drug_Histories %>% group_by(patient) %>% filter(Month==min(Month))
New_Rx <- DIA_Drug_Histories %>% filter(Month>=49) %>% select(patient) %>% distinct()
New_Rx$New <- "New"
New_Rx <- New_Rx %>% ungroup()

Treatment_exp_Vector <- Treatment_exp_Vector %>% left_join(New_Rx) %>% mutate(New=ifelse(is.na(New), "Old", New))
Treatment_exp_Vector <- Treatment_exp_Vector %>% inner_join(Groups %>% filter(group=="Biguanide")) %>% select(patient, New)


Groups <- Groups %>% left_join(Treatment_exp_Vector) %>% mutate(group=ifelse(group=="Biguanide"&New=="New", "New", group)) %>% select(-New) %>% drop_na()

Groups %>% group_by(group) %>% summarise(n=sum(weight))

DANU_Measures %>% inner_join(Groups) %>% 
  group_by(group,     `HbA1c Level` , BMI) %>% summarise(n=sum(weight)) %>%
  spread(key=`HbA1c Level`, value=n)

# ----------------------

# Obesity ASCVD HFpEF CKD OSLAP -----------------------------

# Targets

# HFpEF 2,213,511
# CKD 18,108,312
# PAD 16,451,350
# PCOS 4,153,631
# OSA 29,665,040

Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 <- fread("Mkt_Comorbidity_Groups_OBE_adjusted_Jun22.txt", sep="\t")
Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 <- Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 %>% filter(diagnosis=="Obesity")

sum(Mkt_Comorbidity_Groups_OBE_adjusted_Jun22$weight)

sum(Mkt_Comorbidity_Groups_OBE_adjusted_Jun22$weight[Mkt_Comorbidity_Groups_OBE_adjusted_Jun22$HFpEF==1]) # 597764.9
sum(Mkt_Comorbidity_Groups_OBE_adjusted_Jun22$weight[Mkt_Comorbidity_Groups_OBE_adjusted_Jun22$CKD==1]) # 4561006
sum(Mkt_Comorbidity_Groups_OBE_adjusted_Jun22$weight[Mkt_Comorbidity_Groups_OBE_adjusted_Jun22$OSLAP==1]) # 11818708

Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 %>% filter(HFpEF==1|CKD==1|OSLAP==1) %>% select(patid, weight) %>% summarise(n=sum(weight))
Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 %>% filter(CKD==1|OSLAP==1) %>% select(patid, weight) %>% summarise(n=sum(weight))


Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 %>% filter(HFpEF==1|CKD==1|OSLAP==1) %>% 
  group_by(HFpEF, CKD, OSLAP) %>% summarise(n=sum(weight))



OBE2_Comorbidity_Inventories <- fread("DIA Analysis Results 1.1/OBE2 Comorbidity Inventories.txt")
OBE2_Comorbidity_Inventories <- OBE2_Comorbidity_Inventories %>% select(patid, diagnosis)

OBE_ASCVD <- OBE2_Comorbidity_Inventories %>% filter(grepl("G45", diagnosis)|
                                         grepl("I63", diagnosis)|
                                         grepl("H34", diagnosis)|
                                         grepl("I65", diagnosis)|
                                         grepl("I66", diagnosis)|
                                         grepl("I69", diagnosis)|
                                         grepl("I20", diagnosis)|
                                         grepl("I21", diagnosis)|
                                         grepl("I22", diagnosis)|
                                         grepl("I23", diagnosis)|
                                         grepl("I24", diagnosis)|
                                         grepl("I25", diagnosis)|
                                         grepl("I70", diagnosis)|
                                         grepl("I73", diagnosis)) %>% select(patid) %>% distinct()

OBE_ASCVD$OBE_ASCVD <- "OBE_ASCVD"

Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 %>% left_join(OBE_ASCVD) %>% group_by(OBE_ASCVD) %>% summarise(n=sum(weight))


Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 %>% left_join(OBE_ASCVD) %>% filter(OBE_ASCVD=="OBE_ASCVD") %>%
  filter(HFpEF==1) %>% summarise(n=sum(weight)) # 460027.1
Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 %>% left_join(OBE_ASCVD) %>% filter(OBE_ASCVD=="OBE_ASCVD") %>%
  filter(CKD==1) %>% summarise(n=sum(weight)) # 2475727
Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 %>% left_join(OBE_ASCVD) %>% filter(OBE_ASCVD=="OBE_ASCVD") %>%
  filter(OSLAP==1) %>% summarise(n=sum(weight)) # 3674358

Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 %>% left_join(OBE_ASCVD) %>% filter(OBE_ASCVD=="OBE_ASCVD") %>%
  filter(CKD==1|OSLAP==1) %>% summarise(n=sum(weight)) # 5671542



Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 %>% left_join(OBE_ASCVD) %>% filter(OBE_ASCVD=="OBE_ASCVD") %>% 
  filter(HFpEF==1|CKD==1|OSLAP==1) %>% 
  group_by(HFpEF, CKD, OSLAP) %>% summarise(n=sum(weight))


# ------------------------
# Obesity ASCVD (w DIA) HFpEF CKD OSLAP -----------------------------

# Targets

# HFpEF 2,213,511
# CKD 18,108,312
# PAD 16,451,350
# PCOS 4,153,631
# OSA 29,665,040

Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 <- fread("Mkt_Comorbidity_Groups_OBE_adjusted_Jun22.txt", sep="\t")
Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 <- Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 %>% filter(diagnosis=="Obesity"|diagnosis=="Diabetes + Obesity")

sum(Mkt_Comorbidity_Groups_OBE_adjusted_Jun22$weight) # 121764029

sum(Mkt_Comorbidity_Groups_OBE_adjusted_Jun22$weight[Mkt_Comorbidity_Groups_OBE_adjusted_Jun22$HFpEF==1]) # 1185670
sum(Mkt_Comorbidity_Groups_OBE_adjusted_Jun22$weight[Mkt_Comorbidity_Groups_OBE_adjusted_Jun22$CKD==1]) # 9955955
sum(Mkt_Comorbidity_Groups_OBE_adjusted_Jun22$weight[Mkt_Comorbidity_Groups_OBE_adjusted_Jun22$OSLAP==1]) # 19631625

Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 %>% filter(HFpEF==1|CKD==1|OSLAP==1) %>% select(patid, weight) %>% summarise(n=sum(weight))
Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 %>% filter(HFpEF==1|CKD==1) %>% select(patid, weight) %>% summarise(n=sum(weight))


Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 %>% filter(HFpEF==1|CKD==1|OSLAP==1) %>% 
  group_by(HFpEF, CKD, OSLAP) %>% summarise(n=sum(weight))



OBE2_Comorbidity_Inventories <- fread("DIA Analysis Results 1.1/OBE2 Comorbidity Inventories.txt")
OBE2_Comorbidity_Inventories <- OBE2_Comorbidity_Inventories %>% select(patid, diagnosis)

OBE_ASCVD <- OBE2_Comorbidity_Inventories %>% filter(grepl("G45", diagnosis)|
                                         grepl("I63", diagnosis)|
                                         grepl("H34", diagnosis)|
                                         grepl("I65", diagnosis)|
                                         grepl("I66", diagnosis)|
                                         grepl("I69", diagnosis)|
                                         grepl("I20", diagnosis)|
                                         grepl("I21", diagnosis)|
                                         grepl("I22", diagnosis)|
                                         grepl("I23", diagnosis)|
                                         grepl("I24", diagnosis)|
                                         grepl("I25", diagnosis)|
                                         grepl("I70", diagnosis)|
                                         grepl("I73", diagnosis)) %>% select(patid) %>% distinct()

OBE_ASCVD$OBE_ASCVD <- "OBE_ASCVD"


DIA_Comorbidity_Inventories <- fread("DIA Analysis Results 1.1/DIA Comorbidity Inventories.txt")
DIA_Comorbidity_Inventories <- DIA_Comorbidity_Inventories %>% select(patid, diagnosis)

DIA_ASCVD <- DIA_Comorbidity_Inventories %>% filter(grepl("G45", diagnosis)|
                                         grepl("I63", diagnosis)|
                                         grepl("H34", diagnosis)|
                                         grepl("I65", diagnosis)|
                                         grepl("I66", diagnosis)|
                                         grepl("I69", diagnosis)|
                                         grepl("I20", diagnosis)|
                                         grepl("I21", diagnosis)|
                                         grepl("I22", diagnosis)|
                                         grepl("I23", diagnosis)|
                                         grepl("I24", diagnosis)|
                                         grepl("I25", diagnosis)|
                                         grepl("I70", diagnosis)|
                                         grepl("I73", diagnosis)) %>% select(patid) %>% distinct()

DIA_ASCVD$DIA_ASCVD <- "DIA_ASCVD"


ASCVD <- DIA_ASCVD %>% select(patid) %>% bind_rows(OBE_ASCVD) %>% distinct() %>% mutate(ASCVD="ASCVD") # 26155656


Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 %>% left_join(ASCVD) %>% group_by(ASCVD) %>% summarise(n=sum(weight))


Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 %>% left_join(ASCVD) %>% filter(ASCVD=="ASCVD") %>%
  filter(HFpEF==1) %>% summarise(n=sum(weight)) # 460027.1
Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 %>% left_join(ASCVD) %>% filter(ASCVD=="ASCVD") %>%
  filter(CKD==1) %>% summarise(n=sum(weight)) # 2475727
Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 %>% left_join(ASCVD) %>% filter(ASCVD=="ASCVD") %>%
  filter(OSLAP==1) %>% summarise(n=sum(weight)) # 3674358

Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 %>% left_join(ASCVD) %>% filter(ASCVD=="ASCVD") %>%
  filter(CKD|OSLAP==1) %>% summarise(n=sum(weight)) # 5671542



Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 %>% left_join(ASCVD) %>% filter(ASCVD=="ASCVD") %>% 
  filter(HFpEF==1|CKD==1|OSLAP==1) %>% 
  group_by(HFpEF, CKD, OSLAP) %>% summarise(n=sum(weight))


# --------------------------
# Obesity ASCVD HFpEF CKD OSLAP  > 65 vs <65 -----------------------------

# Targets

# HFpEF 2,213,511
# CKD 18,108,312
# PAD 16,451,350
# PCOS 4,153,631
# OSA 29,665,040




Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 <- fread("Mkt_Comorbidity_Groups_OBE_adjusted_Jun22.txt", sep="\t")
Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 <- Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 %>% filter(diagnosis=="Obesity")

sum(Mkt_Comorbidity_Groups_OBE_adjusted_Jun22$weight)

sum(Mkt_Comorbidity_Groups_OBE_adjusted_Jun22$weight[Mkt_Comorbidity_Groups_OBE_adjusted_Jun22$HFpEF==1]) # 597764.9
sum(Mkt_Comorbidity_Groups_OBE_adjusted_Jun22$weight[Mkt_Comorbidity_Groups_OBE_adjusted_Jun22$CKD==1]) # 4561006
sum(Mkt_Comorbidity_Groups_OBE_adjusted_Jun22$weight[Mkt_Comorbidity_Groups_OBE_adjusted_Jun22$OSLAP==1]) # 11818708

Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 %>% filter(HFpEF==1|CKD==1|OSLAP==1) %>% select(patid, weight) %>% summarise(n=sum(weight))
Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 %>% filter(CKD==1|OSLAP==1) %>% select(patid, weight) %>% summarise(n=sum(weight))


Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 %>% filter(HFpEF==1|CKD==1|OSLAP==1) %>% 
  group_by(HFpEF, CKD, OSLAP) %>% summarise(n=sum(weight))



DANU_Demographics <- fread("DANU Demographics 1.1/DANU Demographics.txt")
DANU_Demographics <- DANU_Demographics %>% filter(diagnosis=="Obesity") %>% select(patid, age)

OBE2_Comorbidity_Inventories <- fread("DIA Analysis Results 1.1/OBE2 Comorbidity Inventories.txt")
OBE2_Comorbidity_Inventories <- OBE2_Comorbidity_Inventories %>% select(patid, diagnosis)

OBE_ASCVD <- OBE2_Comorbidity_Inventories %>% filter(grepl("G45", diagnosis)|
                                         grepl("I63", diagnosis)|
                                         grepl("H34", diagnosis)|
                                         grepl("I65", diagnosis)|
                                         grepl("I66", diagnosis)|
                                         grepl("I69", diagnosis)|
                                         grepl("I20", diagnosis)|
                                         grepl("I21", diagnosis)|
                                         grepl("I22", diagnosis)|
                                         grepl("I23", diagnosis)|
                                         grepl("I24", diagnosis)|
                                         grepl("I25", diagnosis)|
                                         grepl("I70", diagnosis)|
                                         grepl("I73", diagnosis)) %>% select(patid) %>% distinct()

OBE_ASCVD$OBE_ASCVD <- "OBE_ASCVD"

Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 %>% left_join(OBE_ASCVD) %>% group_by(OBE_ASCVD) %>% summarise(n=sum(weight))


Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 %>% left_join(OBE_ASCVD) %>% filter(OBE_ASCVD=="OBE_ASCVD") %>%
  filter(HFpEF==1) %>% summarise(n=sum(weight)) # 460027.1
Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 %>% left_join(OBE_ASCVD) %>% filter(OBE_ASCVD=="OBE_ASCVD") %>%
  filter(CKD==1) %>% summarise(n=sum(weight)) # 2475727
Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 %>% left_join(OBE_ASCVD) %>% filter(OBE_ASCVD=="OBE_ASCVD") %>%
  filter(OSLAP==1) %>% summarise(n=sum(weight)) # 3674358

Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 %>% left_join(OBE_ASCVD) %>% filter(OBE_ASCVD=="OBE_ASCVD") %>%
  filter(CKD==1|OSLAP==1) %>% summarise(n=sum(weight)) # 5671542




Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 %>% left_join(OBE_ASCVD) %>% left_join(DANU_Demographics) %>%
  filter(age>=65) %>%
  filter(HFpEF==0&CKD==0&OSLAP==0&OBE_ASCVD=="OBE_ASCVD")  %>% summarise(n=sum(weight))


Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 %>% left_join(OBE_ASCVD) %>% filter(OBE_ASCVD=="OBE_ASCVD") %>% 
  filter(HFpEF==1|CKD==1|OSLAP==1) %>% 
  group_by(HFpEF, CKD, OSLAP) %>% summarise(n=sum(weight))


# ----------------------
# Obesity ASCVD (w DIA)  HFpEF CKD OSLAP  > 65 vs <65 -----------------------------
 
# Targets

# HFpEF 2,213,511
# CKD 18,108,312
# PAD 16,451,350
# PCOS 4,153,631
# OSA 29,665,040




Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 <- fread("Mkt_Comorbidity_Groups_OBE_adjusted_Jun22.txt", sep="\t")
Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 <- Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 %>% filter(diagnosis=="Obesity"|diagnosis=="Diabetes + Obesity")

sum(Mkt_Comorbidity_Groups_OBE_adjusted_Jun22$weight)

sum(Mkt_Comorbidity_Groups_OBE_adjusted_Jun22$weight[Mkt_Comorbidity_Groups_OBE_adjusted_Jun22$HFpEF==1]) # 597764.9
sum(Mkt_Comorbidity_Groups_OBE_adjusted_Jun22$weight[Mkt_Comorbidity_Groups_OBE_adjusted_Jun22$CKD==1]) # 4561006
sum(Mkt_Comorbidity_Groups_OBE_adjusted_Jun22$weight[Mkt_Comorbidity_Groups_OBE_adjusted_Jun22$OSLAP==1]) # 11818708

Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 %>% filter(HFpEF==1|CKD==1|OSLAP==1) %>% select(patid, weight) %>% summarise(n=sum(weight))
Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 %>% filter(CKD==1|OSLAP==1) %>% select(patid, weight) %>% summarise(n=sum(weight))


Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 %>% filter(HFpEF==1|CKD==1|OSLAP==1) %>% 
  group_by(HFpEF, CKD, OSLAP) %>% summarise(n=sum(weight))



DANU_Demographics <- fread("DANU Demographics 1.1/DANU Demographics.txt")
DANU_Demographics <- DANU_Demographics %>% filter(diagnosis=="Obesity"|diagnosis=="Diabetes + Obesity") %>% select(patid, age)

OBE2_Comorbidity_Inventories <- fread("DIA Analysis Results 1.1/OBE2 Comorbidity Inventories.txt")
OBE2_Comorbidity_Inventories <- OBE2_Comorbidity_Inventories %>% select(patid, diagnosis)

OBE_ASCVD <- OBE2_Comorbidity_Inventories %>% filter(grepl("G45", diagnosis)|
                                         grepl("I63", diagnosis)|
                                         grepl("H34", diagnosis)|
                                         grepl("I65", diagnosis)|
                                         grepl("I66", diagnosis)|
                                         grepl("I69", diagnosis)|
                                         grepl("I20", diagnosis)|
                                         grepl("I21", diagnosis)|
                                         grepl("I22", diagnosis)|
                                         grepl("I23", diagnosis)|
                                         grepl("I24", diagnosis)|
                                         grepl("I25", diagnosis)|
                                         grepl("I70", diagnosis)|
                                         grepl("I73", diagnosis)) %>% select(patid) %>% distinct()

OBE_ASCVD$OBE_ASCVD <- "OBE_ASCVD"


DIA_Comorbidity_Inventories <- fread("DIA Analysis Results 1.1/DIA Comorbidity Inventories.txt")
DIA_Comorbidity_Inventories <- DIA_Comorbidity_Inventories %>% select(patid, diagnosis)

DIA_ASCVD <- DIA_Comorbidity_Inventories %>% filter(grepl("G45", diagnosis)|
                                         grepl("I63", diagnosis)|
                                         grepl("H34", diagnosis)|
                                         grepl("I65", diagnosis)|
                                         grepl("I66", diagnosis)|
                                         grepl("I69", diagnosis)|
                                         grepl("I20", diagnosis)|
                                         grepl("I21", diagnosis)|
                                         grepl("I22", diagnosis)|
                                         grepl("I23", diagnosis)|
                                         grepl("I24", diagnosis)|
                                         grepl("I25", diagnosis)|
                                         grepl("I70", diagnosis)|
                                         grepl("I73", diagnosis)) %>% select(patid) %>% distinct()

DIA_ASCVD$DIA_ASCVD <- "DIA_ASCVD"


ASCVD <- DIA_ASCVD %>% select(patid) %>% bind_rows(OBE_ASCVD) %>% distinct() %>% mutate(ASCVD="ASCVD") # 26155656


Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 %>% left_join(ASCVD) %>% group_by(ASCVD) %>% summarise(n=sum(weight))


Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 %>% left_join(ASCVD) %>% filter(ASCVD=="ASCVD") %>%
  filter(HFpEF==1) %>% summarise(n=sum(weight)) # 460027.1
Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 %>% left_join(ASCVD) %>% filter(ASCVD=="ASCVD") %>%
  filter(CKD==1) %>% summarise(n=sum(weight)) # 2475727
Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 %>% left_join(ASCVD) %>% filter(ASCVD=="ASCVD") %>%
  filter(OSLAP==1) %>% summarise(n=sum(weight)) # 3674358

Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 %>% left_join(ASCVD) %>% filter(ASCVD=="ASCVD") %>%
  filter(CKD|OSLAP==1) %>% summarise(n=sum(weight)) # 5671542



Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 %>% left_join(ASCVD) %>% filter(ASCVD=="ASCVD") %>% 
  filter(HFpEF==1|CKD==1|OSLAP==1) %>% 
  group_by(HFpEF, CKD, OSLAP) %>% summarise(n=sum(weight))


Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 %>% left_join(ASCVD) %>% left_join(DANU_Demographics) %>%
  filter(age<65) %>%
  filter(HFpEF==1|CKD==1|OSLAP==1|ASCVD=="ASCVD")  %>% summarise(n=sum(weight))





# --------------------
# T2D rank vs BMI bucket v3 profile ---------------------------------------

DANU_Ingredients <- fread("DIA Analysis Results 1.1/DANU Ingredients.txt", integer64 = "character", stringsAsFactors = F)
DANU_Ingredients <- DANU_Ingredients %>%  separate(drug_id, c('class', 'molecule'))
DANU_Ingredients <- DANU_Ingredients %>% select(molecule, drug_group)
DANU_Ingredients$molecule <- as.numeric(DANU_Ingredients$molecule)

DIA_Drug_Histories <- fread("DIA Analysis Results 1.1/DIA Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
Treatment_exp_Vector <- fread("DIA Analysis Results 1.1/Treatment_exp_Vector.txt")
DIA_Drug_Histories <- Treatment_exp_Vector %>% left_join(DIA_Drug_Histories)
DIA_Drug_Histories <- gather(DIA_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
DIA_Drug_Histories <- DIA_Drug_Histories %>% select(patient, weight, Drugs) %>% distinct() %>% filter(Drugs!="-")
DIA_Drug_Histories <- separate_rows(DIA_Drug_Histories, Drugs, sep = ",", convert=T)
DIA_Drug_Histories$Drugs <- as.numeric(DIA_Drug_Histories$Drugs)
DIA_Drug_Histories <- DIA_Drug_Histories %>% select(patient, weight, Drugs) %>% distinct() 
DIA_Drug_Histories <- DIA_Drug_Histories %>% left_join(DANU_Ingredients, by=c("Drugs"="molecule")) %>% select(patient, weight, drug_group) %>% distinct()

Ranks <- DIA_Drug_Histories %>% mutate(drug_group=ifelse(drug_group=="GLP1 Injectable", 1, 
                                                ifelse(drug_group=="Insulin", 2,
                                                       ifelse(drug_group=="GLP1 Oral", 3,
                                                              ifelse(drug_group=="SGLT2", 4,
                                                                     ifelse(drug_group=="DPP4"|drug_group=="Antidiabetc",5,6)))))) %>%
  group_by(patient) %>% filter(drug_group==min(drug_group)) %>% slice(1) %>% ungroup()

sum(Ranks$weight)



DIA_Drug_Histories <- fread("DIA Analysis Results 1.1/DIA Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
Treatment_exp_Vector <- fread("DIA Analysis Results 1.1/Treatment_exp_Vector.txt")
DIA_Drug_Histories <- Treatment_exp_Vector %>% left_join(DIA_Drug_Histories) %>% select(-c(disease))
DIA_Drug_Histories %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(weight))
DIA_Drug_Histories <- gather(DIA_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
DIA_Drug_Histories <- DIA_Drug_Histories %>% filter(Drugs!="-")
DIA_Drug_Histories$Month <- as.character(DIA_Drug_Histories$Month)
DIA_Drug_Histories$Month <- parse_number(DIA_Drug_Histories$Month)
  
DANU_Ingredients       <- fread("DIA Analysis Results 1.1/DANU Ingredients.txt", integer64 = "character", stringsAsFactors = F)
DANU_Ingredients       <- DANU_Ingredients %>%  separate(drug_id, c('class', 'molecule'))
string_Injectables       <- paste0("\\b(",paste0(DANU_Ingredients$molecule[DANU_Ingredients$drug_group == "GLP1 Injectable"], collapse = "|"),")\\b")

DIA_Drug_Histories <- DIA_Drug_Histories %>% mutate(Inj = ifelse( grepl(string_Injectables, Drugs),"I", "O"))

DIA_Drug_Histories_2 <- DIA_Drug_Histories %>% select(patient, weight, Month, Inj) %>%
  filter(Inj=="I") %>% select(patient) %>% distinct() %>%
  left_join(DIA_Drug_Histories %>% select(patient, weight, Month, Inj))

DIA_Drug_Histories_2 <- DIA_Drug_Histories_2 %>% spread(key=Month, value=Inj)
DIA_Drug_Histories_2[is.na(DIA_Drug_Histories_2)] <- "-"
DIA_Drug_Histories_2 <- gather(DIA_Drug_Histories_2, Month, Drugs, `1`:`60`, factor_key=TRUE)
DIA_Drug_Histories_2 <- DIA_Drug_Histories_2 %>% arrange(patient, Month)

DIA_Drug_Histories_3 <- DIA_Drug_Histories_2 %>% group_by(patient, weight) %>% 
  slice(if(any(Drugs=="I")) which.max(Drugs=="I"):which.max(Month=="60") else NA) 

DIA_Drug_Histories_3 <- DIA_Drug_Histories_3 %>% group_by(patient) %>% mutate(grp = rle(Drugs)$lengths %>% {rep(seq(length(.)), .)})
DIA_Drug_Histories_3 <- DIA_Drug_Histories_3 %>% arrange(patient, Month)
DIA_Drug_Histories_3 %>% ungroup() %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(weight)) # 6444437 Inj exp

GLP1_Injectable_Pats <- DIA_Drug_Histories_3  %>% ungroup() %>% select(patient, weight) %>% distinct()

DIA_Drug_Histories_3 %>% ungroup() %>% filter(grp!=1) %>%
   filter(!grepl("I", Drugs)) %>% select(patient, weight, Drugs, grp) %>%
  group_by(patient, weight, grp) %>% count() %>% ungroup() %>%
  filter(n>=6) %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(weight)) 

Stopped_Inj_GLP1 <- DIA_Drug_Histories_3 %>% ungroup() %>% filter(grp!=1) %>%
   filter(!grepl("I", Drugs)) %>% select(patient, weight, Drugs, grp) %>%
  group_by(patient, weight, grp) %>% count() %>% ungroup() %>%
  filter(n>=6) %>% select(patient, weight) %>% distinct()

GLP1_Injectable_Pats <- GLP1_Injectable_Pats %>% anti_join(Stopped_Inj_GLP1)

sum(GLP1_Injectable_Pats$weight)
sum(Stopped_Inj_GLP1$weight)


DIA_Drug_Histories <- fread("DIA Analysis Results 1.1/DIA Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
Treatment_exp_Vector <- fread("DIA Analysis Results 1.1/Treatment_exp_Vector.txt")
DIA_Drug_Histories <- Treatment_exp_Vector %>% left_join(DIA_Drug_Histories) %>% select(-c(disease))
DIA_Drug_Histories %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(weight))
DIA_Drug_Histories <- gather(DIA_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
DIA_Drug_Histories <- DIA_Drug_Histories %>% filter(Drugs!="-")
DIA_Drug_Histories$Month <- as.character(DIA_Drug_Histories$Month)
DIA_Drug_Histories$Month <- parse_number(DIA_Drug_Histories$Month)
  
DANU_Ingredients       <- fread("DIA Analysis Results 1.1/DANU Ingredients.txt", integer64 = "character", stringsAsFactors = F)
DANU_Ingredients       <- DANU_Ingredients %>%  separate(drug_id, c('class', 'molecule'))
string_Injectables       <- paste0("\\b(",paste0(DANU_Ingredients$molecule[DANU_Ingredients$drug_group == "Insulin"], collapse = "|"),")\\b")

DIA_Drug_Histories <- DIA_Drug_Histories %>% mutate(Inj = ifelse( grepl(string_Injectables, Drugs),"I", "O"))

DIA_Drug_Histories_2 <- DIA_Drug_Histories %>% select(patient, weight, Month, Inj) %>%
  filter(Inj=="I") %>% select(patient) %>% distinct() %>%
  left_join(DIA_Drug_Histories %>% select(patient, weight, Month, Inj))

DIA_Drug_Histories_2 <- DIA_Drug_Histories_2 %>% spread(key=Month, value=Inj)
DIA_Drug_Histories_2[is.na(DIA_Drug_Histories_2)] <- "-"
DIA_Drug_Histories_2 <- gather(DIA_Drug_Histories_2, Month, Drugs, `1`:`60`, factor_key=TRUE)
DIA_Drug_Histories_2 <- DIA_Drug_Histories_2 %>% arrange(patient, Month)

DIA_Drug_Histories_3 <- DIA_Drug_Histories_2 %>% group_by(patient, weight) %>% 
  slice(if(any(Drugs=="I")) which.max(Drugs=="I"):which.max(Month=="60") else NA) 

DIA_Drug_Histories_3 <- DIA_Drug_Histories_3 %>% group_by(patient) %>% mutate(grp = rle(Drugs)$lengths %>% {rep(seq(length(.)), .)})
DIA_Drug_Histories_3 <- DIA_Drug_Histories_3 %>% arrange(patient, Month)
DIA_Drug_Histories_3 %>% ungroup() %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(weight)) # 11602149 Insulin exp

Insulin_Pats <- DIA_Drug_Histories_3  %>% ungroup() %>% select(patient, weight) %>% distinct()

DIA_Drug_Histories_3 %>% ungroup() %>% filter(grp!=1) %>%
   filter(!grepl("I", Drugs)) %>% select(patient, weight, Drugs, grp) %>%
  group_by(patient, weight, grp) %>% count() %>% ungroup() %>%
  filter(n>=6) %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(weight)) 

Stopped_Insulin <- DIA_Drug_Histories_3 %>% ungroup() %>% filter(grp!=1) %>%
   filter(!grepl("I", Drugs)) %>% select(patient, weight, Drugs, grp) %>%
  group_by(patient, weight, grp) %>% count() %>% ungroup() %>%
  filter(n>=6) %>% select(patient, weight) %>% distinct()

Insulin_Pats <- Insulin_Pats %>% anti_join(Stopped_Insulin)


GLP1_Injectable_Pats$GLP1_Injectable_Pats <- "GLP1_Injectable_Pats"
Insulin_Pats$Insulin_Pats <- "Insulin_Pats"
Stopped_Inj_GLP1$Stopped_Inj_GLP1 <- "Stopped_Inj_GLP1"
Stopped_Insulin$Stopped_Insulin <- "Stopped_Insulin"


Stopped_Insulin <- Stopped_Insulin %>% anti_join(Stopped_Inj_GLP1) %>% anti_join(GLP1_Injectable_Pats)
Insulin_Pats <- Insulin_Pats %>% anti_join(Stopped_Inj_GLP1) %>% anti_join(GLP1_Injectable_Pats)


temp <- Ranks %>% left_join(GLP1_Injectable_Pats) %>% left_join(Insulin_Pats) %>%  left_join(Stopped_Inj_GLP1) %>% left_join(Stopped_Insulin) 
temp[is.na(temp)] <- "-"

Groups <- temp %>% mutate(group=ifelse(GLP1_Injectable_Pats=="GLP1_Injectable_Pats", "GLP1_Injectable_Pats",
                      ifelse(Stopped_Inj_GLP1=="Stopped_Inj_GLP1", "Stopped_Inj_GLP1",
                             ifelse(Insulin_Pats=="Insulin_Pats", "Insulin_Pats",
                                    ifelse(Stopped_Insulin=="Stopped_Insulin" , "Stopped_Insulin", 
                                    ifelse(drug_group==3|drug_group==4|drug_group==5, "AdvancedOral", "Biguanide")))))) %>%
 select(patient, weight, group)

sum(Groups$weight)

Groups %>% group_by(group) %>% summarise(n=sum(weight))

DANU_Measures <- fread("DANU Measures 1.1/DANU Measures.txt",  integer64 = "character", stringsAsFactors = F)
DANU_Measures <- DANU_Measures %>% filter(test=="BMI"|test=="HbA1c Level")
DANU_Measures <- Groups %>% select(patient) %>% inner_join(DANU_Measures, by=c("patient"="patid"))
DANU_Measures <- DANU_Measures %>% select(patient,test, value) %>% distinct() %>% group_by(patient, test ) %>% filter(value==max(value)) %>% slice(1)
DANU_Measures <- DANU_Measures %>% mutate(value=ifelse(test=="BMI"&value>=30, "BMI>30",
                                                       ifelse(test=="BMI", "BMI<30",
                                                              ifelse(test=="HbA1c Level"&value<7.5, "HbA1c<7.5",
                                                                     ifelse(test=="HbA1c Level"&value<9, "HbA1c<9", "HbA1c>9")))))
DANU_Measures <- DANU_Measures %>% select(patient, value, test, value) %>% spread(key=test, value=value) %>% drop_na()





DIA_Drug_Histories <- fread("DIA Analysis Results 1.1/DIA Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
Treatment_exp_Vector <- fread("DIA Analysis Results 1.1/Treatment_exp_Vector.txt")
DIA_Drug_Histories <- Treatment_exp_Vector %>% left_join(DIA_Drug_Histories) %>% select(-c(disease))
DIA_Drug_Histories <- gather(DIA_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
DIA_Drug_Histories <- DIA_Drug_Histories %>% filter(Drugs!="-")
DIA_Drug_Histories$Month <- as.character(DIA_Drug_Histories$Month)
DIA_Drug_Histories$Month <- parse_number(DIA_Drug_Histories$Month)
DIA_Drug_Histories <- DIA_Drug_Histories %>% group_by(patient) %>% filter(Month==min(Month))
New_Rx <- DIA_Drug_Histories %>% filter(Month>=49) %>% select(patient) %>% distinct()
New_Rx$New <- "New"
New_Rx <- New_Rx %>% ungroup()

Treatment_exp_Vector <- Treatment_exp_Vector %>% left_join(New_Rx) %>% mutate(New=ifelse(is.na(New), "Old", New))
Treatment_exp_Vector <- Treatment_exp_Vector %>% inner_join(Groups %>% filter(group=="Biguanide")) %>% select(patient, New)


Groups <- Groups %>% left_join(Treatment_exp_Vector) %>% mutate(group=ifelse(group=="Biguanide"&New=="New", "New", group)) %>% select(-New) %>% drop_na()

Groups %>% group_by(group) %>% summarise(n=sum(weight))


DIA_Drug_Histories <- fread("DIA Analysis Results 1.1/DIA Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
Treatment_exp_Vector <- fread("DIA Analysis Results 1.1/Treatment_exp_Vector.txt")
DIA_Drug_Histories <- Treatment_exp_Vector %>% left_join(DIA_Drug_Histories) %>% select(-c(disease))
DIA_Drug_Histories <- gather(DIA_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
DIA_Drug_Histories <- DIA_Drug_Histories %>% filter(Drugs!="-")
DIA_Drug_Histories <- DIA_Drug_Histories %>% select(patient, Drugs) %>% distinct()
DIA_Drug_Histories <- separate_rows(DIA_Drug_Histories, Drugs, sep = ",", convert=T)
DIA_Drug_Histories <- DIA_Drug_Histories %>% select(patient, Drugs) %>% distinct()
DIA_Drug_Histories <- DIA_Drug_Histories %>% group_by(patient) %>% count()

Groups %>% left_join(DIA_Drug_Histories) %>%
  group_by(group) %>% summarise(n=weighted.mean(n,weight))



DIA_Drug_Histories <- fread("DIA Analysis Results 1.1/DIA Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
Treatment_exp_Vector <- fread("DIA Analysis Results 1.1/Treatment_exp_Vector.txt")
DIA_Drug_Histories <- Treatment_exp_Vector %>% left_join(DIA_Drug_Histories) %>% select(-c(disease))
DIA_Drug_Histories <- gather(DIA_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
DIA_Drug_Histories <- DIA_Drug_Histories %>% filter(Drugs!="-") %>% filter(Month=="month60") %>% select(-Month) 
DIA_Drug_Histories <- DIA_Drug_Histories %>% select(patient, Drugs) %>% distinct()
DIA_Drug_Histories <- separate_rows(DIA_Drug_Histories, Drugs, sep = ",", convert=T)
DIA_Drug_Histories <- DIA_Drug_Histories %>% select(patient, Drugs) %>% distinct()
DIA_Drug_Histories <- DIA_Drug_Histories %>% group_by(patient) %>% count()

Groups %>% left_join(DIA_Drug_Histories) %>% mutate(n=ifelse(is.na(n),0,n)) %>%
  group_by(group) %>% summarise(n=weighted.mean(n,weight))


DIA_Drug_Histories <- fread("DIA Analysis Results 1.1/DIA Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
Treatment_exp_Vector <- fread("DIA Analysis Results 1.1/Treatment_exp_Vector.txt")
DIA_Drug_Histories <- Treatment_exp_Vector %>% left_join(DIA_Drug_Histories) %>% select(-c(disease))
DIA_Drug_Histories <- gather(DIA_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
DIA_Drug_Histories <- DIA_Drug_Histories %>% filter(Drugs=="-") %>% filter(Month=="month60") %>% select(-Month) 
DIA_Drug_Histories <- DIA_Drug_Histories %>% select(patient, Drugs) %>% distinct()

Groups %>% left_join(DIA_Drug_Histories) %>% 
  group_by(group, Drugs) %>% summarise(n=sum(weight)) %>%
  spread(key=Drugs, value=n) %>%
  mutate(Perc=`-` / (`-`+`<NA>`))



DIA_Drug_Histories <- fread("DIA Analysis Results 1.1/DIA Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
Treatment_exp_Vector <- fread("DIA Analysis Results 1.1/Treatment_exp_Vector.txt")
DIA_Drug_Histories <- Treatment_exp_Vector %>% left_join(DIA_Drug_Histories) %>% select(-c(disease))
DIA_Drug_Histories <- gather(DIA_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
DIA_Drug_Histories$Month <- as.character(DIA_Drug_Histories$Month)
DIA_Drug_Histories$Month <- parse_number(DIA_Drug_Histories$Month)
DIA_Drug_Histories <- DIA_Drug_Histories %>% filter(Drugs!="-") %>%  filter(Month>=49) %>% filter(grepl("47", Drugs)|Drugs=="47") %>% select(-Month) 
DIA_Drug_Histories <- DIA_Drug_Histories %>% select(patient) %>% distinct() %>% mutate(Oral="Oral")

Groups %>% left_join(DIA_Drug_Histories) %>% 
  group_by(group, Oral) %>% summarise(n=sum(weight)) %>%
  spread(key=Oral, value=n) %>%
  mutate(Perc=Oral / (Oral+`<NA>`))


DIA_Flows_Aux_Long <- fread("DIA Analysis Results 1.1/DIA_Flows_Aux_Long.txt", integer64 = "character", stringsAsFactors = F)
Treatment_exp_Vector <- fread("DIA Analysis Results 1.1/Treatment_exp_Vector.txt")
DIA_Flows_Aux_Long <- Treatment_exp_Vector %>% inner_join(DIA_Flows_Aux_Long)
DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% filter(p1 >=48)
DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% filter(stops!=1) %>% group_by(patient) %>% summarise(n=sum(flow))

Groups %>% left_join(DIA_Flows_Aux_Long) %>%
  group_by(group) %>% summarise(n=weighted.mean(n,weight))





DIA_Drug_Histories <- fread("DIA Analysis Results 1.1/DIA Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
Treatment_exp_Vector <- fread("DIA Analysis Results 1.1/Treatment_exp_Vector.txt")
DIA_Drug_Histories <- Treatment_exp_Vector %>% left_join(DIA_Drug_Histories) %>% select(-c(disease))
DIA_Drug_Histories <- gather(DIA_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
DIA_Drug_Histories <- DIA_Drug_Histories %>% filter(Drugs!="-") %>% select(-Month) 
DIA_Drug_Histories <- DIA_Drug_Histories %>% select(patient, Drugs) %>% distinct()
DIA_Drug_Histories <- DIA_Drug_Histories %>% group_by(patient) %>% count()

Groups %>% left_join(DIA_Drug_Histories) %>% mutate(n=ifelse(is.na(n),0,n)) %>%
  group_by(group) %>% summarise(n=weighted.mean(n,weight))




DANU_Measures <- fread("DANU Measures 1.1/DANU Measures.txt",  integer64 = "character", stringsAsFactors = F)
DANU_Measures <- DANU_Measures %>% filter(test=="BMI")
DANU_Measures <- Groups %>% select(patient) %>% inner_join(DANU_Measures, by=c("patient"="patid"))
DANU_Measures <- DANU_Measures %>% select(patient,value) %>% distinct() %>% group_by(patient) %>% filter(value==max(value)) %>% slice(1)


Groups %>% inner_join(DANU_Measures) %>% 
  group_by(group) %>% summarise(n=weighted.mean(value,weight))




DANU_Measures <- fread("DANU Measures 1.1/DANU Measures.txt",  integer64 = "character", stringsAsFactors = F)
DANU_Measures <- DANU_Measures %>% filter(test=="HbA1c Level")
DANU_Measures <- Groups %>% select(patient) %>% inner_join(DANU_Measures, by=c("patient"="patid"))
DANU_Measures <- DANU_Measures %>% select(patient,value) %>% distinct() %>% group_by(patient) %>% filter(value==max(value)) %>% slice(1)


Groups %>% inner_join(DANU_Measures) %>% 
  group_by(group) %>% summarise(n=weighted.mean(value,weight))

# --------------

# % per BMI bucket per comorbidity -----------------------------

DANU_Measures <- fread("DANU Measures 1.1/DANU Measures.txt",  integer64 = "character", stringsAsFactors = F)
BMI <- DANU_Measures %>% filter(test=="BMI") %>% select(patid, value)
BMI <- BMI %>% select(patid, value) %>% distinct() %>% group_by(patid ) %>% filter(value==max(value)) %>% slice(1)

BMI <- BMI %>%  mutate(value=ifelse(value<=30, "27-30",
                                                 ifelse(value<=35, "30-35", ">35")))


Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 <- fread("Mkt_Comorbidity_Groups_OBE_adjusted_Jun22.txt", sep="\t")
Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 <- Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 %>% filter(diagnosis=="Diabetes + Obesity")

sum(Mkt_Comorbidity_Groups_OBE_adjusted_Jun22$weight[Mkt_Comorbidity_Groups_OBE_adjusted_Jun22$ASCVD=="ASCVD"])

Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 %>% inner_join(BMI) %>%
  filter(ASCVD=="ASCVD") %>% group_by(value) %>%
  summarise(n=sum(weight))

# --------------------------
# Breakdown by BMI groups - HF CKD OSLAP - -------------------------------------
# HF

DANU_Measures_Full <- fread("DANU Measures Full.txt")
DANU_Measures_Full <- DANU_Measures_Full %>% filter(test=="BMI")
names(DANU_Measures_Full)[1] <- "patient"

Predicted_Stages_gbm_All <- fread("Predicted_Stages_gbm_All.txt")
First_Diastolic_All_L5y <- fread("First_Diastolic_All_L5y.txt")

First_Diastolic_All_L5y <- First_Diastolic_All_L5y %>% select(-earliest) %>% inner_join(Predicted_Stages_gbm_All)

First_Diastolic_All_L5y <-First_Diastolic_All_L5y %>% inner_join(DANU_Measures_Full)

First_Diastolic_All_L5y <- First_Diastolic_All_L5y %>% group_by(patient) %>%filter(value==max(value)) %>% slice(1) %>% 
  filter(value>=27) %>% select(patient, weight, value, Predicted.Stage)

DANU_Demographics_Full <- fread("DANU Demographics Full.txt")
DANU_Demographics_Full <- DANU_Demographics_Full %>% filter(diagnosis=="Obesity"|diagnosis=="Diabetes + Obesity") %>% select(patid, diagnosis)
names(DANU_Demographics_Full)[1] <- "patient"

First_Diastolic_All_L5y <- First_Diastolic_All_L5y %>% left_join(DANU_Demographics_Full)
First_Diastolic_All_L5y <- First_Diastolic_All_L5y %>% mutate(value=ifelse(value>=35, ">35",
                                                ifelse(value>=30, "30-35",
                                                       ifelse(value>=27, "27-30", NA)))) %>% ungroup() 

First_Diastolic_All_L5y <- First_Diastolic_All_L5y %>% filter(!is.na(diagnosis))

First_Diastolic_All_L5y %>% group_by(diagnosis) %>% summarise(n=sum(weight))

First_Diastolic_All_L5y %>% group_by(diagnosis, value) %>% summarise(n=sum(weight)) %>%
  mutate(n=ifelse(diagnosis=="Obesity", n/510498, n/875302))
  
First_Diastolic_All_L5y %>% filter(Predicted.Stage!=1) %>% group_by(diagnosis) %>% summarise(n=sum(weight))

First_Diastolic_All_L5y %>% filter(Predicted.Stage!=1) %>% group_by(diagnosis, value) %>% summarise(n=sum(weight)) %>%
  mutate(n=ifelse(diagnosis=="Obesity", n/431000, n/807994))
  


HF_Drug_Histories <- fread("HF Drug Histories.txt", colClasses = "character")
HF_Drug_Histories <- gather(HF_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
HF_Drug_Histories$Month <- as.character(HF_Drug_Histories$Month)
HF_Drug_Histories$Month <- parse_number(HF_Drug_Histories$Month)
HF_Drug_Histories <- HF_Drug_Histories %>% filter(Month>=49)
HF_Drug_Histories <- HF_Drug_Histories %>% filter(Drugs!="-") %>% select(patient) %>% distinct()

First_Diastolic_All_L5y %>% filter(Predicted.Stage!=1) %>% inner_join(HF_Drug_Histories) %>%
  group_by(diagnosis) %>%  summarise(n=sum(weight))

First_Diastolic_All_L5y %>% filter(Predicted.Stage!=1) %>% inner_join(HF_Drug_Histories) %>%
  group_by(diagnosis, value) %>%  summarise(n=sum(weight)) %>%
    mutate(n=ifelse(diagnosis=="Obesity", n/378831, n/722123))




DANU_Ingredients <- fread("DANU Ingredients.txt", integer64 = "character", stringsAsFactors = F)
DANU_Ingredients <- DANU_Ingredients %>%  separate(drug_id, c('class', 'molecule'))
DANU_Ingredients  <- DANU_Ingredients %>% select(molecule, drug_group)
names(DANU_Ingredients)[1] <- "Drugs"
DANU_Ingredients$Drugs <- as.numeric(DANU_Ingredients$Drugs)
string_GLP1  <- paste0("\\b(",paste0(DANU_Ingredients$Drugs[DANU_Ingredients$drug_group == "GLP1 Injectable"|DANU_Ingredients$drug_group == "GLP1 Oral"], collapse = "|"),")\\b")

DIA_Drug_Histories <- fread("DIA Drug Histories.txt", integer64 = "character", stringsAsFactors = F)

OBE_Drug_Histories <- fread("OBE Drug Histories.txt", integer64 = "character", stringsAsFactors = F)

Drug_Histories <-  DIA_Drug_Histories %>% select(-disease) %>%
  bind_rows(OBE_Drug_Histories %>% select(-disease))

Drug_Histories <- gather(Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
Drug_Histories <- Drug_Histories %>% filter(Drugs!="-")
Drug_Histories$Month <- as.character(Drug_Histories$Month)
Drug_Histories$Month <- parse_number(Drug_Histories$Month)

Drug_Histories <- Drug_Histories %>% filter(Month>=49)
Drug_Histories <- Drug_Histories %>% select(patient, Drugs) %>% distinct()

Drug_Histories <- Drug_Histories %>% filter(grepl(string_GLP1, Drugs)) %>% select(patient) %>% distinct() 

First_Diastolic_All_L5y %>% filter(Predicted.Stage!=1) %>% inner_join(Drug_Histories) %>%
  group_by(diagnosis) %>%  summarise(n=sum(weight))

First_Diastolic_All_L5y %>% filter(Predicted.Stage!=1) %>% inner_join(Drug_Histories) %>%
  group_by(diagnosis, value) %>%  summarise(n=sum(weight)) %>%
    mutate(n=ifelse(diagnosis=="Obesity", n/382, n/60247))







# CKD


DANU_Measures <- fread("DANU Measures.txt")
DANU_Measures <- DANU_Measures %>% filter(test=="BMI")
names(DANU_Measures)[1] <- "patient"


CKD_Stages_Complete_FilledIn <- fread("CKD_Stages_Complete_FilledIn.txt")
CKD_Pts_Cmbdt_flags_Cmbdt_Hierchy_grps <- fread("CKD_Pts_Cmbdt_flags_&_Cmbdt_Hierchy_grps.csv")
CKD_Pts_Cmbdt_flags_Cmbdt_Hierchy_grps <- CKD_Pts_Cmbdt_flags_Cmbdt_Hierchy_grps %>% select(patid)
names(CKD_Pts_Cmbdt_flags_Cmbdt_Hierchy_grps)[1] <- "patient"

CKD_Stages_Complete_FilledIn <- CKD_Stages_Complete_FilledIn %>% inner_join(CKD_Pts_Cmbdt_flags_Cmbdt_Hierchy_grps)

CKD_Stages_Complete_FilledIn <-CKD_Stages_Complete_FilledIn %>% inner_join(DANU_Measures)

CKD_Stages_Complete_FilledIn <- CKD_Stages_Complete_FilledIn %>% group_by(patient) %>%filter(value==max(value)) %>% slice(1) %>% 
  filter(value>=27) %>% select(patient, weight, value, Stage )

CKD_Demographics <- fread("CKD Demographics.txt")
CKD_Demographics <- CKD_Demographics %>% filter(diagnosis=="Obesity"|diagnosis=="Diabetes + Obesity") %>% select(patid, diagnosis, weight)
names(CKD_Demographics)[1] <- "patient"

CKD_Stages_Complete_FilledIn <- CKD_Stages_Complete_FilledIn %>% inner_join(CKD_Demographics)
CKD_Stages_Complete_FilledIn <- CKD_Stages_Complete_FilledIn %>% mutate(value=ifelse(value>=35, ">35",
                                                ifelse(value>=30, "30-35",
                                                       ifelse(value>=27, "27-30", NA)))) %>% ungroup() 

CKD_Stages_Complete_FilledIn %>% group_by(diagnosis) %>% summarise(n=sum(weight))

CKD_Stages_Complete_FilledIn %>% group_by(diagnosis, value) %>% summarise(n=sum(weight)) %>%
  mutate(n=ifelse(diagnosis=="Obesity", n/1741428, n/2465599))
  
CKD_Stages_Complete_FilledIn %>% filter(Stage  ==  "Stage3") %>% group_by(diagnosis) %>% summarise(n=sum(weight))

CKD_Stages_Complete_FilledIn %>% filter(Stage ==  "Stage3") %>% group_by(diagnosis, value) %>% summarise(n=sum(weight)) %>%
  mutate(n=ifelse(diagnosis=="Obesity", n/1038864, n/1252555))
  

CKD_Drug_Histories <- fread("CKD Drug Histories.txt", colClasses = "character")
CKD_Drug_Histories <- gather(CKD_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
CKD_Drug_Histories$Month <- as.character(CKD_Drug_Histories$Month)
CKD_Drug_Histories$Month <- parse_number(CKD_Drug_Histories$Month)

CKD_Drug_Histories <- CKD_Drug_Histories %>% filter(Month>=49)
CKD_Drug_Histories <- CKD_Drug_Histories %>% filter(Drugs!="-") %>% select(patient) %>% distinct()

CKD_Stages_Complete_FilledIn %>% filter(Stage ==  "Stage3") %>% inner_join(CKD_Drug_Histories) %>%
  group_by(diagnosis) %>%  summarise(n=sum(weight))

CKD_Stages_Complete_FilledIn %>% filter(Stage ==  "Stage3") %>% inner_join(CKD_Drug_Histories) %>%
  group_by(diagnosis, value) %>%  summarise(n=sum(weight)) %>%
    mutate(n=ifelse(diagnosis=="Obesity", n/927798, n/1126960))




DANU_Ingredients <- fread("DANU Ingredients.txt", integer64 = "character", stringsAsFactors = F)
DANU_Ingredients <- DANU_Ingredients %>%  separate(drug_id, c('class', 'molecule'))
DANU_Ingredients  <- DANU_Ingredients %>% select(molecule, drug_group)
names(DANU_Ingredients)[1] <- "Drugs"
DANU_Ingredients$Drugs <- as.numeric(DANU_Ingredients$Drugs)
string_GLP1  <- paste0("\\b(",paste0(DANU_Ingredients$Drugs[DANU_Ingredients$drug_group == "GLP1 Injectable"|DANU_Ingredients$drug_group == "GLP1 Oral"], collapse = "|"),")\\b")

DIA_Drug_Histories <- fread("DIA Drug Histories.txt", integer64 = "character", stringsAsFactors = F)

OBE_Drug_Histories <- fread("OBE Drug Histories.txt", integer64 = "character", stringsAsFactors = F)

Drug_Histories <-  DIA_Drug_Histories %>% select(-disease) %>%
  bind_rows(OBE_Drug_Histories %>% select(-disease))

Drug_Histories <- gather(Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
Drug_Histories <- Drug_Histories %>% filter(Drugs!="-")
Drug_Histories$Month <- as.character(Drug_Histories$Month)
Drug_Histories$Month <- parse_number(Drug_Histories$Month)

Drug_Histories <- Drug_Histories %>% filter(Month>=49)
Drug_Histories <- Drug_Histories %>% select(patient, Drugs) %>% distinct()

Drug_Histories <- Drug_Histories %>% filter(grepl(string_GLP1, Drugs)) %>% select(patient) %>% distinct() 


CKD_Stages_Complete_FilledIn %>% filter(Stage ==  "Stage3") %>% inner_join(Drug_Histories) %>%
  group_by(diagnosis) %>%  summarise(n=sum(weight))

CKD_Stages_Complete_FilledIn %>% filter(Stage ==  "Stage3")  %>% inner_join(Drug_Histories) %>%
  group_by(diagnosis, value) %>%  summarise(n=sum(weight)) %>%
    mutate(n=ifelse(diagnosis=="Obesity", n/1653, n/128451))


# OSLAP


New_Comorbidity_Groups_Jun1 <- fread("Mkt_Comorbidity_Groups_OBE_adjusted_Jun22.txt", sep="\t")
New_Comorbidity_Groups_Jun1 <- New_Comorbidity_Groups_Jun1 %>% filter(OSLAP==1 & (diagnosis=="Obesity" | diagnosis=="Diabetes + Obesity")) %>% select(patid, weight, diagnosis)



DANU_Measures <- fread("DANU Measures 1.1/DANU Measures.txt",  integer64 = "character", stringsAsFactors = F)
DANU_Measures <- DANU_Measures %>% select(-weight) %>% inner_join(New_Comorbidity_Groups_Jun1 %>% select(patid, weight, diagnosis))
DANU_Measures <- DANU_Measures %>% filter(test=="BMI")
DANU_Measures <- DANU_Measures %>% select(patid, value, weight, diagnosis,) %>% distinct() %>% group_by(patid ) %>%
  filter(value==max(value)) %>% slice(1) %>% ungroup()

DANU_Measures <- DANU_Measures %>% filter(value>=27)

DANU_Measures <- DANU_Measures %>% mutate(value=ifelse(value>=35, ">35",
                                                ifelse(value>=30, "30-35",
                                                       ifelse(value>=27, "27-30", NA)))) %>% ungroup() 


DANU_Measures %>%  group_by(diagnosis) %>%  summarise(n=sum(weight))

DANU_Measures %>% 
  group_by(diagnosis, value) %>%  summarise(n=sum(weight)) %>%
    mutate(n=ifelse(diagnosis=="Obesity", n/11818708, n/4844037))



DANU_Ingredients <- fread("DANU Ingredients.txt", integer64 = "character", stringsAsFactors = F)
DANU_Ingredients <- DANU_Ingredients %>%  separate(drug_id, c('class', 'molecule'))
DANU_Ingredients  <- DANU_Ingredients %>% select(molecule, drug_group)
names(DANU_Ingredients)[1] <- "Drugs"
DANU_Ingredients$Drugs <- as.numeric(DANU_Ingredients$Drugs)
string_GLP1  <- paste0("\\b(",paste0(DANU_Ingredients$Drugs[DANU_Ingredients$drug_group == "GLP1 Injectable"|DANU_Ingredients$drug_group == "GLP1 Oral"], collapse = "|"),")\\b")

DIA_Drug_Histories <- fread("DIA Drug Histories.txt", integer64 = "character", stringsAsFactors = F)

OBE_Drug_Histories <- fread("OBE Drug Histories.txt", integer64 = "character", stringsAsFactors = F)

Drug_Histories <-  DIA_Drug_Histories %>% select(-disease) %>%
  bind_rows(OBE_Drug_Histories %>% select(-disease))

Drug_Histories <- gather(Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
Drug_Histories <- Drug_Histories %>% filter(Drugs!="-")
Drug_Histories$Month <- as.character(Drug_Histories$Month)
Drug_Histories$Month <- parse_number(Drug_Histories$Month)

Drug_Histories <- Drug_Histories %>% filter(Month>=49)
Drug_Histories <- Drug_Histories %>% select(patient, Drugs) %>% distinct()

Drug_Histories <- Drug_Histories %>% filter(grepl(string_GLP1, Drugs)) %>% select(patient) %>% distinct() 

names(DANU_Measures)[1] <- "patient"

DANU_Measures %>% inner_join(Drug_Histories) %>% group_by(diagnosis) %>%  summarise(n=sum(weight))


DANU_Measures %>% inner_join(Drug_Histories) %>%
  group_by(diagnosis, value) %>%  summarise(n=sum(weight)) %>%
    mutate(n=ifelse(diagnosis=="Obesity", n/5221, n/160914))

# ---------------------------------------
# Risk factors in non-comorbid ------------------------------

Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 <- fread("Mkt_Comorbidity_Groups_OBE_adjusted_Jun22.txt", sep="\t")
Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 <- Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 %>% filter(diagnosis=="Obesity"|diagnosis=="Diabetes + Obesity")
sum(Mkt_Comorbidity_Groups_OBE_adjusted_Jun22$weight)

Mkt_Comorbidity_Groups_OBE_adjusted_Jun22

DIA_Comorbidity_Inventories <- fread("DIA Analysis Results 1.1/DIA Comorbidity Inventories.txt")
OBE2_Comorbidity_Inventories <- fread("DIA Analysis Results 1.1/OBE2 Comorbidity Inventories.txt")

Comorbidity_Inventories <- DIA_Comorbidity_Inventories %>% bind_rows(OBE2_Comorbidity_Inventories)

DIA <- Comorbidity_Inventories %>% filter(diagnosis=="E11") %>% select(patid) %>% distinct() %>% mutate(DIA=1)
DYSL <- Comorbidity_Inventories %>% filter(diagnosis=="E78") %>% select(patid) %>% distinct() %>% mutate(DYSL=1)
HTN <- Comorbidity_Inventories %>% filter(diagnosis=="I10") %>% select(patid) %>% distinct() %>% mutate(HTN=1)
SMOKE <- Comorbidity_Inventories %>% filter(diagnosis=="F17"|diagnosis=="Z72") %>% select(patid) %>% distinct() %>% mutate(SMOKE=1)

Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 <- Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 %>% left_join(DIA) %>% left_join(DYSL) %>% left_join(HTN)  %>% left_join(SMOKE)

Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 <- Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 %>% select(-c(PAD_restrict, PCOS))

Mkt_Comorbidity_Groups_OBE_adjusted_Jun22[is.na(Mkt_Comorbidity_Groups_OBE_adjusted_Jun22)] <-0

Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 <- Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 %>% mutate(total=DIA+DYSL+HTN+SMOKE)

Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 %>% filter(OSLAP==0&CKD==0&HFpEF==0&ASCVD=="0") %>% group_by(total) %>% summarise(n=sum(weight))


DANU_Demographics <- fread("DANU Demographics 1.1/DANU Demographics.txt")
DANU_Demographics <- DANU_Demographics %>% select(patid, age)

Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 <- Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 %>% left_join(DANU_Demographics) %>% mutate(age=ifelse(age<65, "<65", ">65"))

Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 %>% filter(OSLAP==0&CKD==0&HFpEF==0&ASCVD=="0") %>% group_by(age, total) %>% summarise(n=sum(weight))



DANU_Measures <- fread("DANU Measures 1.1/DANU Measures.txt",  integer64 = "character", stringsAsFactors = F)
BMI <- DANU_Measures %>% filter(test=="BMI") %>% select(patid, value)
BMI <- BMI %>% select(patid, value) %>% distinct() %>% group_by(patid ) %>% filter(value==max(value)) %>% slice(1)

BMI <- BMI %>%  filter(value>27) %>% mutate(value=ifelse(value<=30, "27-30",
                                                 ifelse(value<=35, "30-35", ">35")))

Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 %>% inner_join(BMI) %>% filter(OSLAP==0&CKD==0&HFpEF==0&ASCVD=="0") %>% group_by(value) %>% summarise(n=sum(weight))

# ---------------------------
# Flows for Hendrik diagram flows -----------------------

Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 <- fread("Mkt_Comorbidity_Groups_OBE_adjusted_Jun22.txt", sep="\t")
Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 <- Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 %>% select(patid, weight, diagnosis)
Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 %>% group_by(diagnosis) %>% summarise(n=sum(weight))

Months_lookup <- fread("Months_lookup.txt")
Months_lookup$Month <- format(as.Date(paste0(Months_lookup$Month,"-1")), "%Y-%m")

Months <- Months_lookup  %>% select(Exact_Month) %>% mutate(Link=1) %>%
  full_join(Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 %>% select(patid) %>% mutate(Link=1))

nrow(Months) ; length(unique(Months$patid))*60

Months <- Months %>% arrange(patid, Exact_Month) %>% select(-Link)  %>% 
  left_join(Mkt_Comorbidity_Groups_OBE_adjusted_Jun22) %>% rename("Final_Box"="diagnosis")

DANU_Demographics <- fread("DANU Demographics 1.1/DANU Demographics.txt", integer64 = "character", stringsAsFactors = F)
DANU_Demographics <- DANU_Demographics %>% filter(diagnosis != "-") %>% select(patid, diabetes_onset, obesity_onset) 


DANU_Demographics$diabetes_onset <- format(as.Date(DANU_Demographics$diabetes_onset), "%Y-%m")
DANU_Demographics$obesity_onset <- format(as.Date(DANU_Demographics$obesity_onset), "%Y-%m")
DANU_Demographics <- DANU_Demographics %>% left_join(Months_lookup, by = c("diabetes_onset" = "Month")) %>% rename("diabetes_onset_exact"="Exact_Month")
DANU_Demographics <- DANU_Demographics %>% left_join(Months_lookup, by = c("obesity_onset" = "Month")) %>% rename("obesity_onset_exact"="Exact_Month")

DANU_Demographics$diabetes_onset_exact[is.na(DANU_Demographics$diabetes_onset_exact)&!is.na(DANU_Demographics$diabetes_onset)] <- 1
DANU_Demographics$obesity_onset_exact[is.na(DANU_Demographics$obesity_onset_exact)&!is.na(DANU_Demographics$obesity_onset)] <- 1
DANU_Demographics <- DANU_Demographics %>% select(-c(diabetes_onset, obesity_onset))

DANU_Demographics <- DANU_Demographics %>% mutate(diabetes=ifelse(is.na(diabetes_onset_exact), 0,1)) %>%
                      mutate(obesity=ifelse(is.na(obesity_onset_exact), 0,1))

DANU_Demographics[is.na(DANU_Demographics)] <- 0

Months <- Months %>% left_join(DANU_Demographics %>% select(patid, diabetes_onset_exact, diabetes), by=c("patid"="patid", "Exact_Month"="diabetes_onset_exact")) 
Months <- Months %>% left_join(DANU_Demographics %>% select(patid, obesity_onset_exact, obesity), by=c("patid"="patid", "Exact_Month"="obesity_onset_exact")) 
Months[is.na(Months)] <- 0
Months <- Months %>% group_by(patid) %>% mutate(cumobe = cumsum(obesity==1)) %>% mutate(cumdia = cumsum(diabetes==1))
Months <- Months %>% select(-c(diabetes, obesity))

Months %>% mutate(cumdia=ifelse(cumdia==1&Final_Box=="Obesity",0,cumdia)) %>% 
  filter(Exact_Month==60) %>% group_by(cumobe, cumdia) %>% summarise(n=sum(weight))


Months <- Months %>% mutate(cumdia=ifelse(cumdia==1&Final_Box=="Obesity",0,cumdia))

DANU_Events <- fread("DANU Events 1.1/DANU Events.txt")
DANU_Events <- DANU_Events %>% select(-prov)
DANU_Diagnosis_Codes <- fread("DIA Analysis Results 1.1/DANU Diagnosis Codes.txt")
DANU_Diagnosis_Codes <- DANU_Diagnosis_Codes %>% filter(diagnosis=="Obesity"&source=="Diagnosis")
#DANU_Diagnosis_Codes <- DANU_Diagnosis_Codes %>% filter(!grepl("V",code)&!grepl("Z",code))
DANU_Diagnosis_Codes <- DANU_Diagnosis_Codes %>% select(code)
DANU_Events <- DANU_Events %>% inner_join(DANU_Diagnosis_Codes)
DANU_Events$claimed <- format(as.Date(DANU_Events$claimed), "%Y-%m")
DANU_Events <- DANU_Events %>% group_by(patid) %>% filter(claimed==min(claimed)) %>% slice(1) %>% select(-c(weight, code))
DANU_Events <- DANU_Events %>% left_join(Months_lookup, by = c("claimed" = "Month")) %>% rename("OBE_diagnosis"="Exact_Month")
DANU_Events[is.na(DANU_Events)] <- 1
DANU_Events <- DANU_Events %>% select(-claimed) %>% ungroup()
DANU_Events$OBE_Dx <- 1

Months <- Months %>% left_join(DANU_Events, by=c("patid"="patid", "Exact_Month"="OBE_diagnosis"))

Months[is.na(Months)] <- 0

Months <- Months %>% group_by(patid) %>% mutate(cumobe_dx = cumsum(OBE_Dx==1)) 
Months <- Months %>% select(-c(OBE_Dx))



OBE2_Drug_Histories <- fread("OBE2 Analysis Results 1.1/OBE2 Drug Histories.txt")
OBE2_Drug_Histories <- gather(OBE2_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
OBE2_Drug_Histories <- OBE2_Drug_Histories %>% filter(Drugs != "-") %>% select(patient, weight, Month) %>% mutate(Rx=1)
OBE2_Drug_Histories$Month <- as.character(OBE2_Drug_Histories$Month)
OBE2_Drug_Histories$Month <- parse_number(OBE2_Drug_Histories$Month)
OBE2_Drug_Histories <- OBE2_Drug_Histories %>% select(-weight) %>% rename("patid"="patient")

Months <- Months %>% left_join(OBE2_Drug_Histories, by=c("patid"="patid", "Exact_Month"="Month"))
Months[is.na(Months)] <- 0
names(Months)[8] <- "Current_OBE_Rx"



DIA_Drug_Histories <- fread("DIA Analysis Results 1.1/DIA Drug Histories.txt")
DIA_Drug_Histories <- gather(DIA_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
DIA_Drug_Histories <- DIA_Drug_Histories %>% filter(Drugs != "-") %>% select(patient, weight, Month) %>% mutate(Rx=1)
DIA_Drug_Histories$Month <- as.character(DIA_Drug_Histories$Month)
DIA_Drug_Histories$Month <- parse_number(DIA_Drug_Histories$Month)
DIA_Drug_Histories <- DIA_Drug_Histories %>% select(-weight) %>% rename("patid"="patient")

Months <- Months %>% left_join(DIA_Drug_Histories, by=c("patid"="patid", "Exact_Month"="Month"))
Months[is.na(Months)] <- 0
names(Months)[9] <- "Current_DIA_Rx"

Months <- Months %>% group_by(patid) %>% mutate(cumobe_Rx = cumsum(Current_OBE_Rx==1)) 
unique(Months$cumobe_Rx)
Months <- Months %>% mutate(cumobe_Rx=ifelse(cumobe_Rx==0,0,1))





Months %>% ungroup() %>% filter(cumdia==1&lag(cumdia)==0&Final_Box=="Diabetes") %>%
  filter(Exact_Month>=49) %>%
   summarise(n=sum(weight))

Months %>% ungroup() %>% filter(cumobe==1&lag(cumobe)==0) %>%
  filter(Exact_Month>=49) %>%
   summarise(n=sum(weight))


Months %>% ungroup() %>% filter(cumobe==1&lag(cumobe)==1&cumobe_dx==1&lag(cumobe_dx)==0) %>%
  filter(Exact_Month>=49) %>%
   summarise(n=sum(weight))

Months %>% ungroup() %>% filter(cumobe==1&lag(cumobe)==1&Current_OBE_Rx==1&lag(Current_OBE_Rx)==0&lag(cumobe_Rx)==0) %>%
  filter(Exact_Month>=49) %>%
   summarise(n=sum(weight))




Months %>% ungroup() %>% filter(cumobe==1&lag(cumobe)==1&cumobe_dx==1&lag(cumobe_dx)==1&Current_OBE_Rx==1&lag(Current_OBE_Rx)==0&lag(cumobe_Rx)==0) %>%
  filter(Exact_Month>=49) %>%
   summarise(n=sum(weight))


Months %>% ungroup() %>% filter(cumobe==1&lag(cumobe)==1&cumobe_dx==1&lag(cumobe_dx)==1&Current_OBE_Rx==0&lag(Current_OBE_Rx)==1) %>%
  filter(Exact_Month>=49) %>%
   summarise(n=sum(weight))


Months %>% ungroup() %>% filter(cumobe==1&lag(cumobe)==1&cumobe_dx==1&lag(cumobe_dx)==0&Current_OBE_Rx==1) %>%
  filter(Exact_Month>=49) %>%
   summarise(n=sum(weight))

Months %>% ungroup() %>% filter(cumdia==1&lag(cumdia)==0&cumobe==1&lag(cumobe)==0&Final_Box=="Diabetes + Obesity") %>%
  filter(Exact_Month>=49) %>%
   summarise(n=sum(weight))


Months %>% ungroup() %>% filter(cumdia==1&lag(cumdia)==1&cumobe==1&lag(cumobe)==0&Final_Box=="Diabetes + Obesity") %>%
  filter(Exact_Month>=49) %>%
   summarise(n=sum(weight))


Months %>% ungroup() %>% filter(cumdia==1&lag(cumdia)==0&cumobe==1&lag(cumobe)==1&Final_Box=="Diabetes + Obesity") %>%
  filter(Exact_Month>=49) %>%
   summarise(n=sum(weight))



Months <- Months %>% mutate(DIA_only=ifelse(cumdia == 1 & cumobe==0 , 1, 0))
Months <- Months %>% mutate(DIA_OBE=ifelse(cumdia == 1 & cumobe==1, 1, 0))
Months <- Months %>% mutate(OBE_only=ifelse(cumobe==1 & cumobe_dx==0 & cumobe_Rx == 0 & cumdia==0, 1, 0))
Months <- Months %>% mutate(OBE_dx=ifelse(cumobe==1 & cumobe_dx==1 & cumobe_Rx == 0 & cumdia==0, 1, 0))
Months <- Months %>% mutate(OBE_rx_exp_lapsed=ifelse(cumobe==1 &  cumobe_Rx == 1 & cumdia==0 & Current_OBE_Rx==0, 1, 0))
Months <- Months %>% mutate(OBE_rx_current=ifelse(cumobe==1 &  cumobe_Rx == 1 & Current_OBE_Rx==1 & cumdia==0, 1, 0))

fwrite(Months, "DANU_MoM_Hendrik.txt", sep="\t")


Months <- fread("DANU_MoM_Hendrik.txt", sep="\t")


Months %>% filter(Exact_Month==1) %>%
  group_by(DIA_only, DIA_OBE, OBE_only, OBE_dx, OBE_rx_exp_lapsed, OBE_rx_current) %>% summarise(n=sum(weight))

temp <- data.frame(Months %>%  group_by(Exact_Month, DIA_only, DIA_OBE, OBE_only, OBE_dx, OBE_rx_exp_lapsed, OBE_rx_current) %>% summarise(n=sum(weight)) %>%
  gather(Group, Value, DIA_only:OBE_rx_current) %>% filter(Value==1) %>% arrange(Exact_Month)) %>%
  select(-Value) %>%
  spread(key=Group, value=n)
 
temp[is.na(temp)] <- 0

fwrite(temp, "DANU_stocks_MoM_Hendrik.txt", sep="\t")




Months <- Months %>% select(Exact_Month, patid, weight, Final_Box, DIA_only, DIA_OBE, OBE_only, OBE_dx, OBE_rx_exp_lapsed, OBE_rx_current) %>% ungroup() 

Months <- Months %>% mutate(Box=ifelse(DIA_only==1, "DIA_only", 
                             ifelse(DIA_OBE==1, "DIA_OBE",
                                    ifelse(OBE_only==1, "OBE_only",
                                           ifelse(OBE_dx==1, "OBE_dx",
                                                  ifelse(OBE_rx_exp_lapsed==1, "OBE_rx_exp_lapsed",
                                                         ifelse(OBE_rx_current==1, "OBE_rx_current", "other")))))))

Months <- Months %>% select(Exact_Month, patid, weight, Final_Box, Box)


Months %>% filter(Exact_Month==60) %>% group_by(Box) %>% summarise(n=sum(weight))

Months <- Months %>% mutate(flow=0) %>%
  group_by(patid) %>% mutate(flow=ifelse(Box!=lag(Box),1,0)) %>% mutate(flow=ifelse(is.na(flow),0,flow))


temp <- Months %>% filter(flow==1|lead(flow)==1)


temp2 <- temp %>% filter(lead(flow)==1) %>% select(Exact_Month, patid, weight, Box)  %>% rename("Source"="Box") %>%
  left_join(temp %>% filter(flow==1) %>% select(Exact_Month, patid, weight, Box) %>% rename("Dest"="Box") %>%  mutate(Exact_Month=Exact_Month-1) 
) %>% mutate(Exact_Month=Exact_Month+1)

unique(temp2$Dest)

temp2 <- temp2 %>% ungroup() %>% group_by(Exact_Month, Source, Dest) %>% summarise(n=sum(weight)) %>%
  spread(key=Dest, value=n)

temp2[is.na(temp2)] <-0

fwrite(temp2, "DANU_flows_MoM_Hendrik.txt", sep="\t")


temp2 %>% filter(Exact_Month>=49) %>% ungroup() %>% filter(Source=="DIA_only") %>% summarise(n=sum(DIA_OBE))


Months <- fread("DANU_MoM_Hendrik.txt", sep="\t")
DANU_Demographics <- fread("DANU Demographics 1.1/DANU Demographics.txt")
DANU_Demographics <- DANU_Demographics %>% select(patid, age)

Months <- Months %>% left_join(DANU_Demographics)

Months <- Months %>% select(Exact_Month, patid, weight, age, Final_Box, cumobe, cumdia,cumobe_dx, Current_OBE_Rx, Current_DIA_Rx, cumobe_Rx, DIA_only, DIA_OBE, OBE_only, OBE_dx, OBE_rx_exp_lapsed, OBE_rx_current)

fwrite(Months, "DANU_MoM_Hendrik.txt", sep="\t")


# ----------------------------------------

# SGLT2 GLP1 combos year over year ---------------
DANU_Ingredients       <- fread("DIA Analysis Results 1.1/DANU Ingredients.txt", integer64 = "character", stringsAsFactors = F)
DANU_Ingredients$drug_id <- unlist(lapply(DANU_Ingredients$drug_id, function(x) as.numeric(unlist(str_extract_all(x,"[:digit:]+$")))))
string_SGLT2 <- paste0("\\b(",paste0(DANU_Ingredients$drug_id[DANU_Ingredients$drug_group == "SGLT2"], collapse = "|"),")\\b")
string_GLP1 <- paste0("\\b(",paste0(DANU_Ingredients$drug_id[DANU_Ingredients$drug_group == "GLP1 Injectable"|DANU_Ingredients$drug_group == "GLP1 Oral"], collapse = "|"),")\\b")


DIA_Drug_Histories <- fread("DIA Analysis Results 1.1/DIA Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
DIA_Drug_Histories <- DIA_Drug_Histories %>% select(-c(disease)) 
DIA_Drug_Histories <- gather(DIA_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
DIA_Drug_Histories <- DIA_Drug_Histories %>% select(patient, Month, Treat, weight) %>% distinct() %>% filter(Treat!="-")
DIA_Drug_Histories <- DIA_Drug_Histories %>% mutate(GLP1_SGLT2_Status = ifelse(grepl(string_GLP1, Treat)&grepl(string_SGLT2, Treat), "Combo", 
                                                                               ifelse(grepl(string_GLP1, Treat), "GLP1",
                                                                                            ifelse(grepl(string_SGLT2, Treat), "SGLT2", "none"))))

DIA_Drug_Histories <- DIA_Drug_Histories %>% select(patient, Month, GLP1_SGLT2_Status, weight) %>% distinct() 

DIA_Drug_Histories$Month <- as.character(DIA_Drug_Histories$Month)
DIA_Drug_Histories$Month <- parse_number(DIA_Drug_Histories$Month)

DIA_Drug_Histories <- DIA_Drug_Histories %>% mutate(Year=ifelse(Month<=12, 1,
                                                                ifelse(Month<=24,2,
                                                                       ifelse(Month<=36,3,
                                                                              ifelse(Month<=48,4,5))))) %>% select(patient, Year, GLP1_SGLT2_Status, weight) %>% distinct() 



DIA_Drug_Histories <- DIA_Drug_Histories %>% mutate(Exp=1) %>% spread(key = GLP1_SGLT2_Status, value=Exp)
DIA_Drug_Histories[is.na(DIA_Drug_Histories)] <- 0

DIA_Drug_Histories <- DIA_Drug_Histories %>% mutate(Group=ifelse(Combo==1, "Combo",
                                           ifelse(GLP1==1&SGLT2==1, "Both_Sep",
                                                  ifelse(GLP1==1, "GLP1",
                                                         ifelse(SGLT2==1, "SGLT2", 
                                                                ifelse(none==1, "Other", NA))))))


DIA_Drug_Histories %>% group_by(Year, Group) %>% summarise(n=sum(weight)) %>% spread(key=Group, value=n)

# ------------
# Path to combos --------------------
DANU_Ingredients       <- fread("DIA Analysis Results 1.1/DANU Ingredients.txt", integer64 = "character", stringsAsFactors = F)
DANU_Ingredients$drug_id <- unlist(lapply(DANU_Ingredients$drug_id, function(x) as.numeric(unlist(str_extract_all(x,"[:digit:]+$")))))
string_SGLT2 <- paste0("\\b(",paste0(DANU_Ingredients$drug_id[DANU_Ingredients$drug_group == "SGLT2"], collapse = "|"),")\\b")
string_GLP1 <- paste0("\\b(",paste0(DANU_Ingredients$drug_id[DANU_Ingredients$drug_group == "GLP1 Injectable"|DANU_Ingredients$drug_group == "GLP1 Oral"], collapse = "|"),")\\b")


DIA_Drug_Histories <- fread("DIA Analysis Results 1.1/DIA Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
DIA_Drug_Histories <- DIA_Drug_Histories %>% select(-c(disease)) 
DIA_Drug_Histories <- gather(DIA_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
DIA_Drug_Histories <- DIA_Drug_Histories %>% select(patient, Month, Treat, weight) %>% distinct() %>% filter(Treat!="-")
DIA_Drug_Histories <- DIA_Drug_Histories %>% mutate(GLP1_SGLT2_Status = ifelse(grepl(string_GLP1, Treat)&grepl(string_SGLT2, Treat), "Combo", 
                                                                               ifelse(grepl(string_GLP1, Treat), "GLP1",
                                                                                            ifelse(grepl(string_SGLT2, Treat), "SGLT2", "none"))))

DIA_Drug_Histories <- DIA_Drug_Histories %>% select(patient, Month, Treat, GLP1_SGLT2_Status, weight) %>% distinct() 

DIA_Drug_Histories_2 <- DIA_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(GLP1_SGLT2_Status=="Combo")) which.max(!grepl("-",Treat)):which.max(GLP1_SGLT2_Status=="Combo") else NA) 


DIA_Drug_Histories_2 <- DIA_Drug_Histories_2 %>% select(patient, Treat, weight) %>% distinct()

DIA_Drug_Histories_2 <- separate_rows(DIA_Drug_Histories_2, Treat, sep = ",", convert=T)

DIA_Drug_Histories_2 <- DIA_Drug_Histories_2 %>% left_join(DANU_Ingredients %>% select(drug_id, drug_group), by=c("Treat"="drug_id"))
DIA_Drug_Histories_2 <- DIA_Drug_Histories_2 %>% select(-Treat) %>% distinct()


data.frame(DIA_Drug_Histories_2 %>% group_by(patient, weight) %>% arrange(drug_group) %>% mutate(drug_group=paste(drug_group, collapse="+"))  %>%
  ungroup() %>% distinct() %>%
  group_by(drug_group) %>% summarise(n=sum(weight)) %>%
  arrange(-n))
  
  
# -----------------------------------------
# Percentage not comorbid HFpEF CKD OSLAP NASH ----------------------

Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 <- fread("Mkt_Comorbidity_Groups_OBE_adjusted_Jun22.txt", sep="\t")
Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 <- Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 %>% filter(diagnosis=="Obesity"|diagnosis=="Diabetes + Obesity")
sum(Mkt_Comorbidity_Groups_OBE_adjusted_Jun22$weight)



DANU_Measures <- fread("DANU Measures 1.1/DANU Measures.txt",  integer64 = "character", stringsAsFactors = F)
BMI <- DANU_Measures %>% filter(test=="BMI") %>% select(patid, value)
BMI <- BMI %>% select(patid, value) %>% distinct() %>% group_by(patid ) %>% filter(value==max(value)) %>% slice(1)
BMI <- BMI %>%  filter(value>27) %>% mutate(value=ifelse(value<=30, "27-30",
                                                 ifelse(value<=35, "30-35", ">35")))

NASH_Drug_Histories <- fread("NASH Analysis Results 1.1/NASH Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
NASH_Drug_Histories <- NASH_Drug_Histories %>% select(patient) %>% rename("patid"="patient") %>% mutate(NASH=1)

Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 <- Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 %>% left_join(BMI) %>%
  left_join(NASH_Drug_Histories) %>%
  select(patid, weight, value, HFpEF, CKD, NASH, OSLAP) %>%
  mutate(NASH=ifelse(is.na(NASH),0,NASH)) %>% drop_na()

Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 %>% group_by(value) %>%summarise(n=sum(weight)) %>%
  left_join(Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 %>% filter(HFpEF==0&CKD==0&OSLAP==0&NASH==0)  %>%
              group_by(value) %>%summarise(n2=sum(weight))) %>%
  mutate(perc=n2/n)

# --------------------

# Lifecycle indication GLP1 forecast --------------------------------

# CKD
Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 <- fread("Mkt_Comorbidity_Groups_OBE_adjusted_Jun22.txt", sep="\t")
Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 <- Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 %>% filter(diagnosis=="Obesity"|diagnosis=="Diabetes + Obesity")
sum(Mkt_Comorbidity_Groups_OBE_adjusted_Jun22$weight)

DANU_Measures <- fread("DANU Measures 1.1/DANU Measures.txt",  integer64 = "character", stringsAsFactors = F)
BMI <- DANU_Measures %>% filter(test=="BMI") %>% select(patid, value)
BMI <- BMI %>% select(patid, value) %>% distinct() %>% group_by(patid ) %>% filter(value==max(value)) %>% slice(1)
BMI <- BMI %>%  filter(value>27) %>% mutate(value=ifelse(value<=30, "27-30",
                                                 ifelse(value<=35, "30-35", ">35")))

Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 <- Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 %>% left_join(BMI) %>% drop_na()

Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 %>% group_by(value, diagnosis, CKD) %>% summarise(n=sum(weight)) %>%
  spread(key=CKD, value = n) %>% mutate(perc=`1`/(`1`+`0`))




DANU_Ingredients <- fread("DIA Analysis Results 1.1/DANU Ingredients.txt", integer64 = "character", stringsAsFactors = F)
DANU_Ingredients <- DANU_Ingredients %>%  separate(drug_id, c('class', 'molecule'))
DANU_Ingredients  <- DANU_Ingredients %>% select(molecule, drug_group)
names(DANU_Ingredients)[1] <- "Drugs"
DANU_Ingredients$Drugs <- as.numeric(DANU_Ingredients$Drugs)
string_GLP1  <- paste0("\\b(",paste0(DANU_Ingredients$Drugs[DANU_Ingredients$drug_group == "GLP1 Injectable"|DANU_Ingredients$drug_group == "GLP1 Oral"], collapse = "|"),")\\b")
string_SGLT2  <- paste0("\\b(",paste0(DANU_Ingredients$Drugs[DANU_Ingredients$drug_group == "SGLT2"], collapse = "|"),")\\b")

DIA_Drug_Histories <- fread("DIA Analysis Results 1.1/DIA Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
OBE2_Drug_Histories <- fread("OBE2 Analysis Results 1.1/OBE2 Drug Histories.txt", integer64 = "character", stringsAsFactors = F)

Drug_Histories <-  DIA_Drug_Histories %>% select(-disease) %>%
  bind_rows(OBE2_Drug_Histories %>% select(-disease))


Drug_Histories <- gather(Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
Drug_Histories <- Drug_Histories %>% filter(Drugs!="-")
Drug_Histories$Month <- as.character(Drug_Histories$Month)
Drug_Histories$Month <- parse_number(Drug_Histories$Month)
Drug_Histories <- Drug_Histories %>% filter(Month>=49)
Drug_Histories <- Drug_Histories %>% select(patient, Drugs) %>% distinct()

GLP1s <- Drug_Histories %>% filter(grepl(string_GLP1, Drugs)) %>% select(patient) %>% distinct() %>% mutate(GLP1="GLP1")
SGLT2 <- Drug_Histories %>% filter(grepl(string_SGLT2, Drugs)) %>% select(patient) %>% distinct() %>% mutate(SGLT2="SGLT2")


Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 %>% filter(CKD==1) %>%
  left_join(GLP1s, by=c("patid"="patient")) %>%
  group_by(value, diagnosis, GLP1) %>% summarise(n=sum(weight)) %>%
  spread(key=GLP1, value = n) %>% mutate(perc=GLP1/(GLP1+`<NA>`))



Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 %>% filter(CKD==1) %>%
  left_join(SGLT2, by=c("patid"="patient")) %>%
  group_by(value, diagnosis, SGLT2) %>% summarise(n=sum(weight)) %>%
  spread(key=SGLT2, value = n) %>% mutate(perc=SGLT2/(SGLT2+`<NA>`))




NAFLD_Drug_Histories <- fread("NAFLD Analysis Results 1.1/NAFLD Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
NAFLD_Drug_Histories <- NAFLD_Drug_Histories %>% select(patient) %>% mutate(NAFLD=1)

NAFLD_Doses <- fread("NAFLD Analysis Results 1.1/NAFLD Doses.txt", colClasses = "character",)
NAFLD_Doses <- NAFLD_Doses %>% filter(paid=="P")
NAFLD_Doses <- NAFLD_Doses %>% filter(from_dt>="2021-07-01")
NAFLD_Doses <- NAFLD_Doses %>% filter(drug_class=="Statin") %>% select(pat_id, generic_name, drug_id, from_dt) %>% distinct()

NASH_Medication_Surveys <- fread("NASH Analysis Results 1.1/NASH Medication Surveys.txt")
NAFLD_Doses <- NAFLD_Doses %>% left_join(NASH_Medication_Surveys %>% select(drug_id, med_strength))
NAFLD_Doses %>% select(generic_name, med_strength) %>% distinct() %>% arrange(generic_name, med_strength)
Rosu <- NAFLD_Doses %>% filter(generic_name=="Rosuvastatin" & grepl("40", med_strength)) %>% select(pat_id) %>% distinct() %>% mutate(Rosu=1)
names(Rosu)[1] <- "patid"
Ator <- NAFLD_Doses %>% filter(generic_name=="Atorvastatin" & grepl("80", med_strength)) %>% select(pat_id) %>% distinct() %>% mutate(Ator=1)
names(Ator)[1] <- "patid"



Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 %>% filter(CKD==1) %>%
  inner_join(NAFLD_Drug_Histories, by=c("patid"="patient")) %>%
  left_join(Rosu) %>%
   group_by(value, diagnosis, Rosu) %>% summarise(n=sum(weight)) %>%
  spread(key=Rosu, value = n) %>% mutate(perc=`1`/(`1`+`<NA>`))



Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 %>% filter(CKD==1) %>%
  inner_join(NAFLD_Drug_Histories, by=c("patid"="patient")) %>%
  left_join(Ator) %>%
   group_by(value, diagnosis, Ator) %>% summarise(n=sum(weight)) %>%
  spread(key=Ator, value = n) %>% mutate(perc=`1`/(`1`+`<NA>`))






# OLD DATA CKD

# CKD Target MAONG OLD DIABETES DATA
DANU_Measures <- fread("DANU Measures.txt")
DANU_Measures <- DANU_Measures %>% filter(test=="BMI")
names(DANU_Measures)[1] <- "patient"
DANU_Measures <- DANU_Measures %>% group_by(patient) %>%filter(value==max(value)) %>% slice(1) %>% 
  filter(value>=27) %>% select(patient, weight, value ) %>% mutate(value=ifelse(value<=30, "27-30",
                                                 ifelse(value<=35, "30-35", ">35")))

DANU_Demographics <- fread("DANU Demographics.txt")
DANU_Demographics <- DANU_Demographics %>% filter(diagnosis=="Obesity"|diagnosis=="Diabetes + Obesity") %>% select(patid, diagnosis, weight)
names(DANU_Demographics)[1] <- "patient"

DANU_Measures <- DANU_Demographics %>% inner_join(DANU_Measures)

CKD_Stages_Complete_FilledIn <- fread("CKD_Stages_Complete_FilledIn.txt")
CKD_Stages_Complete_FilledIn <- CKD_Stages_Complete_FilledIn %>% filter(Stage=="Stage3"|Stage=="Stage4") %>% select(patient) %>% mutate(CKD=1)

DANU_Measures %>% left_join(CKD_Stages_Complete_FilledIn) %>% group_by(value, diagnosis, CKD) %>% summarise(n=sum(weight)) %>%
  spread(key=CKD, value = n) %>% mutate(perc=`1`/(`1`+`<NA>`))

CKD_Drug_Histories <- fread("CKD Drug Histories.txt" )
CKD_Drug_Histories <- gather(CKD_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
CKD_Drug_Histories$Month <- as.character(CKD_Drug_Histories$Month)
CKD_Drug_Histories$Month <- parse_number(CKD_Drug_Histories$Month)
CKD_Drug_Histories <- CKD_Drug_Histories %>% filter(Month>=49)
CKD_Drug_Histories <- CKD_Drug_Histories %>% filter(Drugs!="-") %>% select(patient) %>% distinct()
CKD_Drug_Histories$Treated <- 1


DANU_Measures %>% left_join(CKD_Stages_Complete_FilledIn) %>% filter(CKD==1) %>%
  left_join(CKD_Drug_Histories) %>%
  group_by(value, diagnosis, Treated) %>% summarise(n=sum(weight)) %>%
  spread(key=Treated, value = n) %>% mutate(perc=`1`/(`1`+`<NA>`))





# HF
Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 <- fread("Mkt_Comorbidity_Groups_OBE_adjusted_Jun22.txt", sep="\t")
Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 <- Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 %>% filter(diagnosis=="Obesity"|diagnosis=="Diabetes + Obesity")
sum(Mkt_Comorbidity_Groups_OBE_adjusted_Jun22$weight)

DANU_Measures <- fread("DANU Measures 1.1/DANU Measures.txt",  integer64 = "character", stringsAsFactors = F)
BMI <- DANU_Measures %>% filter(test=="BMI") %>% select(patid, value)
BMI <- BMI %>% select(patid, value) %>% distinct() %>% group_by(patid ) %>% filter(value==max(value)) %>% slice(1)
BMI <- BMI %>%  filter(value>27) %>% mutate(value=ifelse(value<=30, "27-30",
                                                 ifelse(value<=35, "30-35", ">35")))

Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 <- Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 %>% left_join(BMI) %>% drop_na()

Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 %>% group_by(value, diagnosis, HFpEF) %>% summarise(n=sum(weight)) %>%
  spread(key=HFpEF, value = n) %>% mutate(perc=`1`/(`1`+`0`))



DANU_Ingredients <- fread("DIA Analysis Results 1.1/DANU Ingredients.txt", integer64 = "character", stringsAsFactors = F)
DANU_Ingredients <- DANU_Ingredients %>%  separate(drug_id, c('class', 'molecule'))
DANU_Ingredients  <- DANU_Ingredients %>% select(molecule, drug_group)
names(DANU_Ingredients)[1] <- "Drugs"
DANU_Ingredients$Drugs <- as.numeric(DANU_Ingredients$Drugs)
string_GLP1  <- paste0("\\b(",paste0(DANU_Ingredients$Drugs[DANU_Ingredients$drug_group == "GLP1 Injectable"|DANU_Ingredients$drug_group == "GLP1 Oral"], collapse = "|"),")\\b")
string_SGLT2  <- paste0("\\b(",paste0(DANU_Ingredients$Drugs[DANU_Ingredients$drug_group == "SGLT2"], collapse = "|"),")\\b")

DIA_Drug_Histories <- fread("DIA Analysis Results 1.1/DIA Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
OBE2_Drug_Histories <- fread("OBE2 Analysis Results 1.1/OBE2 Drug Histories.txt", integer64 = "character", stringsAsFactors = F)

Drug_Histories <-  DIA_Drug_Histories %>% select(-disease) %>%
  bind_rows(OBE2_Drug_Histories %>% select(-disease))

Drug_Histories <- gather(Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
Drug_Histories <- Drug_Histories %>% filter(Drugs!="-")
Drug_Histories$Month <- as.character(Drug_Histories$Month)
Drug_Histories$Month <- parse_number(Drug_Histories$Month)
Drug_Histories <- Drug_Histories %>% filter(Month>=49)
Drug_Histories <- Drug_Histories %>% select(patient, Drugs) %>% distinct()

GLP1s <- Drug_Histories %>% filter(grepl(string_GLP1, Drugs)) %>% select(patient) %>% distinct() %>% mutate(GLP1="GLP1")
SGLT2 <- Drug_Histories %>% filter(grepl(string_SGLT2, Drugs)) %>% select(patient) %>% distinct() %>% mutate(SGLT2="SGLT2")


Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 %>% filter(HFpEF==1) %>%
  left_join(GLP1s, by=c("patid"="patient")) %>%
  group_by(value, diagnosis, GLP1) %>% summarise(n=sum(weight)) %>%
  spread(key=GLP1, value = n) %>% mutate(perc=GLP1/(GLP1+`<NA>`))


Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 %>% filter(HFpEF==1) %>%
  left_join(SGLT2, by=c("patid"="patient")) %>%
  group_by(value, diagnosis, SGLT2) %>% summarise(n=sum(weight)) %>%
  spread(key=SGLT2, value = n) %>% mutate(perc=SGLT2/(SGLT2+`<NA>`))




NAFLD_Drug_Histories <- fread("NAFLD Analysis Results 1.1/NAFLD Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
NAFLD_Drug_Histories <- NAFLD_Drug_Histories %>% select(patient) %>% mutate(NAFLD=1)

NAFLD_Doses <- fread("NAFLD Analysis Results 1.1/NAFLD Doses.txt", colClasses = "character",)
NAFLD_Doses <- NAFLD_Doses %>% filter(paid=="P")
NAFLD_Doses <- NAFLD_Doses %>% filter(from_dt>="2021-07-01")
NAFLD_Doses <- NAFLD_Doses %>% filter(drug_class=="Statin") %>% select(pat_id, generic_name, drug_id, from_dt) %>% distinct()

NASH_Medication_Surveys <- fread("NASH Analysis Results 1.1/NASH Medication Surveys.txt")
NAFLD_Doses <- NAFLD_Doses %>% left_join(NASH_Medication_Surveys %>% select(drug_id, med_strength))
NAFLD_Doses %>% select(generic_name, med_strength) %>% distinct() %>% arrange(generic_name, med_strength)
Rosu <- NAFLD_Doses %>% filter(generic_name=="Rosuvastatin" & grepl("40", med_strength)) %>% select(pat_id) %>% distinct() %>% mutate(Rosu=1)
names(Rosu)[1] <- "patid"
Ator <- NAFLD_Doses %>% filter(generic_name=="Atorvastatin" & grepl("80", med_strength)) %>% select(pat_id) %>% distinct() %>% mutate(Ator=1)
names(Ator)[1] <- "patid"



Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 %>% filter(HFpEF==1) %>%
  inner_join(NAFLD_Drug_Histories, by=c("patid"="patient")) %>%
  left_join(Rosu) %>%
   group_by(value, diagnosis, Rosu) %>% summarise(n=sum(weight)) %>%
  spread(key=Rosu, value = n) %>% mutate(perc=`1`/(`1`+`<NA>`))



Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 %>% filter(HFpEF==1) %>%
  inner_join(NAFLD_Drug_Histories, by=c("patid"="patient")) %>%
  left_join(Ator) %>%
   group_by(value, diagnosis, Ator) %>% summarise(n=sum(weight)) %>%
  spread(key=Ator, value = n) %>% mutate(perc=`1`/(`1`+`<NA>`))





# OLD DATA HF

# HF Target MAONG OLD DIABETES DATA
DANU_Measures <- fread("DANU Measures.txt")
DANU_Measures <- DANU_Measures %>% filter(test=="BMI")
names(DANU_Measures)[1] <- "patient"
DANU_Measures <- DANU_Measures %>% group_by(patient) %>%filter(value==max(value)) %>% slice(1) %>% 
  filter(value>=27) %>% select(patient, weight, value ) %>% mutate(value=ifelse(value<=30, "27-30",
                                                 ifelse(value<=35, "30-35", ">35")))

DANU_Demographics <- fread("DANU Demographics.txt")
DANU_Demographics <- DANU_Demographics %>% filter(diagnosis=="Obesity"|diagnosis=="Diabetes + Obesity") %>% select(patid, diagnosis, weight)
names(DANU_Demographics)[1] <- "patient"

DANU_Measures <- DANU_Demographics %>% inner_join(DANU_Measures)

Predicted_Stages_gbm_All <- fread("Predicted_Stages_gbm_All.txt")
First_Diastolic_All_L5y <- fread("First_Diastolic_All_L5y.txt")
First_Diastolic_All_L5y <- First_Diastolic_All_L5y %>% inner_join(Predicted_Stages_gbm_All) %>% filter(Predicted.Stage==2|Predicted.Stage==3|Predicted.Stage==4) %>% select(-earliest)
First_Diastolic_All_L5y <- First_Diastolic_All_L5y %>% mutate(HF=1)

DANU_Measures %>% left_join(First_Diastolic_All_L5y) %>% group_by(value, diagnosis, HF) %>% summarise(n=sum(weight)) %>%
  spread(key=HF, value = n) %>% mutate(perc=`1`/(`1`+`<NA>`))

HF_Drug_Histories <- fread("HF Drug Histories.txt" )
HF_Drug_Histories <- gather(HF_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
HF_Drug_Histories$Month <- as.character(HF_Drug_Histories$Month)
HF_Drug_Histories$Month <- parse_number(HF_Drug_Histories$Month)
HF_Drug_Histories <- HF_Drug_Histories %>% filter(Month>=49)
HF_Drug_Histories <- HF_Drug_Histories %>% filter(Drugs!="-") %>% select(patient) %>% distinct()
HF_Drug_Histories$Treated <- 1


DANU_Measures %>% left_join(First_Diastolic_All_L5y) %>% filter(HF==1) %>%
  left_join(HF_Drug_Histories) %>%
  group_by(value, diagnosis, Treated) %>% summarise(n=sum(weight)) %>%
  spread(key=Treated, value = n) %>% mutate(perc=`1`/(`1`+`<NA>`))




# ----------------------------
# Obesity only funnel ---------------------------------

Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 <- fread("Mkt_Comorbidity_Groups_OBE_adjusted_Jun22.txt", sep="\t")
Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 <- Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 %>% filter(diagnosis=="Obesity"|diagnosis=="Diabetes + Obesity")
sum(Mkt_Comorbidity_Groups_OBE_adjusted_Jun22$weight)
Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 %>% group_by(diagnosis) %>% summarise(n=sum(weight))

DANU_Measures <- fread("DANU Measures 1.1/DANU Measures.txt",  integer64 = "character", stringsAsFactors = F)
BMI <- DANU_Measures %>% filter(test=="BMI") %>% select(patid, value)
BMI <- BMI %>% select(patid, value) %>% distinct() %>% group_by(patid ) %>% filter(value==max(value)) %>% slice(1)
BMI <- BMI %>%  filter(value>=27) %>% mutate(value=ifelse(value<30, "27-30",
                                                 ifelse(value<35, "30-35", ">35")))

Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 <- Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 %>% left_join(BMI) %>% drop_na()

Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 %>% group_by(diagnosis) %>% summarise(n=sum(weight))

Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 <- Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 %>% mutate(weight=ifelse(diagnosis=="Diabetes + Obesity", weight*1.684745 , weight))
Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 <- Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 %>% select(-c(PCOS, PAD_restrict, ASCVD, Comorb))

Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 %>% filter(diagnosis=="Obesity") %>% group_by(value) %>% summarise(n=sum(weight))
Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 <- Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 %>% filter(diagnosis=="Obesity")


NASH_Drug_Histories <- fread("NASH Analysis Results 1.1/NASH Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
NASH_Drug_Histories <- NASH_Drug_Histories %>% select(patient) %>% rename("patid"="patient") %>% mutate(NASH=1)


Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 <- Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 %>% 
  left_join(NASH_Drug_Histories) %>%
  select(patid, weight, diagnosis, value, HFpEF, CKD, NASH, OSLAP) %>%
  mutate(NASH=ifelse(is.na(NASH),0,NASH)) %>% drop_na()

Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 %>% filter(HFpEF==1|CKD==1|OSLAP==1|NASH==1) %>%
  group_by(value) %>% summarise(n=sum(weight))

Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 <- Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 %>% filter(HFpEF==0&CKD==0&OSLAP==0&NASH==0) %>% select(-c(HFpEF, CKD, OSLAP, NASH)) 
Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 %>% group_by(value) %>% summarise(n=sum(weight))

DANU_Events <- fread("DANU Events 1.1/DANU Events.txt")
DANU_Events <- DANU_Events %>% filter(claimed>="2021-05-31"&claimed<="2022-04-30")  %>% select(patid) %>% distinct() 

Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 %>% inner_join(DANU_Events) %>% group_by(value) %>% summarise(n=sum(weight))
Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 <- Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 %>% inner_join(DANU_Events) 

OBE2_Doses <- fread("OBE2 Analysis Results 1.1/OBE2 Doses.txt", integer64 = "character", stringsAsFactors = F)
OBE2_Doses <- OBE2_Doses %>% filter(from_dt>="2021-05-31"&from_dt<="2022-04-30")  %>% select(pat_id) %>% distinct() %>% rename("patid"="pat_id")

Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 %>% inner_join(OBE2_Doses) %>% group_by(value) %>% summarise(n=sum(weight))
Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 <- Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 %>% inner_join(OBE2_Doses) 

OBE2_Doses <- fread("OBE2 Analysis Results 1.1/OBE2 Doses.txt", integer64 = "character", stringsAsFactors = F)
OBE2_Doses <- OBE2_Doses %>% filter(from_dt>="2021-05-31"&from_dt<="2022-04-30")  %>% filter(grepl("GLP1", drug_group)) %>% select(pat_id) %>% distinct() %>% rename("patid"="pat_id")

Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 %>% inner_join(OBE2_Doses) %>% group_by(value) %>% summarise(n=sum(weight))
Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 <- Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 %>% inner_join(OBE2_Doses) 


OBE2_Doses <- fread("OBE2 Analysis Results 1.1/OBE2 Doses.txt", integer64 = "character", stringsAsFactors = F)
OBE2_Doses <- OBE2_Doses %>% filter(from_dt>="2021-05-31"&from_dt<="2022-04-30")  %>% filter(grepl("Oral", drug_group)) %>% select(pat_id) %>% distinct() %>% rename("patid"="pat_id")

Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 %>% inner_join(OBE2_Doses) %>% group_by(value) %>% summarise(n=sum(weight))
Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 <- Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 %>% inner_join(OBE2_Doses) 

# ------------------------

# GLP1 Injectable big BMi drop vs small BMI drop -------------------

DANU_Ingredients <- fread("DIA Analysis Results 1.1/DANU Ingredients.txt", integer64 = "character", stringsAsFactors = F)
DANU_Ingredients <- DANU_Ingredients %>%  separate(drug_id, c('class', 'molecule'))
DANU_Ingredients <- DANU_Ingredients %>% select(molecule, drug_group)
names(DANU_Ingredients)[1] <- "Drugs"
DANU_Ingredients$Drugs <- as.numeric(DANU_Ingredients$Drugs)

string_InjectableGLP1 <- paste0("\\b(",paste0(DANU_Ingredients$Drugs[DANU_Ingredients$drug_group == "GLP1 Injectable"], collapse = "|"),")\\b")

DIA_Drug_Histories <- read.table("DIA Analysis Results 1.1/DIA Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
DIA_Drug_Histories <- gather(DIA_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
DIA_Drug_Histories$Month <- as.character(DIA_Drug_Histories$Month)
DIA_Drug_Histories$Month <- parse_number(DIA_Drug_Histories$Month)
DIA_Drug_Histories <- DIA_Drug_Histories %>% select(-c(disease, weight)) %>% mutate(ON=ifelse(grepl(string_InjectableGLP1, Drugs), "ON", "OFF"))
DIA_Drug_Histories <- DIA_Drug_Histories %>% select(-c(Drugs))
unique(DIA_Drug_Histories$ON)

Months_lookup <- fread("Months_lookup.txt")
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")
  
DANU_Measures <- fread("DANU Measures 1.1/DANU Measures.txt",  integer64 = "character", stringsAsFactors = F)
DANU_Measures <- DANU_Measures %>% select(patid, weight, value, test, claimed) %>% distinct()
DANU_Measures <- DANU_Measures %>% filter(test=="BMI")

DANU_Measures$Month_Yr <- format(as.Date(DANU_Measures$claimed), "%Y-%m")
DANU_Measures <- DANU_Measures %>% left_join(Months_lookup, by = c("Month_Yr" = "Month")) %>%  filter(!is.na(Exact_Month)) 
DANU_Measures <- DANU_Measures %>% select(patid, weight, value, Exact_Month)
DANU_Measures <- DANU_Measures %>% group_by(patid, weight, Exact_Month) %>% summarise(value=max(value))
names(DANU_Measures)[1] <- "patient"
DANU_Measures <- DANU_Measures %>% ungroup() %>% select(-weight)
names(DANU_Measures)[2] <- "Month"

DANU_Measures <- DANU_Measures %>% inner_join(DIA_Drug_Histories)

unique(DANU_Measures$ON)

Starts <- DANU_Measures %>% filter(ON=="ON") %>% group_by(patient) %>% filter(Month==min(Month)) %>% 
  slice(1) %>% ungroup() %>% select(patient, Month) %>% rename("Start"="Month")

DANU_Measures <- Starts %>% inner_join(DANU_Measures) %>% mutate(Month=Month-Start)

DANU_Measures <- DANU_Measures %>% filter(Month<=36&Month>=(-36))

DANU_Measures <- DANU_Measures %>%  filter(Month>=(-24) & Month<=24) 

temp <- DANU_Measures %>% group_by(patient) %>% filter(value==max(value) & Month<0 ) %>% filter(Month==max(Month)) %>% select(patient, Month, value)  %>% rename("MonthMax"="Month", "Max"="value") %>%
  inner_join(
DANU_Measures %>% group_by(patient) %>% filter(value==min(value) & Month>0) %>% filter(Month==max(Month)) %>%  select(patient, Month, value)  %>% rename("MonthMin"="Month", "Min"="value")
) %>% mutate(Diff=(Min-Max)/Max) %>%
  inner_join(DANU_Measures)

temp %>% select(Diff) %>%
  ggplot(aes(Diff)) +
  geom_density()

temp <- temp %>% mutate(Diff=ifelse(Diff>=(-0.05), "-5%",
                            ifelse(Diff>=(-0.1), "-10%",
                                   ifelse(Diff>=(-0.15), "-15%",
                                          ifelse(Diff>=(-0.2), "-20%",
                                                 ifelse(Diff>=(-0.25), "-25%",
                                                        ifelse(Diff>=(-0.3), "-30%",
                                                               ifelse(Diff>=(-0.35), "-35%",
                                                                      ifelse(Diff>=(-0.4), "-40%", "-40%")))))))))


temp <- temp %>% mutate(Diff = factor(Diff, levels=c("-5%", "-10%", "-15%", "-20%", "-25%", "-30%", "-35%", "-40%"))) 



temp %>% 
  filter(Month>=(-24) & Month<=24) %>%
  filter(Diff!="-30%"&Diff!="-35%"&Diff!="-40%") %>%
  ggplot(aes(Month, value, colour=Diff, fill=Diff)) +
  geom_smooth(se=F) +
  theme_minimal() +
  xlab("\n Elapsed Months relative to GLP1 Initiation") + ylab("BMI \n") +
  scale_colour_viridis_d()
  

temp %>%
    filter(Month>=(-24) & Month<=24) %>%
  filter(ON=="ON") %>%
  group_by(patient) %>% filter(Month==max(Month)) %>%
  select(patient, Diff, Month) %>%
  group_by(Diff)  %>% summarise(mean=mean(Month))

# 1 -5%    8.27
# 2 -10%   9.86
# 3 -15%  11.5 
# 4 -20%  11.7 
# 5 -25%  11.8 
# 6 -30%  11.1 
# 7 -35%  10.4 
# 8 -40%  10.3 


DANU_Demographics <- fread("DIA Analysis Results 1.1/DANU Demographics.txt")
DANU_Demographics <- DANU_Demographics %>% select(patid, gender, age)

temp %>% inner_join(DANU_Demographics, by=c("patient"="patid")) %>%
  select(patient, gender, Diff) %>% distinct() %>%
  group_by(Diff, gender) %>% count() %>%
  spread(key=gender, value=n)

#   Diff      F     M
# 1 -5%     170   167
# 2 -10%    577   527
# 3 -15%    529   390
# 4 -20%    316   178
# 5 -25%    154    77
# 6 -30%     55    31
# 7 -35%     46    19
# 8 -40%     50    26

temp %>% inner_join(DANU_Demographics, by=c("patient"="patid")) %>%
  select(patient, gender, Diff) %>% distinct() %>%
  group_by(gender) %>% summarise(n=mean(Diff))


temp %>% inner_join(DANU_Demographics, by=c("patient"="patid")) %>%
  select(patient, gender, Diff) %>% distinct() %>%
  group_by(gender) %>% count()

# 1 F       1897
# 2 M       1415


temp %>% inner_join(DANU_Demographics, by=c("patient"="patid")) %>%
  select(patient, gender, Diff) %>% distinct() %>%
  filter(Diff>=(-0.4)) %>%
  group_by(gender) %>% sample_n(1000) %>% ungroup() %>%
  ggplot(aes(Diff, colour=gender, fill=gender)) +
  geom_density(alpha=0.5) +
  theme_minimal()


temp %>% inner_join(DANU_Demographics, by=c("patient"="patid")) %>%
  select(patient, age, Diff) %>% distinct() %>%
  group_by(Diff) %>% summarise(n=mean(age))


DIA_Drug_Histories <- fread("DIA Analysis Results 1.1/DIA Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
DIA_Drug_Histories <- DIA_Drug_Histories %>% select(-c(disease))
DIA_Drug_Histories <- gather(DIA_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
DIA_Drug_Histories <- DIA_Drug_Histories %>% filter(Drugs!="-") %>% select(patient, Drugs) %>% distinct()
DIA_Drug_Histories <- DIA_Drug_Histories %>% group_by(patient) %>% count()
temp %>% left_join(DIA_Drug_Histories) %>% group_by(Diff) %>% summarise(lines=mean(n))



DIA_Flows_Aux_Long <- fread("DIA Analysis Results 1.1/DIA_Flows_Aux_Long.txt", integer64 = "character", stringsAsFactors = F)
DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% select(patient, flow, stops)
DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% filter(flow==1&stops==0) %>% group_by(patient) %>% summarise(n=sum(flow))
temp %>% left_join(DIA_Flows_Aux_Long) %>% group_by(Diff) %>% summarise(flows=mean(n))

temp %>% select(patient, Diff) %>% distinct() %>% group_by(Diff) %>% count()

DIA_Comorbidity_Inventories <- fread("DIA Analysis Results 1.1/DIA Comorbidity Inventories.txt")
DIA_Comorbidity_Inventories <- temp %>% select(patient, Diff) %>% distinct() %>% left_join(DIA_Comorbidity_Inventories, by=c("patient"="patid"))


DIA_Comorbidity_Inventories <- DIA_Comorbidity_Inventories %>% group_by(Diff, diagnosis) %>% count() %>%
  spread(key=Diff, value=n) %>%
  filter(grepl("A", diagnosis)|
           grepl("B", diagnosis)|
           grepl("C", diagnosis)|
           grepl("D", diagnosis)|
           grepl("E", diagnosis)|
           grepl("F", diagnosis)|
           grepl("G", diagnosis)|
           grepl("H", diagnosis)|
           grepl("I", diagnosis)|
           grepl("J", diagnosis)|
           grepl("K", diagnosis)|
           grepl("L", diagnosis)|
           grepl("M", diagnosis)|
           grepl("N", diagnosis))

DIA_Comorbidity_Inventories[is.na(DIA_Comorbidity_Inventories)] <- 0

DIA_Comorbidity_Inventories$`-5%` <- DIA_Comorbidity_Inventories$`-5%` / 337
DIA_Comorbidity_Inventories$`-10%` <- DIA_Comorbidity_Inventories$`-10%` / 1104
DIA_Comorbidity_Inventories$`-15%` <- DIA_Comorbidity_Inventories$`-15%` / 919
DIA_Comorbidity_Inventories$`-20%` <- DIA_Comorbidity_Inventories$`-20%` / 494
DIA_Comorbidity_Inventories$`-25%` <- DIA_Comorbidity_Inventories$`-25%` / 231
DIA_Comorbidity_Inventories$`-30%` <- DIA_Comorbidity_Inventories$`-30%` / 86
DIA_Comorbidity_Inventories$`-35%` <- DIA_Comorbidity_Inventories$`-35%` / 65
DIA_Comorbidity_Inventories$`-40%` <- DIA_Comorbidity_Inventories$`-40%` / 76

data.frame(DIA_Comorbidity_Inventories %>% filter(`-40%`>=0.1))[112:146,]



# ---------------------------
# GLP1 Injectable + Insulin fixed combo ------------------------------
#DANU_Demographics <- fread("DIA Analysis Results 1.1/DANU Demographics.txt")
#DANU_Demographics <- DANU_Demographics %>% filter(diabetes_onset>="2020-05-31") %>% select(patid)  %>% rename("pat_id"="patid")

Dia_US_Doses <- fread("DIA Doses 1.1/DIA Doses.txt")
#Dia_US_Doses <- DANU_Demographics %>% inner_join(Dia_US_Doses)
Dia_US_Doses <- Dia_US_Doses %>% filter(from_dt>="2020-05-31")  
Dia_US_Doses <- Dia_US_Doses %>% select(-c(prov, prov_type, specialty, taxonomy1, taxonomy2, status))
Dia_US_Doses <- Dia_US_Doses %>% filter(paid=="P")

#Fixed Combo
#8:51:337:11571
#8:52:339:11574
#8:52:339:11575


unique(Dia_US_Doses$drug_class)

Dia_US_Doses %>% filter(drug_id=="8:51:337:11571"|drug_id=="8:52:339:11575"|drug_id=="8:52:339:11574") %>%
  summarise(n=sum(weight))

Dia_US_Doses %>% filter(drug_id=="8:51:337:11571"|drug_id=="8:52:339:11575"|drug_id=="8:52:339:11574") %>%
  select(pat_id, weight) %>% distinct() %>% summarise(n=sum(weight))

Dia_US_Doses %>% filter(drug_class=="GLP1 Injectable") %>%
  summarise(n=sum(weight))

Dia_US_Doses %>% filter(drug_class=="GLP1 Injectable") %>%
  select(pat_id, weight) %>% distinct() %>% summarise(n=sum(weight))

Dia_US_Doses %>% filter(drug_class=="Insulin Long") %>%
  summarise(n=sum(weight))

Dia_US_Doses %>% filter(drug_class=="Insulin Long") %>%
  select(pat_id, weight) %>% distinct() %>% summarise(n=sum(weight))


Dia_US_Doses %>% filter(drug_group=="Insulin") %>% select(pat_id) %>% distinct() %>%
  inner_join(Dia_US_Doses %>% filter(drug_group=="GLP1 Injectable") %>% select(pat_id) %>% distinct()) %>%
  inner_join(Dia_US_Doses) %>%
   filter(drug_class=="GLP1 Injectable"|drug_group=="Insulin") %>%
  summarise(n=sum(weight))

Dia_US_Doses %>% filter(drug_group=="Insulin") %>% select(pat_id) %>% distinct() %>%
  inner_join(Dia_US_Doses %>% filter(drug_group=="GLP1 Injectable") %>% select(pat_id) %>% distinct()) %>%
  inner_join(Dia_US_Doses) %>%
   filter(drug_class=="GLP1 Injectable"|drug_group=="Insulin") %>%
  select(pat_id, weight) %>% distinct() %>% summarise(n=sum(weight))



Dia_US_Doses %>% filter(drug_group=="Insulin") %>% select(pat_id) %>% distinct() %>%
  inner_join(Dia_US_Doses %>% filter(drug_group=="GLP1 Injectable") %>% select(pat_id) %>% distinct()) %>%
  inner_join(Dia_US_Doses) %>%
   filter(drug_class=="GLP1 Injectable"|drug_class=="Insulin Long") %>%
  summarise(n=sum(weight))


Dia_US_Doses %>% filter(drug_group=="Insulin") %>% select(pat_id, from_dt) %>% mutate(from_dt = str_sub(from_dt, 1L, 7L)) %>% distinct() %>%
  inner_join(Dia_US_Doses %>% filter(drug_group=="GLP1 Injectable") %>% select(pat_id, from_dt) %>% mutate(from_dt = str_sub(from_dt, 1L, 7L)) %>% distinct()) %>%
  select(pat_id) %>%distinct() %>%
  inner_join(Dia_US_Doses) %>%
   filter(drug_class=="GLP1 Injectable"|drug_class=="Insulin Long") %>%
  select(pat_id, weight) %>% distinct() %>% summarise(n=sum(weight))




# -------------------
 # GLP! + SGLT2 Combos, breakdown by individual molecules ------------------

DANU_Ingredients       <- fread("DIA Analysis Results 1.1/DANU Ingredients.txt", integer64 = "character", stringsAsFactors = F)
DANU_Ingredients$drug_id <- unlist(lapply(DANU_Ingredients$drug_id, function(x) as.numeric(unlist(str_extract_all(x,"[:digit:]+$")))))
string_SGLT2 <- paste0("\\b(",paste0(DANU_Ingredients$drug_id[DANU_Ingredients$drug_group == "SGLT2"], collapse = "|"),")\\b")
string_GLP1 <- paste0("\\b(",paste0(DANU_Ingredients$drug_id[DANU_Ingredients$drug_group == "GLP1 Injectable"|DANU_Ingredients$drug_group == "GLP1 Oral"], collapse = "|"),")\\b")


DIA_Drug_Histories <- fread("DIA Analysis Results 1.1/DIA Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
DIA_Drug_Histories <- DIA_Drug_Histories %>% select(-c(disease)) 
DIA_Drug_Histories <- gather(DIA_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
DIA_Drug_Histories <- DIA_Drug_Histories %>% select(patient, Month, Treat, weight) %>% distinct() %>% filter(Treat!="-")
DIA_Drug_Histories <- DIA_Drug_Histories %>% mutate(GLP1_SGLT2_Status = ifelse(grepl(string_GLP1, Treat)&grepl(string_SGLT2, Treat), "Combo", 
                                                                               ifelse(grepl(string_GLP1, Treat), "GLP1",
                                                                                            ifelse(grepl(string_SGLT2, Treat), "SGLT2", "none"))))

DIA_Drug_Histories <- DIA_Drug_Histories %>% select(patient, GLP1_SGLT2_Status, Month, Treat, weight) %>% distinct() 

Combos <- DIA_Drug_Histories %>% filter(GLP1_SGLT2_Status=="Combo") %>% select(patient, weight) %>% distinct()

Molecules <- Combos %>% left_join(DIA_Drug_Histories) %>% filter(GLP1_SGLT2_Status=="Combo") %>%
  select(patient, weight, Month, Treat)

Molecules <- separate_rows(Molecules, Treat, sep = ",", convert=T)

Molecules <- Molecules %>% filter(grepl(string_GLP1, Treat)|grepl(string_SGLT2, Treat)) %>%
  left_join(DANU_Ingredients %>% select(drug_id, generic_name, drug_group), by=c("Treat"="drug_id")) %>%
  select(-Treat) %>% distinct() 


Molecules_2 <- Molecules %>% group_by(patient, Month) %>% count() %>% filter(n==2) %>% select(patient) %>%
  left_join(Molecules) %>% group_by(patient, Month) %>% spread(key=drug_group, value=generic_name)

Molecules_2 <- Molecules_2 %>% mutate(GLP1=ifelse(!is.na(`GLP1 Injectable`), `GLP1 Injectable`, `GLP1 Oral`)) %>% select(-c(`GLP1 Injectable`, `GLP1 Oral`))
  

data.frame(Molecules_2 %>% group_by(SGLT2) %>% summarise(n=sum(weight)))



data.frame(Molecules_2 %>% mutate(GLP1=ifelse(GLP1=="Albiglutide"|GLP1=="Exenatide"|GLP1=="Liraglutide"|GLP1=="Lixisenatide", "Other", GLP1)) %>% 
             group_by(SGLT2, GLP1) %>% summarise(n=sum(weight)))





#            SGLT2                   GLP1          n
# 1  Canagliflozin            Albiglutide   45102.11
# 2  Canagliflozin            Dulaglutide 2034246.28
# 3  Canagliflozin              Exenatide  598127.75
# 4  Canagliflozin            Liraglutide 1554695.92
# 5  Canagliflozin           Lixisenatide  118510.81
# 6  Canagliflozin Semaglutide Injectable  855112.93
# 7  Canagliflozin       Semaglutide Oral   82764.66
# 8  Dapagliflozin            Albiglutide   24337.83
# 9  Dapagliflozin            Dulaglutide 2764196.13
# 10 Dapagliflozin              Exenatide  734427.38
# 11 Dapagliflozin            Liraglutide 1273480.99
# 12 Dapagliflozin           Lixisenatide  155847.26
# 13 Dapagliflozin Semaglutide Injectable 1667477.18
# 14 Dapagliflozin       Semaglutide Oral  265801.40
# 15 Empagliflozin            Albiglutide   41165.56
# 16 Empagliflozin            Dulaglutide 5991574.01
# 17 Empagliflozin              Exenatide 1065371.38
# 18 Empagliflozin            Liraglutide 3217487.52
# 19 Empagliflozin           Lixisenatide  287945.82
# 20 Empagliflozin Semaglutide Injectable 3997516.81
# 21 Empagliflozin       Semaglutide Oral  475659.67
# 22 Ertugliflozin            Albiglutide    1028.07
# 23 Ertugliflozin            Dulaglutide  190548.39
# 24 Ertugliflozin              Exenatide   67706.71
# 25 Ertugliflozin            Liraglutide  100067.91
# 26 Ertugliflozin           Lixisenatide   18016.85
# 27 Ertugliflozin Semaglutide Injectable  115233.55
# 28 Ertugliflozin       Semaglutide Oral   25242.75


# -----------------------------

# Outflows from Oral GLP1 / SGLT2 --------------------------

DANU_Ingredients <- fread("DIA Analysis Results 1.1/DANU Ingredients.txt", integer64 = "character", stringsAsFactors = F)
DANU_Ingredients <- DANU_Ingredients %>%  separate(drug_id, c('class', 'molecule'))
string_Biguanide       <- paste0("\\b(",paste0(DANU_Ingredients$molecule[DANU_Ingredients$drug_group == "Biguanide"], collapse = "|"),")\\b")
string_Antidiabetic    <- paste0("\\b(",paste0(DANU_Ingredients$molecule[DANU_Ingredients$drug_group == "Antidiabetic"], collapse = "|"),")\\b")
string_DPP4            <- paste0("\\b(",paste0(DANU_Ingredients$molecule[DANU_Ingredients$drug_group == "DPP4"], collapse = "|"),")\\b")
string_SGLT2           <- paste0("\\b(",paste0(DANU_Ingredients$molecule[DANU_Ingredients$drug_group == "SGLT2"], collapse = "|"),")\\b")
string_Insulin         <- paste0("\\b(",paste0(DANU_Ingredients$molecule[DANU_Ingredients$drug_group == "Insulin"], collapse = "|"),")\\b")
string_OralGLP1        <- paste0("\\b(",paste0(DANU_Ingredients$molecule[DANU_Ingredients$drug_group == "GLP1 Oral"], collapse = "|"),")\\b")
string_InjectableGLP1  <- paste0("\\b(",paste0(DANU_Ingredients$molecule[DANU_Ingredients$drug_group == "GLP1 Injectable"], collapse = "|"),")\\b")


DIA_Flows_Aux_Long <- fread("DIA Analysis Results 1.1/DIA_Flows_Aux_Long.txt", integer64 = "character", stringsAsFactors = F)
Treatment_exp_Vector <- fread("DIA Analysis Results 1.1/Treatment_exp_Vector.txt")
DIA_Flows_Aux_Long <- Treatment_exp_Vector %>% inner_join(DIA_Flows_Aux_Long)
DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% select(patient, weight, p1, p2, d1, d2, s1, s2, flow) 
DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% filter(p2>=49)

DIA_Flows_Aux_Long %>% filter(grepl(string_SGLT2, d1)&!grepl(string_SGLT2, d2)) %>% 
  group_by(s2) %>% summarise(n=sum(weight)/1602352)


DIA_Flows_Aux_Long %>% filter(grepl(string_SGLT2, d1)&!grepl(string_SGLT2, d2)) %>%
  filter(s2=="G") %>% filter(grepl(string_SGLT2, d2)) %>% summarise(n=sum(weight)) 

DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% filter(grepl(string_SGLT2, d1)&!grepl(string_SGLT2, d2)) %>% group_by(patient) %>% filter(p2==min(p2)) %>%
  select(patient, p2) %>%
  rename("Month0"="p2") %>% left_join(DIA_Flows_Aux_Long)

DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% filter(p2>=Month0)
DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% mutate(p2=p2-Month0) %>% select(-Month0)

DIA_Flows_Aux_Long %>% filter(p2<=6) %>% group_by(p2, s2) %>% summarise(n=sum(weight)) %>%
  spread(key=p2, value=n)


DIA_Flows_Aux_Long %>% filter(p2<=6) %>% filter(s2=="g") %>% group_by(p2) %>% summarise(n=sum(weight)) 

DIA_Flows_Aux_Long %>% filter(p2<=6) %>% filter(s2=="g" & grepl(string_SGLT2, d2)) %>% group_by(p2) %>% summarise(n=sum(weight)) 

# -------------------------
# August 4 Harsh CKD DIA ------------------------

# CKD
Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 <- fread("Mkt_Comorbidity_Groups_OBE_adjusted_Jun22.txt", sep="\t")
Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 <- Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 %>% filter(diagnosis=="Diabetes"|diagnosis=="Diabetes + Obesity")
sum(Mkt_Comorbidity_Groups_OBE_adjusted_Jun22$weight)

DANU_Ingredients <- fread("DIA Analysis Results 1.1/DANU Ingredients.txt", integer64 = "character", stringsAsFactors = F)
DANU_Ingredients <- DANU_Ingredients %>%  separate(drug_id, c('class', 'molecule'))
DANU_Ingredients  <- DANU_Ingredients %>% select(molecule, drug_group)
names(DANU_Ingredients)[1] <- "Drugs"
DANU_Ingredients$Drugs <- as.numeric(DANU_Ingredients$Drugs)
string_GLP1  <- paste0("\\b(",paste0(DANU_Ingredients$Drugs[DANU_Ingredients$drug_group == "GLP1 Injectable"|DANU_Ingredients$drug_group == "GLP1 Oral"], collapse = "|"),")\\b")
string_SGLT2  <- paste0("\\b(",paste0(DANU_Ingredients$Drugs[DANU_Ingredients$drug_group == "SGLT2"], collapse = "|"),")\\b")

DIA_Drug_Histories <- fread("DIA Analysis Results 1.1/DIA Drug Histories.txt", integer64 = "character", stringsAsFactors = F)

Drug_Histories <-  DIA_Drug_Histories

Drug_Histories <- gather(Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
Drug_Histories <- Drug_Histories %>% filter(Drugs!="-")
Drug_Histories$Month <- as.character(Drug_Histories$Month)
Drug_Histories$Month <- parse_number(Drug_Histories$Month)
Drug_Histories <- Drug_Histories %>% filter(Month>=49)
Drug_Histories <- Drug_Histories %>% select(patient, Drugs) %>% distinct()

GLP1s <- Drug_Histories %>% filter(grepl(string_GLP1, Drugs)) %>% select(patient) %>% distinct() %>% mutate(GLP1="GLP1")
SGLT2 <- Drug_Histories %>% filter(grepl(string_SGLT2, Drugs)) %>% select(patient) %>% distinct() %>% mutate(SGLT2="SGLT2")

Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 %>% filter(CKD==1) %>%
  left_join(GLP1s, by=c("patid"="patient")) %>%
  group_by(diagnosis, GLP1) %>% summarise(n=sum(weight)) %>%
  spread(key=GLP1, value = n) %>% mutate(perc=GLP1/(GLP1+`<NA>`))

Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 %>% filter(CKD==1) %>%
  left_join(SGLT2, by=c("patid"="patient")) %>%
  group_by(diagnosis, SGLT2) %>% summarise(n=sum(weight)) %>%
  spread(key=SGLT2, value = n) %>% mutate(perc=SGLT2/(SGLT2+`<NA>`))




NAFLD_Drug_Histories <- fread("NAFLD Analysis Results 1.1/NAFLD Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
NAFLD_Drug_Histories <- NAFLD_Drug_Histories %>% select(patient) %>% mutate(NAFLD=1)

NAFLD_Doses <- fread("NAFLD Analysis Results 1.1/NAFLD Doses.txt", colClasses = "character",)
NAFLD_Doses <- NAFLD_Doses %>% filter(paid=="P")
NAFLD_Doses <- NAFLD_Doses %>% filter(from_dt>="2021-07-01")
NAFLD_Doses <- NAFLD_Doses %>% filter(drug_class=="Statin") %>% select(pat_id, generic_name, drug_id, from_dt) %>% distinct()

NASH_Medication_Surveys <- fread("NASH Analysis Results 1.1/NASH Medication Surveys.txt")
NAFLD_Doses <- NAFLD_Doses %>% left_join(NASH_Medication_Surveys %>% select(drug_id, med_strength))
NAFLD_Doses %>% select(generic_name, med_strength) %>% distinct() %>% arrange(generic_name, med_strength)
Rosu <- NAFLD_Doses %>% filter(generic_name=="Rosuvastatin" & grepl("20", med_strength)) %>% select(pat_id) %>% distinct() %>% mutate(Rosu=1)
names(Rosu)[1] <- "patid"
Ator <- NAFLD_Doses %>% filter(generic_name=="Atorvastatin" & grepl("40", med_strength)) %>% select(pat_id) %>% distinct() %>% mutate(Ator=1)
names(Ator)[1] <- "patid"


Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 %>% filter(CKD==1) %>%
  inner_join(NAFLD_Drug_Histories, by=c("patid"="patient")) %>%
  left_join(Rosu) %>%
   group_by(diagnosis, Rosu) %>% summarise(n=sum(weight)) %>%
  spread(key=Rosu, value = n) %>% mutate(perc=`1`/(`1`+`<NA>`))


Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 %>% filter(CKD==1) %>%
  inner_join(NAFLD_Drug_Histories, by=c("patid"="patient")) %>%
  left_join(Ator) %>%
   group_by(diagnosis, Ator) %>% summarise(n=sum(weight)) %>%
  spread(key=Ator, value = n) %>% mutate(perc=`1`/(`1`+`<NA>`))

# OLD DATA CKD
DANU_Demographics <- fread("DANU Demographics.txt")
DANU_Demographics <- DANU_Demographics %>% filter(diagnosis=="Diabetes"|diagnosis=="Diabetes + Obesity") %>% select(patid, diagnosis, weight)
names(DANU_Demographics)[1] <- "patient"

CKD_Drug_Histories <- fread("CKD Drug Histories.txt" )
CKD_Drug_Histories <- gather(CKD_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
CKD_Drug_Histories$Month <- as.character(CKD_Drug_Histories$Month)
CKD_Drug_Histories$Month <- parse_number(CKD_Drug_Histories$Month)
CKD_Drug_Histories <- CKD_Drug_Histories %>% filter(Month>=49)
CKD_Drug_Histories <- CKD_Drug_Histories %>% filter(Drugs!="-") %>% select(patient) %>% distinct()
CKD_Drug_Histories$Treated <- 1

CKD_Stages_Complete_FilledIn <- fread("CKD_Stages_Complete_FilledIn.txt")
CKD_Stages_Complete_FilledIn <- CKD_Stages_Complete_FilledIn %>% filter(Stage=="Stage3"|Stage=="Stage4") %>% select(patient) %>% mutate(CKD=1)


DANU_Demographics %>% left_join(CKD_Stages_Complete_FilledIn) %>% filter(CKD==1) %>%
  left_join(CKD_Drug_Histories) %>%
  group_by(diagnosis, Treated) %>% summarise(n=sum(weight)) %>%
  spread(key=Treated, value = n) %>% mutate(perc=`1`/(`1`+`<NA>`))

# ----------------------

# August 4 Harsh OSLAP ------------------------
# OSLAP
Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 <- fread("Mkt_Comorbidity_Groups_OBE_adjusted_Jun22.txt", sep="\t")
Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 <- Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 %>% filter(diagnosis=="Obesity"|diagnosis=="Diabetes + Obesity")
sum(Mkt_Comorbidity_Groups_OBE_adjusted_Jun22$weight)

Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 %>% group_by(diagnosis) %>% summarise(n=sum(weight))

# 1 Diabetes + Obesity 26257291.
# 2 Obesity            95506738.

DANU_Measures <- fread("DANU Measures 1.1/DANU Measures.txt",  integer64 = "character", stringsAsFactors = F)
BMI <- DANU_Measures %>% filter(test=="BMI") %>% select(patid, value)
BMI <- BMI %>% select(patid, value) %>% distinct() %>% group_by(patid ) %>% filter(value==max(value)) %>% slice(1)
BMI <- BMI %>%  filter(value>27) %>% mutate(value=ifelse(value<=30, "27-30",
                                                 ifelse(value<=35, "30-35", ">35")))

Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 <- Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 %>% left_join(BMI) %>% drop_na()


Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 %>% group_by(value, diagnosis) %>% summarise(n=(sum(weight))) 

# 1 27-30 Diabetes + Obesity  2653230.
# 2 27-30 Obesity            22572603.
# 3 30-35 Diabetes + Obesity  4799102.
# 4 30-35 Obesity            39610538.
# 5 >35   Diabetes + Obesity  8072950.
# 6 >35   Obesity            32711788.

95506738.*(22572603./(22572603.+39610538.+32711788.))
95506738.*(39610538./(22572603.+39610538.+32711788.))
95506738.*(32711788./(22572603.+39610538.+32711788.))

26257291.*(2653230./(2653230.+4799102.+8072950.))
26257291.*(4799102./(2653230.+4799102.+8072950.))
26257291.*(8072950./(2653230.+4799102.+8072950.))

Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 %>% group_by(value, diagnosis, OSLAP) %>% summarise(n=sum(weight)) %>%
  spread(key=OSLAP, value = n) %>% mutate(perc=`1`/(`1`+`0`))

DANU_Ingredients <- fread("DIA Analysis Results 1.1/DANU Ingredients.txt", integer64 = "character", stringsAsFactors = F)
DANU_Ingredients <- DANU_Ingredients %>%  separate(drug_id, c('class', 'molecule'))
DANU_Ingredients  <- DANU_Ingredients %>% select(molecule, drug_group)
names(DANU_Ingredients)[1] <- "Drugs"
DANU_Ingredients$Drugs <- as.numeric(DANU_Ingredients$Drugs)
string_GLP1  <- paste0("\\b(",paste0(DANU_Ingredients$Drugs[DANU_Ingredients$drug_group == "GLP1 Injectable"|DANU_Ingredients$drug_group == "GLP1 Oral"], collapse = "|"),")\\b")
string_SGLT2  <- paste0("\\b(",paste0(DANU_Ingredients$Drugs[DANU_Ingredients$drug_group == "SGLT2"], collapse = "|"),")\\b")

DIA_Drug_Histories <- fread("DIA Analysis Results 1.1/DIA Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
OBE2_Drug_Histories <- fread("OBE2 Analysis Results 1.1/OBE2 Drug Histories.txt", integer64 = "character", stringsAsFactors = F)

Drug_Histories <-  DIA_Drug_Histories %>% select(-disease) %>%
  bind_rows(OBE2_Drug_Histories %>% select(-disease))

Drug_Histories <- gather(Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
Drug_Histories <- Drug_Histories %>% filter(Drugs!="-")
Drug_Histories$Month <- as.character(Drug_Histories$Month)
Drug_Histories$Month <- parse_number(Drug_Histories$Month)
Drug_Histories <- Drug_Histories %>% filter(Month>=49)
Drug_Histories <- Drug_Histories %>% select(patient, Drugs) %>% distinct()

GLP1s <- Drug_Histories %>% filter(grepl(string_GLP1, Drugs)) %>% select(patient) %>% distinct() %>% mutate(GLP1="GLP1")
SGLT2 <- Drug_Histories %>% filter(grepl(string_SGLT2, Drugs)) %>% select(patient) %>% distinct() %>% mutate(SGLT2="SGLT2")

Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 %>% filter(OSLAP==1) %>%
  left_join(GLP1s, by=c("patid"="patient")) %>%
  group_by(value, diagnosis, GLP1) %>% summarise(n=sum(weight)) %>%
  spread(key=GLP1, value = n) %>% mutate(perc=GLP1/(GLP1+`<NA>`))

Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 %>% filter(CKD==1) %>%
  left_join(SGLT2, by=c("patid"="patient")) %>%
  group_by(value, diagnosis, SGLT2) %>% summarise(n=sum(weight)) %>%
  spread(key=SGLT2, value = n) %>% mutate(perc=SGLT2/(SGLT2+`<NA>`))

NAFLD_Drug_Histories <- fread("NAFLD Analysis Results 1.1/NAFLD Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
NAFLD_Drug_Histories <- NAFLD_Drug_Histories %>% select(patient) %>% mutate(NAFLD=1)

NAFLD_Doses <- fread("NAFLD Analysis Results 1.1/NAFLD Doses.txt", colClasses = "character",)
NAFLD_Doses <- NAFLD_Doses %>% filter(paid=="P")
NAFLD_Doses <- NAFLD_Doses %>% filter(from_dt>="2021-07-01")
NAFLD_Doses <- NAFLD_Doses %>% filter(drug_class=="Statin") %>% select(pat_id, generic_name, drug_id, from_dt) %>% distinct()

NASH_Medication_Surveys <- fread("NASH Analysis Results 1.1/NASH Medication Surveys.txt")
NAFLD_Doses <- NAFLD_Doses %>% left_join(NASH_Medication_Surveys %>% select(drug_id, med_strength))
NAFLD_Doses %>% select(generic_name, med_strength) %>% distinct() %>% arrange(generic_name, med_strength)
Rosu <- NAFLD_Doses %>% filter(generic_name=="Rosuvastatin" & grepl("20", med_strength)) %>% select(pat_id) %>% distinct() %>% mutate(Rosu=1)
names(Rosu)[1] <- "patid"
Ator <- NAFLD_Doses %>% filter(generic_name=="Atorvastatin" & grepl("40", med_strength)) %>% select(pat_id) %>% distinct() %>% mutate(Ator=1)
names(Ator)[1] <- "patid"

Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 %>% filter(OSLAP==1) %>%
  inner_join(NAFLD_Drug_Histories, by=c("patid"="patient")) %>%
  left_join(Rosu) %>%
   group_by(value, diagnosis, Rosu) %>% summarise(n=sum(weight)) %>%
  spread(key=Rosu, value = n) %>% mutate(perc=`1`/(`1`+`<NA>`))

Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 %>% filter(OSLAP==1) %>%
  inner_join(NAFLD_Drug_Histories, by=c("patid"="patient")) %>%
  left_join(Ator) %>%
   group_by(value, diagnosis, Ator) %>% summarise(n=sum(weight)) %>%
  spread(key=Ator, value = n) %>% mutate(perc=`1`/(`1`+`<NA>`))

# --------------------------

# August 7 Harsh HF DIA -------------------
Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 <- fread("Mkt_Comorbidity_Groups_OBE_adjusted_Jun22.txt", sep="\t")
Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 <- Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 %>% filter(diagnosis=="Diabetes"|diagnosis=="Diabetes + Obesity")
sum(Mkt_Comorbidity_Groups_OBE_adjusted_Jun22$weight)



Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 %>% group_by(diagnosis, HFpEF) %>% summarise(n=sum(weight))

DANU_Ingredients <- fread("DIA Analysis Results 1.1/DANU Ingredients.txt", integer64 = "character", stringsAsFactors = F)
DANU_Ingredients <- DANU_Ingredients %>%  separate(drug_id, c('class', 'molecule'))
DANU_Ingredients  <- DANU_Ingredients %>% select(molecule, drug_group)
names(DANU_Ingredients)[1] <- "Drugs"
DANU_Ingredients$Drugs <- as.numeric(DANU_Ingredients$Drugs)
string_GLP1  <- paste0("\\b(",paste0(DANU_Ingredients$Drugs[DANU_Ingredients$drug_group == "GLP1 Injectable"|DANU_Ingredients$drug_group == "GLP1 Oral"], collapse = "|"),")\\b")
string_SGLT2  <- paste0("\\b(",paste0(DANU_Ingredients$Drugs[DANU_Ingredients$drug_group == "SGLT2"], collapse = "|"),")\\b")

DIA_Drug_Histories <- fread("DIA Analysis Results 1.1/DIA Drug Histories.txt", integer64 = "character", stringsAsFactors = F)

Drug_Histories <-  DIA_Drug_Histories

Drug_Histories <- gather(Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
Drug_Histories <- Drug_Histories %>% filter(Drugs!="-")
Drug_Histories$Month <- as.character(Drug_Histories$Month)
Drug_Histories$Month <- parse_number(Drug_Histories$Month)
Drug_Histories <- Drug_Histories %>% filter(Month>=49)
Drug_Histories <- Drug_Histories %>% select(patient, Drugs) %>% distinct()

GLP1s <- Drug_Histories %>% filter(grepl(string_GLP1, Drugs)) %>% select(patient) %>% distinct() %>% mutate(GLP1="GLP1")
SGLT2 <- Drug_Histories %>% filter(grepl(string_SGLT2, Drugs)) %>% select(patient) %>% distinct() %>% mutate(SGLT2="SGLT2")

Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 %>% filter(HFpEF==1) %>%
  left_join(GLP1s, by=c("patid"="patient")) %>%
  group_by(diagnosis, GLP1) %>% summarise(n=sum(weight)) %>%
  spread(key=GLP1, value = n) %>% mutate(perc=GLP1/(GLP1+`<NA>`))

Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 %>% filter(HFpEF==1) %>%
  left_join(SGLT2, by=c("patid"="patient")) %>%
  group_by(diagnosis, SGLT2) %>% summarise(n=sum(weight)) %>%
  spread(key=SGLT2, value = n) %>% mutate(perc=SGLT2/(SGLT2+`<NA>`))




NAFLD_Drug_Histories <- fread("NAFLD Analysis Results 1.1/NAFLD Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
NAFLD_Drug_Histories <- NAFLD_Drug_Histories %>% select(patient) %>% mutate(NAFLD=1)

NAFLD_Doses <- fread("NAFLD Analysis Results 1.1/NAFLD Doses.txt", colClasses = "character",)
NAFLD_Doses <- NAFLD_Doses %>% filter(paid=="P")
NAFLD_Doses <- NAFLD_Doses %>% filter(from_dt>="2021-07-01")
NAFLD_Doses <- NAFLD_Doses %>% filter(drug_class=="Statin") %>% select(pat_id, generic_name, drug_id, from_dt) %>% distinct()

NASH_Medication_Surveys <- fread("NASH Analysis Results 1.1/NASH Medication Surveys.txt")
NAFLD_Doses <- NAFLD_Doses %>% left_join(NASH_Medication_Surveys %>% select(drug_id, med_strength))
NAFLD_Doses %>% select(generic_name, med_strength) %>% distinct() %>% arrange(generic_name, med_strength)
Rosu <- NAFLD_Doses %>% filter(generic_name=="Rosuvastatin" & grepl("40", med_strength)) %>% select(pat_id) %>% distinct() %>% mutate(Rosu=1)
names(Rosu)[1] <- "patid"
Ator <- NAFLD_Doses %>% filter(generic_name=="Atorvastatin" & grepl("80", med_strength)) %>% select(pat_id) %>% distinct() %>% mutate(Ator=1)
names(Ator)[1] <- "patid"


Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 %>% filter(HFpEF==1) %>%
  inner_join(NAFLD_Drug_Histories, by=c("patid"="patient")) %>%
  left_join(Rosu) %>%
   group_by(diagnosis, Rosu) %>% summarise(n=sum(weight)) %>%
  spread(key=Rosu, value = n) %>% mutate(perc=`1`/(`1`+`<NA>`))


Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 %>% filter(HFpEF==1) %>%
  inner_join(NAFLD_Drug_Histories, by=c("patid"="patient")) %>%
  left_join(Ator) %>%
   group_by(diagnosis, Ator) %>% summarise(n=sum(weight)) %>%
  spread(key=Ator, value = n) %>% mutate(perc=`1`/(`1`+`<NA>`))

# OLD DATA CKD
DANU_Demographics <- fread("DANU Demographics.txt")
DANU_Demographics <- DANU_Demographics %>% filter(diagnosis=="Diabetes"|diagnosis=="Diabetes + Obesity") %>% select(patid, diagnosis, weight)
names(DANU_Demographics)[1] <- "patient"

HF_Drug_Histories <- fread("HF Drug Histories.txt" )
HF_Drug_Histories <- gather(HF_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
HF_Drug_Histories$Month <- as.character(HF_Drug_Histories$Month)
HF_Drug_Histories$Month <- parse_number(HF_Drug_Histories$Month)
HF_Drug_Histories <- HF_Drug_Histories %>% filter(Month>=49)
HF_Drug_Histories <- HF_Drug_Histories %>% filter(Drugs!="-") %>% select(patient) %>% distinct()
HF_Drug_Histories$Treated <- 1

Predicted_Stages_gbm_All <- fread("Predicted_Stages_gbm_All.txt")
Predicted_Stages_gbm_All <- Predicted_Stages_gbm_All %>% filter(Predicted.Stage!=1) %>% select(patient)

DANU_Demographics %>% inner_join(Predicted_Stages_gbm_All) %>%
  left_join(HF_Drug_Histories) %>%
  group_by(diagnosis, Treated) %>% summarise(n=sum(weight)) %>%
  spread(key=Treated, value = n) %>% mutate(perc=`1`/(`1`+`<NA>`))


# ---------------------------------
# GLP1 usage based on age ---------------
DANU_Ingredients       <- fread("DIA Analysis Results 1.1/DANU Ingredients.txt", integer64 = "character", stringsAsFactors = F)
DANU_Ingredients$drug_id <- unlist(lapply(DANU_Ingredients$drug_id, function(x) as.numeric(unlist(str_extract_all(x,"[:digit:]+$")))))
string_GLP1 <- paste0("\\b(",paste0(DANU_Ingredients$drug_id[DANU_Ingredients$drug_group == "GLP1 Injectable"|DANU_Ingredients$drug_group == "GLP1 Oral"], collapse = "|"),")\\b")


Treatment_exp_Vector <- fread("DIA Analysis Results 1.1/Treatment_exp_Vector.txt")
DIA_Drug_Histories <- fread("DIA Analysis Results 1.1/DIA Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
Dems_Labs_TreatExp <- Treatment_exp_Vector %>% inner_join(DIA_Drug_Histories)
DIA_Drug_Histories <- DIA_Drug_Histories %>% select(-c(disease)) 
DIA_Drug_Histories <- gather(DIA_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
DIA_Drug_Histories <- DIA_Drug_Histories  %>% select(patient, weight, Treat) %>% distinct() %>%
  filter(grepl(string_GLP1, Treat)) %>% select(patient, weight) %>% distinct() %>% mutate(GLP1exp="GLP1exp")

DANU_Demographics <- fread("DANU Demographics 1.1/DANU Demographics.txt")
DANU_Demographics <- DANU_Demographics %>% select(patid, age) %>% rename("patient"="patid")

DANU_Demographics %>% inner_join(Treatment_exp_Vector) %>%
  left_join(DIA_Drug_Histories) %>%
  group_by(age, GLP1exp) %>% summarise(n=sum(weight)) %>%
  ungroup() %>% spread(key=GLP1exp, value=n) %>%
  rename("No_GLP1"="<NA>") %>%
  mutate(Perc=GLP1exp/(GLP1exp+No_GLP1)) %>%
  ggplot(aes(age, Perc)) +
  geom_col(width=0.6, alpha=0.4) +
  geom_smooth(size=2, colour="firebrick", fill="firebrick", alpha=0.5) +
  scale_y_continuous(labels = scales::percent)+
  xlab(" \n Age (years)") + ylab("% of Treatment-experienced patients \n Who have tried GLP1") +
  theme_minimal()





# 
# Treatment_exp_Vector <- fread("DIA Analysis Results 1.1/Treatment_exp_Vector.txt")
# DIA_Drug_Histories <- fread("DIA Analysis Results 1.1/DIA Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
# DIA_Drug_Histories <- Treatment_exp_Vector %>% inner_join(DIA_Drug_Histories)
# DIA_Drug_Histories <- gather(DIA_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
# DIA_Drug_Histories <- DIA_Drug_Histories %>% filter(Drugs!="-")
# DIA_Drug_Histories <- DIA_Drug_Histories %>% select(-Month) %>% distinct() 
# DIA_Drug_Histories <- separate_rows(DIA_Drug_Histories, Drugs, sep = ",", convert=T)
# DIA_Drug_Histories <- DIA_Drug_Histories %>% select(-disease) %>% distinct() 
# 
# DANU_Ingredients       <- fread("DIA Analysis Results 1.1/DANU Ingredients.txt", integer64 = "character", stringsAsFactors = F)
# DANU_Ingredients$drug_id <- unlist(lapply(DANU_Ingredients$drug_id, function(x) as.numeric(unlist(str_extract_all(x,"[:digit:]+$")))))
# string_GLP1 <- paste0("\\b(",paste0(DANU_Ingredients$drug_id[DANU_Ingredients$drug_group == "GLP1 Injectable"|DANU_Ingredients$drug_group == "GLP1 Oral"], collapse = "|"),")\\b")
# 
# GLP1_pats <- DIA_Drug_Histories %>% filter(grepl(string_GLP1, Drugs)) %>% select(patient) %>% distinct()
# GLP1_pats$GLP1 <- 1
# GLP1_pats <- Treatment_exp_Vector %>% left_join(GLP1_pats) %>% mutate(GLP1=ifelse(is.na(GLP1),0,1))
# 
# 
# DANU_Measures <- fread("DANU Measures 1.1/DANU Measures.txt",  integer64 = "character", stringsAsFactors = F)
# BMI <- DANU_Measures %>% filter(test=="BMI") %>% select(patid, value)
# BMI <- BMI %>% select(patid, value) %>% distinct() %>% group_by(patid ) %>% filter(value==max(value)) %>% slice(1)
# names(BMI)[1] <- "patient"
# 
# 
# GLP1_pats %>% 
#   inner_join(BMI) %>%
#   filter(value<=1000) %>%
#   ggplot(aes(value, GLP1)) +
#  # geom_jitter() +
#    geom_smooth(method = "glm", 
#     method.args = list(family = "binomial"), 
#     se = T, colour="firebrick", linewidth=2) +
#   theme_minimal() +
#     xlab("\n Triglycerides") + ylab("Probablity of having been ON Hospitalization   \n")
  



Treatment_exp_Vector <- fread("DIA Analysis Results 1.1/Treatment_exp_Vector.txt")
DIA_Drug_Histories <- fread("DIA Analysis Results 1.1/DIA Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
DIA_Drug_Histories <- Treatment_exp_Vector %>% inner_join(DIA_Drug_Histories)
DIA_Drug_Histories <- gather(DIA_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
DIA_Drug_Histories <- DIA_Drug_Histories %>% filter(Drugs!="-")
DIA_Drug_Histories <- DIA_Drug_Histories %>% select(-Month) %>% distinct()
DIA_Drug_Histories <- separate_rows(DIA_Drug_Histories, Drugs, sep = ",", convert=T)
DIA_Drug_Histories <- DIA_Drug_Histories %>% select(-disease) %>% distinct()

DANU_Ingredients       <- fread("DIA Analysis Results 1.1/DANU Ingredients.txt", integer64 = "character", stringsAsFactors = F)
DANU_Ingredients <- DANU_Ingredients %>%  separate(drug_id, c('class', 'molecule'))
DANU_Ingredients  <- DANU_Ingredients %>% select(molecule, drug_group)
names(DANU_Ingredients)[1] <- "Drugs"
DANU_Ingredients$Drugs <- as.numeric(DANU_Ingredients$Drugs)

DIA_Drug_Histories <- DIA_Drug_Histories %>% left_join(DANU_Ingredients) %>% select(patient, weight, drug_group) %>% distinct()

DANU_Demographics <- fread("DANU Demographics 1.1/DANU Demographics.txt")
DANU_Demographics <- DANU_Demographics %>% select(patid, age) %>% rename("patient"="patid")
DANU_Demographics <- DANU_Demographics %>% inner_join(Treatment_exp_Vector)
DANU_Demographics <- DANU_Demographics %>% mutate(age=ifelse(age<35, "18-35",
                                        ifelse(age<45, "35-45",
                                               ifelse(age<55,"45-55",
                                                      ifelse(age<65,"55-65",
                                                             ifelse(age<75, "65-75",
                                                                    ifelse(age<85, "75-85", "85+")))))))

DANU_Demographics %>% group_by(age) %>% summarise(n=sum(weight))

DANU_Demographics %>% left_join(DIA_Drug_Histories) %>%
  group_by(age, drug_group) %>% summarise(n=sum(weight)) %>%
  spread(key=drug_group, value=n)


DIA_Box_Histories     <- fread("DIA Analysis Results 1.1/DIA Box Histories.txt", integer64 = "character", stringsAsFactors = F)
DIA_Box_Histories <- Treatment_exp_Vector %>% left_join(DIA_Box_Histories)
DIA_Box_Histories <- DIA_Box_Histories %>% select(patient, weight, month60)
DIA_Box_Histories <- DIA_Box_Histories %>% mutate(month60 = str_sub(month60, 2L, 2L))


DANU_Demographics %>% left_join(DIA_Box_Histories) %>%
  group_by(age, month60) %>% summarise(n=sum(weight)) %>%
  spread(key=month60, value=n)



DANU_Demographics <- fread("DANU Demographics 1.1/DANU Demographics.txt")
DANU_Demographics <- DANU_Demographics %>% select(patid, age) %>% rename("patient"="patid")

DANU_Measures <- fread("DANU Measures 1.1/DANU Measures.txt",  integer64 = "character", stringsAsFactors = F)
BMI <- DANU_Measures %>% filter(test=="BMI") %>% select(patid, value)
BMI <- BMI %>% select(patid, value) %>% distinct() %>% group_by(patid ) %>% filter(value==max(value)) %>% slice(1)
names(BMI)[1] <- "patient"

Treatment_exp_Vector <- fread("DIA Analysis Results 1.1/Treatment_exp_Vector.txt")

DANU_Demographics %>% inner_join(BMI) %>% inner_join(Treatment_exp_Vector) %>%
  ggplot(aes(age, value)) +
  geom_smooth(size=2, colour="firebrick", fill="firebrick", alpha=0.5) +
  xlab(" \n Age (years)") + ylab("BMI \n") +
  theme_minimal()


# -----------
# Flows for Hendrik diagram flows WITH GLP1s -----------------------


Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 <- fread("Mkt_Comorbidity_Groups_OBE_adjusted_Jun22.txt", sep="\t")
Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 <- Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 %>% select(patid, weight, diagnosis)
Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 %>% group_by(diagnosis) %>% summarise(n=sum(weight))

Months_lookup <- fread("Months_lookup.txt")
Months_lookup$Month <- format(as.Date(paste0(Months_lookup$Month,"-1")), "%Y-%m")

Months <- Months_lookup  %>% select(Exact_Month) %>% mutate(Link=1) %>%
  full_join(Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 %>% select(patid) %>% mutate(Link=1))

nrow(Months) ; length(unique(Months$patid))*60

Months <- Months %>% arrange(patid, Exact_Month) %>% select(-Link)  %>% 
  left_join(Mkt_Comorbidity_Groups_OBE_adjusted_Jun22) %>% rename("Final_Box"="diagnosis")

DANU_Demographics <- fread("DANU Demographics 1.1/DANU Demographics.txt", integer64 = "character", stringsAsFactors = F)
DANU_Demographics <- DANU_Demographics %>% filter(diagnosis != "-") %>% select(patid, diabetes_onset, obesity_onset) 

DANU_Demographics$diabetes_onset <- format(as.Date(DANU_Demographics$diabetes_onset), "%Y-%m")
DANU_Demographics$obesity_onset <- format(as.Date(DANU_Demographics$obesity_onset), "%Y-%m")
DANU_Demographics <- DANU_Demographics %>% left_join(Months_lookup, by = c("diabetes_onset" = "Month")) %>% rename("diabetes_onset_exact"="Exact_Month")
DANU_Demographics <- DANU_Demographics %>% left_join(Months_lookup, by = c("obesity_onset" = "Month")) %>% rename("obesity_onset_exact"="Exact_Month")

DANU_Demographics$diabetes_onset_exact[is.na(DANU_Demographics$diabetes_onset_exact)&!is.na(DANU_Demographics$diabetes_onset)] <- 1
DANU_Demographics$obesity_onset_exact[is.na(DANU_Demographics$obesity_onset_exact)&!is.na(DANU_Demographics$obesity_onset)] <- 1
DANU_Demographics <- DANU_Demographics %>% select(-c(diabetes_onset, obesity_onset))

DANU_Demographics <- DANU_Demographics %>% mutate(diabetes=ifelse(is.na(diabetes_onset_exact), 0,1)) %>%
                      mutate(obesity=ifelse(is.na(obesity_onset_exact), 0,1))

DANU_Demographics[is.na(DANU_Demographics)] <- 0

Months <- Months %>% left_join(DANU_Demographics %>% select(patid, diabetes_onset_exact, diabetes), by=c("patid"="patid", "Exact_Month"="diabetes_onset_exact")) 
Months <- Months %>% left_join(DANU_Demographics %>% select(patid, obesity_onset_exact, obesity), by=c("patid"="patid", "Exact_Month"="obesity_onset_exact")) 
Months[is.na(Months)] <- 0
Months <- Months %>% group_by(patid) %>% mutate(cumobe = cumsum(obesity==1)) %>% mutate(cumdia = cumsum(diabetes==1))
Months <- Months %>% select(-c(diabetes, obesity))

Months %>% mutate(cumdia=ifelse(cumdia==1&Final_Box=="Obesity",0,cumdia)) %>% 
  filter(Exact_Month==60) %>% group_by(cumobe, cumdia) %>% summarise(n=sum(weight))

Months <- Months %>% mutate(cumdia=ifelse(cumdia==1&Final_Box=="Obesity",0,cumdia))

DANU_Events <- fread("DANU Events 1.1/DANU Events.txt")
DANU_Events <- DANU_Events %>% select(-prov)
DANU_Diagnosis_Codes <- fread("DIA Analysis Results 1.1/DANU Diagnosis Codes.txt")
DANU_Diagnosis_Codes <- DANU_Diagnosis_Codes %>% filter(diagnosis=="Obesity"&source=="Diagnosis")
#DANU_Diagnosis_Codes <- DANU_Diagnosis_Codes %>% filter(!grepl("V",code)&!grepl("Z",code))
DANU_Diagnosis_Codes <- DANU_Diagnosis_Codes %>% select(code)
DANU_Events <- DANU_Events %>% inner_join(DANU_Diagnosis_Codes)
DANU_Events$claimed <- format(as.Date(DANU_Events$claimed), "%Y-%m")
DANU_Events <- DANU_Events %>% group_by(patid) %>% filter(claimed==min(claimed)) %>% slice(1) %>% select(-c(weight, code))
DANU_Events <- DANU_Events %>% left_join(Months_lookup, by = c("claimed" = "Month")) %>% rename("OBE_diagnosis"="Exact_Month")
DANU_Events[is.na(DANU_Events)] <- 1
DANU_Events <- DANU_Events %>% select(-claimed) %>% ungroup()
DANU_Events$OBE_Dx <- 1

Months <- Months %>% left_join(DANU_Events, by=c("patid"="patid", "Exact_Month"="OBE_diagnosis"))

Months[is.na(Months)] <- 0

Months <- Months %>% group_by(patid) %>% mutate(cumobe_dx = cumsum(OBE_Dx==1)) 
Months <- Months %>% select(-c(OBE_Dx))

DANU_Ingredients       <- fread("DIA Analysis Results 1.1/DANU Ingredients.txt", integer64 = "character", stringsAsFactors = F)
DANU_Ingredients$drug_id <- unlist(lapply(DANU_Ingredients$drug_id, function(x) as.numeric(unlist(str_extract_all(x,"[:digit:]+$")))))
string_GLP1 <- paste0("\\b(",paste0(DANU_Ingredients$drug_id[DANU_Ingredients$drug_group == "GLP1 Injectable"|DANU_Ingredients$drug_group == "GLP1 Oral"], collapse = "|"),")\\b")
string_Other <- paste0("\\b(",paste0(DANU_Ingredients$drug_id[DANU_Ingredients$drug_group != "GLP1 Injectable"&DANU_Ingredients$drug_group != "GLP1 Oral"], collapse = "|"),")\\b")


OBE2_Drug_Histories <- fread("OBE2 Analysis Results 1.1/OBE2 Drug Histories.txt")
OBE2_Drug_Histories <- gather(OBE2_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
OBE2_Drug_Histories <- OBE2_Drug_Histories %>% filter(Drugs != "-") %>% select(patient, weight, Month, Drugs) %>% 
  mutate(Rx_GLP1=ifelse(grepl(string_GLP1, Drugs),1,0)) %>% mutate(Rx_Other=ifelse(grepl(string_Other, Drugs),1,0)) %>% select(-Drugs)
OBE2_Drug_Histories$Month <- as.character(OBE2_Drug_Histories$Month)
OBE2_Drug_Histories$Month <- parse_number(OBE2_Drug_Histories$Month)
OBE2_Drug_Histories <- OBE2_Drug_Histories %>% select(-weight) %>% rename("patid"="patient")

Months <- Months %>% left_join(OBE2_Drug_Histories, by=c("patid"="patid", "Exact_Month"="Month"))
Months[is.na(Months)] <- 0
names(Months)[8] <- "Current_OBE_Rx_GLP1"
names(Months)[9] <- "Current_OBE_Rx_Other"


DIA_Drug_Histories <- fread("DIA Analysis Results 1.1/DIA Drug Histories.txt")
DIA_Drug_Histories <- gather(DIA_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
DIA_Drug_Histories <- DIA_Drug_Histories %>% filter(Drugs != "-") %>% select(patient, weight, Month, Drugs) %>% 
  mutate(Rx_GLP1=ifelse(grepl(string_GLP1, Drugs),1,0)) %>% mutate(Rx_Other=ifelse(grepl(string_Other, Drugs),1,0)) %>% select(-Drugs)
DIA_Drug_Histories$Month <- as.character(DIA_Drug_Histories$Month)
DIA_Drug_Histories$Month <- parse_number(DIA_Drug_Histories$Month)
DIA_Drug_Histories <- DIA_Drug_Histories %>% select(-weight) %>% rename("patid"="patient")

Months <- Months %>% left_join(DIA_Drug_Histories, by=c("patid"="patid", "Exact_Month"="Month"))
Months[is.na(Months)] <- 0
names(Months)[10] <- "Current_DIA_Rx_GLP1"
names(Months)[11] <- "Current_DIA_Rx_Other"


Months <- Months %>% group_by(patid) %>% mutate(cumobe_Rx_GLP1 = cumsum(Current_OBE_Rx_GLP1==1)) 
Months <- Months %>% mutate(cumobe_Rx_GLP1=ifelse(cumobe_Rx_GLP1==0,0,1))
Months <- Months %>% group_by(patid) %>% mutate(cumobe_Rx_Other = cumsum(Current_OBE_Rx_Other==1)) 
Months <- Months %>% mutate(cumobe_Rx_Other=ifelse(cumobe_Rx_Other==0,0,1))

Months <- Months %>% group_by(patid) %>% mutate(cumdia_Rx_GLP1 = cumsum(Current_DIA_Rx_GLP1==1)) 
Months <- Months %>% mutate(cumdia_Rx_GLP1=ifelse(cumdia_Rx_GLP1==0,0,1))
Months <- Months %>% group_by(patid) %>% mutate(cumdia_Rx_Other = cumsum(Current_DIA_Rx_Other==1)) 
Months <- Months %>% mutate(cumdia_Rx_Other=ifelse(cumdia_Rx_Other==0,0,1))

names(Months)
Months <- Months %>% mutate(DIA_only=ifelse(cumdia == 1 & cumobe==0 , 1, 0))
Months <- Months %>% mutate(DIA_OBE=ifelse(cumdia == 1 & cumobe==1, 1, 0))
Months <- Months %>% mutate(OBE_only=ifelse(cumobe==1 & cumobe_dx==0 & cumobe_Rx_GLP1 == 0 &  cumobe_Rx_Other==0 & cumdia==0, 1, 0))
Months <- Months %>% mutate(OBE_dx=ifelse(cumobe==1 & cumobe_dx==1 & cumobe_Rx_GLP1 == 0 & cumobe_Rx_Other == 0 & cumdia==0, 1, 0))
Months <- Months %>% mutate(OBE_rx_exp_lapsed=ifelse( (cumobe==1 & cumobe_Rx_GLP1 == 1 & cumdia==0 & Current_OBE_Rx_GLP1==0 & Current_OBE_Rx_Other==0) |
                                                       (cumobe==1 & cumobe_Rx_Other == 1 & cumdia==0 & Current_OBE_Rx_GLP1==0 & Current_OBE_Rx_Other==0), 1, 0))


Months <- Months %>% mutate(OBE_rx_glp1_current=ifelse(cumobe==1 &  cumobe_Rx_GLP1 == 1 & Current_OBE_Rx_GLP1==1 & cumdia==0, 1, 0))
Months <- Months %>% mutate(OBE_rx_other_current=ifelse(cumobe==1 &  cumobe_Rx_Other == 1 & Current_OBE_Rx_Other==1 & cumdia==0, 1, 0))

Months <- Months %>% mutate(DIA_GLP1=ifelse(cumdia == 1 & Current_DIA_Rx_GLP1==1, 1, 0))
Months <- Months %>% mutate(DIA_only=ifelse(DIA_GLP1==1,0, DIA_only)) %>% mutate(DIA_OBE=ifelse(DIA_GLP1==1,0, DIA_OBE))


Months <- Months %>% mutate(OBE_rx_exp_lapsed=ifelse(OBE_rx_exp_lapsed==0 & 
                                             (cumobe_Rx_GLP1==1|cumobe_Rx_Other==1) & 
                                             Current_OBE_Rx_GLP1==0&Current_OBE_Rx_Other==0,1,OBE_rx_exp_lapsed)) 


Months %>% filter(Exact_Month==60) %>%
  group_by(DIA_only, DIA_OBE, DIA_GLP1, OBE_only, OBE_dx, OBE_rx_exp_lapsed, OBE_rx_glp1_current,OBE_rx_other_current ) %>% summarise(n=sum(weight))

Months <- Months %>% mutate(OBE_rx_other_current=ifelse(OBE_rx_glp1_current==1,0,OBE_rx_other_current))


fwrite(Months, "DANU_MoM_Hendrik_glp1.txt", sep="\t")
Months <- fread("DANU_MoM_Hendrik_glp1.txt", sep="\t")

temp <- data.frame(Months %>%  group_by(Exact_Month, DIA_only, DIA_OBE, DIA_GLP1,  OBE_only, OBE_dx, OBE_rx_exp_lapsed, OBE_rx_glp1_current, OBE_rx_other_current) %>% summarise(n=sum(weight)) %>%
  gather(Group, Value, DIA_only:OBE_rx_other_current) %>% filter(Value==1) %>% arrange(Exact_Month)) %>%
  select(-Value) %>%
  spread(key=Group, value=n)
 
temp[is.na(temp)] <- 0

fwrite(temp, "DANU_stocks_MoM_Hendrik_glp1.txt", sep="\t")

Months <- Months %>% select(Exact_Month, patid, weight, Final_Box, DIA_only, DIA_OBE, DIA_GLP1, OBE_only, OBE_dx, OBE_rx_exp_lapsed, OBE_rx_glp1_current, OBE_rx_other_current) %>% ungroup() 

Months <- Months %>% mutate(Box=ifelse(DIA_only==1, "DIA_only", 
                             ifelse(DIA_OBE==1, "DIA_OBE",
                                    ifelse(DIA_GLP1==1, "DIA_GLP1",
                                    ifelse(OBE_only==1, "OBE_only",
                                           ifelse(OBE_dx==1, "OBE_dx",
                                                  ifelse(OBE_rx_exp_lapsed==1, "OBE_rx_exp_lapsed",
                                                         ifelse(OBE_rx_glp1_current==1, "Current_OBE_Rx_GLP1", 
                                                                ifelse(OBE_rx_other_current==1, "Current_OBE_Rx_Other", "other")))))))))

Months <- Months %>% select(Exact_Month, patid, weight, Final_Box, Box)

Months %>% filter(Exact_Month==60) %>% group_by(Box) %>% summarise(n=sum(weight))

Months <- Months %>% mutate(flow=0) %>%
  group_by(patid) %>% mutate(flow=ifelse(Box!=lag(Box),1,0)) %>% mutate(flow=ifelse(is.na(flow),0,flow))

temp <- Months %>% filter(flow==1|lead(flow)==1)

temp %>% filter(Box=="other"&lag(Box)=="OBE_rx_exp_lapsed")

temp2 <- temp %>% filter(lead(flow)==1) %>% select(Exact_Month, patid, weight, Box)  %>% rename("Source"="Box") %>%
  left_join(temp %>% filter(flow==1) %>% select(Exact_Month, patid, weight, Box) %>% rename("Dest"="Box") %>%  mutate(Exact_Month=Exact_Month-1) 
) %>% mutate(Exact_Month=Exact_Month+1)

unique(temp2$Dest)

temp2 <- temp2 %>% ungroup() %>% group_by(Exact_Month, Source, Dest) %>% summarise(n=sum(weight)) %>%
  spread(key=Dest, value=n)

temp2[is.na(temp2)] <-0

fwrite(temp2, "DANU_flows_MoM_Hendrik_glp1.txt", sep="\t")

temp2 <- fread("DANU_flows_MoM_Hendrik_glp1.txt", sep="\t")


temp2 %>% 
  filter(Exact_Month==60)


Months <- fread("DANU_MoM_Hendrik_glp1.txt", sep="\t")
DANU_Demographics <- fread("DANU Demographics 1.1/DANU Demographics.txt")
DANU_Demographics <- DANU_Demographics %>% select(patid, age)

Months <- Months %>% left_join(DANU_Demographics)

names(Months)


Months <- Months %>% select(Exact_Month, patid, weight, age, Final_Box, cumobe, cumdia, cumobe_dx, Current_OBE_Rx_GLP1, Current_OBE_Rx_Other,
                            cumdia_Rx_GLP1, cumdia_Rx_Other, 
                            Current_DIA_Rx_GLP1, Current_DIA_Rx_Other, cumobe_Rx_GLP1, cumobe_Rx_Other, DIA_only, DIA_OBE, DIA_GLP1, OBE_only, OBE_dx, OBE_rx_exp_lapsed, OBE_rx_glp1_current, OBE_rx_other_current)

fwrite(Months, "DANU_MoM_Hendrik_glp1.txt", sep="\t")



# ---------------------
# --------------
# Age groups for Harsh --------------------
Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 <- fread("Mkt_Comorbidity_Groups_OBE_adjusted_Jun22.txt", sep="\t")
Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 <- Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 %>% filter(diagnosis=="Obesity"|diagnosis=="Diabetes + Obesity") %>% select(patid, weight)
sum(Mkt_Comorbidity_Groups_OBE_adjusted_Jun22$weight)

DANU_Demographics <- fread("DANU Demographics 1.1/DANU Demographics.txt", integer64 = "character", stringsAsFactors = F)
DANU_Demographics <- DANU_Demographics %>% select(patid, age)

Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 <- Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 %>% left_join(DANU_Demographics)

Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 %>% 
  mutate(age=ifelse(age>=18&age<25, "18-25", 
                    ifelse(age>=25&age<30, "25-30", 
                           ifelse(age>=30&age<35, "30-35", 
                                  ifelse(age>=35&age<40, "35-40", 
                                         ifelse(age>=40&age<45, "40-45", 
                                                ifelse(age>=45&age<50, "45-50", 
                                                       ifelse(age>=50&age<55, "50-55", 
                                                              ifelse(age>=55&age<60, "55-60", 
                                                                     ifelse(age>=60&age<65, "60-65", 
                                                                            ifelse(age>=65&age<70, "65-70", 
                                                                                   ifelse(age>=70&age<75, "70-75", 
                                                                                          ifelse(age>=75&age<80, "75-80", "+80"))))))))))))) %>%
  group_by(age) %>% summarise(n=sum(weight))

# ----------------------
# Persistency per GLP1 molecule 1st episode ------------------------------------------------------------
DANU_Ingredients <- fread("DIA Analysis Results 1.1/DANU Ingredients.txt", integer64 = "character", stringsAsFactors = F)
DANU_Ingredients <- DANU_Ingredients %>%  separate(drug_id, c('class', 'molecule'))
DANU_Ingredients <- DANU_Ingredients %>% select(molecule, generic_name, drug_group)
names(DANU_Ingredients)[1] <- "Drugs"
DANU_Ingredients$Drugs <- as.numeric(DANU_Ingredients$Drugs)
DANU_Ingredients <- DANU_Ingredients %>% filter(grepl("GLP", drug_group))

string_GLP <- paste0("\\b(",paste0(DANU_Ingredients$Drugs[DANU_Ingredients$drug_group == "GLP1 Oral"|DANU_Ingredients$drug_group == "GLP1 Injectable"], collapse = "|"),")\\b")


DIA_Drug_Histories <- read.table("DIA Analysis Results 1.1/DIA Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
DIA_Drug_Histories <- gather(DIA_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
DIA_Drug_Histories$Month <- as.character(DIA_Drug_Histories$Month)
DIA_Drug_Histories$Month <- parse_number(DIA_Drug_Histories$Month)
DIA_Drug_Histories <- DIA_Drug_Histories %>% filter(Drugs != "-") %>% filter(grepl(string_GLP, Drugs)) %>% select(-c(disease))
DIA_Drug_Histories <- separate_rows(DIA_Drug_Histories, Drugs, sep = ",", convert=T)
DIA_Drug_Histories <- DIA_Drug_Histories %>% left_join(DANU_Ingredients %>% select(-drug_group))
DIA_Drug_Histories <- DIA_Drug_Histories %>% select(-Drugs) %>% distinct() %>% filter(!is.na(generic_name))

DIA_Drug_Histories <- DIA_Drug_Histories %>% arrange(patient, weight, generic_name, Month)

DIA_Drug_Histories <- DIA_Drug_Histories %>% group_by(patient, weight, generic_name) %>% mutate(Elapsed=Month-lag(Month))
DIA_Drug_Histories[is.na(DIA_Drug_Histories)] <- 0

Crop <- DIA_Drug_Histories %>% ungroup() %>% filter(Elapsed>1) %>% select(-Elapsed) %>%
  group_by(patient, weight, generic_name) %>% filter(Month==min(Month))  %>% rename("Crop"="Month")

DIA_Drug_Histories_2 <- DIA_Drug_Histories %>% left_join(Crop) %>% mutate(Crop=ifelse(is.na(Crop), 100, Crop)) %>% filter(Month<Crop)

DIA_Drug_Histories_2 <- DIA_Drug_Histories_2 %>% group_by(patient, weight, generic_name) %>% count()

DIA_Drug_Histories_2 %>% ungroup() %>% group_by(generic_name) %>% summarise(mean=weighted.mean(n, as.numeric(weight)))

#   generic_name            mean
# 1 Albiglutide             7.36
# 2 Dulaglutide            12.4 
# 3 Exenatide              10.2 
# 4 Liraglutide            11.7 
# 5 Lixisenatide           10.5 
# 6 Semaglutide Injectable  8.47
# 7 Semaglutide Oral        5.55


DIA_Drug_Histories_2 <- DIA_Drug_Histories_2 %>% ungroup() %>% group_by(generic_name, n) %>% summarise(Total=sum(as.numeric(weight))) %>% arrange(generic_name, n)



temp <- data.frame(DIA_Drug_Histories_2 %>% group_by(generic_name) %>% mutate(Total_cum=cumsum(Total)) %>% mutate(grand_Total=sum(Total)) %>%
             mutate(Remain=grand_Total-lag(Total_cum)) %>% mutate(Remain=ifelse(is.na(Remain), grand_Total, Remain)) %>%
             mutate(Remain=Remain/grand_Total) %>%
             select(generic_name, n, Remain)) 

data.frame(temp)

fwrite(temp, "Persistency_GLP1s_All_Episodes.csv")

temp %>%
  mutate(generic_name=ifelse(generic_name=="Semaglutide Oral", "Semaglutide Oral (5.55m)", 
                             ifelse(generic_name=="Semaglutide Injectable", "Semaglutide Injectable (8.47m)",
                                    ifelse(generic_name=="Lixisenatide", "Lixisenatide (10.5m)",
                                           ifelse(generic_name=="Liraglutide", "Liraglutide (11.7m)",
                                                  ifelse(generic_name=="Exenatide", "Exenatide (10.2m)",
                                                         ifelse(generic_name=="Dulaglutide", "Dulaglutide (12.4m)", "Albiglutide (7.36m)"
                                    
                                    ))))))) %>%
  mutate(generic_name=factor(generic_name, levels=c("Semaglutide Oral (5.55m)", "Semaglutide Injectable (8.47m)", "Lixisenatide (10.5m)", "Liraglutide (11.7m)", "Exenatide (10.2m)", "Dulaglutide (12.4m)", "Albiglutide (7.36m)"))) %>%
  ggplot(aes(n, Remain, colour=generic_name)) +
  geom_smooth(se=F, size=2, alpha=0.6) +
  ylim(0,1) +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent, breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)) +
  #scale_x_continuous(breaks = c(-60, -50, -40, -30, -20, -10, 0)) +
  scale_colour_manual(values=c("#a52a2a","#7d95be","#0d2b4e","#ff9933","#e87496", "#7cc49a","#c49a7c")) +
  xlab("\n Number of Months Elapsed Since Therapy Initiation \n (1st Episode only) ") + ylab("Proportion of Patients \n Still ON Each GLP1 Molecule \n")

# ------------------------

# Persistency per GLP1 molecule ALL episodes ------------------------------------------------------------
DANU_Ingredients <- fread("DIA Analysis Results 1.1/DANU Ingredients.txt", integer64 = "character", stringsAsFactors = F)
DANU_Ingredients <- DANU_Ingredients %>%  separate(drug_id, c('class', 'molecule'))
DANU_Ingredients <- DANU_Ingredients %>% select(molecule, generic_name, drug_group)
names(DANU_Ingredients)[1] <- "Drugs"
DANU_Ingredients$Drugs <- as.numeric(DANU_Ingredients$Drugs)
DANU_Ingredients <- DANU_Ingredients %>% filter(grepl("GLP", drug_group))

string_GLP <- paste0("\\b(",paste0(DANU_Ingredients$Drugs[DANU_Ingredients$drug_group == "GLP1 Oral"|DANU_Ingredients$drug_group == "GLP1 Injectable"], collapse = "|"),")\\b")


DIA_Drug_Histories <- read.table("DIA Analysis Results 1.1/DIA Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
DIA_Drug_Histories <- gather(DIA_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
DIA_Drug_Histories$Month <- as.character(DIA_Drug_Histories$Month)
DIA_Drug_Histories$Month <- parse_number(DIA_Drug_Histories$Month)
DIA_Drug_Histories <- DIA_Drug_Histories %>% filter(Drugs != "-") %>% filter(grepl(string_GLP, Drugs)) %>% select(-c(disease))
DIA_Drug_Histories <- separate_rows(DIA_Drug_Histories, Drugs, sep = ",", convert=T)
DIA_Drug_Histories <- DIA_Drug_Histories %>% left_join(DANU_Ingredients %>% select(-drug_group))
DIA_Drug_Histories <- DIA_Drug_Histories %>% select(-Drugs) %>% distinct() %>% filter(!is.na(generic_name))

DIA_Drug_Histories <- DIA_Drug_Histories %>% arrange(patient, weight, generic_name, Month)

DIA_Drug_Histories <- DIA_Drug_Histories %>% group_by(patient, weight, generic_name) %>% count()

DIA_Drug_Histories %>% ungroup() %>% group_by(generic_name) %>% summarise(mean=weighted.mean(n, as.numeric(weight)))

#   generic_name            mean
# 1 Albiglutide             7.75
# 2 Dulaglutide            15.4 
# 3 Exenatide              12.9 
# 4 Liraglutide            15.9 
# 5 Lixisenatide           14.1 
# 6 Semaglutide Injectable 10.2 
# 7 Semaglutide Oral        6.03

DIA_Drug_Histories <- DIA_Drug_Histories %>% ungroup() %>% group_by(generic_name, n) %>% summarise(Total=sum(as.numeric(weight))) %>% arrange(generic_name, n)


temp <- data.frame(DIA_Drug_Histories %>% group_by(generic_name) %>% mutate(Total_cum=cumsum(Total)) %>% mutate(grand_Total=sum(Total)) %>%
             mutate(Remain=grand_Total-lag(Total_cum)) %>% mutate(Remain=ifelse(is.na(Remain), grand_Total, Remain)) %>%
             mutate(Remain=Remain/grand_Total) %>%
             select(generic_name, n, Remain)) 



temp %>%
  mutate(generic_name=ifelse(generic_name=="Semaglutide Oral", "Semaglutide Oral (6.03m)", 
                             ifelse(generic_name=="Semaglutide Injectable", "Semaglutide Injectable (10.2m)",
                                    ifelse(generic_name=="Lixisenatide", "Lixisenatide (14.1m)",
                                           ifelse(generic_name=="Liraglutide", "Liraglutide (15.9m)",
                                                  ifelse(generic_name=="Exenatide", "Exenatide (12.9m)",
                                                         ifelse(generic_name=="Dulaglutide", "Dulaglutide (15.4m)", "Albiglutide (7.75m)"
                                    
                                    ))))))) %>%
  mutate(generic_name=factor(generic_name, levels=c("Semaglutide Oral (6.03m)", "Semaglutide Injectable (10.2m)", "Lixisenatide (14.1m)", "Liraglutide (15.9m)", "Exenatide (12.9m)", "Dulaglutide (15.4m)", "Albiglutide (7.75m)"))) %>%
  ggplot(aes(n, Remain, colour=generic_name)) +
  geom_smooth(se=F, size=2, alpha=0.6) +
  ylim(0,1) +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent, breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)) +
  #scale_x_continuous(breaks = c(-60, -50, -40, -30, -20, -10, 0)) +
  scale_colour_manual(values=c("#a52a2a","#7d95be","#0d2b4e","#ff9933","#e87496", "#7cc49a","#c49a7c")) +
  xlab("\n Number of Months Elapsed Since Therapy Initiation \n (1st Episode only) ") + ylab("Proportion of Patients \n Still ON Each GLP1 Molecule \n")

# ------------------
# BMI <65 vs >65 Diabetes and Obesity --------------------------


Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 <- fread("Mkt_Comorbidity_Groups_OBE_adjusted_Jun22.txt", sep="\t")
Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 <- Mkt_Comorbidity_Groups_OBE_adjusted_Jun22  %>% select(patid, weight, diagnosis)
sum(Mkt_Comorbidity_Groups_OBE_adjusted_Jun22$weight)

Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 %>% group_by(diagnosis) %>% summarise(n=sum(weight))

DANU_Demographics <- fread("DANU Demographics 1.1/DANU Demographics.txt", integer64 = "character", stringsAsFactors = F)
DANU_Demographics <- DANU_Demographics %>% select(patid, age)

Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 <- Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 %>% left_join(DANU_Demographics) %>% mutate(age=ifelse(age>=65, "65+", "64"))

DANU_Measures <- fread("DANU Measures 1.1/DANU Measures.txt",  integer64 = "character", stringsAsFactors = F)
DANU_Measures <- DANU_Measures %>% filter(test=="BMI")

DANU_Measures <- DANU_Measures %>% select(patid, claimed, value) %>% distinct() %>% mutate(claimed=as.Date(claimed)) %>%
  group_by(patid) %>% filter(claimed==max(claimed)) %>% filter(value==max(value)) %>% slice(1) %>% ungroup()

DANU_Measures <- DANU_Measures %>% select(-claimed)


Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 <- DANU_Measures %>% inner_join(Mkt_Comorbidity_Groups_OBE_adjusted_Jun22)

Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 <- Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 %>% mutate(value=ifelse(value>=35, ">=35",
                                                                  ifelse(value>=30, ">=30",
                                                                         ifelse(value>=27, ">=27",
                                                                                ifelse(value>=25, ">=25", "<25")))))

Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 <- Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 %>% mutate(diagnosis=ifelse(grepl("Diabetes", diagnosis), "Diabetes", diagnosis)) 

Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 %>% group_by(diagnosis, age, value) %>% summarise(n=sum(weight)) %>%
  spread(key=diagnosis, value=n)

# -----------------
# Physicians matrix trial 1 ------------------------
Dia_US_Doses <- fread("DIA Doses 1.1/DIA Doses.txt", colClasses = "character")
Dia_US_Doses <- Dia_US_Doses %>% select(pat_id, from_dt, specialty)

Months_lookup <- fread("Months_lookup.txt")
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")

Dia_US_Doses$from_dt <- format(as.Date(Dia_US_Doses$from_dt), "%Y-%m")
Dia_US_Doses <- Dia_US_Doses %>% left_join(Months_lookup, by = c("from_dt" = "Month")) %>%  filter(!is.na(Exact_Month)) 
Dia_US_Doses <- Dia_US_Doses %>% select(pat_id, specialty, Exact_Month)  


DIA_Specialty_codes <- fread("DIA Analysis Results 1.1/DANU Specialty Codes.txt", colClasses = "character")
DIA_Specialty_codes <- DIA_Specialty_codes %>% select(code, specialty)
names(DIA_Specialty_codes)[2] <- "TYPE" ;  names(DIA_Specialty_codes)[1] <- "specialty"

Dia_US_Doses <- Dia_US_Doses %>% inner_join(DIA_Specialty_codes) %>% select(-specialty)

unique(Dia_US_Doses$TYPE)

Dia_US_Doses <- Dia_US_Doses %>% filter(TYPE!="Unknown" & TYPE!="Facility" )

Dia_US_Doses$TYPE <- str_replace_all(Dia_US_Doses$TYPE, " ", "_")

Dia_US_Doses <- Dia_US_Doses %>% mutate(exp=1) %>% distinct()


length(unique(Dia_US_Doses$pat_id))
length(unique(Dia_US_Doses$Exact_Month))
dim(Dia_US_Doses)[1]

Dia_US_Doses <- Dia_US_Doses %>% select(pat_id) %>% distinct() %>% mutate(flag=1) %>%
  full_join(Dia_US_Doses %>% select(Exact_Month) %>% distinct() %>% mutate(flag=1)) %>%
  select(-flag) %>% left_join(Dia_US_Doses)


Dia_US_Doses <- Dia_US_Doses %>% arrange(pat_id , Exact_Month)

Dia_US_Doses <- Dia_US_Doses  %>% spread(key=TYPE, value=exp) 

Dia_US_Doses[is.na(Dia_US_Doses)] <- 0

physician_histories <- Dia_US_Doses




Dia_US_Doses <- fread("DIA Doses 1.1/DIA Doses.txt", colClasses = "character")
Dia_US_Doses <- Dia_US_Doses %>% select(pat_id, from_dt, drug_class)

Months_lookup <- fread("Months_lookup.txt")
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")

Dia_US_Doses$from_dt <- format(as.Date(Dia_US_Doses$from_dt), "%Y-%m")
Dia_US_Doses <- Dia_US_Doses %>% left_join(Months_lookup, by = c("from_dt" = "Month")) %>%  filter(!is.na(Exact_Month)) 
Dia_US_Doses <- Dia_US_Doses %>% select(pat_id, drug_class, Exact_Month)  

Dia_US_Doses$drug_class <- str_replace_all(Dia_US_Doses$drug_class, " ", "_")

Dia_US_Doses <- Dia_US_Doses %>% mutate(exp=1) %>% distinct()

Dia_US_Doses <- Dia_US_Doses %>% spread(key=drug_class, value=exp)

Dia_US_Doses[is.na(Dia_US_Doses)] <- 0

unique(Dia_US_Doses$Exact_Month)

distinct(Dia_US_Doses)

dim(Dia_US_Doses)[1]

Dia_US_Doses %>% select(pat_id, Exact_Month) %>% distinct() %>% count()

dim(physician_histories)[1]

physician_histories %>% select(pat_id, Exact_Month) %>% distinct() %>% count()


Dia_US_Doses <- physician_histories %>% select(-`<NA>`) %>%
left_join(Dia_US_Doses %>% select(pat_id, Exact_Month, GLP1_Oral) %>% distinct()) 

temp <- Dia_US_Doses %>% select(-pat_id)

dim(temp)

temp[is.na(temp)] <- 0


summary(glm(GLP1_Oral ~ . , data=temp))



# ----------------
# Predict a rybelsus patient ------------------

library(tidyverse)
library(data.table)
library(hacksaw)
library(splitstackshape)
library(spatstat)
library(lubridate)
options(scipen = 999)

DANU_Ingredients <- fread("DIA Analysis Results 1.1/DANU Ingredients.txt", integer64 = "character", stringsAsFactors = F)
DANU_Ingredients <- DANU_Ingredients %>%  separate(drug_id, c('class', 'molecule'))

string_Biguanide       <- paste0("\\b(",paste0(DANU_Ingredients$molecule[DANU_Ingredients$drug_group == "Biguanide"], collapse = "|"),")\\b")
string_Antidiabetic    <- paste0("\\b(",paste0(DANU_Ingredients$molecule[DANU_Ingredients$drug_group == "Antidiabetic"], collapse = "|"),")\\b")
string_DPP4            <- paste0("\\b(",paste0(DANU_Ingredients$molecule[DANU_Ingredients$drug_group == "DPP4"], collapse = "|"),")\\b")
string_SGLT2           <- paste0("\\b(",paste0(DANU_Ingredients$molecule[DANU_Ingredients$drug_group == "SGLT2"], collapse = "|"),")\\b")
string_Insulin         <- paste0("\\b(",paste0(DANU_Ingredients$molecule[DANU_Ingredients$drug_group == "Insulin"], collapse = "|"),")\\b")
string_OralGLP1        <- paste0("\\b(",paste0(DANU_Ingredients$molecule[DANU_Ingredients$drug_group == "GLP1 Oral"], collapse = "|"),")\\b")
string_InjectableGLP1  <- paste0("\\b(",paste0(DANU_Ingredients$molecule[DANU_Ingredients$drug_group == "GLP1 Injectable"], collapse = "|"),")\\b")

DIA_Flows_Aux_Long <- fread("DIA Analysis Results 1.1/DIA_Flows_Aux_Long.txt", integer64 = "character", stringsAsFactors = F)
Treatment_exp_Vector <- fread("DIA Analysis Results 1.1/Treatment_exp_Vector.txt")
DIA_Flows_Aux_Long <- Treatment_exp_Vector %>% inner_join(DIA_Flows_Aux_Long)
DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% select(patient, weight, p1, p2, d1, d2, flow) 
DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% filter(p2<49 & p2>=37 )

DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% mutate(BiguanideExp = ifelse(grepl(string_Biguanide,d1)|grepl(string_Biguanide,d2),1,0))
DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% group_by(patient) %>% mutate(BiguanideExp = cumsum(BiguanideExp))
DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% group_by(patient) %>% mutate(BiguanideExp = ifelse(BiguanideExp==0,0,1))

DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% mutate(AntidiabeticExp = ifelse(grepl(string_Antidiabetic,d1)|grepl(string_Antidiabetic,d2),1,0))
DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% group_by(patient) %>% mutate(AntidiabeticExp = cumsum(AntidiabeticExp))
DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% group_by(patient) %>% mutate(AntidiabeticExp = ifelse(AntidiabeticExp==0,0,1))
 
DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% mutate(DPP4Exp = ifelse(grepl(string_DPP4,d1)|grepl(string_DPP4,d2),1,0))
DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% group_by(patient) %>% mutate(DPP4Exp = cumsum(DPP4Exp))
DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% group_by(patient) %>% mutate(DPP4Exp = ifelse(DPP4Exp==0,0,1))

DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% mutate(SGLT2Exp = ifelse(grepl(string_SGLT2,d1)|grepl(string_SGLT2,d2),1,0))
DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% group_by(patient) %>% mutate(SGLT2Exp = cumsum(SGLT2Exp))
DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% group_by(patient) %>% mutate(SGLT2Exp = ifelse(SGLT2Exp==0,0,1))
 
DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% mutate(OralExp = ifelse(grepl(string_OralGLP1,d1)|grepl(string_OralGLP1,d2),1,0))
DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% group_by(patient) %>% mutate(OralExp = ifelse(OralExp==0,0,1))
 
DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% mutate(InjExp = ifelse(grepl(string_InjectableGLP1,d1)|grepl(string_InjectableGLP1,d2),1,0))
DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% group_by(patient) %>% mutate(InjExp = cumsum(InjExp))
DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% group_by(patient) %>% mutate(InjExp = ifelse(InjExp==0,0,1))

DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% mutate(InsulinExp = ifelse(grepl(string_Insulin,d1)|grepl(string_Insulin,d2),1,0))
DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% group_by(patient) %>% mutate(InsulinExp = cumsum(InsulinExp))
DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% group_by(patient) %>% mutate(InsulinExp = ifelse(InsulinExp==0,0,1))


DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% left_join(DIA_Flows_Aux_Long %>% group_by(patient) %>% summarise(N_flows=sum(flow)))
DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% left_join(
  DIA_Flows_Aux_Long %>% select(patient, d2) %>% distinct() %>% filter(d2!="-") %>% group_by(patient) %>% count() %>% rename("N_lines"="n"))


DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% select(-c(p1, d1, d2, weight))

DANU_Demographics <- fread("DANU Demographics 1.1/DANU Demographics.txt", integer64 = "character", stringsAsFactors = F)
DANU_Demographics <- DANU_Demographics %>% select(c(patid, age, gender)) %>% rename("patient"="patid")
DANU_Demographics <- DANU_Demographics %>% mutate(gender=ifelse(gender=="F",1,0))

DIA_Flows_Aux_Long <- DANU_Demographics %>% inner_join(DIA_Flows_Aux_Long)

DANU_Measures <- fread("DANU Measures 1.1/DANU Measures.txt",  integer64 = "character", stringsAsFactors = F)
DANU_Measures <- DANU_Measures %>% filter(test=="HbA1c Level"|test=="BMI") %>% rename("patient"="patid") %>% 
  inner_join(DIA_Flows_Aux_Long %>% select(patient) %>% distinct())
DANU_Measures <- DANU_Measures %>% select(patient, test,  value) %>% distinct()
DANU_Measures <- DANU_Measures %>% group_by(patient, test) %>% filter(value==max(value)) %>% distinct() %>% ungroup()


DANU_Measures <- DANU_Measures %>% filter(test=="HbA1c Level") %>% select(-test) %>% rename("HbA1c"="value") %>%
  full_join(
    DANU_Measures %>% filter(test=="BMI") %>% select(-test) %>% rename("BMI"="value")
  ) %>% distinct() %>% select(patient, HbA1c, BMI) %>% drop_na()


DIA_Flows_Aux_Long <- DIA_Flows_Aux_Long %>% inner_join(DANU_Measures %>% select(patient) %>% distinct()) %>% left_join(DANU_Measures)

df <- DIA_Flows_Aux_Long %>% filter(p2==48) %>% select(-p2)

df %>% group_by(OralExp) %>% count()

DANU_Events <- fread("DANU Events 1.1/DANU Events.txt")
DANU_Events <- DANU_Events %>% filter(grepl("E11", code))
DANU_Events <- DANU_Events %>% select(patid, claimed) %>% distinct() %>% mutate(claimed=as.Date(claimed)) %>%
  filter(claimed>="2020-07-01" & claimed<="2021-07-01") %>% group_by(patid) %>% count() %>% rename("Dx_dates"="n")

df <- df %>% left_join(DANU_Events, by=c("patient"="patid")) %>% mutate(Dx_dates=ifelse(is.na(Dx_dates),0,Dx_dates))


library(randomForest)
library(xgboost)
library(caret)

sum(is.na(df))
df[is.na(df)] <- 0 

names(df)
df <- df %>% select(-flow)

train <- df %>% ungroup() %>% sample_n(60000) 
test <- df %>% ungroup() %>% anti_join(train) 

train %>% group_by(OralExp) %>% count()
test %>% group_by(OralExp) %>% count()

train <- train %>% select(-c(patient))
test <- test %>% select(-c(patient))

train <- train %>% group_by(OralExp) %>% sample_n(250) %>% ungroup()
test <- test %>% group_by(OralExp) %>% sample_n(125, replace = T) %>% ungroup()

train$OralExp <- as.factor(train$OralExp)
test$OralExp <- as.factor(test$OralExp)

modelAll_1_randomForest <- randomForest(as.factor(OralExp) ~ . , data = train, type="classification")

data.frame(modelAll_1_randomForest$importance) %>% arrange(-MeanDecreaseGini)

predict <- predict(modelAll_1_randomForest, test, type= 'response')

predict <- test %>% bind_cols(data.frame(predict))

predict  %>% group_by(predict, OralExp) %>% count() 


library(breakDown)
library(DALEX)
library(ranger)

explainer_ranger <- explain(modelAll_1_randomForest, data = train, y =  train$OralExp, label = "model_ranger")

bd_ranger <- predict_parts_break_down(explainer_ranger, new_observation = train[977,])
head(bd_ranger)
plot(bd_ranger)


bd_ranger <- predict_parts_break_down(explainer_ranger, new_observation = train[232,])
head(bd_ranger)
plot(bd_ranger)










shap.score.rank <- function(xgb_model = xgb_mod, shap_approx = TRUE, 
                            X_train = mydata$train_mm){
  require(xgboost)
  require(data.table)
  shap_contrib <- predict(xgb_model, X_train,
                          predcontrib = TRUE, approxcontrib = shap_approx)
  shap_contrib <- as.data.table(shap_contrib)
  shap_contrib[,BIAS:=NULL]
  cat('make SHAP score by decreasing order\n\n')
  mean_shap_score <- colMeans(abs(shap_contrib))[order(colMeans(abs(shap_contrib)), decreasing = T)]
  return(list(shap_score = shap_contrib,
              mean_shap_score = (mean_shap_score)))
}

# a function to standardize feature values into same range
std1 <- function(x){
  return ((x - min(x, na.rm = T))/(max(x, na.rm = T) - min(x, na.rm = T)))
}


# prep shap data
shap.prep <- function(shap  = shap_result, X_train = mydata$train_mm, top_n){
  require(ggforce)
  # descending order
  if (missing(top_n)) top_n <- dim(X_train)[2] # by default, use all features
  if (!top_n%in%c(1:dim(X_train)[2])) stop('supply correct top_n')
  require(data.table)
  shap_score_sub <- as.data.table(shap$shap_score)
  shap_score_sub <- shap_score_sub[, names(shap$mean_shap_score)[1:top_n], with = F]
  shap_score_long <- melt.data.table(shap_score_sub, measure.vars = colnames(shap_score_sub))
  
  # feature values: the values in the original dataset
  fv_sub <- as.data.table(X_train)[, names(shap$mean_shap_score)[1:top_n], with = F]
  # standardize feature values
  fv_sub_long <- melt.data.table(fv_sub, measure.vars = colnames(fv_sub))
  fv_sub_long[, stdfvalue := std1(value), by = "variable"]
  # SHAP value: value
  # raw feature value: rfvalue; 
  # standarized: stdfvalue
  names(fv_sub_long) <- c("variable", "rfvalue", "stdfvalue" )
  shap_long2 <- cbind(shap_score_long, fv_sub_long[,c('rfvalue','stdfvalue')])
  shap_long2[, mean_value := mean(abs(value)), by = variable]
  setkey(shap_long2, variable)
  return(shap_long2) 
}

plot.shap.summary <- function(data_long){
  x_bound <- max(abs(data_long$value))
  require('ggforce') # for `geom_sina`
  plot1 <- ggplot(data = data_long)+
    coord_flip() + 
    # sina plot: 
    geom_sina(aes(x = variable, y = value, color = stdfvalue)) +
    # print the mean absolute value: 
    geom_text(data = unique(data_long[, c("variable", "mean_value"), with = F]),
              aes(x = variable, y=-Inf, label = sprintf("%.3f", mean_value)),
              size = 1, alpha = 0.5,
              hjust = -0.2, 
              fontface = "bold") + # bold
    # # add a "SHAP" bar notation
    # annotate("text", x = -Inf, y = -Inf, vjust = -0.2, hjust = 0, size = 3,
    #          label = expression(group("|", bar(SHAP), "|"))) + 
    scale_color_gradient(low="gold1", high="blue4", 
                         breaks=c(0,1), labels=c("Low","High")) +
    theme_bw() + 
    theme(axis.line.y = element_blank(), axis.ticks.y = element_blank(), # remove axis line
          legend.position="bottom") + 
    geom_hline(yintercept = 0) + # the vertical line
    scale_y_continuous(limits = c(-x_bound, x_bound)) +
    # reverse the order of features
    scale_x_discrete(limits = rev(levels(data_long$variable)) 
    ) + 
    labs(y = "SHAP value (impact on model output)", x = "", color = "Feature value") 
  return(plot1)
}






var_importance <- function(shap_result, top_n=10)
{
  var_importance=tibble(var=names(shap_result$mean_shap_score), importance=shap_result$mean_shap_score)
  
  var_importance=var_importance[1:top_n,]
  
  ggplot(var_importance, aes(x=reorder(var,importance), y=importance)) + 
    geom_bar(stat = "identity") + 
    coord_flip() + 
    theme_light() + 
    theme(axis.title.y=element_blank()) 
}


names(train)

model_hd = xgboost(data = as.matrix(train[,-7]),
                   nround = 5000,
                   objective = "binary:logistic",
                   label=as.matrix(train[,7]))  



shap_result = shap.score.rank(xgb_model = model_hd, 
                              X_train = as.matrix(train[,-7]),
                              shap_approx = F)

var_importance(shap_result, top_n=8)

shap_long_hd = shap.prep(X_train = as.matrix(train[,-8]) , top_n = 7)

plot.shap.summary(data_long = shap_long_hd)


result <- data.frame(train %>% group_by(OralExp) %>% summarise_all(mean))

result <- data.frame(names(result)) %>% bind_cols(result %>% transpose())
 
# -----------------

# Persistency per  drug_group ALL episodes ------------------------------------------------------------
DANU_Ingredients <- fread("DIA Analysis Results 1.1/DANU Ingredients.txt", integer64 = "character", stringsAsFactors = F)
DANU_Ingredients <- DANU_Ingredients %>%  separate(drug_id, c('class', 'molecule'))
DANU_Ingredients <- DANU_Ingredients %>% select(molecule, generic_name, drug_group)
names(DANU_Ingredients)[1] <- "Drugs"
DANU_Ingredients$Drugs <- as.numeric(DANU_Ingredients$Drugs)


DIA_Drug_Histories <- read.table("DIA Analysis Results 1.1/DIA Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
DIA_Drug_Histories <- gather(DIA_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
DIA_Drug_Histories$Month <- as.character(DIA_Drug_Histories$Month)
DIA_Drug_Histories$Month <- parse_number(DIA_Drug_Histories$Month)
DIA_Drug_Histories <- DIA_Drug_Histories %>% filter(Drugs != "-")  %>% select(-c(disease))
DIA_Drug_Histories <- separate_rows(DIA_Drug_Histories, Drugs, sep = ",", convert=T)
DIA_Drug_Histories <- DIA_Drug_Histories %>% left_join(DANU_Ingredients %>% select(-generic_name))
DIA_Drug_Histories <- DIA_Drug_Histories %>% select(-Drugs) %>% distinct() 

DIA_Drug_Histories <- DIA_Drug_Histories %>% arrange(patient, weight, drug_group, Month)

DIA_Drug_Histories <- DIA_Drug_Histories %>% group_by(patient, weight, drug_group) %>% count()

DIA_Drug_Histories %>% ungroup() %>% group_by(drug_group) %>% summarise(mean=weighted.mean(n, as.numeric(weight)))

# 1 Antidiabetic    26.7 
# 2 Biguanide       27.4 
# 3 DPP4            21.7 
# 4 GLP1 Injectable 17.4 
# 5 GLP1 Oral        6.03
# 6 Insulin         22.5 
# 7 SGLT2           18.3 

DIA_Drug_Histories <- DIA_Drug_Histories %>% ungroup() %>% group_by(drug_group, n) %>% summarise(Total=sum(as.numeric(weight))) %>% arrange(drug_group, n)


temp <- data.frame(DIA_Drug_Histories %>% group_by(drug_group) %>% mutate(Total_cum=cumsum(Total)) %>% mutate(grand_Total=sum(Total)) %>%
             mutate(Remain=grand_Total-lag(Total_cum)) %>% mutate(Remain=ifelse(is.na(Remain), grand_Total, Remain)) %>%
             mutate(Remain=Remain/grand_Total) %>%
             select(drug_group, n, Remain)) 



temp %>%
  mutate(drug_group=ifelse(drug_group=="GLP1 Oral", "GLP1 Oral (6.03m)", 
                             ifelse(drug_group=="GLP1 Injectable", "GLP1 Injectable (17.4m)",
                                    ifelse(drug_group=="SGLT2", "SGLT2 (18.3m)",
                                           ifelse(drug_group=="DPP4", "DPP4 (21.7m)",
                                                  ifelse(drug_group=="Insulin", "Insulin (22.5m)",
                                                         ifelse(drug_group=="Antidiabetic", "Antidiabetic (26.7m)", "Biguanide (27.4m)"
                                    
                                    ))))))) %>%
  mutate(drug_group=factor(drug_group, levels=c("GLP1 Oral (6.03m)", "GLP1 Injectable (17.4m)", "SGLT2 (18.3m)", 
                                                    "DPP4 (21.7m)", "Insulin (22.5m)", "Antidiabetic (26.7m)", "Biguanide (27.4m)"))) %>%
  ggplot(aes(n, Remain, colour=drug_group, fill=drug_group)) +
  geom_smooth(se=T, linewidth=1, alpha=0.4) +
  ylim(0,1) +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent, breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)) +
  #scale_x_continuous(breaks = c(-60, -50, -40, -30, -20, -10, 0)) +
  scale_colour_manual(values=c("#C01E00","#A053B1","#FCCC29","#211897","#7DA21B", "#188597","#ACBEC0")) +
  scale_fill_manual(values=c("#C01E00","#A053B1","#FCCC29","#211897","#7DA21B", "#188597","#ACBEC0")) +
  xlab("\n Number of Months Elapsed Since Therapy Initiation \n ") + 
  ylab("Proportion of Patients \n Still ON Each Class \n")

# -------------


# % Of time on each class START Year 2-3  ------------------------------------------------------------
DANU_Ingredients <- fread("DIA Analysis Results 1.1/DANU Ingredients.txt", integer64 = "character", stringsAsFactors = F)
DANU_Ingredients <- DANU_Ingredients %>%  separate(drug_id, c('class', 'molecule'))
DANU_Ingredients <- DANU_Ingredients %>% select(molecule, generic_name, drug_group)
names(DANU_Ingredients)[1] <- "Drugs"
DANU_Ingredients$Drugs <- as.numeric(DANU_Ingredients$Drugs)


DIA_Drug_Histories <- read.table("DIA Analysis Results 1.1/DIA Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
DIA_Drug_Histories <- gather(DIA_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
DIA_Drug_Histories$Month <- as.character(DIA_Drug_Histories$Month)
DIA_Drug_Histories$Month <- parse_number(DIA_Drug_Histories$Month)
DIA_Drug_Histories <- DIA_Drug_Histories %>% filter(Drugs != "-")  %>% select(-c(disease))
DIA_Drug_Histories <- separate_rows(DIA_Drug_Histories, Drugs, sep = ",", convert=T)
DIA_Drug_Histories <- DIA_Drug_Histories %>% left_join(DANU_Ingredients %>% select(-generic_name))
DIA_Drug_Histories <- DIA_Drug_Histories %>% select(-Drugs) %>% distinct() 

DIA_Drug_Histories <- DIA_Drug_Histories %>% arrange(patient, weight, drug_group, Month)

First_Month <- DIA_Drug_Histories %>% group_by(patient, weight, drug_group) %>% filter(Month==min(Month))
First_Month <- First_Month %>% ungroup()
names(First_Month)[3] <- "First"

First_Month <- First_Month %>% filter(First>=12&First<=37) %>% select(patient, drug_group)


DIA_Drug_Histories <- DIA_Drug_Histories %>% inner_join(First_Month)


DIA_Drug_Histories <- DIA_Drug_Histories %>% group_by(patient, weight, drug_group) %>% count()

DIA_Drug_Histories %>% ungroup() %>% group_by(drug_group) %>% summarise(mean=weighted.mean(n, as.numeric(weight)))

# 1 Antidiabetic     19.0
# 4 GLP1 Injectable  18.8


DIA_Drug_Histories <- DIA_Drug_Histories %>% ungroup() %>% group_by(drug_group, n) %>% summarise(Total=sum(as.numeric(weight))) %>% arrange(drug_group, n)


temp <- data.frame(DIA_Drug_Histories %>% group_by(drug_group) %>% mutate(Total_cum=cumsum(Total)) %>% mutate(grand_Total=sum(Total)) %>%
             mutate(Remain=grand_Total-lag(Total_cum)) %>% mutate(Remain=ifelse(is.na(Remain), grand_Total, Remain)) %>%
             mutate(Remain=Remain/grand_Total) %>%
             select(drug_group, n, Remain)) 



temp %>%
  mutate(drug_group=ifelse(drug_group=="GLP1 Oral", "GLP1 Oral (11.0m)", 
                             ifelse(drug_group=="Insulin", "Insulin (12.7m)",
                                    ifelse(drug_group=="DPP4", "DPP4 (16.5m)",
                                           ifelse(drug_group=="Biguanide", "Biguanide (18.2m)",
                                                  ifelse(drug_group=="SGLT2", "SGLT2 (18.7m)",
                                                         ifelse(drug_group=="GLP1 Injectable", "GLP1 Injectable (18.8m)", "Antidiabetic (19.0m)"
                                    
                                    ))))))) %>%
  mutate(drug_group=factor(drug_group, levels=c("GLP1 Oral (11.0m)", "Insulin (12.7m)", "DPP4 (16.5m)", 
                                                    "Biguanide (18.2m)", "SGLT2 (18.7m)", "GLP1 Injectable (18.8m)", "Antidiabetic (19.0m)"))) %>%
  ggplot(aes(n, Remain, colour=drug_group, fill=drug_group)) +
  geom_smooth(se=T, linewidth=1, alpha=0.4) +
  ylim(0,1) +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent, breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)) +
  #scale_x_continuous(breaks = c(-60, -50, -40, -30, -20, -10, 0)) +
  scale_colour_manual(values=c("#C01E00", "#7DA21B" ,"#211897", "#ACBEC0","#FCCC29", "#A053B1", "#188597")) +
  scale_fill_manual(values=c("#C01E00", "#7DA21B" ,"#211897", "#ACBEC0","#FCCC29", "#A053B1", "#188597")) +
  xlab("\n Number of Months Elapsed Since Therapy Initiation \n ") + 
  ylab("Proportion of Patients \n Still ON Each Class \n")


# ----------

# Persistency per drug_class ALL episodes ------------------------------------------------------------
DANU_Ingredients <- fread("DIA Analysis Results 1.1/DANU Ingredients.txt", integer64 = "character", stringsAsFactors = F)
DANU_Ingredients <- DANU_Ingredients %>%  separate(drug_id, c('class', 'molecule'))
DANU_Ingredients <- DANU_Ingredients %>% select(molecule, generic_name, drug_class)
names(DANU_Ingredients)[1] <- "Drugs"
DANU_Ingredients$Drugs <- as.numeric(DANU_Ingredients$Drugs)


DIA_Drug_Histories <- read.table("DIA Analysis Results 1.1/DIA Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
DIA_Drug_Histories <- gather(DIA_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
DIA_Drug_Histories$Month <- as.character(DIA_Drug_Histories$Month)
DIA_Drug_Histories$Month <- parse_number(DIA_Drug_Histories$Month)
DIA_Drug_Histories <- DIA_Drug_Histories %>% filter(Drugs != "-")  %>% select(-c(disease))
DIA_Drug_Histories <- separate_rows(DIA_Drug_Histories, Drugs, sep = ",", convert=T)
DIA_Drug_Histories <- DIA_Drug_Histories %>% left_join(DANU_Ingredients %>% select(-generic_name))
DIA_Drug_Histories <- DIA_Drug_Histories %>% select(-Drugs) %>% distinct() 

DIA_Drug_Histories <- DIA_Drug_Histories %>% arrange(patient, weight, drug_class, Month)

DIA_Drug_Histories <- DIA_Drug_Histories %>% group_by(patient, weight, drug_class) %>% count()

DIA_Drug_Histories %>% ungroup() %>% group_by(drug_class) %>% 
  summarise(mean=weighted.mean(n, as.numeric(weight))) %>%
  arrange(-mean)

#    drug_class       mean
#  1 Biguanide       27.4 
#  2 Sulfonylurea    26.6 
#  3 Insulin Long    24.9 
#  4 Glitazone       22.4 
#  5 Insulin Short   21.8 
#  6 DPP4            21.7 
#  7 SGLT2           18.3 
#  8 GLP1 Injectable 17.4 
#  9 Glinide         17.3 
# 10 AGI             13.8 
# 11 Antidiabetic    13.6 
# 12 GLP1 Oral        6.03
# 13 Insulin Therapy  5.82

DIA_Drug_Histories <- DIA_Drug_Histories %>% ungroup() %>% group_by(drug_class, n) %>% summarise(Total=sum(as.numeric(weight))) %>% arrange(drug_class, n)


DIA_Drug_Histories %>% group_by(drug_class) %>% summarise(n=sum(Total))


temp <- data.frame(DIA_Drug_Histories %>% group_by(drug_class) %>% mutate(Total_cum=cumsum(Total)) %>% mutate(grand_Total=sum(Total)) %>%
             mutate(Remain=grand_Total-lag(Total_cum)) %>% mutate(Remain=ifelse(is.na(Remain), grand_Total, Remain)) %>%
             mutate(Remain=Remain/grand_Total) %>%
             select(drug_class, n, Remain)) 



temp %>%
  filter(drug_class %in% c("GLP1 Injectable", "Insulin Long", "Insulin Short", "Insulin Therapy")) %>%
  mutate(drug_class=ifelse(drug_class=="Insulin Therapy", "Insulin Therapy (5.8m)", 
                             ifelse(drug_class=="GLP1 Injectable", "GLP1 Injectable (17.4m)",
                                    ifelse(drug_class=="Insulin Short", "Insulin Short (21.8m)", "Insulin Long (24.9m)")))) %>%
  mutate(drug_class=factor(drug_class, levels=c("Insulin Therapy (5.8m)", "GLP1 Injectable (17.4m)", "Insulin Short (21.8m)", "Insulin Long (24.9m)"))) %>%
  ggplot(aes(n, Remain, colour=drug_class, fill=drug_class)) +
  geom_smooth(se=T, linewidth=1, alpha=0.4) +
  ylim(0,1) +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent, breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)) +
  #scale_x_continuous(breaks = c(-60, -50, -40, -30, -20, -10, 0)) +
  scale_colour_manual(values=c("#BFEA7B","#DB3831","#22B7C3","#135F97")) +
  scale_fill_manual(values=c("#BFEA7B","#DB3831","#22B7C3","#135F97")) +
  xlab("\n Number of Months Elapsed Since Therapy Initiation \n ") + 
  ylab("Proportion of Patients \n Still ON Each Class \n")

# -------------
# % Of time on each class  ------------------------------------------------------------
DANU_Ingredients <- fread("DIA Analysis Results 1.1/DANU Ingredients.txt", integer64 = "character", stringsAsFactors = F)
DANU_Ingredients <- DANU_Ingredients %>%  separate(drug_id, c('class', 'molecule'))
DANU_Ingredients <- DANU_Ingredients %>% select(molecule, generic_name, drug_group)
names(DANU_Ingredients)[1] <- "Drugs"
DANU_Ingredients$Drugs <- as.numeric(DANU_Ingredients$Drugs)


DIA_Drug_Histories <- read.table("DIA Analysis Results 1.1/DIA Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
DIA_Drug_Histories <- gather(DIA_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
DIA_Drug_Histories$Month <- as.character(DIA_Drug_Histories$Month)
DIA_Drug_Histories$Month <- parse_number(DIA_Drug_Histories$Month)
DIA_Drug_Histories <- DIA_Drug_Histories %>% filter(Drugs != "-")  %>% select(-c(disease))
DIA_Drug_Histories <- separate_rows(DIA_Drug_Histories, Drugs, sep = ",", convert=T)
DIA_Drug_Histories <- DIA_Drug_Histories %>% left_join(DANU_Ingredients %>% select(-generic_name))
DIA_Drug_Histories <- DIA_Drug_Histories %>% select(-Drugs) %>% distinct() 

DIA_Drug_Histories <- DIA_Drug_Histories %>% arrange(patient, weight, drug_group, Month)

First_Month <- DIA_Drug_Histories %>% group_by(patient, weight, drug_group) %>% filter(Month==min(Month))
First_Month <- First_Month %>% ungroup()
names(First_Month)[3] <- "First"


First_Month <- First_Month %>%  mutate(VIZ=60-First) %>% select(patient, drug_group, VIZ)


DIA_Drug_Histories <- DIA_Drug_Histories %>% group_by(patient, weight, drug_group) %>% count()

DIA_Drug_Histories %>% left_join(First_Month) %>%
  ungroup() %>% mutate(perc=n/VIZ) %>% filter(VIZ!=0) %>%
  group_by(drug_group) %>% summarise(mean=weighted.mean(perc, as.numeric(weight)))


# 1 Antidiabetic    0.643
# 2 Biguanide       0.657
# 3 DPP4            0.562
# 4 GLP1 Injectable 0.673
# 5 GLP1 Oral       0.689
# 6 Insulin         0.545
# 7 SGLT2           0.682



DIA_Drug_Histories <- DIA_Drug_Histories %>% left_join(First_Month) %>%
  ungroup() %>% mutate(perc=(n-1)/VIZ) %>% filter(VIZ!=0) %>% filter(perc>0)

DIA_Drug_Histories %>% group_by(drug_group) %>% summarise(mean=weighted.mean(perc, as.numeric(weight)))
DIA_Drug_Histories %>% group_by(drug_group) %>% summarise(sd=sd(perc))



DIA_Drug_Histories %>%
  filter(drug_group=="Insulin"|grepl("GLP",drug_group)) %>%
  ggplot(aes(perc, colour=drug_group, fill=drug_group)) +
  geom_density(alpha=0.5) +
  theme_minimal()


DIA_Drug_Histories %>%
  filter(drug_group=="Insulin"|grepl("GLP",drug_group)) %>%
  mutate(drug_group=ifelse(drug_group=="Insulin", "Insulin (58%)",
                           ifelse(drug_group=="GLP1 Oral", "GLP1 Oral (66%)", "GLP1 Injectable (63%)"))) %>%
  mutate(drug_group=factor(drug_group, levels=c("GLP1 Oral (66%)", "GLP1 Injectable (63%)", "Insulin (58%)"))) %>%
  ggplot(aes(drug_group, perc*100, colour=drug_group, fill=drug_group)) +
  geom_boxplot(alpha=0.3, notch = TRUE, notchwidth = 0.2) +
  theme_minimal() +
  scale_colour_manual(values=c("#C01E00","#188597","#7DA21B")) +
  scale_fill_manual(values=c("#C01E00","#188597","#7DA21B")) +
  xlab("\n Drug Group \n ") + 
  ylab("% of Time \n Actually Spent ON each Class \n")



# -----------
