library(tidyverse)
library(data.table)
library(hacksaw)
library(splitstackshape)
library(spatstat)
library(lubridate)
library(openxlsx)
options(scipen = 999)

DANU_Drug_Utilizations <- fread("DANU utilizations/DANU Drug Utilizations.txt")

counts_df <- DANU_Drug_Utilizations %>% select(patid, weight, drug_ahfs_class) %>%
  distinct() %>% group_by(patid, weight) %>% count()

length(unique(counts_df$patid))

counts_df %>% ungroup() %>%
  mutate(n=ifelse(n>=60,60, n)) %>%
  ggplot(aes(n)) +
  geom_density()

counts_df <- counts_df %>% ungroup() %>% rename("rxs"="n")

DIA_Comorbidity_Annual_Inventories <- fread("DANU Comorbidity Annual Inventories/DIA Comorbidity Annual Inventories.txt")

gerd_pats <- DIA_Comorbidity_Annual_Inventories %>% mutate(tot=year1+year2+year3+year4+year5) %>% filter(tot!=0) %>%
  filter(diagnosis=="K21") %>% select(patid) %>% distinct()

countds_df_2 <- DIA_Comorbidity_Annual_Inventories %>% mutate(tot=year1+year2+year3+year4+year5) %>% filter(tot!=0) %>%
  group_by(patid, weight) %>% count()

countds_df_2 <- countds_df_2 %>% ungroup() %>% rename("dxs"="n")

temp <- countds_df_2 %>% inner_join(counts_df) %>% drop_na() 

temp %>% # inner_join(gerd_pats) %>% 
  ggplot(aes(x = dxs, y = rxs)) +
  geom_hex(bins = 40) +
  xlim(0,125) + ylim(0,40) +
  scale_fill_viridis_c() +
  theme_minimal()

DIA_Doses <- fread("DIA Doses 2.1/DIA Doses.txt")
DIA_Doses <- DIA_Doses %>% filter(grepl("GLP1", drug_class)) %>% select(pat_id) %>% distinct()
names(DIA_Doses) <- "patid"

plot <- temp %>% # inner_join(DIA_Doses) %>%
  # inner_join(gerd_pats) %>%
  ggplot(aes(x = dxs, y = rxs)) +
  geom_hex(bins = 40) +
  xlim(0,125) + ylim(0,50) +
  scale_fill_viridis_c() +
  theme_minimal() +
  xlab("\n # Unique ICD10 Codes [5 years]") + 
  ylab("# Unique Therapy Classes [5 years]") +
  theme_minimal() +
  theme(
    text = element_text(face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) 

plot

ggsave(file="../plot.svg", plot=plot, width=7, height=7)


library(tidyverse)

df <- tribble(
  ~group,        ~segment,                 ~pop,
  "Non-GERD",    "Non-GERD",               190,
  "GERD Rx",     "Comorbidities",          10,
  "GERD Rx",     "No Comorbidities",       15,
  "GERD No Rx",  "Comorbidities",          2.5,
  "GERD No Rx",  "No Comorbidities",       22.5
)

total_pop <- 240

df <- df %>%
  group_by(group) %>%
  mutate(
    group_total = sum(pop),
    width = group_total / total_pop,
    height = pop / group_total
  ) %>%
  ungroup()




df <- df %>%
  distinct(group, width) %>%
  mutate(
    xmax = cumsum(width),
    xmin = lag(xmax, default = 0)
  ) %>%
  right_join(df, by = c("group","width"))



df <- df %>%
  group_by(group) %>%
  arrange(segment) %>%
  mutate(
    ymax = cumsum(height),
    ymin = lag(ymax, default = 0)
  ) %>%
  ungroup()


df <- df %>%
  mutate(bucket = case_when(
    group == "Non-GERD" ~ "Non-GERD",
    group == "GERD Rx" & segment == "Comorbidities" ~ "GERD Rx - Sign. Comorb.",
    group == "GERD Rx" & segment == "No Comorbidities" ~ "GERD Rx - No Sign. Comorb.",
    group == "GERD No Rx" & segment == "Comorbidities" ~ "GERD No Rx - Sign Comorb.",
    group == "GERD No Rx" & segment == "No Comorbidities" ~ "GERD No Rx - No Sign. Comorb."
  ))


df <- df %>%
  mutate(
    pct_total = pop / 240,
    label = paste0(bucket, "\n", scales::percent(pct_total, accuracy = 0.1))
  )


plot <- ggplot(df) +
  geom_rect(
    aes(
      xmin = xmin, xmax = xmax,
      ymin = ymin, ymax = ymax,
      fill = bucket
    ),
    color = "white",
    linewidth = 1.7
  ) +
  geom_text(
    aes(
      x = (xmin + xmax)/2,
      y = (ymin + ymax)/2,
      label = label
    ),
    size = 4, fontface = "bold"
  ) +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values=c("#e2a3c7", "#91648E", "#bfd3e6", "#4D6FB0", "#E6E6E6")) +
  labs(
    x = "",
    y = "",
    fill = "Population segment"
  ) +
   theme_minimal() +
  theme_minimal() +
  theme(legend.position = "none",
    text = element_text(face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) 


plot

ggsave(file="../plot.svg", plot=plot, width=11, height=5)
