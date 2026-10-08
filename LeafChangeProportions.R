## This file filters the Organ Score data to generate Figure 5 of manuscript
#---Distribution of the change in # leaves per 0.5 weeks for each species & treatment

#Libraries
library(tidyverse)

#Load data
#-- Species: 1=Arec, 2=Bbra, 3=Pmir, 4=Vraf
#-- Treatments: 1=No N, 2=Low N, 3=High N
#-- Organ Scores: 0=Seed, 1=seed coat split, 2=root radical, 3=hypocotyl/cotyledon
#                 4=1 leaf, 5=2 leaves, 6=3 leaves, 7=4+ leaves
data <- read_csv("By_Seed_CSV3.csv", show_col_types = FALSE)

temp <- data %>%
  select(Genus,Nitrogen,
         OrganScore3,OrganScore7,OrganScore10,OrganScore14,OrganScore17,OrganScore21) %>%
  mutate(plant_id = row_number())

#------------------------
#Filter Organ Score Data 
#------------------------
OSRateData <- data %>%
  select(Genus,Nitrogen,
         OrganScore3,OrganScore7,OrganScore10,OrganScore14,OrganScore17,OrganScore21) %>%
  mutate(plant_id = row_number()) %>%
  rowwise() %>%
  mutate(Day3to7 = {
    idx <- which(!is.na(OrganScore3) & !is.na(OrganScore7) & OrganScore7 >= 4 & OrganScore7!=0)
    if (length(idx)==0) NA_integer_ else (OrganScore7 - max(3,OrganScore3))
  }) %>%
  mutate(Day7to10 = {
    idx <- which(!is.na(OrganScore7) & !is.na(OrganScore10) & OrganScore10 >= 4 & OrganScore10!=0)
    if (length(idx)==0) NA_integer_ else (OrganScore10 - max(3,OrganScore7))
  }) %>%
  mutate(Day10to14 = {
    idx <- which(!is.na(OrganScore10) & !is.na(OrganScore14) & OrganScore14 >= 4 & OrganScore14!=0)
    if (length(idx)==0) NA_integer_ else (OrganScore14 - max(3,OrganScore10))
  }) %>%
  mutate(Day14to17 = {
    idx <- which(!is.na(OrganScore14) & !is.na(OrganScore17) & OrganScore17 >= 4 & OrganScore17!=0)
    if (length(idx)==0) NA_integer_ else (OrganScore17 - max(3,OrganScore14))
  }) %>%
  mutate(Day17to21 = {
    idx <- which(!is.na(OrganScore17) & !is.na(OrganScore21) & OrganScore21 >= 4 & OrganScore21!=0)
    if (length(idx)==0) NA_integer_ else (OrganScore21 - max(3,OrganScore17))
  }) %>%
  ungroup() %>%
  mutate(Species = case_when(
    Genus == 1 ~ "italic('Aechmea recurvata')",
    Genus == 2 ~ "italic('Billbergia brasiliensis')",
    Genus == 3 ~ "italic('Puya mirabilis')",
    Genus == 4 ~ "italic('Vriesea rafaelii')",
    TRUE       ~ NA_character_)) %>%
  mutate(Treatment = case_when(
    Nitrogen == 1 ~ "No~Nitrogen",
    Nitrogen == 2 ~ "Low~Nitrogen",
    Nitrogen == 3 ~ "High~Nitrogen",
    TRUE       ~ NA_character_)) %>%
  select(plant_id,Species,Treatment,
         OrganScore3, Day3to7,
         OrganScore7, Day7to10,
         OrganScore10,Day10to14,
         OrganScore14,Day14to17,
         OrganScore17,Day17to21) %>%
  pivot_longer(cols = starts_with("Day"),
               names_to = "DayΔ",
               values_to = "OSΔ") %>%
  pivot_longer(cols = starts_with("OrganScore"),
               names_to = "Day",
               values_to = "OrganScore") %>%
  mutate(Day = parse_number(Day)) %>%
  filter(Day == as.numeric(str_extract(DayΔ, "\\d+"))) %>%
  select(plant_id,Species,Treatment,Day,OrganScore,DayΔ,OSΔ)

#--- Calculate proportion of seedlings in each change category
OSRateProportions <- OSRateData %>% 
  summarize(.by = c(Species, Treatment, DayΔ),
            num_seedlings = sum(OrganScore>0, na.rm=TRUE),
            countneg1 = sum(OSΔ==-1, na.rm=TRUE),
            count0 = sum(OSΔ==0, na.rm=TRUE),
            count1 = sum(OSΔ==1, na.rm=TRUE),
            count2 = sum(OSΔ==2, na.rm=TRUE),
            proportionneg1 = if_else(num_seedlings>0, countneg1/num_seedlings, 0),
            proportion0 = if_else(num_seedlings>0, count0/num_seedlings, 0),
            proportion1 = if_else(num_seedlings>0, count1/num_seedlings, 0),
            proportion2 = if_else(num_seedlings>0, count2/num_seedlings, 0))

#--- Pivot proprtions dataframe for ease of graphing
OSRateProportions_pivot <- OSRateProportions %>%
  select(Species,Treatment,DayΔ,proportionneg1,proportion0,proportion1,proportion2) %>%
  pivot_longer(cols = starts_with("proportion"),
               names_to = "LeafNumΔ",
               values_to = "proportion") %>%
  mutate(LeafNumΔ = case_when(
    LeafNumΔ=="proportionneg1" ~ as.numeric(-1),
    LeafNumΔ=="proportion0" ~ as.numeric(0),
    LeafNumΔ=="proportion1" ~ as.numeric(1),
    LeafNumΔ=="proportion2" ~ as.numeric(2) )) %>%
  mutate(DayΔ = case_when(
    DayΔ == "Day3to7"   ~ "0.5",
    DayΔ == "Day7to10"  ~ "1.0",
    DayΔ == "Day10to14" ~ "1.5",
    DayΔ == "Day14to17" ~ "2.0",
    DayΔ == "Day17to21" ~ "2.5",
    TRUE       ~ NA_character_))

#-----------------------------------
# Distribution Graph - by Time Step 
#-----------------------------------
ggplot(OSRateProportions_pivot,
  aes(x=DayΔ, y=proportion, fill=factor(LeafNumΔ))) +
  geom_col() + 
  facet_grid(Treatment~Species, drop=FALSE, labeller=label_parsed) + 
  theme_bw() +
  theme(legend.position="bottom",
        legend.justification=c("center","bottom"),
        legend.text = element_text(size=9),
        strip.text = element_text(size=10),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10)) +
  labs(x="Week", y="Proportion of Seedlings", fill="Change in # Leaves") +
  scale_fill_manual(values=c("-1"="#8856A7","0"="#E5F5E0","1"="#74C476","2"="#006D2C"))

