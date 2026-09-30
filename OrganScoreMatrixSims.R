# Libraries
library(tidyverse)

# Set working directory
setwd("/Users/toribarrow/Desktop/Megafolder/Project Seedling/MiscThesis/")

# Import data from matrix model simulations
data <- read_csv("OrganScoreSims.csv", show_col_types = FALSE)
OSsims <- data %>%
  mutate(Species = case_when(
    species == "Arec" ~ "italic('Aechmea recurvata')",
    species == "Bbra" ~ "italic('Billbergia brasiliensis')",
    species == "Pmir" ~ "italic('Puya mirabilis')",
    TRUE       ~ NA_character_)) %>%
  mutate(Treatment = case_when(
    treatment == "No N" ~ "No~Nitrogen",
    treatment == "Low N" ~ "Low~Nitrogen",
    treatment == "High N" ~ "High~Nitrogen",
    TRUE       ~ NA_character_)) %>%
  pivot_longer(cols = starts_with("x"),
             names_to = "State",
             values_to = "Num_Seedlings") %>%
  mutate(OrganScore = case_when(
    State == "x1" ~ "seed",
    State == "x2" ~ "split seed coat",
    State == "x3" ~ "root radical emergence",
    State == "x4" ~ "hypocotyl/cotyledon",
    State == "x5" ~ "1 leaf",
    State == "x6" ~ "≥2 leaves"))

# Ensure the correct state ordering
OSsims$OrganScore <- factor(OSsims$OrganScore, 
                            levels=c("seed","split seed coat",
                                     "root radical emergence","hypocotyl/cotyledon",
                                     "1 leaf","≥2 leaves"))

# Generate graphs
ggplot(OSsims, aes(x=week, y=Num_Seedlings, fill=OrganScore)) +
  geom_col() +
  facet_grid(Treatment~Species, drop=FALSE, labeller=label_parsed) +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.text = element_text(size=9),
        strip.text = element_text(size=10),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10)) +
  guides(fill = guide_legend(direction = "horizontal", nrow=1)) + 
  labs(x="Weeks", 
       y="Seedling Count (all simulations start with 100 seeds at wk 0)", 
       fill=NULL) +
  scale_fill_brewer(palette="YlGn", direction=1)  



