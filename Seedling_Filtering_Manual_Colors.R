#Libraries
library(tidyverse)
library(broom)
library(RColorBrewer)

#Load data
#-- Species: 1=Arec, 2=Bbra, 3=Pmir, 4=Vraf
#-- Treatments: 1=No N, 2=Low N, 3=High N
data <- read_csv("By_Seed_CSV3.csv", show_col_types = FALSE)

#----------------------------------------
#Filter Root Length Data for Regressions
#----------------------------------------
RootData <- data %>% 
  dplyr::select(Nitrogen,Genus,RootLength7,RootLength14,RootLength21) %>%
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
  mutate(plant_id = row_number()) %>%                                            # create ID for each plant   
  pivot_longer(cols = starts_with("RootLength"),
               names_to = "RootLengthDay",
               values_to = "RootLength") %>%
  mutate(day = parse_number(RootLengthDay)) %>%                                  # Parse out just the day number
  arrange(plant_id,day) %>%
  group_by(plant_id,Nitrogen,Species) %>%
  mutate(next_RootLength=lead(RootLength), next_day=lead(day)) %>%
  filter(!is.na(RootLength), !is.na(next_RootLength), next_day - day == 7) %>%
  ungroup() %>%
  dplyr::select(plant_id,Nitrogen,Genus,Species,Treatment,day,RootLength,next_day,next_RootLength)

#Regressions
for (i in 1:4){ # species i
  for (j in 1:3){ # treatment j
    model <- lm(next_RootLength ~ RootLength, data = RootData %>% filter(Genus==i & Nitrogen==j))
    cat("\n\nGenus ", i, "   Treatment ", j)
    print(tidy(model, conf.int=TRUE, conf.level=0.95))
    res <- resid(model)
    cat("Mean.Res=", mean(res), "  SD.Res=", sd(res))
}}

#Plot Root Length Data
ggplot(RootData, aes(x=RootLength, y=next_RootLength, color=Treatment)) +
  geom_point(alpha=0.25, size=2.8) + 
  geom_smooth(method="lm", se=FALSE, linewidth=1.2) +
  scale_color_manual(
    values = c(
      "No~Nitrogen" = "#377EB8",
      "Low~Nitrogen" = "#4DAF4A",
      "High~Nitrogen" = "#E41A1C"
    ),
    labels = function(x) parse(text = x)
  )+
  facet_wrap(~Species, scales="free", drop=FALSE, labeller=label_parsed) +
  theme_bw() +
  theme(legend.position=c(0.99,0.27),
        legend.justification=c("right","bottom"),
        legend.text = element_text(size=9),
        strip.text = element_text(size=10),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10)) +
  labs(x=expression(bold("Root Length at time") ~ italic(t) ~ bold("(mm)")), 
       y=expression(bold("Root Length at time") ~ italic(t+1) ~ bold("(mm)")),
       color=NULL)
  

#----------------------------------------
#Filter Shoot Length Data for Regressions
#----------------------------------------
ShootData <- data %>% 
  dplyr::select(Nitrogen,Genus,ShootLength7,ShootLength14,ShootLength21) %>%
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
  mutate(plant_id = row_number()) %>%                                            # create ID for each plant   
  pivot_longer(cols = starts_with("ShootLength"),
               names_to = "ShootLengthDay",
               values_to = "ShootLength") %>%
  mutate(day = parse_number(ShootLengthDay)) %>%                                  # Parse out just the day number
  arrange(plant_id,day) %>%
  group_by(plant_id,Nitrogen,Species) %>%
  mutate(next_ShootLength=lead(ShootLength), next_day=lead(day)) %>%
  filter(!is.na(ShootLength), !is.na(next_ShootLength), next_day - day == 7) %>%
  ungroup() %>%
  dplyr::select(plant_id,Nitrogen,Genus,Species,Treatment,day,ShootLength,next_day,next_ShootLength)

#Regressions
for (i in 1:4){ # species i
  for (j in 1:3){ # treatment j
    model <- lm(next_ShootLength ~ ShootLength, data = ShootData %>% filter(Genus==i & Nitrogen==j))
    cat("\n\nGenus ", i, "   Treatment ", j)
    print(tidy(model, conf.int=TRUE, conf.level=0.95))
    res <- resid(model)
    cat("Mean.Res=", mean(res), "  SD.Res=", sd(res))
  }}

#Plot Shoot Length Data
ggplot(ShootData, aes(x=ShootLength, y=next_ShootLength, color=Treatment)) +
  geom_point(alpha=0.25, size=2.8) + 
  geom_smooth(method="lm", se=FALSE, linewidth=1.2) +
  scale_color_manual(
    values = c(
      "No~Nitrogen" = "#377EB8",
      "Low~Nitrogen" = "#4DAF4A",
      "High~Nitrogen" = "#E41A1C"
    ),
    labels = function(x) parse(text = x)
  )+
  facet_wrap(~Species, scales="free", drop=FALSE, labeller=label_parsed) +
  theme_bw() +
  theme(legend.position=c(0.99,0.01),
        legend.justification=c("right","bottom"),
        legend.text = element_text(size=9),
        strip.text = element_text(size=10),
        axis.title = element_text(size = 12, face="bold"),
        axis.text = element_text(size = 10)) +
  labs(x=expression(bold("Shoot Length at time") ~ italic(t) ~ bold("(mm)")), 
       y=expression(bold("Shoot Length at time") ~ italic(t+1) ~ bold("(mm)")),
       color = NULL)


#----------------------------------------
#-- Stochastic Root/Shoot Ratio Models --
#----------------------------------------
#Define stochastic linear difference eqn
f_linear_stoch <- function(x,amin,amax,bmin,bmax){
  a <- runif(1, min=amin, max=amax)
  b <- runif(1, min=bmin, max=bmax)
  a*x+b
}

#Define recursion over fixed number of time steps (T=10)
simulate_diff_eq <- function(f, s0, r0, n_steps, genus, nitrogen, 
                             amin,amax,bmin,bmax,rmin,rmax,smin,smax) {
  x <- matrix(NA, nrow=n_steps+1, ncol=6)
  x[ ,1] <- matrix(genus, n_steps+1, 1)
  x[ ,2] <- matrix(nitrogen, n_steps+1, 1)
  x[ ,3] <- 1:(n_steps+1)
  x[1,4] <- s0 #initial shoot length
  x[1,5] <- r0 #initial root length
  x[1,6] <- r0/s0 #initial root-to-shoot ratio
  for (t in 1:n_steps) {
    x[t+1,4] <- f(x[t,4],amin,amax,bmin,bmax) #next shoot length
    x[t+1,5] <- f(x[t,5],rmin,rmax,smin,smax) #next root length
    x[t+1,6] <- x[t+1,5]/x[t+1,4] #next root-to-shoot ratio
  }
  x
}

#--Run stochastic simulations over all species and treatments
set.seed(1)
sims <- data.frame(genus=numeric(), nitrogen=numeric(), wk=numeric(),
                   Slength=numeric(), Rlength=numeric(), RSratio=numeric())
for (i in 1:4){ # species i
  for (j in 1:3){ # treatment j
    # Generate Shoot Regression
    modelS <- lm(next_ShootLength ~ ShootLength, data=ShootData %>% filter(Genus==i & Nitrogen==j))
    ciS <- confint(modelS)
    sm7 <- ShootData %>% filter(Genus==i & Nitrogen==j & day==7) %>% pull(ShootLength) %>% mean()
    
    # Generate Root Regression
    modelR <- lm(next_RootLength ~ RootLength, data=RootData %>% filter(Genus==i & Nitrogen==j))
    ciR <- confint(modelR)
    rm7 <- RootData %>% filter(Genus==i & Nitrogen==j & day==7) %>% pull(RootLength) %>% mean()
    
    for (n in 1:1000){ # n simulations
      traj <- simulate_diff_eq(f=f_linear_stoch, s0=sm7, r0=rm7, n_steps=10, genus=i, nitrogen=j, 
                             amin=ciS[2,1], amax=ciS[2,2], bmin=ciS[1,1], bmax=ciS[1,2],
                             rmin=ciR[2,1], rmax=ciR[2,2], smin=ciR[1,1], smax=ciR[1,2])
      colnames(traj) <- colnames(sims)
      sims <- rbind(sims, as.data.frame(traj))
    }
  }
}

#Add species names and treatment labels
simulations <- sims %>% 
    mutate(species = case_when(
      genus == 1 ~ "italic('Aechmea recurvata')",
      genus == 2 ~ "italic('Billbergia brasiliensis')",
      genus == 3 ~ "italic('Puya mirabilis')",
      genus == 4 ~ "italic('Vriesea rafaelii')",
      TRUE       ~ NA_character_)) %>%
    mutate(treatment = case_when(
      nitrogen == 1 ~ "No~Nitrogen",
      nitrogen == 2 ~ "Low~Nitrogen",
      nitrogen == 3 ~ "High~Nitrogen",
      TRUE       ~ NA_character_))

#Plot stochastic simulations (median, IQR, range)
ggplot(simulations,aes(x=wk, y=RSratio)) +
  stat_summary(aes(color=treatment, fill=treatment),
    fun.min = function(z) { min(z) }, # Calculate min
    fun.max = function(z) { max(z) }, # Calculate max
    geom = "ribbon", 
    alpha = 0.15) +
  stat_summary(aes(color=treatment, fill=treatment),
    fun.min = function(z) { quantile(z, 0.25) }, # Calculate Q1
    fun.max = function(z) { quantile(z, 0.75) }, # Calculate Q3
    geom = "ribbon", 
    alpha = 0.30) +
  stat_summary(aes(color=treatment),
    fun = median, 
    geom = "line",
    lwd = 1) +
  theme_bw() +
  theme(legend.position="none", 
        legend.text = element_text(size=9),
        strip.text = element_text(size=10),
        axis.title = element_text(size = 12, face="bold"),
        axis.text = element_text(size = 10)) +
  scale_color_manual(values = c(
    "No~Nitrogen" = "#377EB8",
    "Low~Nitrogen" = "#4DAF4A",
    "High~Nitrogen" = "#E41A1C"
  )) + 
  scale_fill_manual(values = c(
    "No~Nitrogen" = "#377EB8",
    "Low~Nitrogen" = "#4DAF4A",
    "High~Nitrogen" = "#E41A1C"
  )) +
  facet_grid(treatment~species, drop=FALSE, labeller=label_parsed) +
  labs(x="Weeks", y="Root-to-Shoot Ratio")

##-- Plot stochastic simulations (median, IQR, range) excluding No Nitrogen
ggplot(simulations%>%filter(nitrogen!=1),aes(x=wk, y=RSratio)) +
  stat_summary(aes(color=treatment, fill=treatment),
               fun.min = function(z) { min(z) }, # Calculate min
               fun.max = function(z) { max(z) }, # Calculate max
               geom = "ribbon", 
               alpha = 0.15) +
  stat_summary(aes(color=treatment, fill=treatment),
               fun.min = function(z) { quantile(z, 0.25) }, # Calculate Q1
               fun.max = function(z) { quantile(z, 0.75) }, # Calculate Q3
               geom = "ribbon", 
               alpha = 0.30) +
  stat_summary(aes(color=treatment),
               fun = median, 
               geom = "line",
               lwd = 1) +
  theme_bw() +
  theme(legend.position="none", 
        legend.text = element_text(size=9),
        strip.text = element_text(size=10),
        axis.title = element_text(size = 12, face="bold"),
        axis.text = element_text(size = 10)) +
  scale_color_brewer(palette="Set1") + 
  scale_fill_brewer(palette="Set1") +
  facet_grid(treatment~species, drop=FALSE, labeller=label_parsed) +
  labs(x="Weeks", y="Root-to-Shoot Ratio")

