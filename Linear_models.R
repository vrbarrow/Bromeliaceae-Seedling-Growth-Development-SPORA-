library(dplyr)
library(tidyr)
library(ggplot2)

dat <- read.csv("By_Seed_CSV3.csv", header = TRUE, na.strings = c("", "NA"))

timepoints <- c(7, 14, 21)

nitrogen_labels <- c("1" = "No Nitrogen", "2" = "Low Nitrogen", "3" = "High Nitrogen")
genus_labels    <- c("1" = "Aechmea recurvata", "2" = "Billbergia brasiliensis",
                     "3" = "Puya mirabilis", "4" = "Vriesea rafaelii")

label_or_self <- function(map, key) {
  k <- as.character(key)
  if (!is.na(map[k])) map[k] else k
}

#Regression function which stacks 7->14 and 14->21 transitions
growth_regression <- function(df) {
  
  stopifnot(all(c("Nitrogen", "Genus") %in% names(df)))
  
  results <- list()
  idx <- 1
  
  for (N in unique(df$Nitrogen)) {
    for (G in unique(df$Genus)) {
      
      subset_df <- df %>% filter(Nitrogen == N, Genus == G)
      
      # collect transitions
      S_t <- c(); S_tp1 <- c()
      R_t <- c(); R_tp1 <- c()
      
      for (i in seq_len(length(timepoints) - 1)) {
        t1 <- timepoints[i]
        t2 <- timepoints[i + 1]
        
        s1_col <- paste0("ShootLength", t1)
        s2_col <- paste0("ShootLength", t2)
        r1_col <- paste0("RootLength",  t1)
        r2_col <- paste0("RootLength",  t2)
        
        needed <- c(s1_col, s2_col, r1_col, r2_col)
        if (!all(needed %in% names(subset_df))) next
        
        tmpS <- subset_df %>%
          transmute(s1 = .data[[s1_col]], s2 = .data[[s2_col]]) %>%
          filter(!is.na(s1), !is.na(s2))
        
        tmpR <- subset_df %>%
          transmute(r1 = .data[[r1_col]], r2 = .data[[r2_col]]) %>%
          filter(!is.na(r1), !is.na(r2))
        
        if (nrow(tmpS) > 0) {
          S_t   <- c(S_t,   tmpS$s1)
          S_tp1 <- c(S_tp1, tmpS$s2)
        }
        
        if (nrow(tmpR) > 0) {
          R_t   <- c(R_t,   tmpR$r1)
          R_tp1 <- c(R_tp1, tmpR$r2)
        }
      }
      
      #Skip if not enough data
      if (length(S_t) < 2 || length(R_t) < 2) next
      
      #Shoot regression
      shoot_dat <- data.frame(S_t = S_t, S_tp1 = S_tp1)
      shoot_fit <- lm(S_tp1 ~ S_t, data = shoot_dat)
      
      a <- unname(coef(shoot_fit)[["S_t"]])
      b <- unname(coef(shoot_fit)[["(Intercept)"]])
      
      shoot_pred <- predict(shoot_fit, newdata = shoot_dat)
      r2_s <- summary(shoot_fit)$r.squared
      rmse_s <- sqrt(mean((shoot_dat$S_tp1 - shoot_pred)^2))
      
      ci_s <- confint(shoot_fit, level = 0.95)
      b_ci <- as.numeric(ci_s["(Intercept)", ])
      a_ci <- as.numeric(ci_s["S_t", ])
      
      #Root regression
      root_dat <- data.frame(R_t = R_t, R_tp1 = R_tp1)
      root_fit <- lm(R_tp1 ~ R_t, data = root_dat)
      
      r <- unname(coef(root_fit)[["R_t"]])
      s <- unname(coef(root_fit)[["(Intercept)"]])
      
      root_pred <- predict(root_fit, newdata = root_dat)
      r2_r <- summary(root_fit)$r.squared
      rmse_r <- sqrt(mean((root_dat$R_tp1 - root_pred)^2))
      
      ci_r <- confint(root_fit, level = 0.95)
      s_ci <- as.numeric(ci_r["(Intercept)", ])
      r_ci <- as.numeric(ci_r["R_t", ])
      
      # Convert labels ONLY if needed (i.e., if numeric codes)
      nitrogen_name <- label_or_self(nitrogen_labels, N)
      genus_name    <- label_or_self(genus_labels, G)
      
      results[[idx]] <- data.frame(
        Nitrogen = nitrogen_name,
        Genus    = genus_name,
        
        Shoot_a = a,
        Shoot_b = b,
        Shoot_r2 = r2_s,
        Shoot_rmse = rmse_s,
        Shoot_a_ci_low  = a_ci[1],
        Shoot_a_ci_high = a_ci[2],
        Shoot_b_ci_low  = b_ci[1],
        Shoot_b_ci_high = b_ci[2],
        
        Root_r = r,
        Root_s = s,
        Root_r2 = r2_r,
        Root_rmse = rmse_r,
        Root_r_ci_low  = r_ci[1],
        Root_r_ci_high = r_ci[2],
        Root_s_ci_low  = s_ci[1],
        Root_s_ci_high = s_ci[2]
      )
      
      idx <- idx + 1
    }
  }
  
  bind_rows(results)
}

results_df <- growth_regression(dat)
print(results_df)

plot_df <- results_df %>%
  mutate(
    Species = case_when(
      Genus == "Aechmea recurvata"        ~ "italic('Aechmea recurvata')",
      Genus == "Billbergia brasiliensis"  ~ "italic('Billbergia brasiliensis')",
      Genus == "Puya mirabilis"           ~ "italic('Puya mirabilis')",
      Genus == "Vriesea rafaelii"         ~ "italic('Vriesea rafaelii')",
      TRUE ~ NA_character_
    ),
    Treatment = Nitrogen
  ) %>%
  filter(!is.na(Species), !is.na(Treatment)) %>%
  filter(Treatment %in% c("No Nitrogen", "High Nitrogen")) %>%
  mutate(Treatment = factor(Treatment, levels = c("No Nitrogen", "High Nitrogen")))

# Put all four parameters into one long dataframe
plot_long <- plot_df %>%
  dplyr::select(Species, Treatment,
                Shoot_a, Shoot_a_ci_low, Shoot_a_ci_high,
                Shoot_b, Shoot_b_ci_low, Shoot_b_ci_high,
                Root_r,  Root_r_ci_low,  Root_r_ci_high,
                Root_s,  Root_s_ci_low,  Root_s_ci_high) %>%
  pivot_longer(
    cols = -c(Species, Treatment),
    names_to = "variable",
    values_to = "value"
  ) %>%
  separate(variable, into = c("organ", "parameter", "ci", "bound"), sep = "_", fill = "right") %>%
  mutate(
    Parameter = case_when(
      organ == "Shoot" & parameter == "a" ~ "a",
      organ == "Shoot" & parameter == "b" ~ "b",
      organ == "Root"  & parameter == "r" ~ "r",
      organ == "Root"  & parameter == "s" ~ "v"
    ),
    stat = case_when(
      is.na(ci) ~ "mean",
      bound == "low" ~ "lo",
      bound == "high" ~ "hi"
    )
  ) %>%
  dplyr::select(Species, Treatment, Parameter, stat, value) %>%
  pivot_wider(names_from = stat, values_from = value) %>%
  mutate(
    Parameter = factor(Parameter, levels = c("a", "b", "r", "v")),
    Treatment = factor(Treatment, levels = c("No Nitrogen", "High Nitrogen"))
  )

# Final graph
ggplot(plot_long, aes(x = Treatment, y = mean, fill = Treatment)) +
  geom_crossbar(aes(ymin = lo, ymax = hi), width = 0.65, fatten = 1) +
  facet_grid(Parameter ~ Species, scales = "free_y", labeller = label_parsed) +
  scale_fill_manual(values = c(
    "No Nitrogen" = "#377EB8",
    "High Nitrogen" = "#E41A1C"
  )) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.22))) +
  theme_bw() +
  theme(
    panel.spacing = grid::unit(0, "lines"),
    legend.position = "bottom",
    legend.title = element_blank(),
    strip.text.x = element_text(size = 10, face = "italic"),
    strip.text.y = element_text(size = 10, face = "bold"),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    plot.margin = margin(10, 10, 10, 10)
  ) +
  labs(
    x = "Parameter",
    y = "95% Confidence Interval"
  )

#####Hypothesis testing#####

# Arec: No Nitrogen vs High Nitrogen
x <- dat %>%
  filter(Genus == 1, Nitrogen %in% c(1, 3))

# Shoot
shoot <- data.frame(
  S_t = c(x$ShootLength7, x$ShootLength14),
  S_tp1 = c(x$ShootLength14, x$ShootLength21), #the tp1 just references timestep "plus" one i.e. next timestep
  Treatment = factor(rep(x$Nitrogen, 2),
                     levels = c(1, 3), # I am pretty sure that I am putting no nitrogen as the reference treatment here
                     labels = c("No", "High"))
) %>%
  drop_na()

shoot_test <- lm(S_tp1 ~ S_t * Treatment, data = shoot)
coef(summary(shoot_test))
nobs(shoot_test)

# Root
root <- data.frame(
  R_t = c(x$RootLength7, x$RootLength14),
  R_tp1 = c(x$RootLength14, x$RootLength21),
  Treatment = factor(rep(x$Nitrogen, 2),
                     levels = c(1, 3), # no nitrogen is reference treatment
                     labels = c("No", "High"))
) %>%
  drop_na()

root_test <- lm(R_tp1 ~ R_t * Treatment, data = root) 
coef(summary(root_test))
nobs(root_test)

arec_results <- data.frame(
  Species = "Aechmea recurvata",
  a = coef(summary(shoot_test))["S_t:TreatmentHigh", "Pr(>|t|)"],
  b = coef(summary(shoot_test))["TreatmentHigh", "Pr(>|t|)"],
  r = coef(summary(root_test))["R_t:TreatmentHigh", "Pr(>|t|)"],
  v = coef(summary(root_test))["TreatmentHigh", "Pr(>|t|)"]
)

# Bbra: No Nitrogen vs High Nitrogen
x <- dat %>%
  filter(Genus == 2, Nitrogen %in% c(1, 3))

# Shoot
shoot <- data.frame(
  S_t = c(x$ShootLength7, x$ShootLength14),
  S_tp1 = c(x$ShootLength14, x$ShootLength21),
  Treatment = factor(rep(x$Nitrogen, 2),
                     levels = c(1, 3),
                     labels = c("No", "High"))
) %>%
  drop_na()

shoot_test <- lm(S_tp1 ~ S_t * Treatment, data = shoot)
coef(summary(shoot_test))
nobs(shoot_test)

# Root
root <- data.frame(
  R_t = c(x$RootLength7, x$RootLength14),
  R_tp1 = c(x$RootLength14, x$RootLength21),
  Treatment = factor(rep(x$Nitrogen, 2),
                     levels = c(1, 3),
                     labels = c("No", "High"))
) %>%
  drop_na()

root_test <- lm(R_tp1 ~ R_t * Treatment, data = root)
coef(summary(root_test))
nobs(root_test)

bbra_results <- data.frame(
  Species = "Billbergia brasiliensis",
  a = coef(summary(shoot_test))["S_t:TreatmentHigh", "Pr(>|t|)"],
  b = coef(summary(shoot_test))["TreatmentHigh", "Pr(>|t|)"],
  r = coef(summary(root_test))["R_t:TreatmentHigh", "Pr(>|t|)"],
  v = coef(summary(root_test))["TreatmentHigh", "Pr(>|t|)"]
)

# Pmir: No Nitrogen vs High Nitrogen
x <- dat %>%
  filter(Genus == 3, Nitrogen %in% c(1, 3))

# Shoot
shoot <- data.frame(
  S_t = c(x$ShootLength7, x$ShootLength14),
  S_tp1 = c(x$ShootLength14, x$ShootLength21),
  Treatment = factor(rep(x$Nitrogen, 2),
                     levels = c(1, 3),
                     labels = c("No", "High"))
) %>%
  drop_na()

shoot_test <- lm(S_tp1 ~ S_t * Treatment, data = shoot)
coef(summary(shoot_test))
nobs(shoot_test)

# Root
root <- data.frame(
  R_t = c(x$RootLength7, x$RootLength14),
  R_tp1 = c(x$RootLength14, x$RootLength21),
  Treatment = factor(rep(x$Nitrogen, 2),
                     levels = c(1, 3),
                     labels = c("No", "High"))
) %>%
  drop_na()

root_test <- lm(R_tp1 ~ R_t * Treatment, data = root)
coef(summary(root_test))
nobs(root_test)

pmir_results <- data.frame(
  Species = "Puya mirabilis",
  a = coef(summary(shoot_test))["S_t:TreatmentHigh", "Pr(>|t|)"],
  b = coef(summary(shoot_test))["TreatmentHigh", "Pr(>|t|)"],
  r = coef(summary(root_test))["R_t:TreatmentHigh", "Pr(>|t|)"],
  v = coef(summary(root_test))["TreatmentHigh", "Pr(>|t|)"]
)

# Vraf: No Nitrogen vs High Nitrogen
x <- dat %>%
  filter(Genus == 4, Nitrogen %in% c(1, 3))

# Shoot
shoot <- data.frame(
  S_t = c(x$ShootLength7, x$ShootLength14),
  S_tp1 = c(x$ShootLength14, x$ShootLength21),
  Treatment = factor(rep(x$Nitrogen, 2),
                     levels = c(1, 3),
                     labels = c("No", "High"))
) %>%
  drop_na()

shoot_test <- lm(S_tp1 ~ S_t * Treatment, data = shoot)
coef(summary(shoot_test))
nobs(shoot_test)

# Root
root <- data.frame(
  R_t = c(x$RootLength7, x$RootLength14),
  R_tp1 = c(x$RootLength14, x$RootLength21),
  Treatment = factor(rep(x$Nitrogen, 2),
                     levels = c(1, 3),
                     labels = c("No", "High"))
) %>%
  drop_na()

root_test <- lm(R_tp1 ~ R_t * Treatment, data = root)
coef(summary(root_test))
nobs(root_test)

vraf_results <- data.frame(
  Species = "Vriesea rafaelii",
  a = coef(summary(shoot_test))["S_t:TreatmentHigh", "Pr(>|t|)"],
  b = coef(summary(shoot_test))["TreatmentHigh", "Pr(>|t|)"],
  r = coef(summary(root_test))["R_t:TreatmentHigh", "Pr(>|t|)"],
  v = coef(summary(root_test))["TreatmentHigh", "Pr(>|t|)"]
)

#table for all p values
pvalue_table <- bind_rows(
  arec_results,
  bbra_results,
  pmir_results,
  vraf_results
)

print(pvalue_table)

