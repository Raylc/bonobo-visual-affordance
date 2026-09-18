## load libraries
library(tidyverse)
library(broom)
library(purrr)
library(kinship2)
## load data
dat<-read.csv("data/core_vs_flake_training.csv")
flakes<-read.csv("data/flake data.csv")
nodules<-read.csv("data/nodule data.csv")

################Visualization################
## calculate the accuracy by session
session_results <- dat %>% group_by(Individual,Session) %>% count(Results)
accuracy<- session_results %>% filter(Results=='f') %>% mutate(Accuracy=n/10)

## plot the diachronic changes of accuracy by individual
p1 <- ggplot(accuracy, aes(x=Session, y=Accuracy, shape=Individual, color=Individual))+
  geom_point()+
  geom_smooth(se=FALSE)+
  ylim(0, 1)
ggplot2::ggsave("accuracy changes.png", width = 10, height = 5, path="output", dpi = 600)

p1 <- ggplot(accuracy, aes(x=Session, y=Accuracy))+
  geom_point()+
  geom_smooth(se=FALSE)+
  ylim(0, 1)
p1 + facet_wrap(~Individual)

ggplot2::ggsave("accuracy changes panel.png", width = 10, height = 5, path="output", dpi = 600)

################Stats################

# Convert response to binary
dat <- dat %>%
  mutate(
    flake = if_else(Results == "f", 1, 0)
  )

# ---------------------------------------------------------
# 1. Overall accuracy and exact binomial tests
# ---------------------------------------------------------

overall <- dat %>%
  group_by(Individual) %>%
  summarise(
    flakes = sum(flake),
    trials = n(),
    accuracy = flakes / trials
  ) %>%
  mutate(
    p_value = map2_dbl(
      flakes, trials,
      ~ binom.test(.x, .y, p = 0.5)$p.value
    ),
    p_adjusted = p.adjust(p_value, method = "holm")
  )

overall

# ---------------------------------------------------------
# 2. Changes across sessions
# ---------------------------------------------------------
# Number of flakes selected in each session
session_dat <- dat %>%
  group_by(Individual, Session) %>%
  summarise(
    flakes = sum(flake),
    non_flakes = n() - flakes,
    trials = n(),
    accuracy = flakes / trials,
    .groups = "drop"
  )

# Run separate binomial logistic regression for each individual
# Individual learning models
individual_models <- session_dat %>%
  group_by(Individual) %>%
  group_modify(~ {
    
    model <- glm(
      cbind(flakes, non_flakes) ~ Session,
      family = binomial,
      data = .x
    )
    
    broom::tidy(model) %>%
      filter(term == "Session") %>%
      mutate(
        OR = exp(estimate),
        CI_lower = exp(estimate - 1.96 * std.error),
        CI_upper = exp(estimate + 1.96 * std.error)
      ) %>%
      select(
        Estimate = estimate,
        SE = std.error,
        OR,
        CI_lower,
        CI_upper,
        p = p.value
      )
  }) %>%
  ungroup()

# Holm correction
individual_models <- individual_models %>%
  mutate(
    p_Holm = p.adjust(p, method = "holm")
  )

# Format for export
table2 <- individual_models %>%
  mutate(
    Estimate = sprintf("%.3f", Estimate),
    SE = sprintf("%.3f", SE),
    OR = sprintf("%.3f", OR),
    CI = paste0(
      sprintf("%.3f", CI_lower),
      " - ",
      sprintf("%.3f", CI_upper)
    ),
    p = if_else(p < 0.001, "<0.001", sprintf("%.3f", p)),
    p_Holm = if_else(
      p_Holm < 0.001,
      "<0.001",
      sprintf("%.3f", p_Holm)
    )
  ) %>%
  select(
    Individual,
    Estimate,
    SE,
    OR,
    CI,
    p,
    p_Holm
  )

# Display
print(table2)

# Export
write_csv(
  table2,
  "output/Table_2_individual_learning_models.csv"
)

############Table 1####################
# Summarize flake data
# Flakes do not have a Thickness column, so NA is assigned to keep dimensions consistent
flake_summary <- flakes %>%
  summarize(
    Artifact = "Flakes",
    `Sample Size` = n(),
    `Average Length` = mean(Length, na.rm = TRUE),
    `SD Length` = sd(Length, na.rm = TRUE),
    `Average Width` = mean(Width, na.rm = TRUE),
    `SD Width` = sd(Width, na.rm = TRUE),
    `Average Thickness` = NA_real_,
    `SD Thickness` = NA_real_,
    `Average Mass` = mean(Mass, na.rm = TRUE),
    `SD Mass` = sd(Mass, na.rm = TRUE)
  )

# Summarize nodule data
# Note that nodule mass is recorded under the column name Weight
nodule_summary <- nodules %>%
  summarize(
    Artifact = "Nodules",
    `Sample Size` = n(),
    `Average Length` = mean(Length, na.rm = TRUE),
    `SD Length` = sd(Length, na.rm = TRUE),
    `Average Width` = mean(Width, na.rm = TRUE),
    `SD Width` = sd(Width, na.rm = TRUE),
    `Average Thickness` = mean(Thickness, na.rm = TRUE),
    `SD Thickness` = sd(Thickness, na.rm = TRUE),
    `Average Mass` = mean(Weight, na.rm = TRUE),
    `SD Mass` = sd(Weight, na.rm = TRUE)
  )

# Summarize and round all numeric columns to two decimal places
summary_table_data <- bind_rows(flake_summary, nodule_summary) %>%
  mutate(across(where(is.numeric), ~ round(.x, 2)))

# Export
write_csv(
  summary_table_data,
  "output/Table_1_metrics.csv"
)


############kinship figure######################
# ------------------------------------------------------------
# Individuals
# ------------------------------------------------------------

id <- c(
  "Bosondjo",
  "Lorel",
  "Matata",
  "P-suke",
  "Panbanisha",
  "Kanzi",
  "Nyota",
  "Maisha",
  "Elikya",
  "Teco"
)


# ------------------------------------------------------------
# Parents
# ------------------------------------------------------------

dadid <- c(
  NA,
  NA,
  NA,
  NA,
  "Bosondjo",     # Panbanisha
  "Bosondjo",     # Kanzi
  "P-suke",       # Nyota
  "P-suke",       # Maisha
  "P-suke",       # Elikya
  "Nyota"         # Teco
)

momid <- c(
  NA,
  NA,
  NA,
  NA,
  "Matata",       # Panbanisha
  "Lorel",        # Kanzi
  "Panbanisha",   # Nyota
  "Matata",       # Maisha
  "Matata",       # Elikya
  "Elikya"        # Teco
)


# ------------------------------------------------------------
# Sex
#
# 1 = male
# 2 = female
# ------------------------------------------------------------

sex <- c(
  1,  # Bosondjo  - male
  2,  # Lorel     - female
  2,  # Matata    - female
  1,  # P-suke    - male
  2,  # Panbanisha- female
  1,  # Kanzi     - male
  1,  # Nyota     - male
  1,  # Maisha    - male
  2,  # Elikya    - female
  1   # Teco      - male
)


# ------------------------------------------------------------
# Create pedigree
# ------------------------------------------------------------

ped <- pedigree(
  id = id,
  dadid = dadid,
  momid = momid,
  sex = sex
)


# Open the PNG device with specified resolution and dimensions
png(
  filename = "output/bonobo_kinship_pedigree.png",
  width = 10,          # Width in inches
  height = 8,          # Height in inches
  units = "in",        # Unit for width/height
  res = 600            # Resolution in DPI
)

# Render your plot
plot(
  ped,
  id = id,
  align = TRUE,
  packed = FALSE,
  cex = 0.9,
  symbolsize = 1.5
)

# Save and close the graphic file
dev.off()
