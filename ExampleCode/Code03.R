
library(tidyverse)
library(WeightIt)

data("GSS7402", package = "AER")

Weight = weightit(
  ethnicity ~ age + year, # D ~ X
  GSS7402, # Data
  method = "ebal", # Use Entropy
  estimand = "ATE" # target = sample mean
)

WeightLong = weightit(
  ethnicity ~ age + year + siblings, # D ~ X
  GSS7402, # Data
  method = "ebal", # Use Entropy
  estimand = "ATE" # target = sample mean
)

lm_weightit(
  education ~ ethnicity + age + year,
  GSS7402,
  weightit = Weight
)

lm_weightit(
  education ~ ethnicity + age + year + siblings,
  GSS7402,
  weightit = WeightLong
)

lm(education ~ ethnicity, GSS7402)

# Ctr + A -> Ctr + Enter