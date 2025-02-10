
library(tidyverse)
library(WeightIt)
library(marginaleffects)

data("GSS7402", package = "AER")

Weight = weightit(
  ethnicity ~ age + year, # D ~ X
  GSS7402, # Data
  method = "ebal", # Use Entropy
  estimand = "ATT" # target = mean among cauc
)

WeightLong = weightit(
  ethnicity ~ age + year + siblings, # D ~ X + Sibling
  GSS7402, # Data
  method = "ebal", # Use Entropy
  estimand = "ATT" # target = mean among cauc
)

Model = lm_weightit(
  education ~ ethnicity + age + year,
  GSS7402,
  weightit = Weight
) # Balancing X

ModelLong = lm_weightit(
  education ~ ethnicity + age + year + siblings,
  GSS7402,
  weightit = WeightLong
) # Balancing X and M

avg_predictions(Model, variables = "ethnicity") # Show balanced mean

avg_predictions(ModelLong, variables = "ethnicity")

avg_comparisons(Model, variables = "ethnicity") # Show balanced difference

avg_comparisons(ModelLong, variables = "ethnicity")


lm(education ~ ethnicity, GSS7402) # Original gap

# Ctr + A -> Ctr + Enter