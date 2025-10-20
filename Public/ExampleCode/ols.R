library(tidyverse)

data("CPS1985", package = "AER")

data <- mutate(
  CPS1985,
  D = if_else(occupation == "technical",
    1,
    0
  ),
  Y = log(wage)
)

estimatr::lm_robust(Y ~ D, data) # No balance

estimatr::lm_robust(
  Y ~ D + age + ethnicity + gender, # Balance demography
  data
)

estimatr::lm_robust(
  Y ~ D + (age + ethnicity + gender)^2 +
    I(age^2), # Compex model
  data
)

# コードの実効: ctr + A -> ctr + Enter
# Save: ctr + S
