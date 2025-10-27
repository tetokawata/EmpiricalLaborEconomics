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

lm(Y ~ D + education + experience + age + ethnicity, data)

hdm::rlasso(
  Y ~ D + education + experience + age + ethnicity,
  data,
  post = FALSE
)

hdm::rlasso(
  Y ~ D + (education + experience + age + ethnicity)^2 +
    I(education^2) + I(experience^2) + I(age^2),
  data,
  post = FALSE
)


# ctr + A -> ctr + Enter (実行)
# ctr + S (Save)
