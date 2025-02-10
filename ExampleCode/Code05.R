library(tidyverse)
library(qte)

data("GSS7402", package = "AER")

ggplot(
  GSS7402,
  aes(
    y = education,
    color = immigrant
  )
) +
  stat_ecdf()

Model = ci.qtet(
  education ~ if_else(immigrant == "yes",1,0),
  probs = seq(0.1,0.9,0.1),
  data = GSS7402,
  se = T,
  iters = 10 # 実践では1000程度に増やす
)

ggqte(Model)

ModelBalance = ci.qtet(
  education ~ if_else(immigrant == "yes",1,0),
  probs = seq(0.1,0.9,0.1),
  xformla = ~ age + year,
  data = GSS7402,
  se = T,
  iters = 10 # 実践では1000程度に増やす
)

ggqte(ModelBalance)

# Ctr + A -> Ctr + Enter
# Ctr + S