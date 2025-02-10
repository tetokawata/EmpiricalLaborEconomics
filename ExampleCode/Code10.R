library(tidyverse)
library(grf)
library(AER)

data("CPS1985")

Y = CPS1985$wage

D = if_else(CPS1985$gender == "female",1,0)

X = model.matrix(~ age + education + ethnicity,
                 CPS1985)

X = X[,-1]

Model = causal_forest(
  Y = Y,
  X = X,
  W = D
)

Pred = predict(Model, estimate.variance = TRUE)

CPS1985$Tau = Pred$predictions
CPS1985$SD = sqrt(Pred$variance.estimates)

CPS1985$Min = CPS1985$Tau - 1.96*CPS1985$SD
CPS1985$Max = CPS1985$Tau + 1.96*CPS1985$SD

hist(CPS1985$Tau)

average_treatment_effect(Model)

best_linear_projection(Model, scale(X))

best_linear_projection(Model, X)

# ctr + A -> ctr + Enter