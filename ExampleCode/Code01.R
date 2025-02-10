
library(tidyverse)
library(cobalt) # Balanceの確認
library(MatchIt)

data("GSS7402", package = "AER")

Tab = bal.tab(
  ethnicity ~ education + immigrant + age,
  GSS7402
)

love.plot(Tab)

Match = matchit(
  ethnicity ~ immigrant,
  GSS7402,
  method = "Exact", # Calculate balancing weights
  estimand = "ATE" # Target is x distrution in data
)

love.plot(Match)

GSS7402$Weights = Match$weights

lm(education ~ ethnicity,
   GSS7402) # Unbalanced comparison

lm(education ~ ethnicity,
   GSS7402,
   weights = Weights) # Balanced comparison

# Ctr + A -> Ctr + Enter