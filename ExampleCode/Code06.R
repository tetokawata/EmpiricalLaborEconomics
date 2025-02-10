
library(tidyverse)

Data = read_csv("Aggregate.csv")

mutate(Data, 
       value2 = value^2) # Make new data with new variable (value*value as values)

# pipe

Data |> # ctr + shift + m (%>%が表示されるかもしれないが、問題ない)
  mutate(value2 = value^2) |> 
  mutate(Mean = mean(value)) |> 
  mutate(Population = sum(value),
         .by = c(D)
         ) # D毎にvalueを足し合わせる

# Total population 
# 
Population = Data |> 
  mutate(Population = sum(value),
         .by = c(D)
         ) |> 
  distinct(D,Population) # Remove duplicats about D,Populaiton

PopulationGroup = Data |> 
  mutate(Population = sum(value),
         .by = c(D,G)
  ) |> 
  distinct(D,G,Population)

# ctr + A -> Ctr + Enter
# 16:15分再開