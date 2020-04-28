library(tidyverse)

#Create data set
budget <- tribble(
  ~ Expenses,             ~ Jan, ~ Feb, ~ Mar, ~ Apr, ~ May, ~ Jun, ~ Jul, ~ Aug, ~ Sep, ~ Oct, ~ Nov, ~ Dec,
  "Domestic Actual",      84853, 84838, 88103, 85072, 88723, 90384, 89374, 95273, 94239, 92394, 96934, 105034,
  "Domestic Budget",      83000, 83830, 84668, 85515, 86370, 87234, 88106, 88987, 89877, 90776, 91684, 92600,
  "International Actual", 12538, 12438, 14934, 14033, 13945, 15938, 14086, 15934, 13945, 17338, 19384, 22394,
  "International Budget", 12000, 12600, 13860, 13200, 13860, 15246, 14520, 15246, 16771, 15972, 16771, 18448
)

#Tidy it 
budget2 <- budget %>%
  pivot_longer(cols = -1,
               names_to = "Month",
               values_to = "Amount") %>%
  separate(Expenses, into = c("Type", "Spending")) %>%
  pivot_wider(names_from = Spending,
              values_from = Amount)

#Reorder factors so they plot sequentially
budget2$Month <- budget2$Month %>%
  factor() %>%
  fct_relevel(c("Jan","Feb","Mar","Apr","May","Jun",
                "Jul","Aug","Sep","Oct","Nov","Dec"))

#Figure 9.8
ggplot(budget2) + 
  geom_line(aes(x = Month, y = Actual-Budget, color = Type, group=Type))

#Figure 9.9
ggplot(budget2) + 
  geom_line(aes(x = Month, y = ((Actual-Budget)/Budget)*100, color = Type, group=Type))














