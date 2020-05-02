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

ggplot(budget2,aes(x = Month, y = Actual-Budget, color = Type, group=Type)) +
  geom_line(size=1.2)+
  geom_point()+
  geom_dl(aes(label = Type), method = list(dl.trans(x = x + .2), "last.points"))+
  ggtitle("Expense Variance from Budget in U.S Dollars")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position = "none")+
  theme(axis.title = element_blank())+
  theme(axis.line = element_line(color = "grey"))+
  theme(axis.line.x = element_blank())+
  expand_limits(x= c(0, 15))+
  expand_limits(y= c(-4000, 14000))+
  theme(axis.ticks=element_blank())+
  geom_hline(yintercept=0, linetype="dashed", color = "grey")+
  
  scale_y_continuous(breaks=c(seq(-4000, 14000, by=2000)))

#Figure 9.9
ggplot(budget2) + 
  geom_line(aes(x = Month, y = ((Actual-Budget)/Budget)*100, color = Type, group=Type))

ggplot(budget2,aes(x = Month, y = ((Actual-Budget)/Budget)*100, color = Type, group=Type)) +
  geom_line(size=1.2)+
  geom_point()+
  geom_dl(aes(label = Type), method = list(dl.trans(x = x + .2), "last.points"))+
  ggtitle("Percentatge Variance of Expenses from Budget")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position = "none")+
  theme(axis.title = element_blank())+
  theme(axis.line = element_line(color = "grey"))+
  theme(axis.line.x = element_blank())+
  expand_limits(x= c(0, 15))+
  expand_limits(y= c(-20, 25))+
  theme(axis.ticks=element_blank())+
  geom_hline(yintercept=0, linetype="dashed", color = "grey")+
  
  scale_y_continuous(breaks=c(seq(-20, 25, by=5)), labels = function(x) paste0(x,"%"))











