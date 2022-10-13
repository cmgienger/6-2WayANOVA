library(tidyverse)


###############CO2 Example#######################################
#The CO2 uptake of plants from Quebec and Mississippi was measured at several 
#levels of ambient CO2 concentration. Half the plants of each type were chilled 
#overnight before the experiment was conducted.
###############CO2 Example#######################################

data_CO2<-CO2 #dataset built into R

?CO2

summary_CO2<-data_CO2 %>%
  group_by(Type, Treatment) %>%
  summarise(
    mean_uptake=mean(uptake),
    n=n(),
    se_uptake=sd(uptake)/sqrt(n)
  )
summary_CO2

plot_CO2 <-ggplot(summary_CO2, aes(x=Type, y=mean_uptake, fill=Treatment)) +
  geom_bar(stat="identity", color="black", position=position_dodge()) +
  geom_errorbar(aes(ymin=mean_uptake-se_uptake, ymax=mean_uptake+se_uptake), width=.2, position=position_dodge(.9))+
  ylab("CO2 uptake rates (umol/m^2 sec)")+
  theme_bw()
plot_CO2

#Making this figure is not as intuitive as you might think because of the position dodge required, 
#the stat="identity", and error bar specification.
#Really messes up things; not an intuitive intro example.
#Note that the reference group in the model summary table is the mean of Quebec non-chilled. Everything else is in reference to that.

model1 <- lm(uptake~Type*Treatment, data=data_CO2)
anova(model1)

summary(model1)

#Multiple Contrasts using EMmeans package
emm_model1<- emmeans(model1, "Type", "Treatment")
emm_model1
pairs(emm_model1)

#visually compare marginal means (LS means)
plot(emm_model1, comparisons = TRUE)+
  coord_flip()

#Because there are two treatments, it breaks them into two panels.

#connecting letters differences report
multcomp::cld(emm_model1)

#Simple Contrasts
contrast(emm_model1, "consec", simple = "each", combine = TRUE, adjust = "mvt")

#Pairwise P-value plots; pwpp plots
model.cells <- emmeans(model1, ~ Type * Treatment)
pwpp(model.cells, type = "response")

#add CLD notation from multcomp::cld
#non-nuanced approach
plot_CO2 +
  annotate("text", x = .75, y = 40, label = "2")+
  annotate("text", x = 1.25, y = 35, label = "2")+
  annotate("text", x = 1.75, y = 30, label = "1")+
  annotate("text", x = 2.25, y = 20, label = "1")+
  annotate("text", x = 2, y = 37.5, label = "Shared letters indicate no pairwise \n differences  (P > 0.05) between groups ")


#####FIX REPORT PAIRWISE COMPARISONS OF CO2. CURRENTLY MISLEADING. 
###CANT COMPARE ACROSS BOTH TYPES AND TREATMENTS.(Cumming paper)
##Levels of mississippi are not sig-different; CLD reports two post hoc tests,
#Not one with all pairwise (like 1 way anova)




