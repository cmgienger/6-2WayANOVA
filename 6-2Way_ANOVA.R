#title: "6-2Way ANOVA"
#author: "C.M. Gienger"

library(readr)
library(dplyr)
library(ggplot2)
library(ggfortify)
library(emmeans)
library(multcomp)
library(multcompView)

#http://r4all.org/posts/importing-data-update/
#can fix by using string as factors option (button)
growth.moo <- read_csv("growth.csv")

glimpse(growth.moo)
#see character data as factors

growth.moo$diet <-as.factor(growth.moo$diet)
levels(growth.moo$diet)

growth.moo$supplement <-as.factor(growth.moo$supplement)
levels(growth.moo$supplement)

# relevel the supplement column
growth.moo <- mutate(growth.moo,
supplement = relevel(supplement, ref="control"))

# check it worked
levels(growth.moo$supplement)

# calculate mean and sd of gain for all 12 combinations
sumMoo <- growth.moo %>%
group_by(diet, supplement) %>%
summarise(meanGrow = mean(gain))

# make sure it worked
sumMoo

#as always, make a plot first
ggplot(sumMoo, aes(x = supplement, y = meanGrow)) +
  geom_point() +
  theme_bw()

ggplot(sumMoo, aes(x = supplement, y = meanGrow,
colour = diet, group = diet)) +
geom_point() +
geom_line() +
theme_bw()

#fit model with interaction
model_cow <- lm(gain ~ diet * supplement, data = growth.moo)
#examine model
autoplot(model_cow, smooth.colour = NA)

anova(model_cow)

summary(model_cow)

# calculate mean and sd of gain for all 12 combinations
sumMoo <- growth.moo %>%
  group_by(diet, supplement) %>%
  summarise(
    meanGrow = mean(gain),
    seGrow = sd(gain)/sqrt(n())
)
sumMoo

ggplot(sumMoo, aes(x = supplement, y = meanGrow,
                   colour = diet, group = diet)) +
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin = meanGrow - seGrow,
                    ymax = meanGrow + seGrow), width = 0.1) +
theme_bw()

###############CO2 Example#######################################
#The CO2 uptake of plants from Quebec and Mississippi was measured at several 
#levels of ambient CO2 concentration. Half the plants of each type were chilled 
#overnight before the experiment was conducted.
###############CO2 Example#######################################

data_CO2<-CO2

summary_CO2<-data_CO2 %>%
  group_by(Type, Treatment) %>%
  summarise(
    mean_uptake=mean(uptake),
    n=n(),
    se_uptake=sd(uptake)/sqrt(n)
  )
summary_CO2

ggplot(summary_CO2, aes(x=Type, y=mean_uptake, fill=Treatment)) +
  geom_bar(stat="identity", color="black", position=position_dodge()) +
  geom_errorbar(aes(ymin=mean_uptake-se_uptake, ymax=mean_uptake+se_uptake), width=.2, position=position_dodge(.9))+
  ylab("CO2 uptake rates (umol/m^2 sec)")+
  theme_bw()

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






