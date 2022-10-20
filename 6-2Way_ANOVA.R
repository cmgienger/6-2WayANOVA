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
growth.moo <- read.csv("growth.csv")

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

# calculate mean weight gain for all 12 combinations
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

# calculate mean and SE of gain for all 12 combinations
sumMoo <- growth.moo %>%
  group_by(diet, supplement) %>%
  summarise(
    meanGrow = mean(gain),
    seGrow = sd(gain)/sqrt(n()))
sumMoo

ggplot(sumMoo, aes(x = supplement, y = meanGrow,
                   colour = diet, group = diet)) +
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin = meanGrow - seGrow,
                    ymax = meanGrow + seGrow), width = 0.1) +
theme_bw()
