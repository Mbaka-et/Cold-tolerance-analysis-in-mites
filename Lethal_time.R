###################
### Lethal time ###
###################

library(sjPlot)
library(ggplot2)
require(foreign)
require(ggplot2)
require(MASS)
library(performance)
library(multcompView)
library(multcomp)
library(emmeans)
library(DHARMa)

##############
# Degenerans #
##############

data.1 <- read.csv("Lethal time degenerans_new.txt", sep = "\t")
data.1

dim(data.1)

# check normality of response variable
hist(data.1$Number.of.days)
shapiro.test(data.1$Number.of.days)

# check the structure of the data
str(data.1)

# define as factors where relevant
data.1$Batch <- as.factor(data.1$Batch)
data.1$Temperature <- as.factor(data.1$Temperature)
data.1$Treatment <- as.factor(data.1$Treatment)

# relevel to set reference temperature as 5ºC
data.1$Temperature <- relevel(data.1$Temperature, ref="5")

# to exclude an outlier
data.1_2 <- data.1[-c(83), ]

# negative binomial model used because of overdispersed count data
model <- glm.nb(Number.of.days ~ Treatment * Temperature + Batch, data = data.1_2, link = log)
summary(model)

# diagnostic plots
simulationOutput_model <- simulateResiduals(fittedModel = model)
plot(simulationOutput_model)
testDispersion(simulationOutput_model)

# for pairwise comparisons
emm <- emmeans(model, ~ Treatment * Temperature)
pairs(emm)
cld(emm,
    alpha=0.05,
    Letters=letters,     
    adjust="tukey")

# Effect plot for interaction temperature x treatment
png("Interaction_treatment_temperature_degenerans.png", width = 17,height = 15, units = "cm", res = 600)
model <- glm.nb(Number.of.days ~ Treatment * Temperature + Batch, data = data.1_2, link = log)
p <- plot_model(model, type="pred", terms = c("Temperature", "Treatment"),
                axis.title = c("Temperature (ºC)", "Lethal time (days)"),
                title = "",
                legend.title = "",
                colors = c("black", "darkgrey"),
                show.data = FALSE, dot.size = 4, dodge = 0.9)
set_theme(
  base = theme_classic(), legend.title.size = 1.4)
p + font_size(axis_title.x = 20, axis_title.y = 20, labels.x = 15, labels.y = 15)
# report
dev.off()


##############
# Limonicus  #
##############

data.2 <- read.csv("Lethal time limonicus_new.txt", sep = "\t")
data.2

dim(data.2)

# check normality of response variable
hist(data.2$Number.of.days)
shapiro.test(data.2$Number.of.days)

# check the structure of the data
str(data.2)

# define factor variables
data.2$Batch <- as.factor(data.2$Batch)
data.2$Temperature <- as.factor(data.2$Temperature)
data.2$Treatment <- as.factor(data.2$Treatment)

# relevel to set 5ºC as reference
data.2$Temperature <- relevel(data.2$Temperature, ref="5")

# poisson model
model2 <- glm(Number.of.days ~ Treatment * Temperature + Batch, data = data.2, family = poisson)
summary(model2)

# diagnostic plots
simulationOutput_model2 <- simulateResiduals(fittedModel = model2)
plot(simulationOutput_model2)
testDispersion(simulationOutput_model2)

# for pairwise comparisons
emm <- emmeans(model2, ~ Temperature)
pairs(emm)
cld(emm,
    alpha=0.05,
    Letters=letters,     
    adjust="tukey")

# Effect plot for days x temperature
png("Days_vs_temperature_limonicus.png", width = 15, height = 15, units = "cm", res = 600)
model2 <- glm(Number.of.days ~ Treatment * Temperature + Batch, data = data.2, family = poisson)
p <- plot_model(model2, type="pred", terms = "Temperature",
                axis.title = c("Temperature (ºC)", "Lethal time (days)"),
                title = "",
                legend.title = "",
                colors = c("black"),
                show.data = FALSE, dot.size = 4)
set_theme(
  base = theme_classic(), legend.title.size = 1.6)
p + font_size(axis_title.x = 20, axis_title.y = 20, labels.x = 15, labels.y = 15) + scale_y_continuous(limits = c(0, 10), breaks = c(0, 2, 4, 6, 8, 10))
#export
dev.off()

# Effect plot for days x treatment
png("Days_vs_treatment_limonicus.png", width = 15, height = 15, units = "cm", res = 600)
model2 <- glm(Number.of.days ~ Treatment * Temperature + Batch, data = data.2, family = poisson)
p<-plot_model(model2, type="pred", terms = "Treatment",
              axis.title = c("Treatment", "Lethal time (days)"),
              title = "",
              legend.title = "",
              colors = c("black"),
              show.data = FALSE, dot.size = 4, axis.lim = c(0,10))
set_theme(
  base = theme_classic(), legend.title.size = 1.4)
p + font_size(axis_title.x = 20, axis_title.y = 20, labels.x = 15, labels.y = 15) + scale_y_continuous(limits = c(0, 10), breaks = c(0, 2, 4, 6, 8, 10))
#export
dev.off()

