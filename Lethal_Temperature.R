##########################
### Lethal temperature ###
##########################

library(sjPlot)
library(ggplot2)
require(foreign)
require(ggplot2)
require(MASS)
library(performance)
library(emmeans)
library(multcomp)
library(multcompView)
library(DHARMa)

##############
# Degenerans #
##############

input <- read.csv("Lethal temperature degenerans_new.txt", sep = "\t")
input

# check table dimensions
dim(input)

# check the structure of the data
str(input)

# to remove -13.13 and -17.75 (which were all zeroes)
input2 <- input[input$Temperature > -13.13, ]

# define factor variables
input2$Temperature = as.factor(input2$Temperature)

# relevel to set 10ºC as reference
input2$Temperature <- relevel(input2$Temperature, ref="10")

# binomial model for 0/1 data
model1 <- glm(Mortality ~ Temperature, data = input2, family = binomial)
summary(model1)

# diagnostic plots
simulationOutput_model1 <- simulateResiduals(fittedModel = model1)
plot(simulationOutput_model1)
testDispersion(simulationOutput_model1)

# for pairwise comparisons
##summary(glht(model1, linfct=mcp(Temperature = "Tukey")))

emm <- emmeans(model1, ~ Temperature)
pairs(emm)
cld(emm,
    alpha=0.05,
    Letters=letters,     
    adjust="tukey")

# effect plot of survival vs temperature
png("mortality_vs_temperature_degenerans.png", width = 15, height = 15, units = "cm", res = 600)
model1 <- glm(Mortality ~ Temperature, data = input2, family = binomial)
p <- plot_model(model1, type="pred", terms = "Temperature",
                axis.title = c("Temperature (ºC)", "Mortality"),
                title = "",
                legend.title = "",
                colors = c("green", "orange"),
                show.data = FALSE, dot.size = 4, axis.lim = c(0,1))
set_theme(
  base = theme_classic(), legend.title.size = 1.4)
p + font_size(axis_title.x = 20, axis_title.y = 20, labels.x = 15, labels.y = 15) + scale_x_reverse()
#export
dev.off()


#############
# Limonicus #
#############

input3 <- read.csv("Lethal temperature limonicus_new.txt", sep = "\t")
input3

# check table dimensions
dim(input3)

# check the structure of the data
str(input3)

# define factor variables
input3$Temperature=as.factor(input3$Temperature)

# relevel to set 10ºC as reference
input3$Temperature <- relevel(input3$Temperature, ref="10")

# binomial model for 0/1 data
model2 <- glm(Survival ~ Temperature, data = input3, family = binomial)
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

# effect plot of survival vs temperature
png("mortality_vs_temperature_limonicus.png", width = 15, height = 15, units = "cm", res = 600)
model2 <- glm(Mortality ~ Temperature, data = input3, family = binomial)
p <- plot_model(model2, type="pred", terms = "Temperature",
                axis.title = c("Temperature (ºC)", "Mortality"),
                title = "",
                legend.title = "",
                colors = c("green", "orange"),
                show.data = FALSE, dot.size = 4, axis.lim = c(0,1))
set_theme(
  base = theme_classic(), legend.title.size = 1.4)
p + font_size(axis_title.x = 20, axis_title.y = 20, labels.x = 15, labels.y = 15) + scale_x_reverse()
#export
dev.off()























