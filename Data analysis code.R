#How Scarcity Captures the Mind and Influences Behaviour:
#The Effects of Scarcity on the Performance in a Word-guessing Game and on the Simon Task

# Packages ----------------------------------------------------------------
install.packages("lme4")
install.packages("lattice")
install.packages("plyr")
install.packages("influence.ME")
install.packages("sjstats")
install.packages("lmerTest")
install.packages("emmeans")
install.packages("sjPlot")
install.packages("lsmeans")
install.packages("predictmeans")
install.packages("ggplot2")

library(lme4)
library(stats)
library(psych)
library(readxl)
library(ggplot2)
library(lattice)
library(plyr)
library(influence.ME)
library(sjstats)
library(lmerTest)
library(emmeans)
library(sjPlot)
library(lsmeans)
library(predictmeans)
library(ggplot2)
# Importing data ----------------------------------------------------------
data <- read_excel("~/Win7/Desktop/Experiment/Edited data/one_file_10.xlsx")
View(data)

#factorize
data$id <- as.factor(data$id)
data$borrow <- as.factor(data$borrow)
data$task <- as.factor(data$task)

#change level names
levels(data$borrow) = c("No borrow", "Borrow")
levels(data$task) = c("Baseline", "Poor", "Rich")

borrow = data[data$borrow =="Borrow",]
nrow(borrow)
no_borrow = data[data$borrow =="No borrow",]
nrow(no_borrow)

base = data[data$task=="Baseline",]
poor = data[data$task == "Poor", ]
rich = data[data$task == "Rich", ]

poor_borrow=poor[poor$borrow =="Borrow",]
rich_borrow=rich[rich$borrow=="Borrow",]
base_borrow=base[base$borrow=="Borrow",]

poor_noborrow=poor[poor$borrow =="No borrow",]
rich_noborrow=rich[rich$borrow=="No borrow",]
base_noborrow=base[base$borrow=="No borrow",]

#adding new variables
data$guesses = data$strikes + data$letters
data$t_letter = data$time/data$guesses
data$correct = data$letters/data$guesses
data$words_raw = data$words
data$letters_raw = data$letters

data=mutate(data, words = ifelse(task == "Poor", words / guesses, words)) 
data=mutate(data, words = ifelse(task == "Rich", words / guesses, words)) 

data$budget = 1
data = mutate(data, budget = ifelse(task == "Poor", budget * 60, budget))
data = mutate(data, budget = ifelse(task == "Rich", budget * 150, budget))

data$used = data$strikes/data$budget

data$fraction_borrowed = data$borrow_count/data$budget
borrow = data[data$borrow =="Borrow",]
nrow(borrow)
no_borrow = data[data$borrow =="No borrow",]
nrow(no_borrow)

base = data[data$task=="Baseline",]
poor = data[data$task == "Poor", ]
rich = data[data$task == "Rich", ]

View(data)

no_base = data[which(data$task=="Poor" | data$task=="Rich"),]
View(no_base)

# Descriptive stats -------------------------------------------------------
#descriptive stats
describe(data)
describe(poor)
describe(rich)
describe(base)

describe(borrow)
describe(no_borrow)

describe(poor_borrow)
describe(poor_noborrow)
describe(rich_borrow)
describe(rich_noborrow)
describe(base_borrow)
describe(base_noborrow)

##descriptives per condition
##words
mean(borrow$words[which(data$task =="Rich")], na.rm=T)
mean(borrow$words[which(data$task =="Poor")], na.rm=T)
mean(no_borrow$words[which(data$task =="Rich")], na.rm=T)
mean(no_borrow$words[which(data$task =="Poor")], na.rm=T)
sd(borrow$words[which(data$task =="Rich")], na.rm=T)
sd(borrow$words[which(data$task =="Poor")], na.rm=T)
sd(no_borrow$words[which(data$task =="Rich")], na.rm=T)
sd(no_borrow$words[which(data$task =="Poor")], na.rm=T)

##proportion
mean(borrow$correct[which(data$task =="Rich")], na.rm=T)
mean(borrow$correct[which(data$task =="Poor")], na.rm=T)
mean(no_borrow$correct[which(data$task =="Rich")], na.rm=T)
mean(no_borrow$correct[which(data$task =="Poor")], na.rm=T)
sd(borrow$correct[which(data$task =="Rich")], na.rm=T)
sd(borrow$correct[which(data$task =="Poor")], na.rm=T)
sd(no_borrow$correct[which(data$task =="Rich")], na.rm=T)
sd(no_borrow$correct[which(data$task =="Poor")], na.rm=T)


##boxplots
#boxplot to see the relationship between different levels (raw data)
par(oma=c(2,4,0,0))
boxplot(st_rt~task*borrow,data, xlab = "Reaction Time", horizontal = TRUE, las = 1)
boxplot(st_acc~task*borrow,data, xlab = "Accuracy", horizontal = TRUE, las = 1)
boxplot(time~task*borrow,data, xlab = "Time played in milliseconds", horizontal = TRUE, las = 1)
boxplot(t_letter~task*borrow, data, xlab = "Time spent guessing each letter in milliseconds", horizontal = T, las = 1)
boxplot(letters~task*borrow,data, xlab = "Correctly guessed letters", horizontal = TRUE, las = 1)
boxplot(strikes~task*borrow,data, xlab = "Strikes", horizontal = TRUE, las = 1)
boxplot(guesses~task*borrow, data, xlab = "Total guesses made", horizontal = T, las = 1)
boxplot(words~task*borrow, data, xlab = "Correct words per guess", horizontal = T, las = 1)
boxplot(words_raw~task*borrow, data, xlab = "Total words guessed", horizontal = T, las = 1)
boxplot(correct~task*borrow, data, xlab = "Proportion correct guesses", horizontal = T, las = 1)

#floor effect
hist(data$borrow_count, main = "Number of borrowed guesses", xlab="Number of borrowed guesses", ylim =c(0,50), xlim=c(0,20))
hist(data$fraction_borrowed, main = "Proportion of budget borrowed", xlab = "Proportion of budget borrowed", ylim =c(0,50), xlim=c(0,.4))
skew(data$borrow_count)
skew(data$fraction_borrowed)

# Questionnire data analysis -------------------------------------------------------
questionnaire <- read_excel("~/Win7/Desktop/Experiment/Edited data/questionnaire_short.xlsx")
View(questionnaire)

describe(questionnaire)

mean_age = mean(questionnaire$Q1)
mean_age #22.05

length(which(questionnaire$Q2 == "1")) #25 males 
length(which(questionnaire$Q2 == "0")) #38 females
length(which(questionnaire$Q2 == "2")) # 1 other

length(which(questionnaire$Q3 == "0"))
length(which(questionnaire$Q3 == "1"))
length(which(questionnaire$Q3 == "2")) #49 --> secondary
length(which(questionnaire$Q3 == "3")) 
length(which(questionnaire$Q3 == "4")) #10 --> undergrad
length(which(questionnaire$Q3 == "5")) #5 --> postgrad

length(which(questionnaire$Q4 == "0")) #3 left handed
length(which(questionnaire$Q4 == "1")) #60 right handed
length(which(questionnaire$Q4 == "2")) #1 both

length(which(questionnaire$Q5 == "0")) #35 english first
length(which(questionnaire$Q5 == "1")) #16 english with another
length(which(questionnaire$Q5 == "2")) #13 english not first

mean_english = mean(questionnaire$Q6)
mean_english #19.92


length(which(questionnaire$Q7 == "2")) #1 limited working english
length(which(questionnaire$Q7 == "3")) #8 professional working
length(which(questionnaire$Q7 == "4")) #55 full professional 


length(which(questionnaire$Q8 == "0")) #26 0 languages besides english
length(which(questionnaire$Q8 == "1")) #26 1 language besides english
length(which(questionnaire$Q8 == "2")) #8 2 languages besides english
length(which(questionnaire$Q8 == "3")) #3 3 languages besides english 
length(which(questionnaire$Q8 == "4")) #1 4 languages besides english 

# VIF function - multicolinearity -----------------------------------------
vif.mer <- function (fit) {
  ## adapted from rms::vif
  
  v <- vcov(fit)
  nam <- names(fixef(fit))
  
  ## exclude intercepts
  ns <- sum(1 * (nam == "Intercept" | nam == "(Intercept)"))
  if (ns > 0) {
    v <- v[-(1:ns), -(1:ns), drop = FALSE]
    nam <- nam[-(1:ns)]
  }
  
  d <- diag(v)^0.5
  v <- diag(solve(v/(d %o% d)))
  names(v) <- nam
  v
}

# Borrow count ------------------------------------------------------
borrow_count.null = lmer(borrow_count~1+(1|id), data = data, REML = F)
borrow_count.within = lmer(borrow_count~task +(1|id), data = data, REML = F)
summary(borrow_count.within)
confint(borrow_count.within)

anova(borrow_count.null, borrow_count.within)

boxplot(poor$borrow_count,rich$borrow_count, names = c("Poor", "Rich"), ylab = "Number of borrowed guesses", xlab = "Scarcity condition", main = "Borrowed guesses per scarcity condition", ylim =c(0,15)) #poor borrow more

influence.borrow = influence(borrow_count.within, "id")
cooks.distance(influence.borrow)
plot(influence.borrow, which = "cook", main = "Borrow Count - Cook's distance plot")
CookD(borrow_count.within)

# Fraction of budget borrowed ---------------------------------------------
fraction.null = lmer(fraction_borrowed~1+(1|id), data = data, REML = F)
fraction.within = lmer(fraction_borrowed~task +(1|id), data = data, REML = F)
summary(fraction.within)
confint(fraction.within)

anova(fraction.null, fraction.within)

boxplot(poor$fraction_borrowed,rich$fraction_borrowed, names = c("Poor", "Rich"), ylab = "Proportion of budget borrowed", xlab = "Scarcity condition", main = "Proportion of budget borrowed per scarcity condition") #poor borrow more

mean(poor$fraction_borrowed, na.rm = T)
sd(poor$fraction_borrowed, na.rm = T)
mean(rich$fraction_borrowed, na.rm = T)
sd(rich$fraction_borrowed, na.rm = T)

influence.fraction = influence(fraction.within, "id")
cooks.distance(influence.fraction)
plot(influence.fraction, which = "cook", main = "Fraction of budget borrowed - Cook's distance plot")
CookD(fraction.within) # all below 1


# Proportion correct ------------------------------------------------------
correct.null = lmer(correct~1+(1|id), data = data, REML = F)
correct.within = lmer(correct~task+(1|id), data = data, REML = F)
summary(correct.within)
confint(correct.within)
correct.between = lmer(correct~borrow+(1|id), data = data, REML = F)
summary(correct.between)
correct.both = lmer(correct~task+borrow+(1|id), data = data, REML = F)
summary(correct.both)
correct.interaction = lmer(correct~task*borrow+(1|id), data = data, REML = F)
summary(correct.interaction)
confint(correct.interaction)

ggplot(no_base, aes(x=task, y=correct, color=borrow, group=borrow))+
  stat_summary(fun.y=mean, geom="point")+
  stat_summary(fun.y=mean, geom="line")+
  labs(x="Scarcity condition")+
  labs(y="Proportion of correctly guesses letters")+
  labs(title="Scarcity x Borrow interaction plot: Proportion correct")

influence.correct = influence(correct.interaction, "id")
cooks.distance(influence.correct)
plot(influence.correct, which = "cook", main = "Proportion correct - Cook's distance plot")
CookD(correct.interaction) #everything well below 1

#anova comparisons
anova(correct.null, correct.within)
anova(correct.null, correct.between)
anova(correct.both, correct.between)
anova(correct.both, correct.within)
anova(correct.both, correct.interaction)

boxplot(poor$correct, rich$correct, names=c('Poor', "Rich"), main = "Proportion of correct guesses per condition", xlab = "Scarcity condition", ylab = "Proportion of correct guesses") 
boxplot(borrow$correct, no_borrow$correct, names=c('Poor', "Rich"), main = "Proportion of correct guesses per condition", xlab = "Borrow condition", ylab = "Proportion of correct guesses") 

# Scaled words guessed ----------------------------------------------------
words.null = lmer(words~1+(1|id), data = data, REML = F)
words.within = lmer(words~task+(1|id), data = data, REML = F)
summary(words.within)
confint(words.within)
words.between = lmer(words~borrow+(1|id), data = data, REML = F)
summary(words.between)
words.both = lmer(words~task+borrow+(1|id), data = data, REML = F)
summary(words.both)
words.interaction = lmer(words~task*borrow+(1|id), data = data, REML = F)
summary(words.interaction)
confint(words.interaction)

ggplot(no_base, aes(x=task, y=words, color=borrow, group=borrow))+
  stat_summary(fun.y=mean, geom="point")+
  stat_summary(fun.y=mean, geom="line")+
  labs(x="Scarcity condition")+
  labs(y="Number of words guessed per guess")+
  labs(title="Scarcity x Borrow interaction plot: Words per guess")

influence.words= influence(words.interaction, "id")
cooks.distance(influence.words)
plot(influence.words, which = "cook", main = "Scaled words guessed - Cook's distance plot")
CookD(words.interaction) #nothing too high

boxplot(poor$words, rich$words, names=c('Poor', "Rich"), main = "Correct words per guess per condition", xlab = "Scarcity condition", ylab = "Number of correct words per guess") 
boxplot(borrow$words, no_borrow$words, names = c("Borrow", "No Borrow"), main = "Correct words per guess per condition", xlab = "Borrow condition", ylab = "Number of correct words per guess") 

#anova comparisons
anova(words.null, words.within)
anova(words.null, words.between)
anova(words.both, words.between)
anova(words.both, words.within)
anova(words.both, words.interaction)

# Time per letter -----------------------------------------
perletter.null = lmer(t_letter~1+(1|id), data = data, REML = F)
perletter.within = lmer(t_letter~task+(1|id), data = data, REML = F)
summary(perletter.within) #lowest AIC, BIC ( 2388.8, 2400.2)
confint(perletter.within, level=0.95 )
perletter.between = lmer(t_letter~borrow+(1|id), data = data, REML = F)
summary(perletter.between)
perletter.both = lmer(t_letter~task+borrow+(1|id), data = data, REML = F)
summary(perletter.both)
perletter.interaction = lmer(t_letter~task*borrow+(1|id), data = data, REML = F)
summary(perletter.interaction)

influence.perletter = influence(perletter.interaction, "id")
cooks.distance(influence.perletter)
plot(influence.perletter, which = "cook", main = "Time per letter - Cook's distance plot")
CookD(perletter.interaction) #everything below 1

#comparisons
anova(perletter.between, perletter.null)
anova(perletter.within, perletter.null) #significant effect!
anova(perletter.both, perletter.between) #task significant beyond between
anova(perletter.both, perletter.within)
anova(perletter.interaction, perletter.both)

#difference plots
boxplot(borrow$t_letter, no_borrow$t_letter, names = c("Borrow", "No Borrow"), main = "Time spent guessing each letter per condition", xlab = "Borrow condition", ylab = "Time spent per letter in milliseconds") #poor do better
boxplot(poor$t_letter, rich$t_letter, names = c("Poor", "Rich"), main = "Time spent guessing each letter per condition", xlab = "Scarcity condition", ylab = "Time spent per letter in milliseconds")
# Reaction time models ----------------------------------------------------
rt.null = lmer(st_rt~1+(1|id), data=data, REML = F)
rt.within = lmer(st_rt~task+(1|id), data=data, REML = F)
summary(rt.within)
confint(rt.within)
rt.between = lmer(st_rt~borrow+(1|id), data=data, REML = F)
summary(rt.between) # lowest AIC, BIC (1788.2, 1801.2)
rt.both = lmer(st_rt~task+borrow + (1|id), data = data, REML = F)
summary(rt.both)
rt.interaction = lmer(st_rt~task+borrow +task:borrow +(1|id), data = data, REML = F)
summary(rt.interaction)

## ASSUMPTIONS
# Linearity of the data - residuals vs fitted
plot(rt.interaction, type = c("p", "smooth"), main = "RT Model - Residuals vs fitted plot")

# Homoscedasticity - scale-location plot
plot(rt.interaction, sqrt(abs(resid(.))) ~ fitted(.), type = c("p", "smooth"), main = "RT model - Scale-Location plot")

# Normality of residuals
hist(resid(rt.interaction), main = "RT Model - Histogram of residuals")
qqmath(rt.interaction, id = 0.05, main = "RT Model - Q-Q plot of residuals")

# Cook's distance
influence.rt.int = influence(rt.interaction, "id")
cooks.distance(influence.rt.int)
plot(influence.rt.int, which = "cook", main = "RT Model - Cook's distance plot")
CookD(rt.interaction)

# Multicolinearity
vif.mer(rt.interaction)

# Normality of random effects
r_int<- ranef(rt.interaction)$id$`(Intercept)`
qqnorm(r_int, main = "RT Model - Q-Q plot of random effects")
qqline(r_int, col = "red")

# Comparisons
#within - null
anova(rt.within, rt.null) #no significant difference
boxplot(poor$st_rt, rich$st_rt, base$st_rt, main = "The three levels of Simon task RT measure",names = c("Poor", "Rich", "Baseline"), ylab = "Reaction time in milliseconds")
#between - null
anova(rt.between, rt.null) #no significant difference
boxplot(borrow$st_rt, no_borrow$st_rt, main = "Effect of borrowing on Simon task RT measure",names = c("Borrow", "No Borrow"), ylab = "Reaction time in milliseconds", xlab="Borrowing condition")
#within - both
anova(rt.within, rt.both) #no significant difference
#between - both
anova(rt.between, rt.both) #no significant difference
#both - interaction
anova(rt.both, rt.interaction) #no significant difference

# Accuracy models ----------------------------------------------------
acc.null = lmer(st_acc~1+(1|id), data=data, REML = F)
acc.within = lmer(st_acc~task+(1|id), data=data, REML = F)
summary(acc.within) #lowest AIC, BIC (1011.9, 1028.2)
confint(acc.within)
acc.between = lmer(st_acc~borrow+(1|id), data=data, REML = F)
summary(acc.between)
acc.both = lmer(st_acc~task+borrow + (1|id), data = data, REML = F)
summary(acc.both)
acc.interaction = lmer(st_acc~task+borrow +task:borrow +(1|id), data = data, REML = F)
summary(acc.interaction)

# Linearity of the data - residuals vs fitted
plot(acc.interaction, type = c("p", "smooth"), main = "Accuracy Model - Residuals vs fitted plot")

# Homoscedasticity - scale-location plot
plot(acc.interaction, sqrt(abs(resid(.))) ~ fitted(.), type = c("p", "smooth"), main = "Accuracy Model - Scale-Location plot")

# Normality of residuals
hist(resid(acc.interaction), main = "Accuracy Model - Histogram of residuals")
qqmath(acc.interaction, id = 0.05, main = "Accuracy Model - Q-Q plot of residuals")

# Cook's distance
influence.acc.int = influence(acc.interaction, "id")
cooks.distance(influence.acc.int)
plot(influence.acc.int, which = "cook", main = "Accuracy Model - Cook's distance plot")
CookD(acc.interaction)

# Multicolinearity
vif.mer(acc.interaction)

# Normality of random effects
r_int<- ranef(acc.interaction)$id$`(Intercept)`
qqnorm(r_int, main = "Accuracy Model - Q-Q plot of random effects")
qqline(r_int, col = "red")

# Comparisons
#within - null
anova(acc.within, acc.null) #significant difference (task has a significant effect on accuracy)
##see significance between levels
summary(acc.within)
data$task <- relevel(data$task, "Rich")
acc.within = lmer(st_acc~task+(1|id), data=data, REML = F)
summary(acc.within)
data$task <- relevel(data$task, "Baseline")

#difference between rich and poor not significant!
boxplot(poor$st_acc, rich$st_acc, base$st_acc, names = c("Poor", "Rich", "Baseline"), main = "The three levels of Simon task accuracy measure", ylab = "Number of correct responses")

#between - null
anova(acc.between, acc.null) #no significant difference
boxplot(borrow$st_acc, no_borrow$st_acc, main = "Effect of borrowing on Simon task accuracy measure",names = c("Borrow", "No Borrow"), ylab = "Number of correct responses", xlab="Borrowing condition")
#within - both
anova(acc.within, acc.both) #no significant difference
#between - both
anova(acc.between, acc.both) #significant difference
#so task si significant beyond borrow
#both - interaction
anova(acc.both, acc.interaction) #no significant difference

# Adding questionnaire to the models --------------------------------------
data$Q2 <- as.factor(data$Q2)
data$Q3 <- as.factor(data$Q3)
data$Q4 <- as.factor(data$Q4)
data$Q5 <- as.factor(data$Q5)
data$Q7 <- as.factor(data$Q7)
data$Q8 <- as.factor(data$Q8)


#reaction time
rt.model.full = lmer(st_rt~task*borrow + Q1+Q2+Q3+Q4+Q5+Q6+Q7+Q8+(1|id),data=data, REML = F)
summary(rt.model.full)
anova(rt.model.full, rt.interaction) #significant difference (full model better)
##but full model has higher BIC!!

#accuracy
acc.model.full = lmer(st_acc~task*borrow + Q1+Q2+Q3+Q4+Q5+Q6+Q7+Q8+(1|id),data=data, REML = F)
summary(acc.model.full)
anova(acc.model.full, acc.interaction) #significant difference
##but full model has a larger BIC

#words
words.model.full = lmer(words~task*borrow + Q1+Q2+Q3+Q4+Q5+Q6+Q7+Q8+(1|id),data=data, REML = F)
summary(words.model.full)
anova(words.model.full, words.interaction) #no significant difference, simple model has a lower BIC

#correct
correct.model.full = lmer(correct~task*borrow + Q1+Q2+Q3+Q4+Q5+Q6+Q7+Q8+(1|id),data=data, REML = F)
summary(correct.model.full)
anova(correct.model.full, correct.interaction) #no significant difference, simple model has lwer BIC

#time per letter
perletter.model.full = lmer(t_letter~task*borrow + Q1+Q2+Q3+Q4+Q5+Q6+Q7+Q8+(1|id),data=data, REML = F)
summary(perletter.model.full)
anova(perletter.model.full, perletter.interaction) #no significant difference, simpler model has a much lower BIC

#borrow count
borrow.model.full = lmer(borrow_count~task + Q1+Q2+Q3+Q4+Q5+Q6+Q7+Q8+(1|id),data=data, REML = F)
summary(borrow.model.full)
anova(borrow_count.within, borrow.model.full)

#fraction borrowed
fraction.model.full = perletter.model.full = lmer(fraction_borrowed~task + Q1+Q2+Q3+Q4+Q5+Q6+Q7+Q8+(1|id),data=data, REML = F)
summary(fraction.model.full)
anova(fraction.model.full, fraction.within)
#in all cases, simpler model has a lower BIC and is hence perfered! 

