################################################################################

#Investigating the impact of age on auditory short-term, long-term, and working memory (2023)

#Gemma Fernández Rubio (gemmafr@clin.au.dk)

################################################################################

#DATA LOADING
library(readxl) #for reading excel files

#set the directory
setwd("/working_directory")

data <- read_excel("data.xlsx")
data <- data.frame(subject_id = data$subject_id, group = data$group, wm = data$wm, melody = data$melody, rhythm = data$rhythm,
                   old = data$old, new = data$new, music_training = data$music_training) 

################################################################################

#DATA VISUALIZATION AND SUMMARY STATISTICS

library(ggpubr) #for creating plots

#create a general data plot
ggboxplot(data,x="group",y=c("wm", "melody","rhythm","old","new","music_training"), merge=T,palette = "npg")

#create separate plots for each variable
wm <- ggboxplot(data,x="group",y="wm", title = "WM",
          merge=T,palette = "npg")
mel <- ggboxplot(data,x="group",y="melody", title = "STM Melody",
          merge=T,palette = "npg")
rhy <- ggboxplot(data,x="group",y="rhythm", title = "STM Rhythm",
          merge=T,palette = "npg")
old <- ggboxplot(data,x="group",y="old", title = "LTM Old",
                 merge=T,palette = "npg")
new <- ggboxplot(data,x="group",y="new", title = "LTM New",
                 merge=T,palette = "npg")
ggarrange(wm,mel,rhy,old,new)

#check the covariate effect
plot(data$wm, data$music_training,xlab="WM", ylab="Music Training")
abline(lm(data$music_training~data$wm), col="red")
plot(data$rhythm, data$music_training,xlab="STM Rhythm", ylab="Music Training")
abline(lm(data$music_training~data$rhythm), col="red")
plot(data$melody, data$music_training,xlab="STM Melody", ylab="Music Training")
abline(lm(data$music_training~data$melody), col="red")
plot(data$old, data$music_training,xlab="LTM Old", ylab="Music Training")
abline(lm(data$music_training~data$old), col="red")
plot(data$new, data$music_training,xlab="LTM New", ylab="Music Training")
abline(lm(data$music_training~data$new), col="red")

#summary statistics
young <- data[data$group=="Y",]
summary(young)

older <- data[data$group=="E",]
summary(older)

################################################################################

#MANCOVA ASSUMPTIONS

################################################################################

#OUTLIERS
#checking that there are no outliers

#convert raw values into z-scores to identify and remove outliers
zscores <- data.frame(subject_id = data$subject_id, group = data$group, wm = scale(data$wm), melody = scale(data$melody), rhythm = scale(data$rhythm),
                       old = scale(data$old), new = scale(data$new), music_training = scale(data$music_training))

#visualize z-scores and outliers
par(mfrow=c(3,2))
plot(zscores$wm,main="WM")
plot(zscores$melody,main="STM Melody")
plot(zscores$rhythm,main="STM Rhythm")
plot(zscores$old,main="LTM Old")
plot(zscores$new,main="LTM New")
plot(zscores$music_training,main="Music Training")

#identify outliers
outliers_old <- subset(zscores,old < -3, old > 3)
outliers_new <- subset(zscores,new < -3, new > 3)

#remove outliers
zscores$old[zscores$old <= -3] <- NA
zscores$new[zscores$new <= -3] <- NA
data$old[data$old <= 7] <- NA
data$new[data$new <= 7] <- NA

################################################################################

#HOMOCEDASTICITY ASSUMPTION

library(car) #use to perform tests, create visualizations and transform data

#check whether the variances are homogeneous
leveneTest(wm ~ group,data)
leveneTest(melody ~ group,data)
leveneTest(rhythm ~ group,data)
leveneTest(old ~ group,data)
leveneTest(new ~ group,data)
leveneTest(music_training ~ group,data)

################################################################################

#MULTIVARIATE NORMALITY ASSUMPTION

#Density plots
#density plots provide a visual judgment about whether the distribution is bell shaped
ggdensity(data$wm)
ggdensity(data$melody)
ggdensity(data$rhythm)
ggdensity(data$old)
ggdensity(data$new)
ggdensity(data$music_training)

#Q-Q plots
#q-q plots draw the correlation between a given sample and the normal distribution (a 45-degree reference line is also plotted)
ggqqplot(data$wm)
ggqqplot(data$melody)
ggqqplot(data$rhythm)
ggqqplot(data$old)
ggqqplot(data$new)
ggqqplot(data$music_training)

#Shapiro-Wilk test (test of univariate normality)
shapiro.test(data$wm)
shapiro.test(data$melody)
shapiro.test(data$rhythm)
shapiro.test(data$old)
shapiro.test(data$new)
shapiro.test(data$music_training)

################################################################################

#LINEARITY ASSUMPTION
#check whether there is a linear relationship between each pair of dependent variables for each group of the independent variable

plot(data[3:8],pch = 20)

################################################################################

#HOMOGENEITY OF VARIANCE-COVARIANCE MATRICES ASSUMPTION

library(rstatix) #for statistical analyses

box_m(data[3:8],as.factor(data$group))

################################################################################

#T-TEST
#testting differences in the covariate
t.test(music_training ~ group, data, var.equal = TRUE)

################################################################################

#MANCOVA
#multivariate analysis of variance and effect sizes

library(jmv) #for mancova analysis
library(effectsize) #for calculating effect sizes

dependent <- cbind(data$wm,data$melody,data$rhythm,data$old,data$new)
independent <- data$group
mancova_model <- manova(dependent ~ independent+data$music_training) #manova + mancova
summary(mancova_model, test = "Wilks")
#summary.aov(mancova_model) #univariate tests
eta_squared(mancova_model, partial = TRUE) #effect size

################################################################################

#ANCOVA
#univariate tests and effect sizes

ancova_WM <- aov(wm ~ group + music_training, data)
summary(ancova_WM)
eta_squared(ancova_WM, partial = TRUE)

ancova_mel <- aov(melody ~ group + music_training, data)
summary(ancova_mel)
eta_squared(ancova_mel, partial = TRUE)

ancova_rhy <- aov(rhythm ~ group + music_training, data)
summary(ancova_rhy)
eta_squared(ancova_rhy, partial = TRUE)

ancova_old <- aov(old ~ group + music_training, data)
summary(ancova_old)
eta_squared(ancova_old, partial = TRUE)

ancova_new <- aov(new ~ group + music_training, data)
summary(ancova_new)
eta_squared(ancova_new, partial = TRUE)

################################################################################

#RAINCLOUD PLOTS

#source: Allen, M., Poggiali, D., Whitaker, K., Marshall, T. R., van Langen, J.,
#& Kievit, R. A.Raincloud plots: a multi-platform tool for robust data visualization 
#[version 2; peer review: 2 approved] Wellcome Open Research 2021, 4:63. 
#https://doi.org/10.12688/wellcomeopenres.15191.2

if (!require(remotes)) {
  install.packages("remotes")
}
remotes::install_github('jorvlan/raincloudplots')

library(raincloudplots)

#repeated measures
young <- zscores[zscores$group=="Y",]
older <- zscores[zscores$group=="E",]

#for the function to work, all variables must be the same length
y_mel <- young$melody
o_mel <- older$melody
length(y_mel) <- length(o_mel)
y_rhy <- young$rhythm
o_rhy <- older$rhythm
length(y_rhy) <- length(o_rhy)
y_old <- young$old
o_old <- older$old
length(y_old) <- length(o_old)
y_new <- young$new
o_new <- older$new
length(y_new) <- length(o_new)
y_wm <- young$wm
o_wm <- older$wm
length(y_wm) <- length(o_wm)
y_mus <- young$musictraining
o_mus <- older$musictraining
length(y_mus) <- length(o_mus)

#STM (Melody and Rhythm) scores and LTM (Old) scores
df_2x3 <- data_2x2(array_1 = y_old, array_2 = o_old, array_3 = y_rhy, array_4 = o_rhy, array_5 = y_mel, array_6 = o_mel,
                   labels = (c('Young','Older')), jit_distance = .09, jit_seed = 321)

colors <- rep(c("indianred1", "dodgerblue"), 3) #choose colors 

raincloud_2x3_repmes(data = df_2x3, colors = colors, fills = colors, size = 1,alpha = .6,ort = "h") +
  scale_x_continuous(breaks = c(1,2,3), limits = c(0.8, 4.3), labels = rep("", 3)) +
  ylab("z-scores") +
  theme_bw() +
  theme(axis.title.y = element_blank())

#LTM (New), WM and Music Training scores
df_2x3 <- data_2x2(array_1 = y_mus, array_2 = o_mus, array_3 = y_wm, array_4 = o_wm, array_5 = y_new, array_6 = o_new,
                   labels = (c('Young','Older')), jit_distance = .09, jit_seed = 321)

raincloud_2x3_repmes(data = df_2x3, colors = colors, fills = colors, size = 1,alpha = .6,ort = "h") +
  scale_x_continuous(breaks = c(1,2,3), limits = c(0.8, 4.3), labels = rep("", 3)) +
  ylab("z-scores") +
  theme_bw() +
  theme(axis.title.y = element_blank())

################################################################################

#ADDITIONAL ANALYSES

################################################################################

#MANOVA
#multivariate analysis of variance (excluding the covariate)

manova_model <- manova(dependent ~ independent, data)
summary(manova_model)
eta_squared(manova_model) #effect size
summary.aov(manova_model) #post-hoc analyses

################################################################################

#CORRELATION

ggscatter(data, x = "music_training", y = "wm", add = "reg.line", conf.int = TRUE, 
                    cor.coef = TRUE, cor.method = "pearson", xlab = "Music Training", ylab = "WM")

ggscatter(data, x = "music_training", y = "melody", add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", xlab = "Music Training", ylab = "Melody")

ggscatter(data, x = "music_training", y = "rhythm", add = "reg.line", conf.int = TRUE, 
                     cor.coef = TRUE, cor.method = "pearson", xlab = "Music Training", ylab = "Rhythm")

ggscatter(data, x = "music_training", y = "old", add = "reg.line", conf.int = TRUE, 
                     cor.coef = TRUE, cor.method = "pearson", xlab = "Music Training", ylab = "LTM Old")

ggscatter(data, x = "music_training", y = "new", add = "reg.line", conf.int = TRUE, 
                     cor.coef = TRUE, cor.method = "pearson", xlab = "Music Training", ylab = "LTM New")

################################################################################

#REGRESSION

wm_reg <- lm(wm ~ music_training, data = data)
summary(wm_reg)
mel_reg <- lm(melody ~ music_training, data = data)
summary(mel_reg)
rhy_reg <- lm(rhythm ~ music_training, data = data)
summary(rhy_reg)
old_reg <- lm(old ~ music_training, data = data)
summary(old_reg)
new_reg <- lm(new ~ music_training, data = data)
summary(new_reg)

par(mfrow=c(3,2))
scatter.smooth(x = data$wm, y = data$music_training, main = "Working Memory")
scatter.smooth(x = data$melody, y = data$music_training, main = "STM Melody")
scatter.smooth(x = data$rhythm, y = data$music_training, main = "STM Rhythm")
scatter.smooth(x = data$old, y = data$music_training, main = "LTM Old")
scatter.smooth(x = data$new, y = data$music_training, main = "LTM New")


