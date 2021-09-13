#Installing the necessary packages

install.packages("tidyverse")
install.packages("readr")
install.packages("dplyr")
install.packages("ggpubr")
install.packages("car")
install.packages("reshape2")
install.packages("hms")
install.packages("BlandAltmanLeh")
install.packages("ggstatsplot")
install.packages("hrbrthemes")
install.packages("blandr")
install.packages("viridis")
install.packages("readxl")

#Loading the necessary packages

library(tidyverse)
library(readr)
library(dplyr)
library(ggpubr)
library(car)
library(reshape2)
library(hms)
library(BlandAltmanLeh)
library(blandr)
library(ggstatsplot)
library(hrbrthemes)
library(viridis)
library(readxl)

#Import dataset and subset selection:

SEL2020 <- read_csv("/Users/pixel/Desktop/datasets_er/SEL2020.csv")

SEL2020$FVC_pre = as.double(SEL2020$FVC_pre)
SEL2020$FEV1_pre = as.double(SEL2020$FEV1_pre)
SEL2020$FEV1FVC_pre = as.double(SEL2020$FEV1FVC_pre)
SEL2020$FEF2575_pre = as.double(SEL2020$FEF2575_pre)

SEL2020$FVC_GLI = as.double(SEL2020$FVC_GLI)
SEL2020$FEV1_GLI = as.double(SEL2020$FEV1_GLI)
SEL2020$FEV1FVC_GLI = as.double(SEL2020$FEV1FVC_GLI)
SEL2020$FEF2575_GLI= as.double(SEL2020$FEF2575_GLI)

SEL2020$MD_FVC_GLI = as.double(SEL2020$MD_FVC_GLI)
SEL2020$MD_FEV1_GLI = as.double(SEL2020$MD_FEV1_GLI)
SEL2020$MD_FEV1FVC_GLI = as.double(SEL2020$MD_FEV1FVC_GLI)
SEL2020$MD_FEF2575_GLI= as.double(SEL2020$MD_FEF2575_GLI)

ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")

#View dataset

View(SEL2020)
view(G21)
view(G21_Boys)
view(G21_Girls)
view(ARIA)
view(ARIA_Boys)
view(ARIA_Girls)

#Descriptive analysis for Derivation Cohort ARIA:

#Selecting subset with only ARIA participants

ARIA <- subset(SEL2020, Project == "ARIA")

# n and % for Gender ARIA:
table(ARIA$Gender)
ggplot(data=ARIA) + geom_bar(mapping = aes(x = Gender, fill = Gender))

#Males:
 n=267
(267/481)*100

#Females:
 n=214
(214/481)*100

# Age range:
summary(ARIA$Idade)
sd(ARIA$Idade)

# Two-samples T-test gender comparissons in age, height, weight and BMI ARIA cohort:
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")

#Age:
t.test(ARIA$Idade ~ ARIA$Gender, var.equal = TRUE)

mean(ARIA_Boys$Idade)
sd(ARIA_Boys$Idade)

mean(ARIA_Girls$Idade)
sd(ARIA_Girls$Idade)

#Height:
t.test(ARIA$Altura_cm ~ ARIA$Gender, var.equal = TRUE)

mean(ARIA$Altura_cm)
sd(ARIA$Altura_cm)

mean(ARIA_Boys$Altura_cm)
sd(ARIA_Boys$Altura_cm)

mean(ARIA_Girls$Altura_cm)
sd(ARIA_Girls$Altura_cm)

ARIA_Boys %>%
  ggplot( aes(x=Altura_cm)) +
  geom_density(fill="skyblue", color="skyblue", alpha=0.8) +
  ggtitle("Height distribution for Boys") +
  theme_ipsum()

ARIA_Girls %>%
  ggplot( aes(x=Altura_cm)) +
  geom_density(fill="pink", color="pink", alpha=0.8) +
  ggtitle("Height distribution for Girls") +
  theme_ipsum()

#Weight:
t.test(ARIA$Peso ~ ARIA$Gender, var.equal = TRUE)

mean(ARIA$Peso)
sd(ARIA$Peso)

mean(ARIA_Boys$Peso)
sd(ARIA_Boys$Peso)

mean(ARIA_Girls$Peso)
sd(ARIA_Girls$Peso)

#BMI:
t.test(ARIA$BMI ~ ARIA$Gender, var.equal = TRUE)

mean(ARIA_Boys$BMI)
sd(ARIA_Boys$BMI)

mean(ARIA_Girls$BMI)
sd(ARIA_Girls$BMI)

#Comparisson two-sample t-test spirometric parameters between Male and Female participants in ARIA:

#FVC_pre:
t.test(ARIA$FVC_pre ~ ARIA$Gender, var.equal = TRUE)

mean(ARIA_Boys$FVC_pre)
sd(ARIA_Boys$FVC_pre)

mean(ARIA_Girls$FVC_pre)
sd(ARIA_Girls$FVC_pre)

ARIA_Boys %>%
  ggplot( aes(x=FVC_pre)) +
  geom_density(fill="skyblue", color="skyblue", alpha=0.8) +
  ggtitle("FVC distribution for Boys") +
  theme_ipsum()

ARIA_Girls %>%
  ggplot( aes(x=FVC_pre)) +
  geom_density(fill="pink", color="pink", alpha=0.8) +
  ggtitle("FVC distribution for Girls") +
  theme_ipsum()

#FEV1_pre:
t.test(ARIA$FEV1_pre ~ ARIA$Gender, var.equal = TRUE)

mean(ARIA_Boys$FEV1_pre)
sd(ARIA_Boys$FEV1_pre)

mean(ARIA_Girls$FEV1_pre)
sd(ARIA_Girls$FEV1_pre)

#FEF2575_pre:
t.test(ARIA$FEF2575_pre ~ ARIA$Gender, var.equal = TRUE)
  #Mean and SD are computed together because no sig. differences were found between Male and Female:
mean(ARIA$FEF2575_pre)
sd(ARIA$FEF2575_pre)


#FEV1FVC_pre:
t.test(ARIA$FEV1FVC_pre ~ ARIA$Gender, var.equal = TRUE)

mean(ARIA_Boys$FEV1FVC_pre)
sd(ARIA_Boys$FEV1FVC_pre)

mean(ARIA_Girls$FEV1FVC_pre)
sd(ARIA_Girls$FEV1FVC_pre)


#Descriptive analysis for Validation Cohort G21:
#Selecting subset with only G21 participants:
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")

# n and % for Gender G21:
table(G21$Gender)
ggplot(data=G21) + geom_bar(mapping = aes(x = Gender, fill = Gender))

#Males:
n=1538
(1538/2986)*100

#Females:
n=1448
(1448/2986)*100

# Age range:
summary(G21$Idade)

# Two-samples T-test gender comparissons in age, height, weight and BMI G21 cohort:
#Age:
t.test(G21$Idade ~ G21$Gender, var.equal = TRUE)

mean(G21$Idade)
sd(G21$Idade)

mean(G21_Boys$Idade)
sd(G21_Boys$Idade)

mean(G21_Girls$Idade)
sd(G21_Girls$Idade)

#Height:
t.test(G21$Altura_cm ~ G21$Gender, var.equal = TRUE)

mean(G21$Altura_cm)
sd(G21$Altura_cm)

mean(G21_Boys$Altura_cm)
sd(G21_Boys$Altura_cm)

mean(G21_Girls$Altura_cm)
sd(G21_Girls$Altura_cm)

#Weight:
t.test(G21$Peso ~ G21$Gender, var.equal = TRUE)

mean(G21_Boys$Peso)
sd(G21_Boys$Peso)

mean(G21_Girls$Peso)
sd(G21_Girls$Peso)

#BMI:
t.test(G21$BMI ~ G21$Gender, var.equal = TRUE)

mean(G21_Boys$BMI)
sd(G21_Boys$BMI)

mean(G21_Girls$BMI)
sd(G21_Girls$BMI)

#Comparisson two-sample t-test spirometric parameters between Male and Female participants in G21:

#FVC_pre:
t.test(G21$FVC_pre ~ G21$Gender, var.equal = TRUE)

mean(G21_Boys$FVC_pre)
sd(G21_Boys$FVC_pre)

mean(G21_Girls$FVC_pre)
sd(G21_Girls$FVC_pre)

#FEV1_pre:
t.test(G21$FEV1_pre ~ G21$Gender, var.equal = TRUE)

mean(G21_Boys$FEV1_pre)
sd(G21_Boys$FEV1_pre)

mean(G21_Girls$FEV1_pre)
sd(G21_Girls$FEV1_pre)

#FEF2575_pre:
t.test(G21$FEF2575_pre ~ G21$Gender, var.equal = TRUE)

mean(G21_Boys$FEF2575_pre)
sd(G21_Boys$FEF2575_pre)

mean(G21_Girls$FEF2575_pre)
sd(G21_Girls$FEF2575_pre)


#FEV1FVC_pre:
t.test(G21$FEV1FVC_pre ~ G21$Gender, var.equal = TRUE)

mean(G21_Boys$FEV1FVC_pre)
sd(G21_Boys$FEV1FVC_pre)

mean(G21_Girls$FEV1FVC_pre)
sd(G21_Girls$FEV1FVC_pre)

#Comparissons between Derivation and Validation cohorts:
#Two-sample t-test comparissons between ARIA and G21 participants:

#Age:
t.test(SEL2020$Idade ~ SEL2020$Project, var.equal = TRUE)

mean(G21$Idade)
sd(G21$Idade)

mean(ARIA$Idade)
sd(ARIA$Idade)

ARIA %>%
  ggplot( aes(x=Idade)) +
  geom_density(fill="skyblue", color="skyblue", alpha=0.8) +
  ggtitle("Age distribution for Boys") +
  theme_ipsum()

G21 %>%
  ggplot( aes(x=Idade)) +
  geom_density(fill="pink", color="pink", alpha=0.8) +
  ggtitle("Age distribution for Girls") +
  theme_ipsum()

#Height:
t.test(SEL2020$Altura_cm ~ SEL2020$Project, var.equal = TRUE)

mean(G21$Altura_cm)
sd(G21$Altura_cm)

mean(ARIA$Altura_cm)
sd(ARIA$Altura_cm)

ARIA %>%
  ggplot( aes(x=Altura_cm)) +
  geom_density(fill="skyblue", color="skyblue", alpha=0.8) +
  ggtitle("Height distribution for Boys") +
  theme_ipsum()

G21 %>%
  ggplot( aes(x=Altura_cm)) +
  geom_density(fill="pink", color="pink", alpha=0.8) +
  ggtitle("Height distribution for Girls") +
  theme_ipsum()

#SEL2020$Project <- as.factor(SEL2020$Project)

ggplot(data=SEL2020, aes(x=FVC_pre, group=Project, fill=Project)) + 
  geom_density(adjust=1.5, alpha=.4) + 
  theme_ipsum() + 
  scale_color_manual(name = "Project", values = c( "0" = "blue", "1" = "orange"), labels = c("0"="ARIA", "1"="G21"))
       
#Weight:
t.test(SEL2020$Peso ~ SEL2020$Project, var.equal = TRUE)

mean(G21$Peso)
sd(G21$Peso)

mean(ARIA$Peso)
sd(ARIA$Peso)

#BMI:
t.test(SEL2020$BMI ~ SEL2020$Project, var.equal = TRUE)

mean(SEL2020$BMI)
sd(SEL2020$BMI)

mean(G21$BMI)
sd(G21$BMI)

mean(ARIA$BMI)
sd(ARIA$BMI)

#Z-score BMI:

SEL2020$BMImean<-mean(SEL2020$BMI)
SEL2020$BMIsd<-sd(SEL2020$BMI)
SEL2020$ZScoreBMI<-(SEL2020$BMI-SEL2020$BMImean)/SEL2020$BMIsd
mean(SEL2020$ZScoreBMI)
sd(SEL2020$ZScoreBMI)

ARIA$ARIABMImean<-mean(ARIA$BMI)
ARIA$ARIABMIsd<-sd(ARIA$BMI)
ARIA$ARIAZScoreBMI<-(ARIA$BMI-ARIA$ARIABMImean)/ARIA$ARIABMIsd
mean(ARIA$ARIAZScoreBMI)
sd(ARIA$ARIAZScoreBMI)

G21$G21BMImean<-mean(G21$BMI)
G21$G21BMIsd<-sd(G21$BMI)
G21$G21ZScoreBMI<-(G21$BMI-G21$G21BMImean)/G21$G21BMIsd
mean(G21$G21ZScoreBMI)
sd(G21$G21ZScoreBMI)

t.test(ARIA$ARIAZScoreBMI, G21$G21ZScoreBMI, var.equal = TRUE)
t.test(SEL2020$BMI ~ SEL2020$Project, var.equal = TRUE)
t.test(SEL2020$ZScoreBMI ~ SEL2020$Project, var.equal = TRUE)

#FVC:
t.test(SEL2020$FVC_pre ~ SEL2020$Project, var.equal = TRUE)

mean(G21$FVC_pre)
sd(G21$FVC_pre)

mean(ARIA$FVC_pre)
sd(ARIA$FVC_pre)

#FEV1:
t.test(SEL2020$FEV1_pre ~ SEL2020$Project, var.equal = TRUE)

mean(G21$FEV1_pre)
sd(G21$FEV1_pre)

mean(ARIA$FEV1_pre)
sd(ARIA$FEV1_pre)

#FEF2575:
t.test(SEL2020$FEF2575_pre ~ SEL2020$Project, var.equal = TRUE)

mean(G21$FEF2575_pre)
sd(G21$FEF2575_pre)

mean(ARIA$FEF2575_pre)
sd(ARIA$FEF2575_pre)

#FEV1/FVC:
t.test(SEL2020$FEV1FVC_pre ~ SEL2020$Project, var.equal = TRUE)

mean(G21$FEV1FVC_pre)
sd(G21$FEV1FVC_pre)

mean(ARIA$FEV1FVC_pre)
sd(ARIA$FEV1FVC_pre)

#Levene's test of variance between ARIA and G21:

#Age:
leveneTest(SEL2020$Idade, SEL2020$Project, center = mean)

#Height:
leveneTest(SEL2020$Altura_cm, SEL2020$Project, center = mean)

#Weight:
leveneTest(SEL2020$Peso, SEL2020$Project, center = mean)

#BMI:
leveneTest(SEL2020$BMI, SEL2020$Project, center = mean)

#FVC:
leveneTest(SEL2020$FVC_pre, SEL2020$Project, center = mean)

#FEV1:
leveneTest(SEL2020$FEV1_pre, SEL2020$Project, center = mean)

#FEF2575:
leveneTest(SEL2020$FEF2575_pre, SEL2020$Project, center = mean)

#FEV1FVC:
leveneTest(SEL2020$FEV1FVC_pre, SEL2020$Project, center = mean)

#Pearson's correlation coefficient ARIA cohort between antropometric and lung function parameters with graphs:

#FVC and Age ARIA:
cor.test(ARIA$FVC_pre, ARIA$Idade, method = "pearson")

ggscatter(ARIA, x = "FVC_pre", y = "Idade", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = "FVC (L)", ylab = "Age (years)", color = "Gender")

ggscatter(ARIA, x = "FVC_pre", y = "Idade", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = "FVC (L)", ylab = "Age (years)")

#FVC and Age ARIA_Boys:
cor.test(ARIA_Boys$FVC_pre, ARIA_Boys$Idade, method = "pearson")

ggscatter(ARIA_Boys, x = "FVC_pre", y = "Idade", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = "FVC (L)", ylab = "Age (years)")

#FVC and Age ARIA_Girls:
cor.test(ARIA_Girls$FVC_pre, ARIA_Girls$Idade, method = "pearson")

ggscatter(ARIA_Girls, x = "FVC_pre", y = "Idade", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = "FVC (L)", ylab = "Age (years)")

#FVC and Height ARIA:
cor.test(ARIA$FVC_pre, ARIA$Altura_cm, method = "pearson")

ggscatter(ARIA, x = "Altura_cm", y = "FVC_pre", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Height (cm)", ylab = "FVC (L)", color = "Gender")

ggscatter(ARIA, x = "Altura_cm", y = "FVC_pre", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Height (cm)", ylab = "FVC (L)", color = "Gender", palette = c("grey70","grey15"))

ggscatter(ARIA, x = "FVC_pre", y = "Altura_cm", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = "FVC (L)", ylab = "Height (cm)")

#FVC and Height ARIA_Boys:
cor.test(ARIA_Boys$FVC_pre, ARIA_Boys$Altura_cm, method = "pearson")

ggscatter(ARIA_Boys, x = "FVC_pre", y = "Altura_cm", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = "FVC (L)", ylab = "Height (cm)")

#FVC and Height ARIA_Girls:
cor.test(ARIA_Girls$FVC_pre, ARIA_Girls$Altura_cm, method = "pearson")

ggscatter(ARIA_Girls, x = "FVC_pre", y = "Altura_cm", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = "FVC (L)", ylab = "Height (cm)")

#FVC and Weight ARIA:
cor.test(ARIA$FVC_pre, ARIA$Peso, method = "pearson")

ggscatter(ARIA, x = "Peso", y = "FVC_pre", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Weight (Kg)", ylab = "FVC (L)", color = "Gender")

ggscatter(ARIA, x = "Peso", y = "FVC_pre", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Weight (Kg)", ylab = "FVC (L)")

#FVC and Weight ARIA_Boys:
cor.test(ARIA_Boys$FVC_pre, ARIA_Boys$Peso, method = "pearson")

ggscatter(ARIA_Boys, x = "FVC_pre", y = "Peso", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = "FVC (L)", ylab = "Weight (kg)")

#FVC and Height ARIA_Girls:
cor.test(ARIA_Girls$FVC_pre, ARIA_Girls$Peso, method = "pearson")

ggscatter(ARIA_Girls, x = "FVC_pre", y = "Peso", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = "FVC (L)", ylab = "Weight (kg)")

#FVC and BMI ARIA:
cor.test(ARIA$FVC_pre, ARIA$BMI, method = "pearson")

ggscatter(ARIA, x = "FVC_pre", y = "BMI", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = "FVC (L)", ylab = "Body Mass Index", color = "Gender")

ggscatter(ARIA, x = "FVC_pre", y = "BMI", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = "FVC (L)", ylab = "Body Mass Index")

#FVC and BMI ARIA_Boys:
cor.test(ARIA_Boys$FVC_pre, ARIA_Boys$BMI, method = "pearson")

ggscatter(ARIA_Boys, x = "FVC_pre", y = "BMI", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = "FVC (L)", ylab = "Body Mass Index")

#FVC and BMI ARIA_Girls:
cor.test(ARIA_Girls$FVC_pre, ARIA_Girls$BMI, method = "pearson")

ggscatter(ARIA_Girls, x = "FVC_pre", y = "BMI", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = "FVC (L)", ylab = "Body Mass Index")

#FEV1 and Age ARIA:
cor.test(ARIA$FEV1_pre, ARIA$Idade, method = "pearson")

ggscatter(ARIA, x = "FEV1_pre", y = "Idade", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = "FEV1 (L)", ylab = "Age (years)", color = "Gender")

ggscatter(ARIA, x = "FEV1_pre", y = "Idade", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = "FEV1 (L)", ylab = "Age (years)")

#FEV1 and Age ARIA_Boys:
cor.test(ARIA_Boys$FEV1_pre, ARIA_Boys$Idade, method = "pearson")

ggscatter(ARIA_Boys, x = "FEV1_pre", y = "Idade", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = "FEV1 (L)", ylab = "Age (years)")

#FEV1 and Age ARIA_Girls:
cor.test(ARIA_Girls$FEV1_pre, ARIA_Girls$Idade, method = "pearson")

ggscatter(ARIA_Girls, x = "FEV1_pre", y = "Idade", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = "FEV1 (L)", ylab = "Age (years)")

#FEV1 and Height ARIA:
cor.test(ARIA$FEV1_pre, ARIA$Altura_cm, method = "pearson")

ggscatter(ARIA, x = "Altura_cm", y = "FEV1_pre", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Height (cm)", ylab = "FEV1 (L)", color = "Gender")

ggscatter(ARIA, x = "Altura_cm", y = "FEV1_pre", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Height (cm)", ylab = "FEV1 (L)", color = "Gender", palette = c("grey70","grey15"))

ggscatter(ARIA, x = "FEV1_pre", y = "Altura_cm", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = "FEV1 (L)", ylab = "Height (cm)")

#FEV1 and Height ARIA_Boys:
cor.test(ARIA_Boys$FEV1_pre, ARIA_Boys$Altura_cm, method = "pearson")

ggscatter(ARIA_Boys, x = "FEV1_pre", y = "Altura_cm", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = "FEV1 (L)", ylab = "Height (cm)")

#FEV1 and Height ARIA_Girls:
cor.test(ARIA_Girls$FEV1_pre, ARIA_Girls$Altura_cm, method = "pearson")

ggscatter(ARIA_Girls, x = "FEV1_pre", y = "Altura_cm", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = "FEV1 (L)", ylab = "Height (cm)")

#FEV1 and Weight ARIA:
cor.test(ARIA$FEV1_pre, ARIA$Peso, method = "pearson")

ggscatter(ARIA, x = "FEV1_pre", y = "Peso", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = "FEV1 (L)", ylab = "Weight (kg)", color = "Gender")

ggscatter(ARIA, x = "FEV1_pre", y = "Peso", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = "FEV1 (L)", ylab = "Weight (kg)", color = "Gender", palette = c("grey70","grey15"))

ggscatter(ARIA, x = "FEV1_pre", y = "Peso", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = "FEV1 (L)", ylab = "Weight (kg)")

#FEV1 and Weight ARIA_Boys:
cor.test(ARIA_Boys$FEV1_pre, ARIA_Boys$Peso, method = "pearson")

ggscatter(ARIA_Boys, x = "FEV1_pre", y = "Peso", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = "FEV1 (L)", ylab = "Weight (kg)")

#FEV1 and weight ARIA_Girls:
cor.test(ARIA_Girls$FEV1_pre, ARIA_Girls$Peso, method = "pearson")

ggscatter(ARIA_Girls, x = "FEV1_pre", y = "Peso", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = "FEV1 (L)", ylab = "Weight (kg)")

#FEV1 and BMI ARIA:
cor.test(ARIA$FEV1_pre, ARIA$BMI, method = "pearson")

ggscatter(ARIA, x = "FEV1_pre", y = "BMI", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = "FEV1 (L)", ylab = "Body Mass Index", color = "Gender")

ggscatter(ARIA, x = "FEV1_pre", y = "BMI", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = "FEV1 (L)", ylab = "Body Mass Index")

#FEV1 and BMI ARIA_Boys:
cor.test(ARIA_Boys$FEV1_pre, ARIA_Boys$BMI, method = "pearson")

ggscatter(ARIA_Boys, x = "FEV1_pre", y = "BMI", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = "FEV1 (L)", ylab = "Body Mass Index")

#FEV1 and BMI ARIA_Girls:
cor.test(ARIA_Girls$FEV1_pre, ARIA_Girls$BMI, method = "pearson")

ggscatter(ARIA_Girls, x = "FEV1_pre", y = "BMI", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = "FEV1 (L)", ylab = "Body Mass Index")

#FEF2575 and Age ARIA:
cor.test(ARIA$FEF2575_pre, ARIA$Idade, method = "pearson")

ggscatter(ARIA, x = "FEF2575_pre", y = "Idade", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = "FEF2575 (L/s)", ylab = "Age (years)", color = "Gender")

ggscatter(ARIA, x = "FEF2575_pre", y = "Idade", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = "FEF2575 (L/s)", ylab = "Age (years)")

#FEF2575 and Age ARIA_Boys:
cor.test(ARIA_Boys$FEF2575_pre, ARIA_Boys$Idade, method = "pearson")

ggscatter(ARIA_Boys, x = "FEF2575_pre", y = "Idade", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = "FEF2575 (L/s)", ylab = "Age (years)")

#FEF2575 and Age ARIA_Girls:
cor.test(ARIA_Girls$FEF2575_pre, ARIA_Girls$Idade, method = "pearson")

ggscatter(ARIA_Girls, x = "FEF2575_pre", y = "Idade", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = "FEF2575 (L/s)", ylab = "Age (years)")

#FEF2575 and Height ARIA:
cor.test(ARIA$FEF2575_pre, ARIA$Altura_cm, method = "pearson")

ggscatter(ARIA, x = "Altura_cm", y = "FEF2575_pre", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Height (cm)", ylab = "FEF2575 (L/s)", color = "Gender")

ggscatter(ARIA, x = "Altura_cm", y = "FEF2575_pre", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Height (cm)", ylab = "FEF2575 (L/s)", color = "Gender", palette = c("grey70","grey15"))

ggscatter(ARIA, x = "FEV1_pre", y = "Altura_cm", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = "FEF2575 (L/s)", ylab = "Height (cm)")

#FEF2575 and Height ARIA_Boys:
cor.test(ARIA_Boys$FEF2575_pre, ARIA_Boys$Altura_cm, method = "pearson")

ggscatter(ARIA_Boys, x = "FEF2575_pre", y = "Altura_cm", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = "FEF2575 (L/s)", ylab = "Height (cm)")

#FEF2575 and Height ARIA_Girls:
cor.test(ARIA_Girls$FEF2575_pre, ARIA_Girls$Altura_cm, method = "pearson")

ggscatter(ARIA_Girls, x = "FEF2575_pre", y = "Altura_cm", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = "FEF2575 (L/s)", ylab = "Height (cm)")

#FEF2575 and Weight ARIA:
cor.test(ARIA$FEF2575_pre, ARIA$Peso, method = "pearson")

ggscatter(ARIA, x = "FEF2575_pre", y = "Peso", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = "FEF2575 (L/s)", ylab = "Weight (kg)", color = "Gender")

ggscatter(ARIA, x = "FEF2575_pre", y = "Peso", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = "FEF2575 (L/s)", ylab = "Weight (kg)")

#FEF2575 and Weight ARIA_Boys:
cor.test(ARIA_Boys$FEF2575_pre, ARIA_Boys$Peso, method = "pearson")

ggscatter(ARIA_Boys, x = "FEF2575_pre", y = "Peso", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = "FEF2575 (L/s)", ylab = "Weight (kg)")

#FEF2575 and Weight ARIA_Girls:
cor.test(ARIA_Girls$FEF2575_pre, ARIA_Girls$Peso, method = "pearson")

ggscatter(ARIA_Girls, x = "FEF2575_pre", y = "Peso", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = "FEF2575 (L/s)", ylab = "Weight (kg)")

#FEF2575 and BMI ARIA:
cor.test(ARIA$FEF2575_pre, ARIA$BMI, method = "pearson")

ggscatter(ARIA, x = "FEF2575_pre", y = "BMI", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = "FEF2575 (L/s)", ylab = "Body Mass Index", color = "Gender")

ggscatter(ARIA, x = "FEF2575_pre", y = "BMI", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = "FEF2575 (L/s)", ylab = "Body Mass Index")

#FEF2575 and BMI ARIA_Boys:
cor.test(ARIA_Boys$FEF2575_pre, ARIA_Boys$BMI, method = "pearson")

ggscatter(ARIA_Boys, x = "FEF2575_pre", y = "BMI", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = "FEF2575 (L/s)", ylab = "Body Mass Index")

#FEF2575 and BMI ARIA_Girls:
cor.test(ARIA_Girls$FEF2575_pre, ARIA_Girls$BMI, method = "pearson")

ggscatter(ARIA_Girls, x = "FEF2575_pre", y = "BMI", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = "FEF2575 (L/s)", ylab = "Body Mass Index")

#FEV1/FVC and Age ARIA:
cor.test(ARIA$FEV1FVC_pre, ARIA$Idade, method = "pearson")

ggscatter(ARIA, x = "FEV1FVC_pre", y = "Idade", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = "FEV1/FVC", ylab = "Age (years)", color = "Gender")

ggscatter(ARIA, x = "FEV1FVC_pre", y = "Idade", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = "FEV1/FVC", ylab = "Age (years)")

#FEV1/FVC and Age ARIA_Boys:
cor.test(ARIA_Boys$FEV1FVC_pre, ARIA_Boys$Idade, method = "pearson")

ggscatter(ARIA_Boys, x = "FEV1FVC_pre", y = "Idade", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = "FEV1/FVC", ylab = "Age (years)")

#FEV1/FVC and Age ARIA_Girls:
cor.test(ARIA_Girls$FEV1FVC_pre, ARIA_Girls$Idade, method = "pearson")

ggscatter(ARIA_Girls, x = "FEV1FVC_pre", y = "Idade", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = "FEV1/FVC", ylab = "Age (years)")

#FEV1/FVC and Height ARIA:
cor.test(ARIA$FEV1FVC_pre, ARIA$Altura_cm, method = "pearson")

ggscatter(ARIA, x = "Altura_cm", y = "FEV1FVC_pre", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Height (cm)", ylab = "FEV1/FVC", color = "Gender")

ggscatter(ARIA, x = "Altura_cm", y = "FEV1FVC_pre", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Height (cm)", ylab = "FEV1/FVC", color = "Gender", palette = c("grey70","grey15"))

ggscatter(ARIA, x = "FEV1FVC_pre", y = "Altura_cm", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = "FEV1/FVC", ylab = "Height (cm)")

#FEV1/FVC and Height ARIA_Boys:
cor.test(ARIA_Boys$FEV1FVC_pre, ARIA_Boys$Altura_cm, method = "pearson")

ggscatter(ARIA_Boys, x = "FEV1FVC_pre", y = "Altura_cm", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = "FEV1FVC", ylab = "Height (cm)")

#FEV1/FVC and Height ARIA_Girls:
cor.test(ARIA_Girls$FEV1FVC_pre, ARIA_Girls$Altura_cm, method = "pearson")

ggscatter(ARIA_Girls, x = "FEV1FVC_pre", y = "Altura_cm", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = "FEV1/FVC", ylab = "Height (cm)")

#FEV1/FVC and Weight ARIA:
cor.test(ARIA$FEV1FVC_pre, ARIA$Peso, method = "pearson")

ggscatter(ARIA, x = "FEV1FVC_pre", y = "Peso", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = "FEV1/FVC", ylab = "Weight (kg)", color = "Gender")

ggscatter(ARIA, x = "FEV1FVC_pre", y = "Peso", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = "FEV1/FVC", ylab = "Weight (kg)")

#FEV1/FVC and Weight ARIA_Boys:
cor.test(ARIA_Boys$FEV1FVC_pre, ARIA_Boys$Peso, method = "pearson")

ggscatter(ARIA_Boys, x = "FEV1FVC_pre", y = "Peso", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = "FEV1/FVC", ylab = "Weight (kg)")

#FEV1/FVC and Weight ARIA_Girls:
cor.test(ARIA_Girls$FEV1FVC_pre, ARIA_Girls$Peso, method = "pearson")

ggscatter(ARIA_Girls, x = "FEV1FVC_pre", y = "Peso", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = "FEV1/FVC", ylab = "Weight (kg)")

#FEV1/FVC and BMI ARIA:
cor.test(ARIA$FEV1FVC_pre, ARIA$BMI, method = "pearson")

ggscatter(ARIA, x = "FEV1FVC_pre", y = "BMI", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = "FEV1/FVC", ylab = "Body Mass Index", color = "Gender")

ggscatter(ARIA, x = "FEV1FVC_pre", y = "BMI", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = "FEV1/FVC", ylab = "Body Mass Index")

#FEV1/FVC and BMI ARIA_Boys:
cor.test(ARIA_Boys$FEV1FVC_pre, ARIA_Boys$BMI, method = "pearson")

ggscatter(ARIA_Boys, x = "FEV1FVC_pre", y = "BMI", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = "FEV1/FVC", ylab = "Body Mass Index")

#FEV1/FVC and BMI ARIA_Girls:
cor.test(ARIA_Girls$FEV1FVC_pre, ARIA_Girls$BMI, method = "pearson")

ggscatter(ARIA_Girls, x = "FEV1FVC_pre", y = "BMI", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = "FEV1/FVC", ylab = "Body Mass Index")

#Spirometry predictive models:

#Multiple Linear Regression FVC Boys: 

#FVC_bmodel 1 Height
FVC_bmodel_1 <- lm(FVC_pre ~ Altura_cm, data = ARIA_Boys)
summary(FVC_bmodel_1)
SEL2020$FVC_bmodel_1 <- (-2.53532 + (0.03378*SEL2020$Altura_cm))
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
#CI for the variables in FVC_bmodel_1:
confint(FVC_bmodel_1)
#Residuals for FVC_bmodel_1:
sigma(FVC_bmodel_1)/mean(ARIA_Boys$FVC_pre)
sigma(FVC_bmodel_1)/mean(G21_Boys$FVC_pre)
#Mean differences:
SEL2020$MD_FVC_bmodel_1 <- (SEL2020$FVC_bmodel_1-SEL2020$FVC_pre)
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
mean(G21_Boys$MD_FVC_bmodel_1)
summary(G21_Boys$MD_FVC_bmodel_1)
max(G21_Boys$MD_FVC_bmodel_1)-min(G21_Boys$MD_FVC_bmodel_1)
summary(G21_Boys$MD_FVC_GLI)
max(G21_Boys$MD_FVC_GLI)-min(G21_Boys$MD_FVC_GLI)
#Model evaluation:
FVC_bmodel_1_RSq_Mean_dif <- (-0.1012548*(1/0.5067))
FVC_bmodel_1_RSq_Mean_dif
AIC(FVC_bmodel_1)
BIC(FVC_bmodel_1)
cor.test(G21_Boys$FVC_pre, G21_Boys$FVC_bmodel_1, method = "pearson")
cor.test(G21_Boys$FVC_pre, G21_Boys$FVC_GLI, method = "pearson")
#Density and bar plots with mean differences compared to GLI:
hrbrthemes::import_roboto_condensed() 
ggplot(G21_Boys, aes(x=x) ) +
  geom_density( aes(x = MD_FVC_bmodel_1, y = ..density..), fill="#69b3a2" ) +
  geom_label( aes(x=0, y=0.25, label="MD_FVC_bmodel_1"), color="#69b3a2") +
  # Bottom
  geom_density( aes(x = MD_FVC_GLI, y = -..density..), fill= "#404080") +
  geom_label( aes(x=0, y=-0.25, label="MD_FVC_GLI"), color="#404080") +
  theme_ipsum() +
  xlab("Differences between predicted and measured FVC")

G21_Boys <- data.frame(
  type = c( rep("G21_Boys$MD_FVC_bmodel_1", 1000), rep("G21_Boys$MD_FVC_GLI", 1000) ),
  value = c( rnorm(1000), rnorm(1000, mean=0) )
)
# Represent it
G21_Boys %>%
  ggplot( aes(x=value, fill=type)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")

#FVC_bmodel_2 Height/Weight:
FVC_bmodel_2 <- lm(FVC_pre ~ Peso, data = ARIA_Boys)
summary(FVC_bmodel_2)
SEL2020$FVC_bmodel_2 <- (1.320088+(0.021958*SEL2020$Peso))
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
#CI for the variables in FVC_bmodel_2:
confint(FVC_bmodel_2)
#Residuals for FVC_bmodel_2:
sigma(FVC_bmodel_2)/mean(ARIA_Boys$FVC_pre)
sigma(FVC_bmodel_2)/mean(G21_Boys$FVC_pre)
#Mean differences:
SEL2020$MD_FVC_bmodel_2 <- (SEL2020$FVC_bmodel_2-SEL2020$FVC_pre)
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
mean(G21_Boys$MD_FVC_bmodel_2)
summary(G21_Boys$MD_FVC_bmodel_2)
max(G21_Boys$MD_FVC_bmodel_2)-min(G21_Boys$MD_FVC_bmodel_2)
summary(G21_Boys$MD_FVC_GLI)
max(G21_Boys$MD_FVC_GLI)-min(G21_Boys$MD_FVC_GLI)
#Model evaluation:
FVC_bmodel_2_RSq_Mean_dif <- (-0.1799345*(1/0.2883))
FVC_bmodel_2_RSq_Mean_dif
AIC(FVC_bmodel_2)
BIC(FVC_bmodel_2)
cor.test(G21_Boys$FVC_pre, G21_Boys$FVC_bmodel_2, method = "pearson")
cor.test(G21_Boys$FVC_pre, G21_Boys$FVC_GLI, method = "pearson")
#Density and bar plots with mean differences compared to GLI:
hrbrthemes::import_roboto_condensed() 
ggplot(G21_Boys, aes(x=x) ) +
  geom_density( aes(x = MD_FVC_bmodel_2, y = ..density..), fill="#69b3a2" ) +
  geom_label( aes(x=0, y=0.25, label="MD_FVC_bmodel_2"), color="#69b3a2") +
  # Bottom
  geom_density( aes(x = MD_FVC_GLI, y = -..density..), fill= "#404080") +
  geom_label( aes(x=0, y=-0.25, label="MD_FVC_GLI"), color="#404080") +
  theme_ipsum() +
  xlab("Differences between predicted and measured FVC")

G21_Boys <- data.frame(
  type = c( rep("G21_Boys$MD_FVC_bmodel_2", 1000), rep("G21_Boys$MD_FVC_GLI", 1000) ),
  value = c( rnorm(1000), rnorm(1000, mean=0) )
)
# Represent it
G21_Boys %>%
  ggplot( aes(x=value, fill=type)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")

#FVC_bmodel_3 Age:
FVC_bmodel_3 <- lm(FVC_pre ~ Idade, data = ARIA_Boys)
summary(FVC_bmodel_3)
SEL2020$FVC_bmodel_3 <- (0.79346+(0.14387*SEL2020$Idade))
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
#CI for the variables in FVC_bmodel_3:
confint(FVC_bmodel_3)
#Residuals for FVC_bmodel_3:
sigma(FVC_bmodel_3)/mean(ARIA_Boys$FVC_pre)
sigma(FVC_bmodel_3)/mean(G21_Boys$FVC_pre)
#Mean differences:
SEL2020$MD_FVC_bmodel_3 <- (SEL2020$FVC_bmodel_3-SEL2020$FVC_pre)
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
mean(G21_Boys$MD_FVC_bmodel_3)
summary(G21_Boys$MD_FVC_bmodel_3)
max(G21_Boys$MD_FVC_bmodel_3)-min(G21_Boys$MD_FVC_bmodel_3)
summary(G21_Boys$MD_FVC_GLI)
max(G21_Boys$MD_FVC_GLI)-min(G21_Boys$MD_FVC_GLI)
#Model evaluation:
FVC_bmodel_3_RSq_Mean_dif <- (-0.08337964*(1/0.1042))
FVC_bmodel_3_RSq_Mean_dif
AIC(FVC_bmodel_3)
BIC(FVC_bmodel_3)
cor.test(G21_Boys$FVC_pre, G21_Boys$FVC_bmodel_3, method = "pearson")
cor.test(G21_Boys$FVC_pre, G21_Boys$FVC_GLI, method = "pearson")
#Density and bar plots with mean differences compared to GLI:
hrbrthemes::import_roboto_condensed() 
ggplot(G21_Boys, aes(x=x) ) +
  geom_density( aes(x = MD_FVC_bmodel_3, y = ..density..), fill="#69b3a2" ) +
  geom_label( aes(x=0, y=0.25, label="MD_FVC_bmodel_3"), color="#69b3a2") +
  # Bottom
  geom_density( aes(x = MD_FVC_GLI, y = -..density..), fill= "#404080") +
  geom_label( aes(x=0, y=-0.25, label="MD_FVC_GLI"), color="#404080") +
  theme_ipsum() +
  xlab("Differences between predicted and measured FVC")

G21_Boys <- data.frame(
  type = c( rep("G21_Boys$MD_FVC_bmodel_3", 1000), rep("G21_Boys$MD_FVC_GLI", 1000) ),
  value = c( rnorm(1000), rnorm(1000, mean=0) )
)
# Represent it
G21_Boys %>%
  ggplot( aes(x=value, fill=type)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")

#FVC_bmodel_4 BMI:
FVC_bmodel_4 <- lm(FVC_pre ~ BMI, data = ARIA_Boys)
summary(FVC_bmodel_4)
SEL2020$FVC_bmodel_4 <- (1.478416+(0.032153*SEL2020$BMI))
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
#CI for the variables in FVC_bmodel_4:
confint(FVC_bmodel_4)
#Residuals for FVC_bmodel_4:
sigma(FVC_bmodel_4)/mean(ARIA_Boys$FVC_pre)
sigma(FVC_bmodel_4)/mean(G21_Boys$FVC_pre)
#Mean differences:
SEL2020$MD_FVC_bmodel_4 <- (SEL2020$FVC_bmodel_4-SEL2020$FVC_pre)
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
mean(G21_Boys$MD_FVC_bmodel_4)
summary(G21_Boys$MD_FVC_bmodel_4)
max(G21_Boys$MD_FVC_bmodel_4)-min(G21_Boys$MD_FVC_bmodel_4)
summary(G21_Boys$MD_FVC_GLI)
max(G21_Boys$MD_FVC_GLI)-min(G21_Boys$MD_FVC_GLI)
#Model evaluation:
FVC_bmodel_4_RSq_Mean_dif <- (-0.2358327*(1/0.09288))
FVC_bmodel_4_RSq_Mean_dif
AIC(FVC_bmodel_4)
BIC(FVC_bmodel_4)
cor.test(G21_Boys$FVC_pre, G21_Boys$FVC_bmodel_4, method = "pearson")
cor.test(G21_Boys$FVC_pre, G21_Boys$FVC_GLI, method = "pearson")
#Density and bar plots with mean differences compared to GLI:
hrbrthemes::import_roboto_condensed() 
ggplot(G21_Boys, aes(x=x) ) +
  geom_density( aes(x = MD_FVC_bmodel_4, y = ..density..), fill="#69b3a2" ) +
  geom_label( aes(x=0, y=0.25, label="MD_FVC_bmodel_4"), color="#69b3a2") +
  # Bottom
  geom_density( aes(x = MD_FVC_GLI, y = -..density..), fill= "#404080") +
  geom_label( aes(x=0, y=-0.25, label="MD_FVC_GLI"), color="#404080") +
  theme_ipsum() +
  xlab("Differences between predicted and measured FVC")

G21_Boys <- data.frame(
  type = c( rep("G21_Boys$MD_FVC_bmodel_4", 1000), rep("G21_Boys$MD_FVC_GLI", 1000) ),
  value = c( rnorm(1000), rnorm(1000, mean=0) )
)
# Represent it
G21_Boys %>%
  ggplot( aes(x=value, fill=type)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")

#FVC_bmodel_5 Height/Weight:
FVC_bmodel_5 <- lm(FVC_pre ~ Altura_cm + Peso, data = ARIA_Boys)
summary(FVC_bmodel_5)
SEL2020$FVC_bmodel_5 <- (-2.290266+(0.031193*SEL2020$Altura_cm)+(0.003167*SEL2020$Peso))
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
#CI for the variables in FVC_bmodel_5:
confint(FVC_bmodel_5)
#Residuals for FVC_bmodel_5:
sigma(FVC_bmodel_5)/mean(ARIA_Boys$FVC_pre)
sigma(FVC_bmodel_5)/mean(G21_Boys$FVC_pre)
#Mean differences:
SEL2020$MD_FVC_bmodel_5 <- (SEL2020$FVC_bmodel_5-SEL2020$FVC_pre)
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
mean(G21_Boys$MD_FVC_bmodel_5)
summary(G21_Boys$MD_FVC_bmodel_5)
max(G21_Boys$MD_FVC_bmodel_5)-min(G21_Boys$MD_FVC_bmodel_5)
summary(G21_Boys$MD_FVC_GLI)
max(G21_Boys$MD_FVC_GLI)-min(G21_Boys$MD_FVC_GLI)
#Model evaluation:
FVC_bmodel_5_RSq_Mean_dif <- (-0.1025177*(1/0.5079))
FVC_bmodel_5_RSq_Mean_dif
AIC(FVC_bmodel_5)
BIC(FVC_bmodel_5)
cor.test(G21_Boys$FVC_pre, G21_Boys$FVC_bmodel_5, method = "pearson")
cor.test(G21_Boys$FVC_pre, G21_Boys$FVC_GLI, method = "pearson")
#Density and bar plots with mean differences compared to GLI:
hrbrthemes::import_roboto_condensed() 
ggplot(G21_Boys, aes(x=x) ) +
  geom_density( aes(x = MD_FVC_bmodel_5, y = ..density..), fill="#69b3a2" ) +
  geom_label( aes(x=0, y=0.25, label="MD_FVC_bmodel_5"), color="#69b3a2") +
  # Bottom
  geom_density( aes(x = MD_FVC_GLI, y = -..density..), fill= "#404080") +
  geom_label( aes(x=0, y=-0.25, label="MD_FVC_GLI"), color="#404080") +
  theme_ipsum() +
  xlab("Differences between predicted and measured FVC")

G21_Boys <- data.frame(
  type = c( rep("G21_Boys$MD_FVC_bmodel_5", 1000), rep("G21_Boys$MD_FVC_GLI", 1000) ),
  value = c( rnorm(1000), rnorm(1000, mean=0) )
)
# Represent it
G21_Boys %>%
  ggplot( aes(x=value, fill=type)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")


#FVC_bmodel_6 Height/Age:   
FVC_bmodel_6 <- lm(FVC_pre ~ Altura_cm + Idade, data = ARIA_Boys)
summary(FVC_bmodel_6)
SEL2020$FVC_bmodel_6 <- (-2.56547+(0.03322*SEL2020$Altura_cm)+(0.01212*SEL2020$Idade))
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
#CI for the variables in FVC_bmodel_6:
confint(FVC_bmodel_6)
#Residuals for FVC_bmodel_6:
sigma(FVC_bmodel_6)/mean(ARIA_Boys$FVC_pre)
sigma(FVC_bmodel_6)/mean(G21_Boys$FVC_pre)
#Mean differences:
SEL2020$MD_FVC_bmodel_6 <- (SEL2020$FVC_bmodel_6-SEL2020$FVC_pre)
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
mean(G21_Boys$MD_FVC_bmodel_6)
summary(G21_Boys$MD_FVC_bmodel_6)
max(G21_Boys$MD_FVC_bmodel_6)-min(G21_Boys$MD_FVC_bmodel_6)
summary(G21_Boys$MD_FVC_GLI)
max(G21_Boys$MD_FVC_GLI)-min(G21_Boys$MD_FVC_GLI)
#Model evaluation:
FVC_bmodel_6_RSq_Mean_dif <- (-0.0891393*(1/0.5054))
FVC_bmodel_6_RSq_Mean_dif
AIC(FVC_bmodel_6)
BIC(FVC_bmodel_6)
cor.test(G21_Boys$FVC_pre, G21_Boys$FVC_bmodel_6, method = "pearson")
cor.test(G21_Boys$FVC_pre, G21_Boys$FVC_GLI, method = "pearson")
#Density and bar plots with mean differences compared to GLI:
hrbrthemes::import_roboto_condensed() 
ggplot(G21_Boys, aes(x=x) ) +
  geom_density( aes(x = MD_FVC_bmodel_6, y = ..density..), fill="#69b3a2" ) +
  geom_label( aes(x=0, y=0.25, label="MD_FVC_bmodel_6"), color="#69b3a2") +
  # Bottom
  geom_density( aes(x = MD_FVC_GLI, y = -..density..), fill= "#404080") +
  geom_label( aes(x=0, y=-0.25, label="MD_FVC_GLI"), color="#404080") +
  theme_ipsum() +
  xlab("Differences between predicted and measured FVC")

G21_Boys <- data.frame(
  type = c( rep("G21_Boys$MD_FVC_bmodel_6", 1000), rep("G21_Boys$MD_FFVC_GLI", 1000) ),
  value = c( rnorm(1000), rnorm(1000, mean=0) )
)
# Represent it
G21_Boys %>%
  ggplot( aes(x=value, fill=type)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")

#FVC_bmodel_7 Height/BMI:   
FVC_bmodel_7 <- lm(FVC_pre ~ Altura_cm + BMI, data = ARIA_Boys)
summary(FVC_bmodel_7)
SEL2020$FVC_bmodel_7 <- (-2.507115+(0.032790*SEL2020$Altura_cm)+(0.005896*SEL2020$BMI))
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
#CI for the variables in FVC_bmodel_7:
confint(FVC_bmodel_7)
#Residuals for FVC_bmodel_7:
sigma(FVC_bmodel_7)/mean(ARIA_Boys$FVC_pre)
sigma(FVC_bmodel_7)/mean(G21_Boys$FVC_pre)
#Mean differences:
SEL2020$MD_FVC_bmodel_7 <- (SEL2020$FVC_bmodel_7-SEL2020$FVC_pre)
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
mean(G21_Boys$MD_FVC_bmodel_7)
summary(G21_Boys$MD_FVC_bmodel_7)
max(G21_Boys$MD_FVC_bmodel_7)-min(G21_Boys$MD_FVC_bmodel_7)
summary(G21_Boys$MD_FVC_GLI)
max(G21_Boys$MD_FVC_GLI)-min(G21_Boys$MD_FVC_GLI)
#Model evaluation:
FVC_bmodel_7_RSq_Mean_dif <- (-0.1024339*(1/0.5076))
FVC_bmodel_7_RSq_Mean_dif
AIC(FVC_bmodel_7)
BIC(FVC_bmodel_7)
cor.test(G21_Boys$FVC_pre, G21_Boys$FVC_bmodel_7, method = "pearson")
cor.test(G21_Boys$FVC_pre, G21_Boys$FVC_GLI, method = "pearson")
#Density and bar plots with mean differences compared to GLI:
hrbrthemes::import_roboto_condensed() 
ggplot(G21_Boys, aes(x=x) ) +
  geom_density( aes(x = MD_FVC_bmodel_7, y = ..density..), fill="#69b3a2" ) +
  geom_label( aes(x=0, y=0.25, label="MD_FVC_bmodel_7"), color="#69b3a2") +
  # Bottom
  geom_density( aes(x = MD_FVC_GLI, y = -..density..), fill= "#404080") +
  geom_label( aes(x=0, y=-0.25, label="MD_FVC_GLI"), color="#404080") +
  theme_ipsum() +
  xlab("Differences between predicted and measured FVC")

G21_Boys <- data.frame(
  type = c( rep("G21_Boys$MD_FVC_bmodel_7", 1000), rep("G21_Boys$MD_FVC_GLI", 1000) ),
  value = c( rnorm(1000), rnorm(1000, mean=0) )
)
# Represent it
G21_Boys %>%
  ggplot( aes(x=value, fill=type)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")

#FVC_bmodel_8 Height/Weight/Age:   
FVC_bmodel_8 <- lm(FVC_pre ~ Altura_cm + Peso + Idade, data = ARIA_Boys)
summary(FVC_bmodel_8)
SEL2020$FVC_bmodel_8 <- (-2.322603+(0.030837*SEL2020$Altura_cm)+(0.003058*SEL2020$Peso)+(0.009612*SEL2020$Idade))
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
#CI for the variables in FVC_bmodel_8:
confint(FVC_bmodel_8)
#Residuals for FVC_bmodel_8:
sigma(FVC_bmodel_8)/mean(ARIA_Boys$FVC_pre)
sigma(FVC_bmodel_8)/mean(G21_Boys$FVC_pre)
#Mean differences:
SEL2020$MD_FVC_bmodel_8 <- (SEL2020$FVC_bmodel_8-SEL2020$FVC_pre)
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
mean(G21_Boys$MD_FVC_bmodel_8)
summary(G21_Boys$MD_FVC_bmodel_8)
max(G21_Boys$MD_FVC_bmodel_8)-min(G21_Boys$MD_FVC_bmodel_8)
summary(G21_Boys$MD_FVC_GLI)
max(G21_Boys$MD_FVC_GLI)-min(G21_Boys$MD_FVC_GLI)
#Model evaluation:
FVC_bmodel_8_RSq_Mean_dif <- (-0.09298677*(1/0.5064))
FVC_bmodel_8_RSq_Mean_dif
AIC(FVC_bmodel_8)
BIC(FVC_bmodel_8)
cor.test(G21_Boys$FVC_pre, G21_Boys$FVC_bmodel_8, method = "pearson")
cor.test(G21_Boys$FVC_pre, G21_Boys$FVC_GLI, method = "pearson")
#Density and bar plots with mean differences compared to GLI:
hrbrthemes::import_roboto_condensed() 
ggplot(G21_Boys, aes(x=x) ) +
  geom_density( aes(x = MD_FVC_bmodel_8, y = ..density..), fill="#69b3a2" ) +
  geom_label( aes(x=0, y=0.25, label="MD_FVC_bmodel_8"), color="#69b3a2") +
  # Bottom
  geom_density( aes(x = MD_FVC_GLI, y = -..density..), fill= "#404080") +
  geom_label( aes(x=0, y=-0.25, label="MD_FVC_GLI"), color="#404080") +
  theme_ipsum() +
  xlab("Differences between predicted and measured FVC")

G21_Boys <- data.frame(
  type = c( rep("G21_Boys$MD_FVC_bmodel_8", 1000), rep("G21_Boys$MD_FVC_GLI", 1000) ),
  value = c( rnorm(1000), rnorm(1000, mean=0) )
)
# Represent it
G21_Boys %>%
  ggplot( aes(x=value, fill=type)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")

#FVC_bmodel_9 Height/Weight/BMI:   
FVC_bmodel_9 <- lm(FVC_pre ~ Altura_cm + Peso + BMI, data = ARIA_Boys)
summary(FVC_bmodel_9)
SEL2020$FVC_bmodel_9 <- (-1.64415+(0.02656*SEL2020$Altura_cm)+(0.01267*SEL2020$Peso)+(-0.01866*SEL2020$BMI))
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
#CI for the variables in FVC_bmodel_9:
confint(FVC_bmodel_9)
#Residuals for FVC_bmodel_9:
sigma(FVC_bmodel_9)/mean(ARIA_Boys$FVC_pre)
sigma(FVC_bmodel_9)/mean(G21_Boys$FVC_pre)
#Mean differences:
SEL2020$MD_FVC_bmodel_9 <- (SEL2020$FVC_bmodel_9-SEL2020$FVC_pre)
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
mean(G21_Boys$MD_FVC_bmodel_9)
summary(G21_Boys$MD_FVC_bmodel_9)
max(G21_Boys$MD_FVC_bmodel_9)-min(G21_Boys$MD_FVC_bmodel_9)
summary(G21_Boys$MD_FVC_GLI)
max(G21_Boys$MD_FVC_GLI)-min(G21_Boys$MD_FVC_GLI)
#Model evaluation:
SEL2020$FVC_bmodel_9_RSq_Mean_dif <- (-0.1030138*(1/0.5064))
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
mean(G21_Boys$FVC_bmodel_9_RSq_Mean_dif)
AIC(FVC_bmodel_9)
BIC(FVC_bmodel_9)
cor.test(G21_Boys$FVC_pre, G21_Boys$FVC_bmodel_9, method = "pearson")
cor.test(G21_Boys$FVC_pre, G21_Boys$FVC_GLI, method = "pearson")
#Density and bar plots with mean differences compared to GLI:
hrbrthemes::import_roboto_condensed() 
ggplot(G21_Boys, aes(x=x) ) +
  geom_density( aes(x = MD_FVC_bmodel_9, y = ..density..), fill="#69b3a2" ) +
  geom_label( aes(x=1, y=0.25, label="MD_FVC_bmodel_9"), color="#69b3a2") +
  # Bottom
  geom_density( aes(x = MD_FVC_GLI, y = -..density..), fill= "#404080") +
  geom_label( aes(x=1, y=-0.25, label="MD_FVC_GLI"), color="#404080") +
  theme_ipsum() +
  xlab("Differences between predicted and measured FVC")

G21_Boys <- data.frame(
  type = c( rep("G21_Boys$MD_FVC_bmodel_9", 1000), rep("G21_Boys$MD_FVC_GLI", 1000) ),
  value = c( rnorm(1000), rnorm(1000, mean=0) )
)
# Represent it
G21_Boys %>%
  ggplot( aes(x=value, fill=type)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")


#FVC_bmodel_10 Height/Weight/BMI/Age:   
FVC_bmodel_10 <- lm(FVC_pre ~ Altura_cm + Peso + BMI + Idade, data = ARIA_Boys)
summary(FVC_bmodel_10)
SEL2020$FVC_bmodel_10 <- (-1.659676+(0.026071*SEL2020$Altura_cm)+(0.012820*SEL2020$Peso)+(-0.019174*SEL2020$BMI)+(0.009859*SEL2020$Idade))
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
#CI for the variables in FVC_bmodel_10:
confint(FVC_bmodel_10)
#Residuals for FVC_bmodel_10:
sigma(FVC_bmodel_10)/mean(ARIA_Boys$FVC_pre)
sigma(FVC_bmodel_10)/mean(G21_Boys$FVC_pre)
#Mean differences:
SEL2020$MD_FVC_bmodel_10 <- (SEL2020$FVC_bmodel_10-SEL2020$FVC_pre)
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
mean(G21_Boys$MD_FVC_bmodel_10)
summary(G21_Boys$MD_FVC_bmodel_10)
max(G21_Boys$MD_FVC_bmodel_10)-min(G21_Boys$MD_FVC_bmodel_10)
summary(G21_Boys$MD_FVC_GLI)
max(G21_Boys$MD_FVC_GLI)-min(G21_Boys$MD_FVC_GLI)
#Model evaluation:
FVC_bmodel_10_RSq_Mean_dif <- (-0.09287665*(1/0.5049))
FVC_bmodel_10_RSq_Mean_dif
AIC(FVC_bmodel_10)
BIC(FVC_bmodel_10)
cor.test(G21_Boys$FVC_pre, G21_Boys$FVC_bmodel_10, method = "pearson")
cor.test(G21_Boys$FVC_pre, G21_Boys$FVC_GLI, method = "pearson")
#Density and bar plots with mean differences compared to GLI:
hrbrthemes::import_roboto_condensed() 
ggplot(G21_Boys, aes(x=x) ) +
  geom_density( aes(x = MD_FVC_bmodel_10, y = ..density..), fill="#69b3a2" ) +
  geom_label( aes(x=0, y=0.25, label="MD_FVC_bmodel_10"), color="#69b3a2") +
  # Bottom
  geom_density( aes(x = MD_FVC_GLI, y = -..density..), fill= "#404080") +
  geom_label( aes(x=0, y=-0.25, label="MD_FVC_GLI"), color="#404080") +
  theme_ipsum() +
  xlab("Differences between predicted and measured FVC")

G21_Boys <- data.frame(
  type = c( rep("G21_Boys$MD_FVC_bmodel_10", 1000), rep("G21_Boys$MD_FVC_GLI", 1000) ),
  value = c( rnorm(1000), rnorm(1000, mean=0) )
)
# Represent it
G21_Boys %>%
  ggplot( aes(x=value, fill=type)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")


#FVC_bmodel_11 Weight/Age:   
FVC_bmodel_11 <- lm(FVC_pre ~ Peso + Idade, data = ARIA_Boys)
summary(FVC_bmodel_11)
SEL2020$FVC_bmodel_11 <- (0.808326+(0.019707*SEL2020$Peso)+(0.066912*SEL2020$Idade))
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
#CI for the variables in FVC_bmodel_11:
confint(FVC_bmodel_11)
#Residuals for FVC_bmodel_11:
sigma(FVC_bmodel_11)/mean(ARIA_Boys$FVC_pre)
sigma(FVC_bmodel_11)/mean(G21_Boys$FVC_pre)
#Mean differences:
SEL2020$MD_FVC_bmodel_11 <- (SEL2020$FVC_bmodel_11-SEL2020$FVC_pre)
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
mean(G21_Boys$MD_FVC_bmodel_11)
summary(G21_Boys$MD_FVC_bmodel_11)
max(G21_Boys$MD_FVC_bmodel_11)-min(G21_Boys$MD_FVC_bmodel_11)
summary(G21_Boys$MD_FVC_GLI)
max(G21_Boys$MD_FVC_GLI)-min(G21_Boys$MD_FVC_GLI)
#Model evaluation:
FVC_bmodel_11_RSq_Mean_dif <- (-0.1072297*(1/0.306))
FVC_bmodel_11_RSq_Mean_dif
AIC(FVC_bmodel_11)
BIC(FVC_bmodel_11)
cor.test(G21_Boys$FVC_pre, G21_Boys$FVC_bmodel_11, method = "pearson")
cor.test(G21_Boys$FVC_pre, G21_Boys$FVC_GLI, method = "pearson")
#Density and bar plots with mean differences compared to GLI:
hrbrthemes::import_roboto_condensed() 
ggplot(G21_Boys, aes(x=x) ) +
  geom_density( aes(x = MD_FVC_bmodel_11, y = ..density..), fill="#69b3a2" ) +
  geom_label( aes(x=0, y=0.25, label="MD_FVC_bmodel_11"), color="#69b3a2") +
  # Bottom
  geom_density( aes(x = MD_FVC_GLI, y = -..density..), fill= "#404080") +
  geom_label( aes(x=0, y=-0.25, label="MD_FVC_GLI"), color="#404080") +
  theme_ipsum() +
  xlab("Differences between predicted and measured FVC")

G21_Boys <- data.frame(
  type = c( rep("G21_Boys$MD_FVC_bmodel_11", 1000), rep("G21_Boys$MD_FVC_GLI", 1000) ),
  value = c( rnorm(1000), rnorm(1000, mean=0) )
)
# Represent it
G21_Boys %>%
  ggplot( aes(x=value, fill=type)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")

#FVC_bmodel_12 Weight/BMI:   
FVC_bmodel_12 <- lm(FVC_pre ~ Peso + BMI, data = ARIA_Boys)
summary(FVC_bmodel_12)
SEL2020$FVC_bmodel_12 <- (2.01820+(0.06451*SEL2020$Peso)+(-0.11825*SEL2020$BMI))
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
#CI for the variables in FVC_bmodel_12:
confint(FVC_bmodel_12)
#Residuals for FVC_bmodel_12:
sigma(FVC_bmodel_12)/mean(ARIA_Boys$FVC_pre)
sigma(FVC_bmodel_12)/mean(G21_Boys$FVC_pre)
#Mean differences:
SEL2020$MD_FVC_bmodel_12 <- (SEL2020$FVC_bmodel_12-SEL2020$FVC_pre)
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
mean(G21_Boys$MD_FVC_bmodel_12)
summary(G21_Boys$MD_FVC_bmodel_12)
max(G21_Boys$MD_FVC_bmodel_12)-min(G21_Boys$MD_FVC_bmodel_12)
summary(G21_Boys$MD_FVC_GLI)
max(G21_Boys$MD_FVC_GLI)-min(G21_Boys$MD_FVC_GLI)
#Model evaluation:
FVC_bmodel_12_RSq_Mean_dif <- (-0.1077863*(1/0.497))
FVC_bmodel_12_RSq_Mean_dif
AIC(FVC_bmodel_12)
BIC(FVC_bmodel_12)
cor.test(G21_Boys$FVC_pre, G21_Boys$FVC_bmodel_12, method = "pearson")
cor.test(G21_Boys$FVC_pre, G21_Boys$FVC_GLI, method = "pearson")
#Density and bar plots with mean differences compared to GLI:
hrbrthemes::import_roboto_condensed() 
ggplot(G21_Boys, aes(x=x) ) +
  geom_density( aes(x = MD_FVC_bmodel_12, y = ..density..), fill="#69b3a2" ) +
  geom_label( aes(x=0, y=0.25, label="MD_FVC_bmodel_12"), color="#69b3a2") +
  # Bottom
  geom_density( aes(x = MD_FVC_GLI, y = -..density..), fill= "#404080") +
  geom_label( aes(x=0, y=-0.25, label="MD_FVC_GLI"), color="#404080") +
  theme_ipsum() +
  xlab("Differences between predicted and measured FVC")

G21_Boys <- data.frame(
  type = c( rep("G21_Boys$MD_FVC_bmodel_12", 1000), rep("G21_Boys$MD_FVC_GLI", 1000) ),
  value = c( rnorm(1000), rnorm(1000, mean=0) )
)
# Represent it
G21_Boys %>%
  ggplot( aes(x=value, fill=type)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")

#FVC_bmodel_13 Weight/BMI/Age:   
FVC_bmodel_13 <- lm(FVC_pre ~ Peso + BMI + Idade, data = ARIA_Boys)
summary(FVC_bmodel_13)
SEL2020$FVC_bmodel_13 <- (1.893964+(0.063296*SEL2020$Peso)+(-0.116265*SEL2020$BMI)+(0.014709*SEL2020$Idade))
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
#CI for the variables in FVC_bmodel_13:
confint(FVC_bmodel_13)
#Residuals for FVC_bmodel_13:
sigma(FVC_bmodel_13)/mean(ARIA_Boys$FVC_pre)
sigma(FVC_bmodel_13)/mean(G21_Boys$FVC_pre)
#Mean differences:
SEL2020$MD_FVC_bmodel_13 <- (SEL2020$FVC_bmodel_13-SEL2020$FVC_pre)
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
mean(G21_Boys$MD_FVC_bmodel_13)
summary(G21_Boys$MD_FVC_bmodel_13)
max(G21_Boys$MD_FVC_bmodel_13)-min(G21_Boys$MD_FVC_bmodel_13)
summary(G21_Boys$MD_FVC_GLI)
max(G21_Boys$MD_FVC_GLI)-min(G21_Boys$MD_FVC_GLI)
#Model evaluation:
FVC_bmodel_13_RSq_Mean_dif <- (-0.0932139*(1/0.496))
FVC_bmodel_13_RSq_Mean_dif
AIC(FVC_bmodel_13)
BIC(FVC_bmodel_13)
cor.test(G21_Boys$FVC_pre, G21_Boys$FVC_bmodel_13, method = "pearson")
cor.test(G21_Boys$FVC_pre, G21_Boys$FVC_GLI, method = "pearson")
#Density and bar plots with mean differences compared to GLI:
hrbrthemes::import_roboto_condensed() 
ggplot(G21_Boys, aes(x=x) ) +
  geom_density( aes(x = MD_FVC_bmodel_13, y = ..density..), fill="#69b3a2" ) +
  geom_label( aes(x=0, y=0.25, label="MD_FVC_bmodel_13"), color="#69b3a2") +
  # Bottom
  geom_density( aes(x = MD_FVC_GLI, y = -..density..), fill= "#404080") +
  geom_label( aes(x=0, y=-0.25, label="MD_FVC_GLI"), color="#404080") +
  theme_ipsum() +
  xlab("Differences between predicted and measured FVC")

G21_Boys <- data.frame(
  type = c( rep("G21_Boys$MD_FVC_bmodel_13", 1000), rep("G21_Boys$MD_FVC_GLI", 1000) ),
  value = c( rnorm(1000), rnorm(1000, mean=0) )
)
# Represent it
G21_Boys %>%
  ggplot( aes(x=value, fill=type)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")

#FVC_bmodel_14 BMI/Age:   
FVC_bmodel_14 <- lm(FVC_pre ~ BMI + Idade, data = ARIA_Boys)
summary(FVC_bmodel_14)
SEL2020$FVC_bmodel_14 <- (0.562853+(0.025468*SEL2020$BMI)+(0.118017*SEL2020$Idade))
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
#CI for the variables in FVC_bmodel_14:
confint(FVC_bmodel_14)
#Residuals for FVC_bmodel_14:
sigma(FVC_bmodel_14)/mean(ARIA_Boys$FVC_pre)
sigma(FVC_bmodel_14)/mean(G21_Boys$FVC_pre)
#Mean differences:
SEL2020$MD_FVC_bmodel_14 <- (SEL2020$FVC_bmodel_14-SEL2020$FVC_pre)
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
mean(G21_Boys$MD_FVC_bmodel_14)
summary(G21_Boys$MD_FVC_bmodel_14)
max(G21_Boys$MD_FVC_bmodel_14)-min(G21_Boys$MD_FVC_bmodel_14)
summary(G21_Boys$MD_FVC_GLI)
max(G21_Boys$MD_FVC_GLI)-min(G21_Boys$MD_FVC_GLI)
#Model evaluation:
FVC_bmodel_14_RSq_Mean_dif <- (-0.09801727*(1/0.1582))
FVC_bmodel_14_RSq_Mean_dif
AIC(FVC_bmodel_14)
BIC(FVC_bmodel_14)
cor.test(G21_Boys$FVC_pre, G21_Boys$FVC_bmodel_14, method = "pearson")
cor.test(G21_Boys$FVC_pre, G21_Boys$FVC_GLI, method = "pearson")
#Density and bar plots with mean differences compared to GLI:
hrbrthemes::import_roboto_condensed() 
ggplot(G21_Boys, aes(x=x) ) +
  geom_density( aes(x = MD_FVC_bmodel_14, y = ..density..), fill="#69b3a2" ) +
  geom_label( aes(x=0, y=0.25, label="MD_FVC_bmodel_14"), color="#69b3a2") +
  # Bottom
  geom_density( aes(x = MD_FVC_GLI, y = -..density..), fill= "#404080") +
  geom_label( aes(x=0, y=-0.25, label="MD_FVC_GLI"), color="#404080") +
  theme_ipsum() +
  xlab("Differences between predicted and measured FVC")

G21_Boys <- data.frame(
  type = c( rep("G21_Boys$MD_FVC_bmodel_14", 1000), rep("G21_Boys$MD_FVC_GLI", 1000) ),
  value = c( rnorm(1000), rnorm(1000, mean=0) )
)
# Represent it
G21_Boys %>%
  ggplot( aes(x=value, fill=type)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")

#Multiple Linear Regression FVC Girls: 

#FVC_gmodel 1 Height
FVC_gmodel_1 <- lm(FVC_pre ~ Altura_cm, data = ARIA_Girls)
summary(FVC_gmodel_1)
SEL2020$FVC_gmodel_1 <- (-1.795825+(0.027785*SEL2020$Altura_cm))
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
#CI for the variables in FVC_gmodel_1:
confint(FVC_gmodel_1)
#Residuals for FVC_gmodel_1:
sigma(FVC_gmodel_1)/mean(ARIA_Girls$FEV1_pre)
sigma(FVC_gmodel_1)/mean(G21_Girls$FEV1_pre)
#Mean differences:
SEL2020$MD_FVC_gmodel_1 <- (SEL2020$FVC_gmodel_1-SEL2020$FVC_pre)
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
mean(G21_Girls$MD_FVC_gmodel_1)
summary(G21_Girls$MD_FVC_gmodel_1)
max(G21_Girls$MD_FVC_gmodel_1)-min(G21_Girls$MD_FVC_gmodel_1)
summary(G21_Girls$MD_FVC_GLI)
max(G21_Girls$MD_FVC_GLI)-min(G21_Girls$MD_FVC_GLI)
#Model evaluation:
FVC_gmodel_1_RSq_Mean_dif <- (-0.07266653*(1/0.4836))
FVC_gmodel_1_RSq_Mean_dif
AIC(FVC_gmodel_1)
BIC(FVC_gmodel_1)
cor.test(G21_Girls$FVC_pre, G21_Girls$FVC_gmodel_1, method = "pearson")
cor.test(G21_Girls$FVC_pre, G21_Girls$FVC_GLI, method = "pearson")
#Density and bar plots with mean differences compared to GLI:
hrbrthemes::import_roboto_condensed() 
ggplot(G21_Girls, aes(x=x) ) +
  geom_density( aes(x = MD_FVC_gmodel_1, y = ..density..), fill="#69b3a2" ) +
  geom_label( aes(x=0, y=0.25, label="MD_FVC_gmodel_1"), color="#69b3a2") +
  # Bottom
  geom_density( aes(x = MD_FVC_GLI, y = -..density..), fill= "#404080") +
  geom_label( aes(x=0, y=-0.25, label="MD_FVC_GLI"), color="#404080") +
  theme_ipsum() +
  xlab("Differences between predicted and measured FVC")

G21_Girls <- data.frame(
  type = c( rep("G21_Girls$MD_FVC_gmodel_1", 1000), rep("G21_Girls$MD_FVC_GLI", 1000) ),
  value = c( rnorm(1000), rnorm(1000, mean=0) )
)
# Represent it
G21_Girls %>%
  ggplot( aes(x=value, fill=type)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")

#FVC_gmodel_2 Height/Weight:
FVC_gmodel_2 <- lm(FVC_pre ~ Peso, data = ARIA_Girls)
summary(FVC_gmodel_2)
SEL2020$FVC_gmodel_2 <- (1.160283+(0.023900*SEL2020$Peso))
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
#CI for the variables in FVC_gmodel_2:
confint(FVC_gmodel_2)
#Residuals for FVC_gmodel_2:
sigma(FVC_gmodel_2)/mean(ARIA_Girls$FVC_pre)
sigma(FVC_gmodel_2)/mean(G21_Girls$FVC_pre)
#Mean differences:
SEL2020$MD_FVC_gmodel_2 <- (SEL2020$FVC_gmodel_2-SEL2020$FVC_pre)
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
mean(G21_Girls$MD_FVC_gmodel_2)
summary(G21_Girls$MD_FVC_gmodel_2)
max(G21_Girls$MD_FVC_gmodel_2)-min(G21_Girls$MD_FVC_gmodel_2)
summary(G21_Girls$MD_FVC_GLI)
max(G21_Girls$MD_FVC_GLI)-min(G21_Girls$MD_FVC_GLI)
#Model evaluation:
FVC_gmodel_2_RSq_Mean_dif <- (-0.1206548*(1/0.3903))
FVC_gmodel_2_RSq_Mean_dif
AIC(FVC_gmodel_2)
BIC(FVC_gmodel_2)
cor.test(G21_Girls$FVC_pre, G21_Girls$FVC_gmodel_2, method = "pearson")
cor.test(G21_Girls$FVC_pre, G21_Girls$FVC_GLI, method = "pearson")
#Density and bar plots with mean differences compared to GLI:
hrbrthemes::import_roboto_condensed() 
ggplot(G21_Girls, aes(x=x) ) +
  geom_density( aes(x = MD_FVC_gmodel_2, y = ..density..), fill="#69b3a2" ) +
  geom_label( aes(x=0, y=0.25, label="MD_FVC_gmodel_2"), color="#69b3a2") +
  # Bottom
  geom_density( aes(x = MD_FVC_GLI, y = -..density..), fill= "#404080") +
  geom_label( aes(x=0, y=-0.25, label="MD_FVC_GLI"), color="#404080") +
  theme_ipsum() +
  xlab("Differences between predicted and measured FVC")

G21_Girls <- data.frame(
  type = c( rep("G21_Girls$MD_FVC_gmodel_2", 1000), rep("G21_Girls$MD_FVC_GLI", 1000) ),
  value = c( rnorm(1000), rnorm(1000, mean=0) )
)
# Represent it
G21_Girls %>%
  ggplot( aes(x=value, fill=type)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")

#FVC_gmodel_3 Age:
FVC_gmodel_3 <- lm(FVC_pre ~ Idade, data = ARIA_Girls)
summary(FVC_gmodel_3)
SEL2020$FVC_gmodel_3 <- (0.47362+(0.17011*SEL2020$Idade))
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
#CI for the variables in FVC_gmodel_3:
confint(FVC_gmodel_3)
#Residuals for FVC_bmodel_3:
sigma(FVC_gmodel_3)/mean(ARIA_Girls$FVC_pre)
sigma(FVC_gmodel_3)/mean(G21_Girls$FVC_pre)
#Mean differences:
SEL2020$MD_FVC_gmodel_3 <- (SEL2020$FVC_gmodel_3-SEL2020$FVC_pre)
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
mean(G21_Girls$MD_FVC_gmodel_3)
summary(G21_Girls$MD_FVC_gmodel_3)
max(G21_Girls$MD_FVC_gmodel_3)-min(G21_Girls$MD_FVC_gmodel_3)
summary(G21_Girls$MD_FVC_GLI)
max(G21_Girls$MD_FVC_GLI)-min(G21_Girls$MD_FVC_GLI)
#Model evaluation:
FVC_gmodel_3_RSq_Mean_dif <- (-0.00959038*(1/0.17011))
FVC_gmodel_3_RSq_Mean_dif
AIC(FVC_gmodel_3)
BIC(FVC_gmodel_3)
cor.test(G21_Girls$FVC_pre, G21_Girls$FVC_gmodel_3, method = "pearson")
cor.test(G21_Girls$FVC_pre, G21_Girls$FVC_GLI, method = "pearson")
#Density and bar plots with mean differences compared to GLI:
hrbrthemes::import_roboto_condensed() 
ggplot(G21_Girls, aes(x=x) ) +
  geom_density( aes(x = MD_FVC_gmodel_3, y = ..density..), fill="#69b3a2" ) +
  geom_label( aes(x=0, y=0.25, label="MD_FVC_gmodel_3"), color="#69b3a2") +
  # Bottom
  geom_density( aes(x = MD_FVC_GLI, y = -..density..), fill= "#404080") +
  geom_label( aes(x=0, y=-0.25, label="MD_FVC_GLI"), color="#404080") +
  theme_ipsum() +
  xlab("Differences between predicted and measured FVC")

G21_Girls <- data.frame(
  type = c( rep("G21_Girls$MD_FVC_gmodel_3", 1000), rep("G21_Girls$MD_FVC_GLI", 1000) ),
  value = c( rnorm(1000), rnorm(1000, mean=0) )
)
# Represent it
G21_Girls %>%
  ggplot( aes(x=value, fill=type)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")

#FVC_gmodel_4 BMI:
FVC_gmodel_4 <- lm(FVC_pre ~ BMI, data = ARIA_Girls)
summary(FVC_gmodel_4)
SEL2020$FVC_gmodel_4 <- (1.40209+(0.03088*SEL2020$BMI))
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
#CI for the variables in FVC_gmodel_4:
confint(FVC_gmodel_4)
#Residuals for FVC_gmodel_4:
sigma(FVC_gmodel_4)/mean(ARIA_Girls$FVC_pre)
sigma(FVC_gmodel_4)/mean(G21_Girls$FVC_pre)
#Mean differences:
SEL2020$MD_FVC_gmodel_4 <- (SEL2020$FVC_gmodel_4-SEL2020$FVC_pre)
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
mean(G21_Girls$MD_FVC_gmodel_4)
summary(G21_Girls$MD_FVC_gmodel_4)
max(G21_Girls$MD_FVC_gmodel_4)-min(G21_Girls$MD_FVC_gmodel_4)
summary(G21_Girls$MD_FVC_GLI)
max(G21_Girls$MD_FVC_GLI)-min(G21_Girls$MD_FVC_GLI)
#Model evaluation:
FVC_gmodel_4_RSq_Mean_dif <- (-0.1959136*(1/0.1138))
FVC_gmodel_4_RSq_Mean_dif
AIC(FVC_gmodel_4)
BIC(FVC_gmodel_4)
cor.test(G21_Girls$FVC_pre, G21_Girls$FVC_gmodel_4, method = "pearson")
cor.test(G21_Girls$FVC_pre, G21_Girls$FVC_GLI, method = "pearson")
#Density and bar plots with mean differences compared to GLI:
hrbrthemes::import_roboto_condensed() 
ggplot(G21_Girls, aes(x=x) ) +
  geom_density( aes(x = MD_FVC_gmodel_4, y = ..density..), fill="#69b3a2" ) +
  geom_label( aes(x=0, y=0.25, label="MD_FVC_gmodel_4"), color="#69b3a2") +
  # Bottom
  geom_density( aes(x = MD_FVC_GLI, y = -..density..), fill= "#404080") +
  geom_label( aes(x=0, y=-0.25, label="MD_FVC_GLI"), color="#404080") +
  theme_ipsum() +
  xlab("Differences between predicted and measured FVC")

G21_Girls <- data.frame(
  type = c( rep("G21_Girls$MD_FVC_gmodel_4", 1000), rep("G21_Girls$MD_FVC_GLI", 1000) ),
  value = c( rnorm(1000), rnorm(1000, mean=0) )
)
# Represent it
G21_Girls %>%
  ggplot( aes(x=value, fill=type)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")

#FVC_gmodel_5 Height/Weight:
FVC_gmodel_5 <- lm(FVC_pre ~ Altura_cm + Peso, data = ARIA_Girls)
summary(FVC_gmodel_5)
SEL2020$FVC_gmodel_5 <- (-1.130567+(0.019973*SEL2020$Altura_cm)+(0.011641*SEL2020$Peso))
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
#CI for the variables in FVC_gmodel_5:
confint(FVC_gmodel_5)
#Residuals for FVC_gmodel_5:
sigma(FVC_gmodel_5)/mean(ARIA_Girls$FVC_pre)
sigma(FVC_gmodel_5)/mean(G21_Girls$FVC_pre)
#Mean differences:
SEL2020$MD_FVC_gmodel_5 <- (SEL2020$FVC_gmodel_5-SEL2020$FVC_pre)
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
mean(G21_Girls$MD_FVC_gmodel_5)
summary(G21_Girls$MD_FVC_gmodel_5)
max(G21_Girls$MD_FVC_gmodel_5)-min(G21_Girls$MD_FVC_gmodel_5)
summary(G21_Girls$MD_FVC_GLI)
max(G21_Girls$MD_FVC_GLI)-min(G21_Girls$MD_FVC_GLI)
#Model evaluation:
FVC_gmodel_5_RSq_Mean_dif <- (-0.06633593*(1/0.5366))
FVC_gmodel_5_RSq_Mean_dif
AIC(FVC_gmodel_5)
BIC(FVC_gmodel_5)
cor.test(G21_Girls$FVC_pre, G21_Girls$FVC_gmodel_5, method = "pearson")
cor.test(G21_Girls$FVC_pre, G21_Girls$FVC_GLI, method = "pearson")
#Density and bar plots with mean differences compared to GLI:
hrbrthemes::import_roboto_condensed() 
ggplot(G21_Girls, aes(x=x) ) +
  geom_density( aes(x = MD_FVC_gmodel_5, y = ..density..), fill="#69b3a2" ) +
  geom_label( aes(x=0, y=0.25, label="MD_FVC_gmodel_5"), color="#69b3a2") +
  # Bottom
  geom_density( aes(x = MD_FVC_GLI, y = -..density..), fill= "#404080") +
  geom_label( aes(x=0, y=-0.25, label="MD_FVC_GLI"), color="#404080") +
  theme_ipsum() +
  xlab("Differences between predicted and measured FVC")

G21_Girls <- data.frame(
  type = c( rep("G21_Girls$MD_FVC_gmodel_5", 1000), rep("G21_Girls$MD_FVC_GLI", 1000) ),
  value = c( rnorm(1000), rnorm(1000, mean=0) )
)
# Represent it
G21_Girls %>%
  ggplot( aes(x=value, fill=type)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")


#FVC_gmodel_6 Height/Age:   
FVC_gmodel_6 <- lm(FVC_pre ~ Altura_cm + Idade, data = ARIA_Girls)
summary(FVC_gmodel_6)
SEL2020$FVC_gmodel_6 <- (-1.908543+(0.025908*SEL2020$Altura_cm)+(0.041818*SEL2020$Idade))
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
#CI for the variables in FVC_gmodel_6:
confint(FVC_gmodel_6)
#Residuals for FVC_gmodel_6:
sigma(FVC_gmodel_6)/mean(ARIA_Girls$FVC_pre)
sigma(FVC_gmodel_6)/mean(G21_Girls$FVC_pre)
#Mean differences:
SEL2020$MD_FVC_gmodel_6 <- (SEL2020$FVC_gmodel_6-SEL2020$FVC_pre)
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
mean(G21_Girls$MD_FVC_gmodel_6)
summary(G21_Girls$MD_FVC_gmodel_6)
max(G21_Girls$MD_FVC_gmodel_6)-min(G21_Girls$MD_FVC_gmodel_6)
summary(G21_Girls$MD_FVC_GLI)
max(G21_Girls$MD_FVC_GLI)-min(G21_Girls$MD_FVC_GLI)
#Model evaluation:
FVC_gmodel_6_RSq_Mean_dif <- (-0.03144365*(1/0.489))
FVC_gmodel_6_RSq_Mean_dif
AIC(FVC_gmodel_6)
BIC(FVC_gmodel_6)
cor.test(G21_Girls$FVC_pre, G21_Girls$FVC_gmodel_6, method = "pearson")
cor.test(G21_Girls$FVC_pre, G21_Girls$FVC_GLI, method = "pearson")
#Density and bar plots with mean differences compared to GLI:
hrbrthemes::import_roboto_condensed() 
ggplot(G21_Girls, aes(x=x) ) +
  geom_density( aes(x = MD_FVC_gmodel_6, y = ..density..), fill="#69b3a2" ) +
  geom_label( aes(x=0, y=0.25, label="MD_FVC_gmodel_6"), color="#69b3a2") +
  # Bottom
  geom_density( aes(x = MD_FVC_GLI, y = -..density..), fill= "#404080") +
  geom_label( aes(x=0, y=-0.25, label="MD_FVC_GLI"), color="#404080") +
  theme_ipsum() +
  xlab("Differences between predicted and measured FVC")

G21_Girls <- data.frame(
  type = c( rep("G21_Girls$MD_FVC_gmodel_6", 1000), rep("G21_Girls$MD_FFVC_GLI", 1000) ),
  value = c( rnorm(1000), rnorm(1000, mean=0) )
)
# Represent it
G21_Girls %>%
  ggplot( aes(x=value, fill=type)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")

#FVC_gmodel_7 Height/BMI:   
FVC_gmodel_7 <- lm(FVC_pre ~ Altura_cm + BMI, data = ARIA_Girls)
summary(FVC_gmodel_7)
SEL2020$FVC_gmodel_7 <- (-2.029196+(0.026424*SEL2020$Altura_cm)+(0.022884*SEL2020$BMI))
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
#CI for the variables in FVC_gmodel_7:
confint(FVC_gmodel_7)
#Residuals for FVC_gmodel_7:
sigma(FVC_gmodel_7)/mean(ARIA_Girls$FVC_pre)
sigma(FVC_gmodel_7)/mean(G21_Girls$FVC_pre)
#Mean differences:
SEL2020$MD_FVC_gmodel_7 <- (SEL2020$FVC_gmodel_7-SEL2020$FVC_pre)
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
mean(G21_Girls$MD_FVC_gmodel_7)
summary(G21_Girls$MD_FVC_gmodel_7)
max(G21_Girls$MD_FVC_gmodel_7)-min(G21_Girls$MD_FVC_gmodel_7)
summary(G21_Girls$MD_FVC_GLI)
max(G21_Girls$MD_FVC_GLI)-min(G21_Girls$MD_FVC_GLI)
#Model evaluation:
FVC_gmodel_7_RSq_Mean_dif <- (-0.06400533*(1/0.5454))
FVC_gmodel_7_RSq_Mean_dif
AIC(FVC_gmodel_7)
BIC(FVC_gmodel_7)
cor.test(G21_Girls$FVC_pre, G21_Girls$FVC_gmodel_7, method = "pearson")
cor.test(G21_Girls$FVC_pre, G21_Girls$FVC_GLI, method = "pearson")
#Density and bar plots with mean differences compared to GLI:
hrbrthemes::import_roboto_condensed() 
ggplot(G21_Girls, aes(x=x) ) +
  geom_density( aes(x = MD_FVC_gmodel_7, y = ..density..), fill="#69b3a2" ) +
  geom_label( aes(x=0, y=0.25, label="MD_FVC_gmodel_7"), color="#69b3a2") +
  # Bottom
  geom_density( aes(x = MD_FVC_GLI, y = -..density..), fill= "#404080") +
  geom_label( aes(x=0, y=-0.25, label="MD_FVC_GLI"), color="#404080") +
  theme_ipsum() +
  xlab("Differences between predicted and measured FVC")

G21_Girls <- data.frame(
  type = c( rep("G21_Girls$MD_FVC_gmodel_7", 1000), rep("G21_Girls$MD_FVC_GLI", 1000) ),
  value = c( rnorm(1000), rnorm(1000, mean=0) )
)
# Represent it
G21_Girls %>%
  ggplot( aes(x=value, fill=type)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")

#FEV1_gmodel_8 Height/Weight/Age:   
FVC_gmodel_8 <- lm(FVC_pre ~ Altura_cm + Peso + Idade, data = ARIA_Girls)
summary(FVC_gmodel_8)
SEL2020$FVC_gmodel_8 <- (-1.244048+(0.017492*SEL2020$Altura_cm)+(0.012006*SEL2020$Peso)+(0.049830*SEL2020$Idade))
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
#CI for the variables in FVC_gmodel_8:
confint(FVC_gmodel_8)
#Residuals for FVC_gmodel_8:
sigma(FVC_gmodel_8)/mean(ARIA_Girls$FVC_pre)
sigma(FVC_gmodel_8)/mean(G21_Girls$FVC_pre)
#Mean differences:
SEL2020$MD_FVC_gmodel_8 <- (SEL2020$FVC_gmodel_8-SEL2020$FVC_pre)
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
mean(G21_Girls$MD_FVC_gmodel_8)
summary(G21_Girls$MD_FVC_gmodel_8)
max(G21_Girls$MD_FVC_gmodel_8)-min(G21_Girls$MD_FVC_gmodel_8)
summary(G21_Girls$MD_FVC_GLI)
max(G21_Girls$MD_FVC_GLI)-min(G21_Girls$MD_FVC_GLI)
#Model evaluation:
FVC_gmodel_8_RSq_Mean_dif <- (-0.01696375*(1/0.5455))
FVC_gmodel_8_RSq_Mean_dif
AIC(FVC_gmodel_8)
BIC(FVC_gmodel_8)
cor.test(G21_Girls$FVC_pre, G21_Girls$FVC_gmodel_8, method = "pearson")
cor.test(G21_Girls$FVC_pre, G21_Girls$FVC_GLI, method = "pearson")
#Density and bar plots with mean differences compared to GLI:
hrbrthemes::import_roboto_condensed() 
ggplot(G21_Girls, aes(x=x) ) +
  geom_density( aes(x = MD_FVC_gmodel_8, y = ..density..), fill="#69b3a2" ) +
  geom_label( aes(x=0, y=0.25, label="MD_FVC_gmodel_8"), color="#69b3a2") +
  # Bottom
  geom_density( aes(x = MD_FVC_GLI, y = -..density..), fill= "#404080") +
  geom_label( aes(x=0, y=-0.25, label="MD_FVC_GLI"), color="#404080") +
  theme_ipsum() +
  xlab("Differences between predicted and measured FVC")

G21_Girls <- data.frame(
  type = c( rep("G21_Girls$MD_FVC_gmodel_8", 1000), rep("G21_Girls$MD_FVC_GLI", 1000) ),
  value = c( rnorm(1000), rnorm(1000, mean=0) )
)
# Represent it
G21_Girls %>%
  ggplot( aes(x=value, fill=type)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")

#FVC_gmodel_9 Height/Weight/BMI:   
FVC_gmodel_9 <- lm(FVC_pre ~ Altura_cm + Peso + BMI, data = ARIA_Girls)
summary(FVC_gmodel_9)
SEL2020$FVC_gmodel_9 <- (-3.897335+(0.040381*SEL2020$Altura_cm)+(-0.024744*SEL2020$Peso)+(0.067408*SEL2020$BMI))
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
#CI for the variables in FVC_gmodel_9:
confint(FVC_gmodel_9)
#Residuals for FVC_gmodel_9:
sigma(FVC_gmodel_9)/mean(ARIA_Girls$FVC_pre)
sigma(FVC_gmodel_9)/mean(G21_Girls$FVC_pre)
#Mean differences:
SEL2020$MD_FVC_gmodel_9 <- (SEL2020$FVC_gmodel_9-SEL2020$FVC_pre)
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
mean(G21_Girls$MD_FVC_gmodel_9)
summary(G21_Girls$MD_FVC_gmodel_9)
max(G21_Girls$MD_FVC_gmodel_9)-min(G21_Girls$MD_FVC_gmodel_9)
summary(G21_Girls$MD_FVC_GLI)
max(G21_Girls$MD_FVC_GLI)-min(G21_Girls$MD_FVC_GLI)
#Model evaluation:
SEL2020$FVC_gmodel_9_RSq_Mean_dif <- (-0.06064114*(1/0.5504))
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
mean(G21_Girls$FVC_gmodel_9_RSq_Mean_dif)
AIC(FVC_gmodel_9)
BIC(FVC_gmodel_9)
cor.test(G21_Girls$FVC_pre, G21_Girls$FVC_gmodel_9, method = "pearson")
cor.test(G21_Girls$FVC_pre, G21_Girls$FVC_GLI, method = "pearson")
#Density and bar plots with mean differences compared to GLI:
hrbrthemes::import_roboto_condensed() 
ggplot(G21_Girls, aes(x=x) ) +
  geom_density( aes(x = MD_FVC_gmodel_9, y = ..density..), fill="#69b3a2" ) +
  geom_label( aes(x=0, y=0.25, label="MD_FVC_gmodel_9"), color="#69b3a2") +
  # Bottom
  geom_density( aes(x = MD_FVC_GLI, y = -..density..), fill= "#404080") +
  geom_label( aes(x=0, y=-0.25, label="MD_FVC_GLI"), color="#404080") +
  theme_ipsum() +
  xlab("Differences between predicted and measured FVC")

G21_Girls <- data.frame(
  type = c( rep("G21_Girls$MD_FVC_gmodel_9", 1000), rep("G21_Girls$MD_FVC_GLI", 1000) ),
  value = c( rnorm(1000), rnorm(1000, mean=0) )
)
# Represent it
G21_Girls %>%
  ggplot( aes(x=value, fill=type)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")


#FVC_gmodel_10 Height/Weight/BMI/Age:   
FVC_gmodel_10 <- lm(FVC_pre ~ Altura_cm + Peso + BMI + Idade, data = ARIA_Girls)
summary(FVC_gmodel_10)
SEL2020$FVC_gmodel_10 <- (-3.513141+(0.035164*SEL2020$Altura_cm)+(-0.018358*SEL2020$Peso)+(0.056062*SEL2020$BMI)+(0.035784*SEL2020$Idade))
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
#CI for the variables in FVC_gmodel_10:
confint(FVC_gmodel_10)
#Residuals for FVC_gmodel_10:
sigma(FVC_gmodel_10)/mean(ARIA_Girls$FVC_pre)
sigma(FVC_gmodel_10)/mean(G21_Girls$FVC_pre)
#Mean differences:
SEL2020$MD_FVC_gmodel_10 <- (SEL2020$FVC_gmodel_10-SEL2020$FVC_pre)
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
mean(G21_Girls$MD_FVC_gmodel_10)
summary(G21_Girls$MD_FVC_gmodel_10)
max(G21_Girls$MD_FVC_gmodel_10)-min(G21_Girls$MD_FVC_gmodel_10)
summary(G21_Girls$MD_FVC_GLI)
max(G21_Girls$MD_FVC_GLI)-min(G21_Girls$MD_FVC_GLI)
#Model evaluation:
FVC_gmodel_10_RSq_Mean_dif <- (-0.02621138*(1/0.5536))
FVC_gmodel_10_RSq_Mean_dif
AIC(FVC_gmodel_10)
BIC(FVC_gmodel_10)
cor.test(G21_Girls$FVC_pre, G21_Girls$FVC_gmodel_10, method = "pearson")
cor.test(G21_Girls$FVC_pre, G21_Girls$FVC_GLI, method = "pearson")
#Density and bar plots with mean differences compared to GLI:
hrbrthemes::import_roboto_condensed() 
ggplot(G21_Girls, aes(x=x) ) +
  geom_density( aes(x = MD_FVC_gmodel_10, y = ..density..), fill="#69b3a2" ) +
  geom_label( aes(x=0, y=0.25, label="MD_FVC_gmodel_10"), color="#69b3a2") +
  # Bottom
  geom_density( aes(x = MD_FVC_GLI, y = -..density..), fill= "#404080") +
  geom_label( aes(x=0, y=-0.25, label="MD_FVC_GLI"), color="#404080") +
  theme_ipsum() +
  xlab("Differences between predicted and measured FVC")

G21_Girls <- data.frame(
  type = c( rep("G21_Girls$MD_FVC_gmodel_10", 1000), rep("G21_Girls$MD_FVC_GLI", 1000) ),
  value = c( rnorm(1000), rnorm(1000, mean=0) )
)
# Represent it
G21_Girls %>%
  ggplot( aes(x=value, fill=type)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")


#FVC_gmodel_11 Weight/Age:   
FVC_gmodel_11 <- lm(FVC_pre ~ Peso + Idade, data = ARIA_Girls)
summary(FVC_gmodel_11)
SEL2020$FVC_gmodel_11 <- (0.279341+(0.021337*SEL2020$Peso)+(0.110286*SEL2020$Idade))
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
#CI for the variables in FVC_gmodel_11:
confint(FVC_gmodel_11)
#Residuals for FVC_gmodel_11:
sigma(FVC_gmodel_11)/mean(ARIA_Girls$FVC_pre)
sigma(FVC_gmodel_11)/mean(G21_Girls$FVC_pre)
#Mean differences:
SEL2020$MD_FVC_gmodel_11 <- (SEL2020$FVC_gmodel_11-SEL2020$FVC_pre)
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
mean(G21_Girls$MD_FVC_gmodel_11)
summary(G21_Girls$MD_FVC_gmodel_11)
max(G21_Girls$MD_FVC_gmodel_11)-min(G21_Girls$MD_FVC_gmodel_11)
summary(G21_Girls$MD_FVC_GLI)
max(G21_Girls$MD_FVC_GLI)-min(G21_Girls$MD_FVC_GLI)
#Model evaluation:
FVC_gmodel_11_RSq_Mean_dif <- (0.003560548*(1/0.4529))
FVC_gmodel_11_RSq_Mean_dif
AIC(FVC_gmodel_11)
BIC(FVC_gmodel_11)
cor.test(G21_Girls$FVC_pre, G21_Girls$FVC_gmodel_11, method = "pearson")
cor.test(G21_Girls$FVC_pre, G21_Girls$FVC_GLI, method = "pearson")
#Density and bar plots with mean differences compared to GLI:
hrbrthemes::import_roboto_condensed() 
ggplot(G21_Girls, aes(x=x) ) +
  geom_density( aes(x = MD_FVC_gmodel_11, y = ..density..), fill="#69b3a2" ) +
  geom_label( aes(x=0, y=0.25, label="MD_FVC_gmodel_11"), color="#69b3a2") +
  # Bottom
  geom_density( aes(x = MD_FVC_GLI, y = -..density..), fill= "#404080") +
  geom_label( aes(x=0, y=-0.25, label="MD_FVC_GLI"), color="#404080") +
  theme_ipsum() +
  xlab("Differences between predicted and measured FVC")

G21_Girls <- data.frame(
  type = c( rep("G21_Girls$MD_FVC_gmodel_11", 1000), rep("G21_Girls$MD_FVC_GLI", 1000) ),
  value = c( rnorm(1000), rnorm(1000, mean=0) )
)
# Represent it
G21_Girls %>%
  ggplot( aes(x=value, fill=type)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")

#FVC_gmodel_12 Weight/BMI:   
FVC_gmodel_12 <- lm(FVC_pre ~ Peso + BMI, data = ARIA_Girls)
summary(FVC_gmodel_12)
SEL2020$FVC_gmodel_12 <- (1.498682+(0.042849*SEL2020$Peso)+(-0.053536*SEL2020$BMI))
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
#CI for the variables in FVC_gmodel_12:
confint(FVC_gmodel_12)
#Residuals for FVC_gmodel_12:
sigma(FVC_gmodel_12)/mean(ARIA_Girls$FVC_pre)
sigma(FVC_gmodel_12)/mean(G21_Girls$FVC_pre)
#Mean differences:
SEL2020$MD_FVC_gmodel_12 <- (SEL2020$FVC_gmodel_12-SEL2020$FVC_pre)
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
mean(G21_Girls$MD_FVC_gmodel_12)
summary(G21_Girls$MD_FVC_gmodel_12)
max(G21_Girls$MD_FVC_gmodel_12)-min(G21_Girls$MD_FVC_gmodel_12)
summary(G21_Girls$MD_FVC_GLI)
max(G21_Girls$MD_FVC_GLI)-min(G21_Girls$MD_FVC_GLI)
#Model evaluation:
FVC_gmodel_12_RSq_Mean_dif <- (-0.08105181*(1/0.4957))
FVC_gmodel_12_RSq_Mean_dif
AIC(FVC_gmodel_12)
BIC(FVC_gmodel_12)
cor.test(G21_Girls$FVC_pre, G21_Girls$FVC_gmodel_12, method = "pearson")
cor.test(G21_Girls$FVC_pre, G21_Girls$FVC_GLI, method = "pearson")
#Density and bar plots with mean differences compared to GLI:
hrbrthemes::import_roboto_condensed() 
ggplot(G21_Girls, aes(x=x) ) +
  geom_density( aes(x = MD_FVC_gmodel_12, y = ..density..), fill="#69b3a2" ) +
  geom_label( aes(x=0, y=0.25, label="MD_FVC_gmodel_12"), color="#69b3a2") +
  # Bottom
  geom_density( aes(x = MD_FVC_GLI, y = -..density..), fill= "#404080") +
  geom_label( aes(x=0, y=-0.25, label="MD_FVC_GLI"), color="#404080") +
  theme_ipsum() +
  xlab("Differences between predicted and measured FVC")

G21_Girls <- data.frame(
  type = c( rep("G21_Girls$MD_FVC_gmodel_12", 1000), rep("G21_Girls$MD_FVC_GLI", 1000) ),
  value = c( rnorm(1000), rnorm(1000, mean=0) )
)
# Represent it
G21_Girls %>%
  ggplot( aes(x=value, fill=type)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")

#FVC_gmodel_13 Weight/BMI/Age:   
FVC_gmodel_13 <- lm(FVC_pre ~ Peso + BMI + Idade, data = ARIA_Girls)
summary(FVC_gmodel_13)
SEL2020$FVC_gmodel_13 <- (0.863364+(0.038086*SEL2020$Peso)+(-0.044849*SEL2020$BMI)+(0.072661*SEL2020$Idade))
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
#CI for the variables in FVC_gmodel_13:
confint(FVC_gmodel_13)
#Residuals for FVC_bmodel_13:
sigma(FVC_gmodel_13)/mean(ARIA_Girls$FVC_pre)
sigma(FVC_gmodel_13)/mean(G21_Girls$FVC_pre)
#Mean differences:
SEL2020$MD_FVC_gmodel_13 <- (SEL2020$FVC_gmodel_13-SEL2020$FVC_pre)
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
mean(G21_Girls$MD_FVC_gmodel_13)
summary(G21_Girls$MD_FVC_gmodel_13)
max(G21_Girls$MD_FVC_gmodel_13)-min(G21_Girls$MD_FVC_gmodel_13)
summary(G21_Girls$MD_FVC_GLI)
max(G21_Girls$MD_FVC_GLI)-min(G21_Girls$MD_FVC_GLI)
#Model evaluation:
FVC_gmodel_13_RSq_Mean_dif <- (-0.005632639*(1/0.519))
FVC_gmodel_13_RSq_Mean_dif
AIC(FVC_gmodel_13)
BIC(FVC_gmodel_13)
cor.test(G21_Girls$FVC_pre, G21_Girls$FVC_gmodel_13, method = "pearson")
cor.test(G21_Girls$FVC_pre, G21_Girls$FVC_GLI, method = "pearson")
#Density and bar plots with mean differences compared to GLI:
hrbrthemes::import_roboto_condensed() 
ggplot(G21_Girls, aes(x=x) ) +
  geom_density( aes(x = MD_FVC_gmodel_13, y = ..density..), fill="#69b3a2" ) +
  geom_label( aes(x=0, y=0.25, label="MD_FVC_gmodel_13"), color="#69b3a2") +
  # Bottom
  geom_density( aes(x = MD_FVC_GLI, y = -..density..), fill= "#404080") +
  geom_label( aes(x=0, y=-0.25, label="MD_FVC_GLI"), color="#404080") +
  theme_ipsum() +
  xlab("Differences between predicted and measured FVC")

G21_Girls <- data.frame(
  type = c( rep("G21_Girls$MD_FVC_gmodel_13", 1000), rep("G21_Girls$MD_FVC_GLI", 1000) ),
  value = c( rnorm(1000), rnorm(1000, mean=0) )
)
# Represent it
G21_Girls %>%
  ggplot( aes(x=value, fill=type)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")

#FVC_gmodel_14 BMI/Age:   
FVC_gmodel_14 <- lm(FVC_pre ~ BMI + Idade, data = ARIA_Girls)
summary(FVC_gmodel_14)
SEL2020$FVC_gmodel_14 <- (-0.007682+(0.029308*SEL2020$BMI)+(0.164008*SEL2020$Idade))
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
#CI for the variables in FVC_gmodel_14:
confint(FVC_gmodel_14)
#Residuals for FVC_gmodel_14:
sigma(FVC_gmodel_14)/mean(ARIA_Girls$FVC_pre)
sigma(FVC_gmodel_14)/mean(G21_Girls$FVC_pre)
#Mean differences:
SEL2020$MD_FVC_gmodel_14 <- (SEL2020$FVC_gmodel_14-SEL2020$FVC_pre)
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
mean(G21_Girls$MD_FVC_gmodel_14)
summary(G21_Girls$MD_FVC_gmodel_14)
max(G21_Girls$MD_FVC_gmodel_14)-min(G21_Girls$MD_FVC_gmodel_14)
summary(G21_Girls$MD_FVC_GLI)
max(G21_Girls$MD_FVC_GLI)-min(G21_Girls$MD_FVC_GLI)
#Model evaluation:
FVC_gmodel_14_RSq_Mean_dif <- (0.003152689*(1/0.2642))
FVC_gmodel_14_RSq_Mean_dif
AIC(FVC_gmodel_14)
BIC(FVC_gmodel_14)
cor.test(G21_Girls$FVC_pre, G21_Girls$FVC_gmodel_14, method = "pearson")
cor.test(G21_Girls$FVC_pre, G21_Girls$FVC_GLI, method = "pearson")
#Density and bar plots with mean differences compared to GLI:
hrbrthemes::import_roboto_condensed() 
ggplot(G21_Girls, aes(x=x) ) +
  geom_density( aes(x = MD_FVC_gmodel_14, y = ..density..), fill="#69b3a2" ) +
  geom_label( aes(x=0, y=0.25, label="MD_FVC_gmodel_14"), color="#69b3a2") +
  # Bottom
  geom_density( aes(x = MD_FVC_GLI, y = -..density..), fill= "#404080") +
  geom_label( aes(x=0, y=-0.25, label="MD_FVC_GLI"), color="#404080") +
  theme_ipsum() +
  xlab("Differences between predicted and measured FVC")

G21_Girls <- data.frame(
  type = c( rep("G21_Girls$MD_FVC_gmodel_14", 1000), rep("G21_Girls$MD_FVC_GLI", 1000) ),
  value = c( rnorm(1000), rnorm(1000, mean=0) )
)
# Represent it
G21_Girls %>%
  ggplot( aes(x=value, fill=type)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")


#Multiple Linear Regression FEV1 Boys: 
#FEV1_bmodel 1 Height
FEV1_bmodel_1 <- lm(FEV1_pre ~ Altura_cm, data = ARIA_Boys)
summary(FEV1_bmodel_1)
SEL2020$FEV1_bmodel_1 <- (-1.976218 + (0.028115*SEL2020$Altura_cm))
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
#CI for the variables in FEV1_bmodel_1:
confint(FEV1_bmodel_1)
#Residuals for FEV1_bmodel_1:
sigma(FEV1_bmodel_1)/mean(ARIA_Boys$FEV1_pre)
sigma(FEV1_bmodel_1)/mean(G21_Boys$FEV1_pre)
#Mean differences:
SEL2020$MD_FEV1_bmodel_1 <- (SEL2020$FEV1_bmodel_1-SEL2020$FEV1_pre)
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
mean(G21_Boys$MD_FEV1_bmodel_1)
summary(G21_Boys$MD_FEV1_bmodel_1)
max(G21_Boys$MD_FEV1_bmodel_1)-min(G21_Boys$MD_FEV1_bmodel_1)
summary(G21_Boys$MD_FEV1_GLI)
max(G21_Boys$MD_FEV1_GLI)-min(G21_Boys$MD_FEV1_GLI)
#Model evaluation:
FEV1_bmodel_1_RSq_Mean_dif <- (-0.05406621*(1/0.4676))
FEV1_bmodel_1_RSq_Mean_dif
AIC(FEV1_bmodel_1)
BIC(FEV1_bmodel_1)
cor.test(G21_Boys$FEV1_pre, G21_Boys$FEV1_bmodel_1, method = "pearson")
cor.test(G21_Boys$FEV1_pre, G21_Boys$FEV1_GLI, method = "pearson")
#Density and bar plots with mean differences compared to GLI:
hrbrthemes::import_roboto_condensed() 
ggplot(G21_Boys, aes(x=x) ) +
  geom_density( aes(x = MD_FEV1_bmodel_1, y = ..density..), fill="#69b3a2" ) +
  geom_label( aes(x=0, y=0.25, label="MD_FEV1_bmodel_1"), color="#69b3a2") +
  # Bottom
  geom_density( aes(x = MD_FEV1_GLI, y = -..density..), fill= "#404080") +
  geom_label( aes(x=0, y=-0.25, label="MD_FEV1_GLI"), color="#404080") +
  theme_ipsum() +
  xlab("Differences between predicted and measured FEV1")

G21_Boys <- data.frame(
  type = c( rep("G21_Boys$MD_FEV1_bmodel_1", 1000), rep("G21_Boys$MD_FEV1_GLI", 1000) ),
  value = c( rnorm(1000), rnorm(1000, mean=0) )
)
# Represent it
G21_Boys %>%
  ggplot( aes(x=value, fill=type)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")

#FEV1_bmodel_2 Height/Weight:
FEV1_bmodel_2 <- lm(FEV1_pre ~ Peso, data = ARIA_Boys)
summary(FEV1_bmodel_2)
SEL2020$FEV1_bmodel_2 <- (1.233441+(0.018263*SEL2020$Peso))
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
#CI for the variables in FEV1_bmodel_2:
confint(FEV1_bmodel_2)
#Residuals for FEV1_bmodel_2:
sigma(FEV1_bmodel_2)/mean(ARIA_Boys$FEV1_pre)
sigma(FEV1_bmodel_2)/mean(G21_Boys$FEV1_pre)
#Mean differences:
SEL2020$MD_FEV1_bmodel_2 <- (SEL2020$FEV1_bmodel_2-SEL2020$FEV1_pre)
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
mean(G21_Boys$MD_FEV1_bmodel_2)
summary(G21_Boys$MD_FEV1_bmodel_2)
max(G21_Boys$MD_FEV1_bmodel_2)-min(G21_Boys$MD_FEV1_bmodel_2)
summary(G21_Boys$MD_FEV1_GLI)
max(G21_Boys$MD_FEV1_GLI)-min(G21_Boys$MD_FEV1_GLI)
#Model evaluation:
FEV1_bmodel_2_RSq_Mean_dif <- (-0.1192031*(1/0.2656))
FEV1_bmodel_2_RSq_Mean_dif
AIC(FEV1_bmodel_2)
BIC(FEV1_bmodel_2)
cor.test(G21_Boys$FEV1_pre, G21_Boys$FEV1_bmodel_2, method = "pearson")
cor.test(G21_Boys$FEV1_pre, G21_Boys$FEV1_GLI, method = "pearson")
#Density and bar plots with mean differences compared to GLI:
hrbrthemes::import_roboto_condensed() 
ggplot(G21_Boys, aes(x=x) ) +
  geom_density( aes(x = MD_FEV1_bmodel_2, y = ..density..), fill="#69b3a2" ) +
  geom_label( aes(x=0, y=0.25, label="MD_FEV1_bmodel_2"), color="#69b3a2") +
  # Bottom
  geom_density( aes(x = MD_FEV1_GLI, y = -..density..), fill= "#404080") +
  geom_label( aes(x=0, y=-0.25, label="MD_FEV1_GLI"), color="#404080") +
  theme_ipsum() +
  xlab("Differences between predicted and measured FEV1")

G21_Boys <- data.frame(
  type = c( rep("G21_Boys$MD_FEV1_bmodel_2", 1000), rep("G21_Boys$MD_FEV1_GLI", 1000) ),
  value = c( rnorm(1000), rnorm(1000, mean=0) )
)
# Represent it
G21_Boys %>%
  ggplot( aes(x=value, fill=type)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")

#FEV1_bmodel_3 Age:
FEV1_bmodel_3 <- lm(FEV1_pre ~ Idade, data = ARIA_Boys)
summary(FEV1_bmodel_3)
SEL2020$FEV1_bmodel_3 <- (0.70906+(0.12950*SEL2020$Idade))
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
#CI for the variables in FEV1_bmodel_3:
confint(FEV1_bmodel_3)
#Residuals for FEV1_bmodel_3:
sigma(FEV1_bmodel_3)/mean(ARIA_Boys$FEV1_pre)
sigma(FEV1_bmodel_3)/mean(G21_Boys$FEV1_pre)
#Mean differences:
SEL2020$MD_FEV1_bmodel_3 <- (SEL2020$FEV1_bmodel_3-SEL2020$FEV1_pre)
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
mean(G21_Boys$MD_FEV1_bmodel_3)
summary(G21_Boys$MD_FEV1_bmodel_3)
max(G21_Boys$MD_FEV1_bmodel_3)-min(G21_Boys$MD_FEV1_bmodel_3)
summary(G21_Boys$MD_FEV1_GLI)
max(G21_Boys$MD_FEV1_GLI)-min(G21_Boys$MD_FEV1_GLI)
#Model evaluation:
FEV1_bmodel_3_RSq_Mean_dif <- (-0.02706776*(1/0.1129))
FEV1_bmodel_3_RSq_Mean_dif
AIC(FEV1_bmodel_3)
BIC(FEV1_bmodel_3)
cor.test(G21_Boys$FEV1_pre, G21_Boys$FEV1_bmodel_3, method = "pearson")
cor.test(G21_Boys$FEV1_pre, G21_Boys$FEV1_GLI, method = "pearson")
#Density and bar plots with mean differences compared to GLI:
hrbrthemes::import_roboto_condensed() 
ggplot(G21_Boys, aes(x=x) ) +
  geom_density( aes(x = MD_FEV1_bmodel_3, y = ..density..), fill="#69b3a2" ) +
  geom_label( aes(x=0, y=0.25, label="MD_FEV1_bmodel_3"), color="#69b3a2") +
  # Bottom
  geom_density( aes(x = MD_FEV1_GLI, y = -..density..), fill= "#404080") +
  geom_label( aes(x=0, y=-0.25, label="MD_FEV1_GLI"), color="#404080") +
  theme_ipsum() +
  xlab("Differences between predicted and measured FEV1")

G21_Boys <- data.frame(
  type = c( rep("G21_Boys$MD_FEV1_bmodel_3", 1000), rep("G21_Boys$MD_FEV1_GLI", 1000) ),
  value = c( rnorm(1000), rnorm(1000, mean=0) )
)
# Represent it
G21_Boys %>%
  ggplot( aes(x=value, fill=type)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")

#FEV1_bmodel_4 BMI:
FEV1_bmodel_4 <- lm(FEV1_pre ~ BMI, data = ARIA_Boys)
summary(FEV1_bmodel_4)
SEL2020$FEV1_bmodel_4 <- (1.360282+(0.027013*SEL2020$BMI))
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
#CI for the variables in FEV1_bmodel_4:
confint(FEV1_bmodel_4)
#Residuals for FEV1_bmodel_4:
sigma(FEV1_bmodel_4)/mean(ARIA_Boys$FEV1_pre)
sigma(FEV1_bmodel_4)/mean(G21_Boys$FEV1_pre)
#Mean differences:
SEL2020$MD_FEV1_bmodel_4 <- (SEL2020$FEV1_bmodel_4-SEL2020$FEV1_pre)
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
mean(G21_Boys$MD_FEV1_bmodel_4)
summary(G21_Boys$MD_FEV1_bmodel_4)
max(G21_Boys$MD_FEV1_bmodel_4)-min(G21_Boys$MD_FEV1_bmodel_4)
summary(G21_Boys$MD_FEV1_GLI)
max(G21_Boys$MD_FEV1_GLI)-min(G21_Boys$MD_FEV1_GLI)
#Model evaluation:
FEV1_bmodel_4_RSq_Mean_dif <- (-0.1655037*(1/0.08716))
FEV1_bmodel_4_RSq_Mean_dif
AIC(FEV1_bmodel_4)
BIC(FEV1_bmodel_4)
cor.test(G21_Boys$FEV1_pre, G21_Boys$FEV1_bmodel_4, method = "pearson")
cor.test(G21_Boys$FEV1_pre, G21_Boys$FEV1_GLI, method = "pearson")
#Density and bar plots with mean differences compared to GLI:
hrbrthemes::import_roboto_condensed() 
ggplot(G21_Boys, aes(x=x) ) +
  geom_density( aes(x = MD_FEV1_bmodel_4, y = ..density..), fill="#69b3a2" ) +
  geom_label( aes(x=0, y=0.25, label="MD_FEV1_bmodel_4"), color="#69b3a2") +
  # Bottom
  geom_density( aes(x = MD_FEV1_GLI, y = -..density..), fill= "#404080") +
  geom_label( aes(x=0, y=-0.25, label="MD_FEV1_GLI"), color="#404080") +
  theme_ipsum() +
  xlab("Differences between predicted and measured FEV1")

G21_Boys <- data.frame(
  type = c( rep("G21_Boys$MD_FEV1_bmodel_4", 1000), rep("G21_Boys$MD_FEV1_GLI", 1000) ),
  value = c( rnorm(1000), rnorm(1000, mean=0) )
)
# Represent it
G21_Boys %>%
  ggplot( aes(x=value, fill=type)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")

#FEV1_bmodel_5 Height/Weight:
FEV1_bmodel_5 <- lm(FEV1_pre ~ Altura_cm + Peso, data = ARIA_Boys)
summary(FEV1_bmodel_5)
SEL2020$FEV1_bmodel_5 <- (-1.774352+(0.025987*SEL2020$Altura_cm)+(0.002609*SEL2020$Peso))
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
#CI for the variables in FEV1_bmodel_5:
confint(FEV1_bmodel_5)
#Residuals for FEV1_bmodel_5:
sigma(FEV1_bmodel_5)/mean(ARIA_Boys$FEV1_pre)
sigma(FEV1_bmodel_5)/mean(G21_Boys$FEV1_pre)
#Mean differences:
SEL2020$MD_FEV1_bmodel_5 <- (SEL2020$FEV1_bmodel_5-SEL2020$FEV1_pre)
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
mean(G21_Boys$MD_FEV1_bmodel_5)
summary(G21_Boys$MD_FEV1_bmodel_5)
max(G21_Boys$MD_FEV1_bmodel_5)-min(G21_Boys$MD_FEV1_bmodel_5)
summary(G21_Boys$MD_FEV1_GLI)
max(G21_Boys$MD_FEV1_GLI)-min(G21_Boys$MD_FEV1_GLI)
#Model evaluation:
FEV1_bmodel_5_RSq_Mean_dif <- (-0.05466959*(1/0.4684))
FEV1_bmodel_5_RSq_Mean_dif
AIC(FEV1_bmodel_5)
BIC(FEV1_bmodel_5)
cor.test(G21_Boys$FEV1_pre, G21_Boys$FEV1_bmodel_5, method = "pearson")
cor.test(G21_Boys$FEV1_pre, G21_Boys$FEV1_GLI, method = "pearson")
#Density and bar plots with mean differences compared to GLI:
hrbrthemes::import_roboto_condensed() 
ggplot(G21_Boys, aes(x=x) ) +
  geom_density( aes(x = MD_FEV1_bmodel_5, y = ..density..), fill="#69b3a2" ) +
  geom_label( aes(x=0, y=0.25, label="MD_FEV1_bmodel_5"), color="#69b3a2") +
  # Bottom
  geom_density( aes(x = MD_FEV1_GLI, y = -..density..), fill= "#404080") +
  geom_label( aes(x=0, y=-0.25, label="MD_FEV1_GLI"), color="#404080") +
  theme_ipsum() +
  xlab("Differences between predicted and measured FEV1")

G21_Boys <- data.frame(
  type = c( rep("G21_Boys$MD_FEV1_bmodel_5", 1000), rep("G21_Boys$MD_FEV1_GLI", 1000) ),
  value = c( rnorm(1000), rnorm(1000, mean=0) )
)
# Represent it
G21_Boys %>%
  ggplot( aes(x=value, fill=type)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")


#FEV1_bmodel_6 Height/Age:   
FEV1_bmodel_6 <- lm(FEV1_pre ~ Altura_cm + Idade, data = ARIA_Boys)
summary(FEV1_bmodel_6)
SEL2020$FEV1_bmodel_6 <- (-2.03101+(0.02710*SEL2020$Altura_cm)+(0.02203*SEL2020$Idade))
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
#CI for the variables in FEV1_bmodel_6:
confint(FEV1_bmodel_6)
#Residuals for FEV1_bmodel_6:
sigma(FEV1_bmodel_6)/mean(ARIA_Boys$FEV1_pre)
sigma(FEV1_bmodel_6)/mean(G21_Boys$FEV1_pre)
#Mean differences:
SEL2020$MD_FEV1_bmodel_6 <- (SEL2020$FEV1_bmodel_6-SEL2020$FEV1_pre)
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
mean(G21_Boys$MD_FEV1_bmodel_6)
summary(G21_Boys$MD_FEV1_bmodel_6)
max(G21_Boys$MD_FEV1_bmodel_6)-min(G21_Boys$MD_FEV1_bmodel_6)
summary(G21_Boys$MD_FEV1_GLI)
max(G21_Boys$MD_FEV1_GLI)-min(G21_Boys$MD_FEV1_GLI)
#Model evaluation:
FEV1_bmodel_6_RSq_Mean_dif <- (-0.03162822*(1/0.4683))
FEV1_bmodel_6_RSq_Mean_dif
AIC(FEV1_bmodel_6)
BIC(FEV1_bmodel_6)
cor.test(G21_Boys$FEV1_pre, G21_Boys$FEV1_bmodel_6, method = "pearson")
cor.test(G21_Boys$FEV1_pre, G21_Boys$FEV1_GLI, method = "pearson")
#Density and bar plots with mean differences compared to GLI:
hrbrthemes::import_roboto_condensed() 
ggplot(G21_Boys, aes(x=x) ) +
  geom_density( aes(x = MD_FEV1_bmodel_6, y = ..density..), fill="#69b3a2" ) +
  geom_label( aes(x=0, y=0.25, label="MD_FEV1_bmodel_6"), color="#69b3a2") +
  # Bottom
  geom_density( aes(x = MD_FEV1_GLI, y = -..density..), fill= "#404080") +
  geom_label( aes(x=0, y=-0.25, label="MD_FEV1_GLI"), color="#404080") +
  theme_ipsum() +
  xlab("Differences between predicted and measured FEV1")

G21_Boys <- data.frame(
  type = c( rep("G21_Boys$MD_FEV1_bmodel_6", 1000), rep("G21_Boys$MD_FEV1_GLI", 1000) ),
  value = c( rnorm(1000), rnorm(1000, mean=0) )
)
# Represent it
G21_Boys %>%
  ggplot( aes(x=value, fill=type)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")

#FEV1_bmodel_7 Height/BMI:   
FEV1_bmodel_7 <- lm(FEV1_pre ~ Altura_cm + BMI, data = ARIA_Boys)
summary(FEV1_bmodel_7)
SEL2020$FEV1_bmodel_7 <- (-1.951359+(0.027246*SEL2020$Altura_cm)+(0.005196*SEL2020$BMI))
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
#CI for the variables in FEV1_bmodel_7:
confint(FEV1_bmodel_7)
#Residuals for FEV1_bmodel_7:
sigma(FEV1_bmodel_7)/mean(ARIA_Boys$FEV1_pre)
sigma(FEV1_bmodel_7)/mean(G21_Boys$FEV1_pre)
#Mean differences:
SEL2020$MD_FEV1_bmodel_7 <- (SEL2020$FEV1_bmodel_7-SEL2020$FEV1_pre)
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
mean(G21_Boys$MD_FEV1_bmodel_7)
summary(G21_Boys$MD_FEV1_bmodel_7)
max(G21_Boys$MD_FEV1_bmodel_7)-min(G21_Boys$MD_FEV1_bmodel_7)
summary(G21_Boys$MD_FEV1_GLI)
max(G21_Boys$MD_FEV1_GLI)-min(G21_Boys$MD_FEV1_GLI)
#Model evaluation:
FEV1_bmodel_7_RSq_Mean_dif <- (-0.05461613*(1/0.4685))
FEV1_bmodel_7_RSq_Mean_dif
AIC(FEV1_bmodel_7)
BIC(FEV1_bmodel_7)
cor.test(G21_Boys$FEV1_pre, G21_Boys$FEV1_bmodel_7, method = "pearson")
cor.test(G21_Boys$FEV1_pre, G21_Boys$FEV1_GLI, method = "pearson")
#Density and bar plots with mean differences compared to GLI:
hrbrthemes::import_roboto_condensed() 
ggplot(G21_Boys, aes(x=x) ) +
  geom_density( aes(x = MD_FEV1_bmodel_7, y = ..density..), fill="#69b3a2" ) +
  geom_label( aes(x=0, y=0.25, label="MD_FEV1_bmodel_7"), color="#69b3a2") +
  # Bottom
  geom_density( aes(x = MD_FEV1_GLI, y = -..density..), fill= "#404080") +
  geom_label( aes(x=0, y=-0.25, label="MD_FEV1_GLI"), color="#404080") +
  theme_ipsum() +
  xlab("Differences between predicted and measured FEV1")

G21_Boys <- data.frame(
  type = c( rep("G21_Boys$MD_FEV1_bmodel_7", 1000), rep("G21_Boys$MD_FEV1_GLI", 1000) ),
  value = c( rnorm(1000), rnorm(1000, mean=0) )
)
# Represent it
G21_Boys %>%
  ggplot( aes(x=value, fill=type)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")

#FEV1_bmodel_8 Height/Weight/Age:   
FEV1_bmodel_8 <- lm(FEV1_pre ~ Altura_cm + Peso + Idade, data = ARIA_Boys)
summary(FEV1_bmodel_8)
SEL2020$FEV1_bmodel_8 <- (-1.841892+(0.025244*SEL2020$Altura_cm)+(0.002381*SEL2020$Peso)+(0.020076*SEL2020$Idade))
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
#CI for the variables in FEV1_bmodel_8:
confint(FEV1_bmodel_8)
#Residuals for FEV1_bmodel_8:
sigma(FEV1_bmodel_8)/mean(ARIA_Boys$FEV1_pre)
sigma(FEV1_bmodel_8)/mean(G21_Boys$FEV1_pre)
#Mean differences:
SEL2020$MD_FEV1_bmodel_8 <- (SEL2020$FEV1_bmodel_8-SEL2020$FEV1_pre)
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
mean(G21_Boys$MD_FEV1_bmodel_8)
summary(G21_Boys$MD_FEV1_bmodel_8)
max(G21_Boys$MD_FEV1_bmodel_8)-min(G21_Boys$MD_FEV1_bmodel_8)
summary(G21_Boys$MD_FEV1_GLI)
max(G21_Boys$MD_FEV1_GLI)-min(G21_Boys$MD_FEV1_GLI)
#Model evaluation:
FEV1_bmodel_8_RSq_Mean_dif <- (-0.03469705*(1/0.4687))
FEV1_bmodel_8_RSq_Mean_dif
AIC(FEV1_bmodel_8)
BIC(FEV1_bmodel_8)
cor.test(G21_Boys$FEV1_pre, G21_Boys$FEV1_bmodel_8, method = "pearson")
cor.test(G21_Boys$FEV1_pre, G21_Boys$FEV1_GLI, method = "pearson")
#Density and bar plots with mean differences compared to GLI:
hrbrthemes::import_roboto_condensed() 
ggplot(G21_Boys, aes(x=x) ) +
  geom_density( aes(x = MD_FEV1_bmodel_8, y = ..density..), fill="#69b3a2" ) +
  geom_label( aes(x=0, y=0.25, label="MD_FEV1_bmodel_8"), color="#69b3a2") +
  # Bottom
  geom_density( aes(x = MD_FEV1_GLI, y = -..density..), fill= "#404080") +
  geom_label( aes(x=0, y=-0.25, label="MD_FEV1_GLI"), color="#404080") +
  theme_ipsum() +
  xlab("Differences between predicted and measured FEV1")

G21_Boys <- data.frame(
  type = c( rep("G21_Boys$MD_FEV1_bmodel_8", 1000), rep("G21_Boys$MD_FEV1_GLI", 1000) ),
  value = c( rnorm(1000), rnorm(1000, mean=0) )
)
# Represent it
G21_Boys %>%
  ggplot( aes(x=value, fill=type)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")

#FEV1_bmodel_9 Height/Weight/BMI:   
FEV1_bmodel_9 <- lm(FEV1_pre ~ Altura_cm + Peso + BMI, data = ARIA_Boys)
summary(FEV1_bmodel_9)
SEL2020$FEV1_bmodel_9 <- (-2.145775+(0.028649*SEL2020$Altura_cm)+(-0.002855*SEL2020$Peso)+(0.010729*SEL2020$BMI))
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
#CI for the variables in FEV1_bmodel_9:
confint(FEV1_bmodel_9)
#Residuals for FEV1_bmodel_9:
sigma(FEV1_bmodel_9)/mean(ARIA_Boys$FEV1_pre)
sigma(FEV1_bmodel_9)/mean(G21_Boys$FEV1_pre)
#Mean differences:
SEL2020$MD_FEV1_bmodel_9 <- (SEL2020$FEV1_bmodel_9-SEL2020$FEV1_pre)
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
mean(G21_Boys$MD_FEV1_bmodel_9)
summary(G21_Boys$MD_FEV1_bmodel_9)
max(G21_Boys$MD_FEV1_bmodel_9)-min(G21_Boys$MD_FEV1_bmodel_9)
summary(G21_Boys$MD_FEV1_GLI)
max(G21_Boys$MD_FEV1_GLI)-min(G21_Boys$MD_FEV1_GLI)
#Model evaluation:
SEL2020$FEV1_bmodel_9_RSq_Mean_dif <- (-0.05456915*(1/0.4665))
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
mean(G21_Boys$FEV1_bmodel_9_RSq_Mean_dif)
AIC(FEV1_bmodel_9)
BIC(FEV1_bmodel_9)
cor.test(G21_Boys$FEV1_pre, G21_Boys$FEV1_bmodel_9, method = "pearson")
cor.test(G21_Boys$FEV1_pre, G21_Boys$FEV1_GLI, method = "pearson")
#Density and bar plots with mean differences compared to GLI:
hrbrthemes::import_roboto_condensed() 
ggplot(G21_Boys, aes(x=x) ) +
  geom_density( aes(x = MD_FEV1_bmodel_9, y = ..density..), fill="#69b3a2" ) +
  geom_label( aes(x=1, y=0.25, label="MD_FEV1_bmodel_9"), color="#69b3a2") +
  # Bottom
  geom_density( aes(x = MD_FEV1_GLI, y = -..density..), fill= "#404080") +
  geom_label( aes(x=1, y=-0.25, label="MD_FEV1_GLI"), color="#404080") +
  theme_ipsum() +
  xlab("Differences between predicted and measured FEV1")

G21_Boys <- data.frame(
  type = c( rep("G21_Boys$MD_FEV1_bmodel_9", 1000), rep("G21_Boys$MD_FEV1_GLI", 1000) ),
  value = c( rnorm(1000), rnorm(1000, mean=0) )
)
# Represent it
G21_Boys %>%
  ggplot( aes(x=value, fill=type)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")


#FEV1_bmodel_10 Height/Weight/BMI/Age:   
FEV1_bmodel_10 <- lm(FEV1_pre ~ Altura_cm + Peso + BMI + Idade, data = ARIA_Boys)
summary(FEV1_bmodel_10)
SEL2020$FEV1_bmodel_10 <- (-2.177205+(0.027655*SEL2020$Altura_cm)+(-0.002556*SEL2020$Peso)+(0.009698*SEL2020$BMI)+(0.019951*SEL2020$Idade))
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
#CI for the variables in FEV1_bmodel_10:
confint(FEV1_bmodel_10)
#Residuals for FEV1_bmodel_10:
sigma(FEV1_bmodel_10)/mean(ARIA_Boys$FEV1_pre)
sigma(FEV1_bmodel_10)/mean(G21_Boys$FEV1_pre)
#Mean differences:
SEL2020$MD_FEV1_bmodel_10 <- (SEL2020$FEV1_bmodel_10-SEL2020$FEV1_pre)
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
mean(G21_Boys$MD_FEV1_bmodel_10)
summary(G21_Boys$MD_FEV1_bmodel_10)
max(G21_Boys$MD_FEV1_bmodel_10)-min(G21_Boys$MD_FEV1_bmodel_10)
summary(G21_Boys$MD_FEV1_GLI)
max(G21_Boys$MD_FEV1_GLI)-min(G21_Boys$MD_FEV1_GLI)
#Model evaluation:
FEV1_bmodel_10_RSq_Mean_dif <- (-0.03468872*(1/0.4668))
FEV1_bmodel_10_RSq_Mean_dif
AIC(FEV1_bmodel_10)
BIC(FEV1_bmodel_10)
cor.test(G21_Boys$FEV1_pre, G21_Boys$FEV1_bmodel_10, method = "pearson")
cor.test(G21_Boys$FEV1_pre, G21_Boys$FEV1_GLI, method = "pearson")
#Density and bar plots with mean differences compared to GLI:
hrbrthemes::import_roboto_condensed() 
ggplot(G21_Boys, aes(x=x) ) +
  geom_density( aes(x = MD_FEV1_bmodel_10, y = ..density..), fill="#69b3a2" ) +
  geom_label( aes(x=0, y=0.25, label="MD_FEV1_bmodel_10"), color="#69b3a2") +
  # Bottom
  geom_density( aes(x = MD_FEV1_GLI, y = -..density..), fill= "#404080") +
  geom_label( aes(x=0, y=-0.25, label="MD_FEV1_GLI"), color="#404080") +
  theme_ipsum() +
  xlab("Differences between predicted and measured FEV1")

G21_Boys <- data.frame(
  type = c( rep("G21_Boys$MD_FEV1_bmodel_10", 1000), rep("G21_Boys$MD_FEV1_GLI", 1000) ),
  value = c( rnorm(1000), rnorm(1000, mean=0) )
)
# Represent it
G21_Boys %>%
  ggplot( aes(x=value, fill=type)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")
  

#FEV1_bmodel_11 Weight/Age:   
FEV1_bmodel_11 <- lm(FEV1_pre ~ Peso + Idade, data = ARIA_Boys)
summary(FEV1_bmodel_11)
SEL2020$FEV1_bmodel_11 <- (0.721138+(0.016011*SEL2020$Peso)+(0.066982*SEL2020$Idade))
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
#CI for the variables in FEV1_bmodel_11:
confint(FEV1_bmodel_11)
#Residuals for FEV1_bmodel_11:
sigma(FEV1_bmodel_11)/mean(ARIA_Boys$FEV1_pre)
sigma(FEV1_bmodel_11)/mean(G21_Boys$FEV1_pre)
#Mean differences:
SEL2020$MD_FEV1_bmodel_11 <- (SEL2020$FEV1_bmodel_11-SEL2020$FEV1_pre)
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
mean(G21_Boys$MD_FEV1_bmodel_11)
summary(G21_Boys$MD_FEV1_bmodel_11)
max(G21_Boys$MD_FEV1_bmodel_11)-min(G21_Boys$MD_FEV1_bmodel_11)
summary(G21_Boys$MD_FEV1_GLI)
max(G21_Boys$MD_FEV1_GLI)-min(G21_Boys$MD_FEV1_GLI)
#Model evaluation:
FEV1_bmodel_11_RSq_Mean_dif <- (-0.04637772*(1/0.29))
FEV1_bmodel_11_RSq_Mean_dif
AIC(FEV1_bmodel_11)
BIC(FEV1_bmodel_11)
cor.test(G21_Boys$FEV1_pre, G21_Boys$FEV1_bmodel_11, method = "pearson")
cor.test(G21_Boys$FEV1_pre, G21_Boys$FEV1_GLI, method = "pearson")
#Density and bar plots with mean differences compared to GLI:
hrbrthemes::import_roboto_condensed() 
ggplot(G21_Boys, aes(x=x) ) +
  geom_density( aes(x = MD_FEV1_bmodel_11, y = ..density..), fill="#69b3a2" ) +
  geom_label( aes(x=0, y=0.25, label="MD_FEV1_bmodel_11"), color="#69b3a2") +
  # Bottom
  geom_density( aes(x = MD_FEV1_GLI, y = -..density..), fill= "#404080") +
  geom_label( aes(x=0, y=-0.25, label="MD_FEV1_GLI"), color="#404080") +
  theme_ipsum() +
  xlab("Differences between predicted and measured FEV1")

G21_Boys <- data.frame(
  type = c( rep("G21_Boys$MD_FEV1_bmodel_11", 1000), rep("G21_Boys$MD_FEV1_GLI", 1000) ),
  value = c( rnorm(1000), rnorm(1000, mean=0) )
)
# Represent it
G21_Boys %>%
  ggplot( aes(x=value, fill=type)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")

#FEV1_bmodel_12 Weight/BMI:   
FEV1_bmodel_12 <- lm(FEV1_pre ~ Peso + BMI, data = ARIA_Boys)
summary(FEV1_bmodel_12)
SEL2020$FEV1_bmodel_12 <- (1.804202+(0.053050*SEL2020$Peso)+(-0.096681*SEL2020$BMI))
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
#CI for the variables in FEV1_bmodel_12:
confint(FEV1_bmodel_12)
#Residuals for FEV1_bmodel_12:
sigma(FEV1_bmodel_12)/mean(ARIA_Boys$FEV1_pre)
sigma(FEV1_bmodel_12)/mean(G21_Boys$FEV1_pre)
#Mean differences:
SEL2020$MD_FEV1_bmodel_12 <- (SEL2020$FEV1_bmodel_12-SEL2020$FEV1_pre)
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
mean(G21_Boys$MD_FEV1_bmodel_12)
summary(G21_Boys$MD_FEV1_bmodel_12)
max(G21_Boys$MD_FEV1_bmodel_12)-min(G21_Boys$MD_FEV1_bmodel_12)
summary(G21_Boys$MD_FEV1_GLI)
max(G21_Boys$MD_FEV1_GLI)-min(G21_Boys$MD_FEV1_GLI)
#Model evaluation:
FEV1_bmodel_12_RSq_Mean_dif <- (-0.06035643*(1/0.4511))
FEV1_bmodel_12_RSq_Mean_dif
AIC(FEV1_bmodel_12)
BIC(FEV1_bmodel_12)
cor.test(G21_Boys$FEV1_pre, G21_Boys$FEV1_bmodel_12, method = "pearson")
cor.test(G21_Boys$FEV1_pre, G21_Boys$FEV1_GLI, method = "pearson")
#Density and bar plots with mean differences compared to GLI:
hrbrthemes::import_roboto_condensed() 
ggplot(G21_Boys, aes(x=x) ) +
  geom_density( aes(x = MD_FEV1_bmodel_12, y = ..density..), fill="#69b3a2" ) +
  geom_label( aes(x=0, y=0.25, label="MD_FEV1_bmodel_12"), color="#69b3a2") +
  # Bottom
  geom_density( aes(x = MD_FEV1_GLI, y = -..density..), fill= "#404080") +
  geom_label( aes(x=0, y=-0.25, label="MD_FEV1_GLI"), color="#404080") +
  theme_ipsum() +
  xlab("Differences between predicted and measured FEV1")

G21_Boys <- data.frame(
  type = c( rep("G21_Boys$MD_FEV1_bmodel_12", 1000), rep("G21_Boys$MD_FEV1_GLI", 1000) ),
  value = c( rnorm(1000), rnorm(1000, mean=0) )
)
# Represent it
G21_Boys %>%
  ggplot( aes(x=value, fill=type)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")

#FEV1_bmodel_13 Weight/BMI/Age:   
FEV1_bmodel_13 <- lm(FEV1_pre ~ Peso + BMI + Idade, data = ARIA_Boys)
summary(FEV1_bmodel_13)
SEL2020$FEV1_bmodel_13 <- (1.592239+(0.050986*SEL2020$Peso)+(-0.093289*SEL2020$BMI)+(0.025095*SEL2020$Idade))
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
#CI for the variables in FEV1_bmodel_13:
confint(FEV1_bmodel_13)
#Residuals for FEV1_bmodel_13:
sigma(FEV1_bmodel_13)/mean(ARIA_Boys$FEV1_pre)
sigma(FEV1_bmodel_13)/mean(G21_Boys$FEV1_pre)
#Mean differences:
SEL2020$MD_FEV1_bmodel_13 <- (SEL2020$FEV1_bmodel_13-SEL2020$FEV1_pre)
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
mean(G21_Boys$MD_FEV1_bmodel_13)
summary(G21_Boys$MD_FEV1_bmodel_13)
max(G21_Boys$MD_FEV1_bmodel_13)-min(G21_Boys$MD_FEV1_bmodel_13)
summary(G21_Boys$MD_FEV1_GLI)
max(G21_Boys$MD_FEV1_GLI)-min(G21_Boys$MD_FEV1_GLI)
#Model evaluation:
FEV1_bmodel_13_RSq_Mean_dif <- (-0.03513173*(1/0.4526))
FEV1_bmodel_13_RSq_Mean_dif
AIC(FEV1_bmodel_13)
BIC(FEV1_bmodel_13)
cor.test(G21_Boys$FEV1_pre, G21_Boys$FEV1_bmodel_13, method = "pearson")
cor.test(G21_Boys$FEV1_pre, G21_Boys$FEV1_GLI, method = "pearson")
#Density and bar plots with mean differences compared to GLI:
hrbrthemes::import_roboto_condensed() 
ggplot(G21_Boys, aes(x=x) ) +
  geom_density( aes(x = MD_FEV1_bmodel_13, y = ..density..), fill="#69b3a2" ) +
  geom_label( aes(x=0, y=0.25, label="MD_FEV1_bmodel_13"), color="#69b3a2") +
  # Bottom
  geom_density( aes(x = MD_FEV1_GLI, y = -..density..), fill= "#404080") +
  geom_label( aes(x=0, y=-0.25, label="MD_FEV1_GLI"), color="#404080") +
  theme_ipsum() +
  xlab("Differences between predicted and measured FEV1")

G21_Boys <- data.frame(
  type = c( rep("G21_Boys$MD_FEV1_bmodel_13", 1000), rep("G21_Boys$MD_FEV1_GLI", 1000) ),
  value = c( rnorm(1000), rnorm(1000, mean=0) )
)
# Represent it
G21_Boys %>%
  ggplot( aes(x=value, fill=type)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")

#FEV1_bmodel_14 BMI/Age:   
FEV1_bmodel_14 <- lm(FEV1_pre ~ BMI + Idade, data = ARIA_Boys)
summary(FEV1_bmodel_14)
SEL2020$FEV1_bmodel_14 <- (0.520015+(0.020878*SEL2020$BMI)+(0.108311*SEL2020$Idade))
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
#CI for the variables in FEV1_bmodel_14:
confint(FEV1_bmodel_14)
#Residuals for FEV1_bmodel_14:
sigma(FEV1_bmodel_14)/mean(ARIA_Boys$FEV1_pre)
sigma(FEV1_bmodel_14)/mean(G21_Boys$FEV1_pre)
#Mean differences:
SEL2020$MD_FEV1_bmodel_14 <- (SEL2020$FEV1_bmodel_14-SEL2020$FEV1_pre)
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
mean(G21_Boys$MD_FEV1_bmodel_14)
summary(G21_Boys$MD_FEV1_bmodel_14)
max(G21_Boys$MD_FEV1_bmodel_14)-min(G21_Boys$MD_FEV1_bmodel_14)
summary(G21_Boys$MD_FEV1_GLI)
max(G21_Boys$MD_FEV1_GLI)-min(G21_Boys$MD_FEV1_GLI)
#Model evaluation:
FEV1_bmodel_14_RSq_Mean_dif <- (-0.03902068*(1/0.1609))
FEV1_bmodel_14_RSq_Mean_dif
AIC(FEV1_bmodel_14)
BIC(FEV1_bmodel_14)
cor.test(G21_Boys$FEV1_pre, G21_Boys$FEV1_bmodel_14, method = "pearson")
cor.test(G21_Boys$FEV1_pre, G21_Boys$FEV1_GLI, method = "pearson")
#Density and bar plots with mean differences compared to GLI:
hrbrthemes::import_roboto_condensed() 
ggplot(G21_Boys, aes(x=x) ) +
  geom_density( aes(x = MD_FEV1_bmodel_14, y = ..density..), fill="#69b3a2" ) +
  geom_label( aes(x=0, y=0.25, label="MD_FEV1_bmodel_14"), color="#69b3a2") +
  # Bottom
  geom_density( aes(x = MD_FEV1_GLI, y = -..density..), fill= "#404080") +
  geom_label( aes(x=0, y=-0.25, label="MD_FEV1_GLI"), color="#404080") +
  theme_ipsum() +
  xlab("Differences between predicted and measured FEV1")

G21_Boys <- data.frame(
  type = c( rep("G21_Boys$MD_FEV1_bmodel_14", 1000), rep("G21_Boys$MD_FEV1_GLI", 1000) ),
  value = c( rnorm(1000), rnorm(1000, mean=0) )
)
# Represent it
G21_Boys %>%
  ggplot( aes(x=value, fill=type)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")
    
    
#Multiple Linear Regression FEV1 Girls: 

#FEV1_gmodel 1 Height
FEV1_gmodel_1 <- lm(FEV1_pre ~ Altura_cm, data = ARIA_Girls)
summary(FEV1_gmodel_1)
SEL2020$FEV1_gmodel_1 <- (-1.518880+(0.024390*SEL2020$Altura_cm))
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
#CI for the variables in FEV1_gmodel_1:
confint(FEV1_gmodel_1)
#Residuals for FEV1_gmodel_1:
sigma(FEV1_gmodel_1)/mean(ARIA_Girls$FEV1_pre)
sigma(FEV1_gmodel_1)/mean(G21_Girls$FEV1_pre)
#Mean differences:
SEL2020$MD_FEV1_gmodel_1 <- (SEL2020$FEV1_gmodel_1-SEL2020$FEV1_pre)
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
mean(G21_Girls$MD_FEV1_gmodel_1)
summary(G21_Girls$MD_FEV1_gmodel_1)
max(G21_Girls$MD_FEV1_gmodel_1)-min(G21_Girls$MD_FEV1_gmodel_1)
summary(G21_Girls$MD_FEV1_GLI)
max(G21_Girls$MD_FEV1_GLI)-min(G21_Girls$MD_FEV1_GLI)
#Model evaluation:
FEV1_gmodel_1_RSq_Mean_dif <- (-0.05408929*(1/0.453))
FEV1_gmodel_1_RSq_Mean_dif
AIC(FEV1_gmodel_1)
BIC(FEV1_gmodel_1)
cor.test(G21_Girls$FEV1_pre, G21_Girls$FEV1_gmodel_1, method = "pearson")
cor.test(G21_Girls$FEV1_pre, G21_Girls$FEV1_GLI, method = "pearson")
#Density and bar plots with mean differences compared to GLI:
hrbrthemes::import_roboto_condensed() 
ggplot(G21_Girls, aes(x=x) ) +
  geom_density( aes(x = MD_FEV1_gmodel_1, y = ..density..), fill="#69b3a2" ) +
  geom_label( aes(x=0, y=0.25, label="MD_FEV1_gmodel_1"), color="#69b3a2") +
  # Bottom
  geom_density( aes(x = MD_FEV1_GLI, y = -..density..), fill= "#404080") +
  geom_label( aes(x=0, y=-0.25, label="MD_FEV1_GLI"), color="#404080") +
  theme_ipsum() +
  xlab("Differences between predicted and measured FEV1")

G21_Girls <- data.frame(
  type = c( rep("G21_Girls$MD_FEV1_gmodel_1", 1000), rep("G21_Girls$MD_FEV1_GLI", 1000) ),
  value = c( rnorm(1000), rnorm(1000, mean=0) )
)
# Represent it
G21_Girls %>%
  ggplot( aes(x=value, fill=type)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")

#FEV1_gmodel_2 Weight:
FEV1_gmodel_2 <- lm(FEV1_pre ~ Peso, data = ARIA_Girls)
summary(FEV1_gmodel_2)
SEL2020$FEV1_gmodel_2 <- (1.061787+(0.021402*SEL2020$Peso))
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
#CI for the variables in FEV1_gmodel_2:
confint(FEV1_gmodel_2)
#Residuals for FVC_gmodel_2:
sigma(FEV1_gmodel_2)/mean(ARIA_Girls$FEV1_pre)
sigma(FEV1_gmodel_2)/mean(G21_Girls$FEV1_pre)
#Mean differences:
SEL2020$MD_FEV1_gmodel_2 <- (SEL2020$FEV1_gmodel_2-SEL2020$FEV1_pre)
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
mean(G21_Girls$MD_FEV1_gmodel_2)
summary(G21_Girls$MD_FEV1_gmodel_2)
max(G21_Girls$MD_FEV1_gmodel_2)-min(G21_Girls$MD_FEV1_gmodel_2)
summary(G21_Girls$MD_FEV1_GLI)
max(G21_Girls$MD_FEV1_GLI)-min(G21_Girls$MD_FEV1_GLI)
#Model evaluation:
FEV1_gmodel_2_RSq_Mean_dif <- (-0.09451826*(1/0.3806))
FEV1_gmodel_2_RSq_Mean_dif
AIC(FEV1_gmodel_2)
BIC(FEV1_gmodel_2)
cor.test(G21_Girls$FEV1_pre, G21_Girls$FEV1_gmodel_2, method = "pearson")
cor.test(G21_Girls$FEV1_pre, G21_Girls$FEV1_GLI, method = "pearson")
#Density and bar plots with mean differences compared to GLI:
hrbrthemes::import_roboto_condensed() 
ggplot(G21_Girls, aes(x=x) ) +
  geom_density( aes(x = MD_FEV1_gmodel_2, y = ..density..), fill="#69b3a2" ) +
  geom_label( aes(x=0, y=0.25, label="MD_FEV1_gmodel_2"), color="#69b3a2") +
  # Bottom
  geom_density( aes(x = MD_FEV1_GLI, y = -..density..), fill= "#404080") +
  geom_label( aes(x=0, y=-0.25, label="MD_FEV1_GLI"), color="#404080") +
  theme_ipsum() +
  xlab("Differences between predicted and measured FEV1")

G21_Girls <- data.frame(
  type = c( rep("G21_Girls$MD_FEV1_gmodel_2", 1000), rep("G21_Girls$MD_FEV1_GLI", 1000) ),
  value = c( rnorm(1000), rnorm(1000, mean=0) )
)
# Represent it
G21_Girls %>%
  ggplot( aes(x=value, fill=type)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")

#FEV1_gmodel_3 Age:
FEV1_gmodel_3 <- lm(FEV1_pre ~ Idade, data = ARIA_Girls)
summary(FEV1_gmodel_3)
SEL2020$FEV1_gmodel_3 <- (0.44740+(0.15227*SEL2020$Idade))
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
#CI for the variables in FEV1_gmodel_3:
confint(FEV1_gmodel_3)
#Residuals for FVC_bmodel_3:
sigma(FEV1_gmodel_3)/mean(ARIA_Girls$FEV1_pre)
sigma(FEV1_gmodel_3)/mean(G21_Girls$FEV1_pre)
#Mean differences:
SEL2020$MD_FEV1_gmodel_3 <- (SEL2020$FEV1_gmodel_3-SEL2020$FEV1_pre)
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
mean(G21_Girls$MD_FEV1_gmodel_3)
summary(G21_Girls$MD_FEV1_gmodel_3)
max(G21_Girls$MD_FEV1_gmodel_3)-min(G21_Girls$MD_FEV1_gmodel_3)
summary(G21_Girls$MD_FEV1_GLI)
max(G21_Girls$MD_FEV1_GLI)-min(G21_Girls$MD_FEV1_GLI)
#Model evaluation:
FEV1_gmodel_3_RSq_Mean_dif <- (0.004842127*(1/0.157))
FEV1_gmodel_3_RSq_Mean_dif
AIC(FEV1_gmodel_3)
BIC(FEV1_gmodel_3)
cor.test(G21_Girls$FEV1_pre, G21_Girls$FEV1_gmodel_3, method = "pearson")
cor.test(G21_Girls$FEV1_pre, G21_Girls$FEV1_GLI, method = "pearson")
#Density and bar plots with mean differences compared to GLI:
hrbrthemes::import_roboto_condensed() 
ggplot(G21_Girls, aes(x=x) ) +
  geom_density( aes(x = MD_FEV1_gmodel_3, y = ..density..), fill="#69b3a2" ) +
  geom_label( aes(x=0, y=0.25, label="MD_FEV1_gmodel_3"), color="#69b3a2") +
  # Bottom
  geom_density( aes(x = MD_FEV1_GLI, y = -..density..), fill= "#404080") +
  geom_label( aes(x=0, y=-0.25, label="MD_FEV1_GLI"), color="#404080") +
  theme_ipsum() +
  xlab("Differences between predicted and measured FEV1")

G21_Girls <- data.frame(
  type = c( rep("G21_Girls$MD_FEV1_gmodel_3", 1000), rep("G21_Girls$MD_FEV1_GLI", 1000) ),
  value = c( rnorm(1000), rnorm(1000, mean=0) )
)
# Represent it
G21_Girls %>%
  ggplot( aes(x=value, fill=type)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")

#FEV1_gmodel_4 BMI:
FEV1_gmodel_4 <- lm(FEV1_pre ~ BMI, data = ARIA_Girls)
summary(FEV1_gmodel_4)
SEL2020$FEV1_gmodel_4 <- (1.264208+(0.028429*SEL2020$BMI))
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
#CI for the variables in FEV1_gmodel_4:
confint(FEV1_gmodel_4)
#Residuals for FVC_gmodel_4:
sigma(FEV1_gmodel_4)/mean(ARIA_Girls$FEV1_pre)
sigma(FEV1_gmodel_4)/mean(G21_Girls$FEV1_pre)
#Mean differences:
SEL2020$MD_FEV1_gmodel_4 <- (SEL2020$FEV1_gmodel_4-SEL2020$FEV1_pre)
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
mean(G21_Girls$MD_FEV1_gmodel_4)
summary(G21_Girls$MD_FEV1_gmodel_4)
max(G21_Girls$MD_FEV1_gmodel_4)-min(G21_Girls$MD_FEV1_gmodel_4)
summary(G21_Girls$MD_FEV1_GLI)
max(G21_Girls$MD_FEV1_GLI)-min(G21_Girls$MD_FEV1_GLI)
#Model evaluation:
FEV1_gmodel_4_RSq_Mean_dif <- (-0.1959136*(1/0.1174))
FEV1_gmodel_4_RSq_Mean_dif
AIC(FEV1_gmodel_4)
BIC(FEV1_gmodel_4)
cor.test(G21_Girls$FEV1_pre, G21_Girls$FEV1_gmodel_4, method = "pearson")
cor.test(G21_Girls$FEV1_pre, G21_Girls$FEV1_GLI, method = "pearson")
#Density and bar plots with mean differences compared to GLI:
hrbrthemes::import_roboto_condensed() 
ggplot(G21_Girls, aes(x=x) ) +
  geom_density( aes(x = MD_FEV1_gmodel_4, y = ..density..), fill="#69b3a2" ) +
  geom_label( aes(x=0, y=0.25, label="MD_FEV1_gmodel_4"), color="#69b3a2") +
  # Bottom
  geom_density( aes(x = MD_FEV1_GLI, y = -..density..), fill= "#404080") +
  geom_label( aes(x=0, y=-0.25, label="MD_FEV1_GLI"), color="#404080") +
  theme_ipsum() +
  xlab("Differences between predicted and measured FEV1")

G21_Girls <- data.frame(
  type = c( rep("G21_Girls$MD_FEV1_gmodel_4", 1000), rep("G21_Girls$MD_FEV1_GLI", 1000) ),
  value = c( rnorm(1000), rnorm(1000, mean=0) )
)
# Represent it
G21_Girls %>%
  ggplot( aes(x=value, fill=type)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")

#FEV1_gmodel_5 Height/Weight:
FEV1_gmodel_5 <- lm(FEV1_pre ~ Altura_cm + Peso, data = ARIA_Girls)
summary(FEV1_gmodel_5)
SEL2020$FEV1_gmodel_5 <- (-0.893851+(0.017051*SEL2020$Altura_cm)+(0.010937*SEL2020$Peso))
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
#CI for the variables in FVC_gmodel_5:
confint(FEV1_gmodel_5)
#Residuals for FVC_gmodel_5:
sigma(FEV1_gmodel_5)/mean(ARIA_Girls$FEV1_pre)
sigma(FEV1_gmodel_5)/mean(G21_Girls$FEV1_pre)
#Mean differences:
SEL2020$MD_FEV1_gmodel_5 <- (SEL2020$FEV1_gmodel_5-SEL2020$FEV1_pre)
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
mean(G21_Girls$MD_FEV1_gmodel_5)
summary(G21_Girls$MD_FEV1_gmodel_5)
max(G21_Girls$MD_FEV1_gmodel_5)-min(G21_Girls$MD_FEV1_gmodel_5)
summary(G21_Girls$MD_FEV1_GLI)
max(G21_Girls$MD_FEV1_GLI)-min(G21_Girls$MD_FEV1_GLI)
#Model evaluation:
FEV1_gmodel_5_RSq_Mean_dif <- (-0.04805942*(1/0.5099))
FEV1_gmodel_5_RSq_Mean_dif
AIC(FEV1_gmodel_5)
BIC(FEV1_gmodel_5)
cor.test(G21_Girls$FEV1_pre, G21_Girls$FEV1_gmodel_5, method = "pearson")
cor.test(G21_Girls$FEV1_pre, G21_Girls$FEV1_GLI, method = "pearson")
#Density and bar plots with mean differences compared to GLI:
hrbrthemes::import_roboto_condensed() 
ggplot(G21_Girls, aes(x=x) ) +
  geom_density( aes(x = MD_FEV1_gmodel_5, y = ..density..), fill="#69b3a2" ) +
  geom_label( aes(x=0, y=0.25, label="MD_FEV1_gmodel_5"), color="#69b3a2") +
  # Bottom
  geom_density( aes(x = MD_FEV1_GLI, y = -..density..), fill= "#404080") +
  geom_label( aes(x=0, y=-0.25, label="MD_FEV1_GLI"), color="#404080") +
  theme_ipsum() +
  xlab("Differences between predicted and measured FEV1")

G21_Girls <- data.frame(
  type = c( rep("G21_Girls$MD_FEV1_gmodel_5", 1000), rep("G21_Girls$MD_FEV1_GLI", 1000) ),
  value = c( rnorm(1000), rnorm(1000, mean=0) )
)
# Represent it
G21_Girls %>%
  ggplot( aes(x=value, fill=type)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")


#FEV1_gmodel_6 Height/Age:   
FEV1_gmodel_6 <- lm(FEV1_pre ~ Altura_cm + Idade, data = ARIA_Girls)
summary(FEV1_gmodel_6)
SEL2020$FEV1_gmodel_6 <- (-1.628045+(0.022572*SEL2020$Altura_cm)+(0.040499*SEL2020$Idade))
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
#CI for the variables in FEV1_gmodel_6:
confint(FEV1_gmodel_6)
#Residuals for FVC_gmodel_6:
sigma(FEV1_gmodel_6)/mean(ARIA_Girls$FEV1_pre)
sigma(FEV1_gmodel_6)/mean(G21_Girls$FEV1_pre)
#Mean differences:
SEL2020$MD_FEV1_gmodel_6 <- (SEL2020$FEV1_gmodel_6-SEL2020$FEV1_pre)
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
mean(G21_Girls$MD_FEV1_gmodel_6)
summary(G21_Girls$MD_FEV1_gmodel_6)
max(G21_Girls$MD_FEV1_gmodel_6)-min(G21_Girls$MD_FEV1_gmodel_6)
summary(G21_Girls$MD_FEV1_GLI)
max(G21_Girls$MD_FEV1_GLI)-min(G21_Girls$MD_FEV1_GLI)
#Model evaluation:
FEV1_gmodel_6_RSq_Mean_dif <- (-0.0141975*(1/0.4594))
FEV1_gmodel_6_RSq_Mean_dif
AIC(FEV1_gmodel_6)
BIC(FEV1_gmodel_6)
cor.test(G21_Girls$FEV1_pre, G21_Girls$FEV1_gmodel_6, method = "pearson")
cor.test(G21_Girls$FEV1_pre, G21_Girls$FEV1_GLI, method = "pearson")
#Density and bar plots with mean differences compared to GLI:
hrbrthemes::import_roboto_condensed() 
ggplot(G21_Girls, aes(x=x) ) +
  geom_density( aes(x = MD_FEV1_gmodel_6, y = ..density..), fill="#69b3a2" ) +
  geom_label( aes(x=0, y=0.25, label="MD_FEV1_gmodel_6"), color="#69b3a2") +
  # Bottom
  geom_density( aes(x = MD_FEV1_GLI, y = -..density..), fill= "#404080") +
  geom_label( aes(x=0, y=-0.25, label="MD_FEV1_GLI"), color="#404080") +
  theme_ipsum() +
  xlab("Differences between predicted and measured FEV1")

G21_Girls <- data.frame(
  type = c( rep("G21_Girls$MD_FEV1_gmodel_6", 1000), rep("G21_Girls$MD_FEV1_GLI", 1000) ),
  value = c( rnorm(1000), rnorm(1000, mean=0) )
)
# Represent it
G21_Girls %>%
  ggplot( aes(x=value, fill=type)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")

#FEV1_gmodel_7 Height/BMI:   
FEV1_gmodel_7 <- lm(FEV1_pre ~ Altura_cm + BMI, data = ARIA_Girls)
summary(FEV1_gmodel_7)
SEL2020$FEV1_gmodel_7 <- (-1.737438+(0.023116*SEL2020$Altura_cm)+(0.021431*SEL2020$BMI))
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
#CI for the variables in FEV1_gmodel_7:
confint(FEV1_gmodel_7)
#Residuals for FVC_gmodel_7:
sigma(FEV1_gmodel_7)/mean(ARIA_Girls$FEV1_pre)
sigma(FEV1_gmodel_7)/mean(G21_Girls$FEV1_pre)
#Mean differences:
SEL2020$MD_FEV1_gmodel_7 <- (SEL2020$FEV1_gmodel_7-SEL2020$FEV1_pre)
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
mean(G21_Girls$MD_FEV1_gmodel_7)
summary(G21_Girls$MD_FEV1_gmodel_7)
max(G21_Girls$MD_FEV1_gmodel_7)-min(G21_Girls$MD_FEV1_gmodel_7)
summary(G21_Girls$MD_FEV1_GLI)
max(G21_Girls$MD_FEV1_GLI)-min(G21_Girls$MD_FEV1_GLI)
#Model evaluation:
FEV1_gmodel_7_RSq_Mean_dif <- (-0.04590055*(1/0.5189))
FEV1_gmodel_7_RSq_Mean_dif
AIC(FEV1_gmodel_7)
BIC(FEV1_gmodel_7)
cor.test(G21_Girls$FEV1_pre, G21_Girls$FEV1_gmodel_7, method = "pearson")
cor.test(G21_Girls$FEV1_pre, G21_Girls$FEV1_GLI, method = "pearson")
#Density and bar plots with mean differences compared to GLI:
hrbrthemes::import_roboto_condensed() 
ggplot(G21_Girls, aes(x=x) ) +
  geom_density( aes(x = MD_FEV1_gmodel_7, y = ..density..), fill="#69b3a2" ) +
  geom_label( aes(x=0, y=0.25, label="MD_FEV1_gmodel_7"), color="#69b3a2") +
  # Bottom
  geom_density( aes(x = MD_FEV1_GLI, y = -..density..), fill= "#404080") +
  geom_label( aes(x=0, y=-0.25, label="MD_FEV1_GLI"), color="#404080") +
  theme_ipsum() +
  xlab("Differences between predicted and measured FEV1")

G21_Girls <- data.frame(
  type = c( rep("G21_Girls$MD_FEV1_gmodel_7", 1000), rep("G21_Girls$MD_FEV1_GLI", 1000) ),
  value = c( rnorm(1000), rnorm(1000, mean=0) )
)
# Represent it
G21_Girls %>%
  ggplot( aes(x=value, fill=type)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")

#FEV1_gmodel_8 Height/Weight/Age:   
FEV1_gmodel_8 <- lm(FEV1_pre ~ Altura_cm + Peso + Idade, data = ARIA_Girls)
summary(FEV1_gmodel_8)
SEL2020$FEV1_gmodel_8 <- (-1.003239+(0.014659*SEL2020$Altura_cm)+(0.011288*SEL2020$Peso)+(0.048033*SEL2020$Idade))
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
#CI for the variables in FEV1_gmodel_8:
confint(FEV1_gmodel_8)
#Residuals for FVC_gmodel_8:
sigma(FEV1_gmodel_8)/mean(ARIA_Girls$FEV1_pre)
sigma(FEV1_gmodel_8)/mean(G21_Girls$FEV1_pre)
#Mean differences:
SEL2020$MD_FEV1_gmodel_8 <- (SEL2020$FEV1_gmodel_8-SEL2020$FEV1_pre)
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
mean(G21_Girls$MD_FEV1_gmodel_8)
summary(G21_Girls$MD_FEV1_gmodel_8)
max(G21_Girls$MD_FEV1_gmodel_8)-min(G21_Girls$MD_FEV1_gmodel_8)
summary(G21_Girls$MD_FEV1_GLI)
max(G21_Girls$MD_FEV1_GLI)-min(G21_Girls$MD_FEV1_GLI)
#Model evaluation:
FEV1_gmodel_8_RSq_Mean_dif <- (-0.0005649971*(1/0.5201))
FEV1_gmodel_8_RSq_Mean_dif
AIC(FEV1_gmodel_8)
BIC(FEV1_gmodel_8)
cor.test(G21_Girls$FEV1_pre, G21_Girls$FEV1_gmodel_8, method = "pearson")
cor.test(G21_Girls$FEV1_pre, G21_Girls$FEV1_GLI, method = "pearson")
#Density and bar plots with mean differences compared to GLI:
hrbrthemes::import_roboto_condensed() 
ggplot(G21_Girls, aes(x=x) ) +
  geom_density( aes(x = MD_FEV1_gmodel_8, y = ..density..), fill="#69b3a2" ) +
  geom_label( aes(x=0, y=0.25, label="MD_FEV1_gmodel_8"), color="#69b3a2") +
  # Bottom
  geom_density( aes(x = MD_FEV1_GLI, y = -..density..), fill= "#404080") +
  geom_label( aes(x=0, y=-0.25, label="MD_FEV1_GLI"), color="#404080") +
  theme_ipsum() +
  xlab("Differences between predicted and measured FEV1")

G21_Girls <- data.frame(
  type = c( rep("G21_Girls$MD_FEV1_gmodel_8", 1000), rep("G21_Girls$MD_FEV1_GLI", 1000) ),
  value = c( rnorm(1000), rnorm(1000, mean=0) )
)
# Represent it
G21_Girls %>%
  ggplot( aes(x=value, fill=type)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")

#FEV1_gmodel_9 Height/Weight/BMI:   
FEV1_gmodel_9 <- lm(FEV1_pre ~ Altura_cm + Peso + BMI, data = ARIA_Girls)
summary(FEV1_gmodel_9)
SEL2020$FEV1_gmodel_9 <- (-3.394978+(0.035499*SEL2020$Altura_cm)+(-0.021955*SEL2020$Peso)+(0.060936*SEL2020$BMI))
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
#CI for the variables in FEV1_gmodel_9:
confint(FEV1_gmodel_9)
#Residuals for FEV1_gmodel_9:
sigma(FEV1_gmodel_9)/mean(ARIA_Girls$FEV1_pre)
sigma(FEV1_gmodel_9)/mean(G21_Girls$FEV1_pre)
#Mean differences:
SEL2020$MD_FEV1_gmodel_9 <- (SEL2020$FEV1_gmodel_9-SEL2020$FEV1_pre)
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
mean(G21_Girls$MD_FEV1_gmodel_9)
summary(G21_Girls$MD_FEV1_gmodel_9)
max(G21_Girls$MD_FEV1_gmodel_9)-min(G21_Girls$MD_FEV1_gmodel_9)
summary(G21_Girls$MD_FEV1_GLI)
max(G21_Girls$MD_FEV1_GLI)-min(G21_Girls$MD_FEV1_GLI)
#Model evaluation:
SEL2020$FEV1_gmodel_9_RSq_Mean_dif <- (-0.04301126*(1/0.5235))
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
mean(G21_Girls$FEV1_gmodel_9_RSq_Mean_dif)
AIC(FEV1_gmodel_9)
BIC(FEV1_gmodel_9)
cor.test(G21_Girls$FEV1_pre, G21_Girls$FEV1_gmodel_9, method = "pearson")
cor.test(G21_Girls$FEV1_pre, G21_Girls$FEV1_GLI, method = "pearson")
#Density and bar plots with mean differences compared to GLI:
hrbrthemes::import_roboto_condensed() 
ggplot(G21_Girls, aes(x=x) ) +
  geom_density( aes(x = MD_FEV1_gmodel_9, y = ..density..), fill="#69b3a2" ) +
  geom_label( aes(x=1, y=0.25, label="MD_FEV1_gmodel_9"), color="#69b3a2") +
  # Bottom
  geom_density( aes(x = MD_FEV1_GLI, y = -..density..), fill= "#404080") +
  geom_label( aes(x=1, y=-0.25, label="MD_FEV1_GLI"), color="#404080") +
  theme_ipsum() +
  xlab("Differences between predicted and measured FEV1")

G21_Girls <- data.frame(
  type = c( rep("G21_Girls$MD_FEV1_gmodel_9", 1000), rep("G21_Girls$MD_FEV1_GLI", 1000) ),
  value = c( rnorm(1000), rnorm(1000, mean=0) )
)
# Represent it
G21_Girls %>%
  ggplot( aes(x=value, fill=type)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")


#FEV1_gmodel_10 Height/Weight/BMI/Age:   
FEV1_gmodel_10 <- lm(FEV1_pre ~ Altura_cm + Peso + BMI + Idade, data = ARIA_Girls)
summary(FEV1_gmodel_10)
SEL2020$FEV1_gmodel_10 <- (-3.012830+(0.030310*SEL2020$Altura_cm)+(-0.015603*SEL2020$Peso)+(0.049651*SEL2020$BMI)+(0.035593*SEL2020$Idade))
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
#CI for the variables in FEV1_gmodel_10:
confint(FEV1_gmodel_10)
#Residuals for FVC_gmodel_10:
sigma(FEV1_gmodel_10)/mean(ARIA_Girls$FEV1_pre)
sigma(FEV1_gmodel_10)/mean(G21_Girls$FEV1_pre)
#Mean differences:
SEL2020$MD_FEV1_gmodel_10 <- (SEL2020$FEV1_gmodel_10- SEL2020$FEV1_pre)
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
mean(G21_Girls$MD_FEV1_gmodel_10)
summary(G21_Girls$MD_FEV1_gmodel_10)
max(G21_Girls$MD_FEV1_gmodel_10)-min(G21_Girls$MD_FEV1_gmodel_10)
summary(G21_Girls$MD_FEV1_GLI)
max(G21_Girls$MD_FEV1_GLI)-min(G21_Girls$MD_FEV1_GLI)
#Model evaluation:
FEV1_gmodel_10_RSq_Mean_dif <- (-0.02621138*(1/0.5536))
FEV1_gmodel_10_RSq_Mean_dif
AIC(FEV1_gmodel_10)
BIC(FEV1_gmodel_10)
cor.test(G21_Girls$FEV1_pre, G21_Girls$FEV1_gmodel_10, method = "pearson")
cor.test(G21_Girls$FEV1_pre, G21_Girls$FEV1_GLI, method = "pearson")
#Density and bar plots with mean differences compared to GLI:
hrbrthemes::import_roboto_condensed() 
ggplot(G21_Girls, aes(x=x) ) +
  geom_density( aes(x = MD_FEV1_gmodel_10, y = ..density..), fill="#69b3a2" ) +
  geom_label( aes(x=0, y=0.25, label="MD_FEV1_gmodel_10"), color="#69b3a2") +
  # Bottom
  geom_density( aes(x = MD_FEV1_GLI, y = -..density..), fill= "#404080") +
  geom_label( aes(x=0, y=-0.25, label="MD_FEV1_GLI"), color="#404080") +
  theme_ipsum() +
  xlab("Differences between predicted and measured FEV1")

G21_Girls <- data.frame(
  type = c( rep("G21_Girls$MD_FEV1_gmodel_10", 1000), rep("G21_Girls$MD_FEV1_GLI", 1000) ),
  value = c( rnorm(1000), rnorm(1000, mean=0) )
)
# Represent it
G21_Girls %>%
  ggplot( aes(x=value, fill=type)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")


#FEV1_gmodel_11 Weight/Age:   
FEV1_gmodel_11 <- lm(FEV1_pre ~ Peso + Idade, data = ARIA_Girls)
summary(FEV1_gmodel_11)
SEL2020$FEV1_gmodel_11 <- (0.27341+(0.01911*SEL2020$Peso)+(0.09870*SEL2020$Idade))
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
#CI for the variables in FEV1_gmodel_11:
confint(FEV1_gmodel_11)
#Residuals for FEV1_gmodel_11:
sigma(FEV1_gmodel_11)/mean(ARIA_Girls$FEV1_pre)
sigma(FEV1_gmodel_11)/mean(G21_Girls$FEV1_pre)
#Mean differences:
SEL2020$MD_FEV1_gmodel_11 <- (SEL2020$FEV1_gmodel_11-SEL2020$FEV1_pre)
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
mean(G21_Girls$MD_FEV1_gmodel_11)
summary(G21_Girls$MD_FEV1_gmodel_11)
max(G21_Girls$MD_FEV1_gmodel_11)-min(G21_Girls$MD_FEV1_gmodel_11)
summary(G21_Girls$MD_FEV1_GLI)
max(G21_Girls$MD_FEV1_GLI)-min(G21_Girls$MD_FEV1_GLI)
#Model evaluation:
FEV1_gmodel_11_RSq_Mean_dif <- (0.01673203*(1/0.4415))
FEV1_gmodel_11_RSq_Mean_dif
AIC(FEV1_gmodel_11)
BIC(FEV1_gmodel_11)
cor.test(G21_Girls$FEV1_pre, G21_Girls$FEV1_gmodel_11, method = "pearson")
cor.test(G21_Girls$FEV1_pre, G21_Girls$FEV1_GLI, method = "pearson")
#Density and bar plots with mean differences compared to GLI:
hrbrthemes::import_roboto_condensed() 
ggplot(G21_Girls, aes(x=x) ) +
  geom_density( aes(x = MD_FEV1_gmodel_11, y = ..density..), fill="#69b3a2" ) +
  geom_label( aes(x=0, y=0.25, label="MD_FEV1_gmodel_11"), color="#69b3a2") +
  # Bottom
  geom_density( aes(x = MD_FEV1_GLI, y = -..density..), fill= "#404080") +
  geom_label( aes(x=0, y=-0.25, label="MD_FEV1_GLI"), color="#404080") +
  theme_ipsum() +
  xlab("Differences between predicted and measured FEV1")

G21_Girls <- data.frame(
  type = c( rep("G21_Girls$MD_FEV1_gmodel_11", 1000), rep("G21_Girls$MD_FEV1_GLI", 1000) ),
  value = c( rnorm(1000), rnorm(1000, mean=0) )
)
# Represent it
G21_Girls %>%
  ggplot( aes(x=value, fill=type)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")

#FEV1_gmodel_12 Weight/BMI:   
FEV1_gmodel_12 <- lm(FEV1_pre ~ Peso + BMI, data = ARIA_Girls)
summary(FEV1_gmodel_12)
SEL2020$FEV1_gmodel_12 <- (1.348671+(0.037467*SEL2020$Peso)+(-0.045386*SEL2020$BMI))
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
#CI for the variables in FEV1_gmodel_12:
confint(FEV1_gmodel_12)
#Residuals for FEV1_gmodel_12:
sigma(FEV1_gmodel_12)/mean(ARIA_Girls$FEV1_pre)
sigma(FEV1_gmodel_12)/mean(G21_Girls$FEV1_pre)
#Mean differences:
SEL2020$MD_FEV1_gmodel_12 <- (SEL2020$FEV1_gmodel_12-SEL2020$FEV1_pre)
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
mean(G21_Girls$MD_FEV1_gmodel_12)
summary(G21_Girls$MD_FEV1_gmodel_12)
max(G21_Girls$MD_FEV1_gmodel_12)-min(G21_Girls$MD_FEV1_gmodel_12)
summary(G21_Girls$MD_FEV1_GLI)
max(G21_Girls$MD_FEV1_GLI)-min(G21_Girls$MD_FEV1_GLI)
#Model evaluation:
FEV1_gmodel_12_RSq_Mean_dif <- (-0.06091763*(1/0.4724))
FEV1_gmodel_12_RSq_Mean_dif
AIC(FEV1_gmodel_12)
BIC(FEV1_gmodel_12)
cor.test(G21_Girls$FEV1_pre, G21_Girls$FEV1_gmodel_12, method = "pearson")
cor.test(G21_Girls$FEV1_pre, G21_Girls$FEV1_GLI, method = "pearson")
#Density and bar plots with mean differences compared to GLI:
hrbrthemes::import_roboto_condensed() 
ggplot(G21_Girls, aes(x=x) ) +
  geom_density( aes(x = MD_FEV1_gmodel_12, y = ..density..), fill="#69b3a2" ) +
  geom_label( aes(x=0, y=0.25, label="MD_FEV1_gmodel_12"), color="#69b3a2") +
  # Bottom
  geom_density( aes(x = MD_FEV1_GLI, y = -..density..), fill= "#404080") +
  geom_label( aes(x=0, y=-0.25, label="MD_FEV1_GLI"), color="#404080") +
  theme_ipsum() +
  xlab("Differences between predicted and measured FEV1")

G21_Girls <- data.frame(
  type = c( rep("G21_Girls$MD_FEV1_gmodel_12", 1000), rep("G21_Girls$MD_FEV1_GLI", 1000) ),
  value = c( rnorm(1000), rnorm(1000, mean=0) )
)
# Represent it
G21_Girls %>%
  ggplot( aes(x=value, fill=type)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")

#FEV1_gmodel_13 Weight/BMI/Age:   
FEV1_gmodel_13 <- lm(FEV1_pre ~ Peso + BMI + Idade, data = ARIA_Girls)
summary(FEV1_gmodel_13)
SEL2020$FEV1_gmodel_13 <- (0.759530+(0.033049*SEL2020$Peso)+(-0.037330*SEL2020$BMI)+(0.067380*SEL2020$Idade))
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
#CI for the variables in FEV1_gmodel_13:
confint(FEV1_gmodel_13)
#Residuals for FEV1_bmodel_13:
sigma(FEV1_gmodel_13)/mean(ARIA_Girls$FEV1_pre)
sigma(FEV1_gmodel_13)/mean(G21_Girls$FEV1_pre)
#Mean differences:
SEL2020$MD_FEV1_gmodel_13 <- (SEL2020$FEV1_gmodel_13-SEL2020$FEV1_pre)
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
mean(G21_Girls$MD_FEV1_gmodel_13)
summary(G21_Girls$MD_FEV1_gmodel_13)
max(G21_Girls$MD_FEV1_gmodel_13)-min(G21_Girls$MD_FEV1_gmodel_13)
summary(G21_Girls$MD_FEV1_GLI)
max(G21_Girls$MD_FEV1_GLI)-min(G21_Girls$MD_FEV1_GLI)
#Model evaluation:
FEV1_gmodel_13_RSq_Mean_dif <- (0.008984878*(1/0.4967))
FEV1_gmodel_13_RSq_Mean_dif
AIC(FEV1_gmodel_13)
BIC(FEV1_gmodel_13)
cor.test(G21_Girls$FEV1_pre, G21_Girls$FEV1_gmodel_13, method = "pearson")
cor.test(G21_Girls$FEV1_pre, G21_Girls$FEV1_GLI, method = "pearson")
#Density and bar plots with mean differences compared to GLI:
hrbrthemes::import_roboto_condensed() 
ggplot(G21_Girls, aes(x=x) ) +
  geom_density( aes(x = MD_FEV1_gmodel_13, y = ..density..), fill="#69b3a2" ) +
  geom_label( aes(x=0, y=0.25, label="MD_FEV1_gmodel_13"), color="#69b3a2") +
  # Bottom
  geom_density( aes(x = MD_FEV1_GLI, y = -..density..), fill= "#404080") +
  geom_label( aes(x=0, y=-0.25, label="MD_FEV1_GLI"), color="#404080") +
  theme_ipsum() +
  xlab("Differences between predicted and measured FEV1")

G21_Girls <- data.frame(
  type = c( rep("G21_Girls$MD_FEV1_gmodel_13", 1000), rep("G21_Girls$MD_FEV1_GLI", 1000) ),
  value = c( rnorm(1000), rnorm(1000, mean=0) )
)
# Represent it
G21_Girls %>%
  ggplot( aes(x=value, fill=type)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")

#FEV1_gmodel_14 BMI/Age:   
FEV1_gmodel_14 <- lm(FEV1_pre ~ BMI + Idade, data = ARIA_Girls)
summary(FEV1_gmodel_14)
SEL2020$FEV1_gmodel_14 <- (0.003665+(0.027020*SEL2020$BMI)+(0.146647*SEL2020$Idade))
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
#CI for the variables in FEV1_gmodel_14:
confint(FEV1_gmodel_14)
#Residuals for FEV1_gmodel_14:
sigma(FEV1_gmodel_14)/mean(ARIA_Girls$FEV1_pre)
sigma(FEV1_gmodel_14)/mean(G21_Girls$FEV1_pre)
#Mean differences:
SEL2020$MD_FEV1_gmodel_14 <- (SEL2020$FEV1_gmodel_14-SEL2020$FEV1_pre)
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
mean(G21_Girls$MD_FEV1_gmodel_14)
summary(G21_Girls$MD_FEV1_gmodel_14)
max(G21_Girls$MD_FEV1_gmodel_14)-min(G21_Girls$MD_FEV1_gmodel_14)
summary(G21_Girls$MD_FEV1_GLI)
max(G21_Girls$MD_FEV1_GLI)-min(G21_Girls$MD_FEV1_GLI)
#Model evaluation:
FEV1_gmodel_14_RSq_Mean_dif <- (0.003152689*(1/0.2636))
FEV1_gmodel_14_RSq_Mean_dif
AIC(FEV1_gmodel_14)
BIC(FEV1_gmodel_14)
cor.test(G21_Girls$FEV1_pre, G21_Girls$FEV1_gmodel_14, method = "pearson")
cor.test(G21_Girls$FEV1_pre, G21_Girls$FEV1_GLI, method = "pearson")
#Density and bar plots with mean differences compared to GLI:
hrbrthemes::import_roboto_condensed() 
ggplot(G21_Girls, aes(x=x) ) +
  geom_density( aes(x = MD_FEV1_gmodel_14, y = ..density..), fill="#69b3a2" ) +
  geom_label( aes(x=0, y=0.25, label="MD_FEV1_gmodel_14"), color="#69b3a2") +
  # Bottom
  geom_density( aes(x = MD_FEV1_GLI, y = -..density..), fill= "#404080") +
  geom_label( aes(x=0, y=-0.25, label="MD_FEV1_GLI"), color="#404080") +
  theme_ipsum() +
  xlab("Differences between predicted and measured FEV1")

G21_Girls <- data.frame(
  type = c( rep("G21_Girls$MD_FEV1_gmodel_14", 1000), rep("G21_Girls$MD_FEV1_GLI", 1000) ),
  value = c( rnorm(1000), rnorm(1000, mean=0) )
)
# Represent it
G21_Girls %>%
  ggplot( aes(x=value, fill=type)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")

#Multiple Linear Regression FEV1FVC Boys:

#FEV1FVC_bmodel 1 Height
FEV1FVC_bmodel_1 <- lm(FEV1FVC_pre ~ Altura_cm, data = ARIA_Boys)
summary(FEV1FVC_bmodel_1)
SEL2020$FEV1FVC_bmodel_1 <- (104.9207 + (-0.1101*SEL2020$Altura_cm))
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
#CI for the variables in FEV1FVC_bmodel_1:
confint(FEV1FVC_bmodel_1)
#Residuals for FEV1FVC_bmodel_1:
sigma(FEV1FVC_bmodel_1)/mean(ARIA_Boys$FEV1FVC_pre)
sigma(FEV1FVC_bmodel_1)/mean(G21_Boys$FEV1FVC_pre)
#Mean differences:
SEL2020$MD_FEV1FVC_bmodel_1 <- (SEL2020$FEV1FVC_bmodel_1-SEL2020$FEV1FVC_pre)
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
mean(G21_Boys$MD_FEV1FVC_bmodel_1)
summary(G21_Boys$MD_FEV1FVC_bmodel_1)
max(G21_Boys$MD_FEV1FVC_bmodel_1)-min(G21_Boys$MD_FEV1FVC_bmodel_1)
summary(G21_Boys$MD_FEV1FVC_GLI)
max(G21_Boys$MD_FEV1FVC_GLI)-min(G21_Boys$MD_FEV1FVC_GLI)
#Model evaluation:
FEV1FVC_bmodel_1_RSq_Mean_dif <- (1.387137*(1/0.02494))
FEV1FVC_bmodel_1_RSq_Mean_dif
AIC(FEV1FVC_bmodel_1)
BIC(FEV1FVC_bmodel_1)
cor.test(G21_Boys$FEV1FVC_pre, G21_Boys$FEV1FVC_bmodel_1, method = "pearson")
cor.test(G21_Boys$FEV1FVC_pre, G21_Boys$FEV1FVC_GLI, method = "pearson")
#Density and bar plots with mean differences compared to GLI:
hrbrthemes::import_roboto_condensed() 
ggplot(G21_Boys, aes(x=x) ) +
  geom_density( aes(x = MD_FEV1FVC_bmodel_1, y = ..density..), fill="#69b3a2" ) +
  geom_label( aes(x=0, y=0.25, label="MD_FEV1FVC_bmodel_1"), color="#69b3a2") +
  # Bottom
  geom_density( aes(x = MD_FEV1FVC_GLI, y = -..density..), fill= "#404080") +
  geom_label( aes(x=0, y=-0.25, label="MD_FEV1FVC_GLI"), color="#404080") +
  theme_ipsum() +
  xlab("Differences between predicted and measured FEV1FVC")

G21_Boys <- data.frame(
  type = c( rep("G21_Boys$MD_FEV1FVC_bmodel_1", 1000), rep("G21_Boys$MD_FEV1FVC_GLI", 1000) ),
  value = c( rnorm(1000), rnorm(1000, mean=0) )
)
# Represent it
G21_Boys %>%
  ggplot( aes(x=value, fill=type)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")

#FEV1FVC_bmodel_2 Height/Weight:
FEV1FVC_bmodel_2 <- lm(FEV1FVC_pre ~ Peso, data = ARIA_Boys)
summary(FEV1FVC_bmodel_2)
SEL2020$FEV1FVC_bmodel_2 <- (92.20371+(-0.06703*SEL2020$Peso))
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
#CI for the variables in FEV1FVC_bmodel_2:
confint(FEV1FVC_bmodel_2)
#Residuals for FEV1FVC_bmodel_2:
sigma(FEV1FVC_bmodel_2)/mean(ARIA_Boys$FEV1FVC_pre)
sigma(FEV1FVC_bmodel_2)/mean(G21_Boys$FEV1FVC_pre)
#Mean differences:
SEL2020$MD_FEV1FVC_bmodel_2 <- (SEL2020$FEV1FVC_bmodel_2-SEL2020$FEV1FVC_pre)
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
mean(G21_Boys$MD_FEV1FVC_bmodel_2)
summary(G21_Boys$MD_FEV1FVC_bmodel_2)
max(G21_Boys$MD_FEV1FVC_bmodel_2)-min(G21_Boys$MD_FEV1FVC_bmodel_2)
summary(G21_Boys$MD_FEV1FVC_GLI)
max(G21_Boys$MD_FEV1FVC_GLI)-min(G21_Boys$MD_FEV1FVC_GLI)
#Model evaluation:
FEV1FVC_bmodel_2_RSq_Mean_dif <- (1.660569*(1/0.01065))
FEV1FVC_bmodel_2_RSq_Mean_dif
AIC(FEV1FVC_bmodel_2)
BIC(FEV1FVC_bmodel_2)
cor.test(G21_Boys$FEV1FVC_pre, G21_Boys$FEV1FVC_bmodel_2, method = "pearson")
cor.test(G21_Boys$FEV1FVC_pre, G21_Boys$FEV1FVC_GLI, method = "pearson")
#Density and bar plots with mean differences compared to GLI:
hrbrthemes::import_roboto_condensed() 
ggplot(G21_Boys, aes(x=x) ) +
  geom_density( aes(x = MD_FEV1FVC_bmodel_2, y = ..density..), fill="#69b3a2" ) +
  geom_label( aes(x=0, y=0.25, label="MD_FEV1FVC_bmodel_2"), color="#69b3a2") +
  # Bottom
  geom_density( aes(x = MD_FEV1FVC_GLI, y = -..density..), fill= "#404080") +
  geom_label( aes(x=0, y=-0.25, label="MD_FEV1FVC_GLI"), color="#404080") +
  theme_ipsum() +
  xlab("Differences between predicted and measured FEV1FVC")

G21_Boys <- data.frame(
  type = c( rep("G21_Boys$MD_FEV1FVC_bmodel_2", 1000), rep("G21_Boys$MD_FEV1FVC_GLI", 1000) ),
  value = c( rnorm(1000), rnorm(1000, mean=0) )
)
# Represent it
G21_Boys %>%
  ggplot( aes(x=value, fill=type)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")

#FEV1FVC_bmodel_3 Age:
FEV1FVC_bmodel_3 <- lm(FEV1FVC_pre ~ Idade, data = ARIA_Boys)
summary(FEV1FVC_bmodel_3)
SEL2020$FEV1FVC_bmodel_3 <- (89.5178+(0.0501*SEL2020$Idade))
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
#CI for the variables in FEV1FVC_bmodel_3:
confint(FEV1FVC_bmodel_3)
#Residuals for FEV1FVC_bmodel_3:
sigma(FEV1FVC_bmodel_3)/mean(ARIA_Boys$FEV1FVC_pre)
sigma(FEV1FVC_bmodel_3)/mean(G21_Boys$FEV1FVC_pre)
#Mean differences:
SEL2020$MD_FEV1FVC_bmodel_3 <- (SEL2020$FEV1FVC_bmodel_3-SEL2020$FEV1FVC_pre)
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
mean(G21_Boys$MD_FEV1FVC_bmodel_3)
summary(G21_Boys$MD_FEV1FVC_bmodel_3)
max(G21_Boys$MD_FEV1FVC_bmodel_3)-min(G21_Boys$MD_FEV1FVC_bmodel_3)
summary(G21_Boys$MD_FEV1FVC_GLI)
max(G21_Boys$MD_FEV1FVC_GLI)-min(G21_Boys$MD_FEV1FVC_GLI)
#Model evaluation:
FEV1FVC_bmodel_3_RSq_Mean_dif <- (1.955282*(1/-0.003704))
FEV1FVC_bmodel_3_RSq_Mean_dif
AIC(FEV1FVC_bmodel_3)
BIC(FEV1FVC_bmodel_3)
cor.test(G21_Boys$FEV1FVC_pre, G21_Boys$FEV1FVC_bmodel_3, method = "pearson")
cor.test(G21_Boys$FEV1FVC_pre, G21_Boys$FEV1FVC_GLI, method = "pearson")
#Density and bar plots with mean differences compared to GLI:
hrbrthemes::import_roboto_condensed() 
ggplot(G21_Boys, aes(x=x) ) +
  geom_density( aes(x = MD_FEV1FVC_bmodel_3, y = ..density..), fill="#69b3a2" ) +
  geom_label( aes(x=0, y=0.25, label="MD_FEV1FVC_bmodel_3"), color="#69b3a2") +
  # Bottom
  geom_density( aes(x = MD_FEV1FVC_GLI, y = -..density..), fill= "#404080") +
  geom_label( aes(x=0, y=-0.25, label="MD_FEV1FVC_GLI"), color="#404080") +
  theme_ipsum() +
  xlab("Differences between predicted and measured FEV1FVC")

G21_Boys <- data.frame(
  type = c( rep("G21_Boys$MD_FEV1FVC_bmodel_3", 1000), rep("G21_Boys$MD_FEV1FVC_GLI", 1000) ),
  value = c( rnorm(1000), rnorm(1000, mean=0) )
)
# Represent it
G21_Boys %>%
  ggplot( aes(x=value, fill=type)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")

#FEV1FVC_bmodel_4 BMI:
FEV1FVC_bmodel_4 <- lm(FEV1FVC_pre ~ BMI, data = ARIA_Boys)
summary(FEV1FVC_bmodel_4)
SEL2020$FEV1FVC_bmodel_4 <- (91.40198+(-0.08043*SEL2020$BMI))
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
#CI for the variables in FEV1FVC_bmodel_4:
confint(FEV1FVC_bmodel_4)
#Residuals for FEV1FVC_bmodel_4:
sigma(FEV1FVC_bmodel_4)/mean(ARIA_Boys$FEV1FVC_pre)
sigma(FEV1FVC_bmodel_4)/mean(G21_Boys$FEV1FVC_pre)
#Mean differences:
SEL2020$MD_FEV1FVC_bmodel_4 <- (SEL2020$FEV1FVC_bmodel_4-SEL2020$FEV1FVC_pre)
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
mean(G21_Boys$MD_FEV1FVC_bmodel_4)
summary(G21_Boys$MD_FEV1FVC_bmodel_4)
max(G21_Boys$MD_FEV1FVC_bmodel_4)-min(G21_Boys$MD_FEV1FVC_bmodel_4)
summary(G21_Boys$MD_FEV1FVC_GLI)
max(G21_Boys$MD_FEV1FVC_GLI)-min(G21_Boys$MD_FEV1FVC_GLI)
#Model evaluation:
FEV1FVC_bmodel_4_RSq_Mean_dif <- (1.84261*(1/-0.0005703))
FEV1FVC_bmodel_4_RSq_Mean_dif
AIC(FEV1FVC_bmodel_4)
BIC(FEV1FVC_bmodel_4)
cor.test(G21_Boys$FEV1FVC_pre, G21_Boys$FEV1FVC_bmodel_4, method = "pearson")
cor.test(G21_Boys$FEV1FVC_pre, G21_Boys$FEV1FVC_GLI, method = "pearson")
#Density and bar plots with mean differences compared to GLI:
hrbrthemes::import_roboto_condensed() 
ggplot(G21_Boys, aes(x=x) ) +
  geom_density( aes(x = MD_FEV1FVC_bmodel_4, y = ..density..), fill="#69b3a2" ) +
  geom_label( aes(x=0, y=0.25, label="MD_FEV1FVC_bmodel_4"), color="#69b3a2") +
  # Bottom
  geom_density( aes(x = MD_FEV1FVC_GLI, y = -..density..), fill= "#404080") +
  geom_label( aes(x=0, y=-0.25, label="MD_FEV1FVC_GLI"), color="#404080") +
  theme_ipsum() +
  xlab("Differences between predicted and measured FEV1FVC")

G21_Boys <- data.frame(
  type = c( rep("G21_Boys$MD_FEV1FVC_bmodel_4", 1000), rep("G21_Boys$MD_FEV1FVC_GLI", 1000) ),
  value = c( rnorm(1000), rnorm(1000, mean=0) )
)
# Represent it
G21_Boys %>%
  ggplot( aes(x=value, fill=type)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")

#FEV1FVC_bmodel_5 Height/Weight:
FEV1FVC_bmodel_5 <- lm(FEV1FVC_pre ~ Altura_cm + Peso, data = ARIA_Boys)
summary(FEV1FVC_bmodel_5)
SEL2020$FEV1FVC_bmodel_5 <- (104.811315+(-0.108929*SEL2020$Altura_cm)+(-0.001414*SEL2020$Peso))
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
#CI for the variables in FEV1FVC_bmodel_5:
confint(FEV1FVC_bmodel_5)
#Residuals for FEV1FVC_bmodel_5:
sigma(FEV1FVC_bmodel_5)/mean(ARIA_Boys$FEV1FVC_pre)
sigma(FEV1FVC_bmodel_5)/mean(G21_Boys$FEV1FVC_pre)
#Mean differences:
SEL2020$MD_FEV1FVC_bmodel_5 <- (SEL2020$FEV1FVC_bmodel_5-SEL2020$FEV1FVC_pre)
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
mean(G21_Boys$MD_FEV1FVC_bmodel_5)
summary(G21_Boys$MD_FEV1FVC_bmodel_5)
max(G21_Boys$MD_FEV1FVC_bmodel_5)-min(G21_Boys$MD_FEV1FVC_bmodel_5)
summary(G21_Boys$MD_FEV1FVC_GLI)
max(G21_Boys$MD_FEV1FVC_GLI)-min(G21_Boys$MD_FEV1FVC_GLI)
#Model evaluation:
FEV1FVC_bmodel_5_RSq_Mean_dif <- (1.38997*(1/0.02125))
FEV1FVC_bmodel_5_RSq_Mean_dif
AIC(FEV1FVC_bmodel_5)
BIC(FEV1FVC_bmodel_5)
cor.test(G21_Boys$FEV1FVC_pre, G21_Boys$FEV1FVC_bmodel_5, method = "pearson")
cor.test(G21_Boys$FEV1FVC_pre, G21_Boys$FEV1FVC_GLI, method = "pearson")
#Density and bar plots with mean differences compared to GLI:
hrbrthemes::import_roboto_condensed() 
ggplot(G21_Boys, aes(x=x) ) +
  geom_density( aes(x = MD_FEV1FVC_bmodel_5, y = ..density..), fill="#69b3a2" ) +
  geom_label( aes(x=0, y=0.25, label="MD_FEV1FVC_bmodel_5"), color="#69b3a2") +
  # Bottom
  geom_density( aes(x = MD_FEV1FVC_GLI, y = -..density..), fill= "#404080") +
  geom_label( aes(x=0, y=-0.25, label="MD_FEV1FVC_GLI"), color="#404080") +
  theme_ipsum() +
  xlab("Differences between predicted and measured FEV1FVC")

G21_Boys <- data.frame(
  type = c( rep("G21_Boys$MD_FEV1FVC_bmodel_5", 1000), rep("G21_Boys$MD_FEV1FVC_GLI", 1000) ),
  value = c( rnorm(1000), rnorm(1000, mean=0) )
)
# Represent it
G21_Boys %>%
  ggplot( aes(x=value, fill=type)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")


#FEV1FVC_bmodel_6 Height/Age:   
FEV1FVC_bmodel_6 <- lm(FEV1FVC_pre ~ Altura_cm + Idade, data = ARIA_Boys)
summary(FEV1FVC_bmodel_6)
SEL2020$FEV1FVC_bmodel_6 <- (103.43803+(-0.13766*SEL2020$Altura_cm)+(0.59609*SEL2020$Idade))
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
#CI for the variables in FEV1FVC_bmodel_6:
confint(FEV1FVC_bmodel_6)
#Residuals for FEV1_bmodel_6:
sigma(FEV1FVC_bmodel_6)/mean(ARIA_Boys$FEV1FVC_pre)
sigma(FEV1FVC_bmodel_6)/mean(G21_Boys$FEV1FVC_pre)
#Mean differences:
SEL2020$MD_FEV1FVC_bmodel_6 <- (SEL2020$FEV1FVC_bmodel_6-SEL2020$FEV1FVC_pre)
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
mean(G21_Boys$MD_FEV1FVC_bmodel_6)
summary(G21_Boys$MD_FEV1FVC_bmodel_6)
max(G21_Boys$MD_FEV1FVC_bmodel_6)-min(G21_Boys$MD_FEV1FVC_bmodel_6)
summary(G21_Boys$MD_FEV1FVC_GLI)
max(G21_Boys$MD_FEV1FVC_GLI)-min(G21_Boys$MD_FEV1FVC_GLI)
#Model evaluation:
FEV1FVC_bmodel_6_RSq_Mean_dif <- (1.980671*(1/0.0293))
FEV1FVC_bmodel_6_RSq_Mean_dif
AIC(FEV1FVC_bmodel_6)
BIC(FEV1FVC_bmodel_6)
cor.test(G21_Boys$FEV1FVC_pre, G21_Boys$FEV1FVC_bmodel_6, method = "pearson")
cor.test(G21_Boys$FEV1FVC_pre, G21_Boys$FEV1FVC_GLI, method = "pearson")
#Density and bar plots with mean differences compared to GLI:
hrbrthemes::import_roboto_condensed() 
ggplot(G21_Boys, aes(x=x) ) +
  geom_density( aes(x = MD_FEV1FVC_bmodel_6, y = ..density..), fill="#69b3a2" ) +
  geom_label( aes(x=0, y=0.25, label="MD_FEV1FVC_bmodel_6"), color="#69b3a2") +
  # Bottom
  geom_density( aes(x = MD_FEV1FVC_GLI, y = -..density..), fill= "#404080") +
  geom_label( aes(x=0, y=-0.25, label="MD_FEV1FVC_GLI"), color="#404080") +
  theme_ipsum() +
  xlab("Differences between predicted and measured FEV1FVC")

G21_Boys <- data.frame(
  type = c( rep("G21_Boys$MD_FEV1FVC_bmodel_6", 1000), rep("G21_Boys$MD_FEV1FVC_GLI", 1000) ),
  value = c( rnorm(1000), rnorm(1000, mean=0) )
)
# Represent it
G21_Boys %>%
  ggplot( aes(x=value, fill=type)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")

#FEV1FVC_bmodel_7 Height/BMI:   
FEV1FVC_bmodel_7 <- lm(FEV1FVC_pre ~ Altura_cm + BMI, data = ARIA_Boys)
summary(FEV1FVC_bmodel_7)
SEL2020$FEV1FVC_bmodel_7 <- (104.963383+(-0.111574*SEL2020$Altura_cm)+(0.008914*SEL2020$BMI))
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
#CI for the variables in FEV1FVC_bmodel_7:
confint(FEV1FVC_bmodel_7)
#Residuals for FEV1FVC_bmodel_7:
sigma(FEV1FVC_bmodel_7)/mean(ARIA_Boys$FEV1FVC_pre)
sigma(FEV1FVC_bmodel_7)/mean(G21_Boys$FEV1FVC_pre)
#Mean differences:
SEL2020$MD_FEV1FVC_bmodel_7 <- (SEL2020$FEV1FVC_bmodel_7-SEL2020$FEV1FVC_pre)
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
mean(G21_Boys$MD_FEV1FVC_bmodel_7)
summary(G21_Boys$MD_FEV1FVC_bmodel_7)
max(G21_Boys$MD_FEV1FVC_bmodel_7)-min(G21_Boys$MD_FEV1FVC_bmodel_7)
summary(G21_Boys$MD_FEV1FVC_GLI)
max(G21_Boys$MD_FEV1FVC_GLI)-min(G21_Boys$MD_FEV1FVC_GLI)
#Model evaluation:
FEV1FVC_bmodel_7_RSq_Mean_dif <- (1.388593*(1/0.02128))
FEV1FVC_bmodel_7_RSq_Mean_dif
AIC(FEV1FVC_bmodel_7)
BIC(FEV1FVC_bmodel_7)
cor.test(G21_Boys$FEV1FVC_pre, G21_Boys$FEV1FVC_bmodel_7, method = "pearson")
cor.test(G21_Boys$FEV1FVC_pre, G21_Boys$FEV1FVC_GLI, method = "pearson")
#Density and bar plots with mean differences compared to GLI:
hrbrthemes::import_roboto_condensed() 
ggplot(G21_Boys, aes(x=x) ) +
  geom_density( aes(x = MD_FEV1FVC_bmodel_7, y = ..density..), fill="#69b3a2" ) +
  geom_label( aes(x=0, y=0.25, label="MD_FEV1FVC_bmodel_7"), color="#69b3a2") +
  # Bottom
  geom_density( aes(x = MD_FEV1FVC_GLI, y = -..density..), fill= "#404080") +
  geom_label( aes(x=0, y=-0.25, label="MD_FEV1FVC_GLI"), color="#404080") +
  theme_ipsum() +
  xlab("Differences between predicted and measured FEV1FVC")

G21_Boys <- data.frame(
  type = c( rep("G21_Boys$MD_FEV1FVC_bmodel_7", 1000), rep("G21_Boys$MD_FEV1FVC_GLI", 1000) ),
  value = c( rnorm(1000), rnorm(1000, mean=0) )
)
# Represent it
G21_Boys %>%
  ggplot( aes(x=value, fill=type)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")

#FEV1FVC_bmodel_8 Height/Weight/Age:   
FEV1FVC_bmodel_8 <- lm(FEV1FVC_pre ~ Altura_cm + Peso + Idade, data = ARIA_Boys)
summary(FEV1FVC_bmodel_8)
SEL2020$FEV1FVC_bmodel_8 <- (102.783195+(-0.131242*SEL2020$Altura_cm)+(-0.008246*SEL2020$Peso)+(0.602845*SEL2020$Idade))
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
#CI for the variables in FEV1FVC_bmodel_8:
confint(FEV1FVC_bmodel_8)
#Residuals for FEV1FVC_bmodel_8:
sigma(FEV1FVC_bmodel_8)/mean(ARIA_Boys$FEV1FVC_pre)
sigma(FEV1FVC_bmodel_8)/mean(G21_Boys$FEV1FVC_pre)
#Mean differences:
SEL2020$MD_FEV1FVC_bmodel_8 <- (SEL2020$FEV1FVC_bmodel_8-SEL2020$FEV1FVC_pre)
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
mean(G21_Boys$MD_FEV1FVC_bmodel_8)
summary(G21_Boys$MD_FEV1FVC_bmodel_8)
max(G21_Boys$MD_FEV1FVC_bmodel_8)-min(G21_Boys$MD_FEV1FVC_bmodel_8)
summary(G21_Boys$MD_FEV1FVC_GLI)
max(G21_Boys$MD_FEV1FVC_GLI)-min(G21_Boys$MD_FEV1FVC_GLI)
#Model evaluation:
FEV1FVC_bmodel_8_RSq_Mean_dif <- (1.989929*(1/0.02572))
FEV1FVC_bmodel_8_RSq_Mean_dif
AIC(FEV1FVC_bmodel_8)
BIC(FEV1FVC_bmodel_8)
cor.test(G21_Boys$FEV1FVC_pre, G21_Boys$FEV1FVC_bmodel_8, method = "pearson")
cor.test(G21_Boys$FEV1FVC_pre, G21_Boys$FEV1FVC_GLI, method = "pearson")
#Density and bar plots with mean differences compared to GLI:
hrbrthemes::import_roboto_condensed() 
ggplot(G21_Boys, aes(x=x) ) +
  geom_density( aes(x = MD_FEV1FVC_bmodel_8, y = ..density..), fill="#69b3a2" ) +
  geom_label( aes(x=0, y=0.25, label="MD_FEV1FVC_bmodel_8"), color="#69b3a2") +
  # Bottom
  geom_density( aes(x = MD_FEV1FVC_GLI, y = -..density..), fill= "#404080") +
  geom_label( aes(x=0, y=-0.25, label="MD_FEV1FVC_GLI"), color="#404080") +
  theme_ipsum() +
  xlab("Differences between predicted and measured FEV1FVC")

G21_Boys <- data.frame(
  type = c( rep("G21_Boys$MD_FEV1FVC_bmodel_8", 1000), rep("G21_Boys$MD_FEV1FVC_GLI", 1000) ),
  value = c( rnorm(1000), rnorm(1000, mean=0) )
)
# Represent it
G21_Boys %>%
  ggplot( aes(x=value, fill=type)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")

#FEV1FVC_bmodel_9 Height/Weight/BMI:   
FEV1FVC_bmodel_9 <- lm(FEV1FVC_pre ~ Altura_cm + Peso + BMI, data = ARIA_Boys)
summary(FEV1FVC_bmodel_9)
SEL2020$FEV1FVC_bmodel_9 <- (73.7495+(0.1137*SEL2020$Altura_cm)+(-0.4584*SEL2020$Peso)+(0.8973*SEL2020$BMI))
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
#CI for the variables in FEV1FVC_bmodel_9:
confint(FEV1FVC_bmodel_9)
#Residuals for FEV1FVC_bmodel_9:
sigma(FEV1FVC_bmodel_9)/mean(ARIA_Boys$FEV1FVC_pre)
sigma(FEV1FVC_bmodel_9)/mean(G21_Boys$FEV1FVC_pre)
#Mean differences:
SEL2020$MD_FEV1FVC_bmodel_9 <- (SEL2020$FEV1FVC_bmodel_9-SEL2020$FEV1FVC_pre)
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
mean(G21_Boys$MD_FEV1FVC_bmodel_9)
summary(G21_Boys$MD_FEV1FVC_bmodel_9)
max(G21_Boys$MD_FEV1FVC_bmodel_9)-min(G21_Boys$MD_FEV1FVC_bmodel_9)
summary(G21_Boys$MD_FEV1FVC_GLI)
max(G21_Boys$MD_FEV1FVC_GLI)-min(G21_Boys$MD_FEV1FVC_GLI)
#Model evaluation:
SEL2020$FEV1FVC_bmodel_9_RSq_Mean_dif <- (1.39895*(1/0.02205))
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
mean(G21_Boys$FEV1FVC_bmodel_9_RSq_Mean_dif)
AIC(FEV1FVC_bmodel_9)
BIC(FEV1FVC_bmodel_9)
cor.test(G21_Boys$FEV1FVC_pre, G21_Boys$FEV1FVC_bmodel_9, method = "pearson")
cor.test(G21_Boys$FEV1FVC_pre, G21_Boys$FEV1FVC_GLI, method = "pearson")
#Density and bar plots with mean differences compared to GLI:
hrbrthemes::import_roboto_condensed() 
ggplot(G21_Boys, aes(x=x) ) +
  geom_density( aes(x = MD_FEV1FVC_bmodel_9, y = ..density..), fill="#69b3a2" ) +
  geom_label( aes(x=1, y=0.25, label="MD_FEV1FVC_bmodel_9"), color="#69b3a2") +
  # Bottom
  geom_density( aes(x = MD_FEV1FVC_GLI, y = -..density..), fill= "#404080") +
  geom_label( aes(x=1, y=-0.25, label="MD_FEV1FVC_GLI"), color="#404080") +
  theme_ipsum() +
  xlab("Differences between predicted and measured FEV1FVC")

G21_Boys <- data.frame(
  type = c( rep("G21_Boys$MD_FEV1FVC_bmodel_9", 1000), rep("G21_Boys$MD_FEV1FVC_GLI", 1000) ),
  value = c( rnorm(1000), rnorm(1000, mean=0) )
)
# Represent it
G21_Boys %>%
  ggplot( aes(x=value, fill=type)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")


#FEV1FVC_bmodel_10 Height/Weight/BMI/Age:   
FEV1FVC_bmodel_10 <- lm(FEV1FVC_pre ~ Altura_cm + Peso + BMI + Idade, data = ARIA_Boys)
summary(FEV1FVC_bmodel_10)
SEL2020$FEV1FVC_bmodel_10 <- (72.81736+(0.08419*SEL2020$Altura_cm)+(-0.44949*SEL2020$Peso)+(0.86669*SEL2020$BMI)+(0.59169*SEL2020$Idade))
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
#CI for the variables in FEV1FVC_bmodel_10:
confint(FEV1FVC_bmodel_10)
#Residuals for FEV1FVC_bmodel_10:
sigma(FEV1FVC_bmodel_10)/mean(ARIA_Boys$FEV1FVC_pre)
sigma(FEV1FVC_bmodel_10)/mean(G21_Boys$FEV1FVC_pre)
#Mean differences:
SEL2020$MD_FEV1FVC_bmodel_10 <- (SEL2020$FEV1FVC_bmodel_10-SEL2020$FEV1FVC_pre)
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
mean(G21_Boys$MD_FEV1FVC_bmodel_10)
summary(G21_Boys$MD_FEV1FVC_bmodel_10)
max(G21_Boys$MD_FEV1FVC_bmodel_10)-min(G21_Boys$MD_FEV1FVC_bmodel_10)
summary(G21_Boys$MD_FEV1FVC_GLI)
max(G21_Boys$MD_FEV1FVC_GLI)-min(G21_Boys$MD_FEV1FVC_GLI)
#Model evaluation:
FEV1FVC_bmodel_10_RSq_Mean_dif <- (1.985157*(1/0.02623))
FEV1FVC_bmodel_10_RSq_Mean_dif
AIC(FEV1FVC_bmodel_10)
BIC(FEV1FVC_bmodel_10)
cor.test(G21_Boys$FEV1FVC_pre, G21_Boys$FEV1FVC_bmodel_10, method = "pearson")
cor.test(G21_Boys$FEV1FVC_pre, G21_Boys$FEV1FVC_GLI, method = "pearson")
#Density and bar plots with mean differences compared to GLI:
hrbrthemes::import_roboto_condensed() 
ggplot(G21_Boys, aes(x=x) ) +
  geom_density( aes(x = MD_FEV1FVC_bmodel_10, y = ..density..), fill="#69b3a2" ) +
  geom_label( aes(x=0, y=0.25, label="MD_FEV1FVC_bmodel_10"), color="#69b3a2") +
  # Bottom
  geom_density( aes(x = MD_FEV1FVC_GLI, y = -..density..), fill= "#404080") +
  geom_label( aes(x=0, y=-0.25, label="MD_FEV1FVC_GLI"), color="#404080") +
  theme_ipsum() +
  xlab("Differences between predicted and measured FEV1FVC")

G21_Boys <- data.frame(
  type = c( rep("G21_Boys$MD_FEV1FVC_bmodel_10", 1000), rep("G21_Boys$MD_FEV1FVC_GLI", 1000) ),
  value = c( rnorm(1000), rnorm(1000, mean=0) )
)
# Represent it
G21_Boys %>%
  ggplot( aes(x=value, fill=type)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")


#FEV1FVC_bmodel_11 Weight/Age:   
FEV1FVC_bmodel_11 <- lm(FEV1FVC_pre ~ Peso + Idade, data = ARIA_Boys)
summary(FEV1FVC_bmodel_11)
SEL2020$FEV1FVC_bmodel_11 <- (89.45811+(-0.07910*SEL2020$Peso)+(0.35898*SEL2020$Idade))
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
#CI for the variables in FEV1FVC_bmodel_11:
confint(FEV1FVC_bmodel_11)
#Residuals for FEV1FVC_bmodel_11:
sigma(FEV1FVC_bmodel_11)/mean(ARIA_Boys$FEV1FVC_pre)
sigma(FEV1FVC_bmodel_11)/mean(G21_Boys$FEV1FVC_pre)
#Mean differences:
SEL2020$MD_FEV1FVC_bmodel_11 <- (SEL2020$FEV1FVC_bmodel_11-SEL2020$FEV1FVC_pre)
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
mean(G21_Boys$MD_FEV1FVC_bmodel_11)
summary(G21_Boys$MD_FEV1FVC_bmodel_11)
max(G21_Boys$MD_FEV1FVC_bmodel_11)-min(G21_Boys$MD_FEV1FVC_bmodel_11)
summary(G21_Boys$MD_FEV1FVC_GLI)
max(G21_Boys$MD_FEV1FVC_GLI)-min(G21_Boys$MD_FEV1FVC_GLI)
#Model evaluation:
FEV1FVC_bmodel_11_RSq_Mean_dif <- (2.050849*(1/0.01))
FEV1FVC_bmodel_11_RSq_Mean_dif
AIC(FEV1FVC_bmodel_11)
BIC(FEV1FVC_bmodel_11)
cor.test(G21_Boys$FEV1FVC_pre, G21_Boys$FEV1FVC_bmodel_11, method = "pearson")
cor.test(G21_Boys$FEV1FVC_pre, G21_Boys$FEV1FVC_GLI, method = "pearson")
#Density and bar plots with mean differences compared to GLI:
hrbrthemes::import_roboto_condensed() 
ggplot(G21_Boys, aes(x=x) ) +
  geom_density( aes(x = MD_FEV1FVC_bmodel_11, y = ..density..), fill="#69b3a2" ) +
  geom_label( aes(x=0, y=0.25, label="MD_FEV1FVC_bmodel_11"), color="#69b3a2") +
  # Bottom
  geom_density( aes(x = MD_FEV1FVC_GLI, y = -..density..), fill= "#404080") +
  geom_label( aes(x=0, y=-0.25, label="MD_FEV1FVC_GLI"), color="#404080") +
  theme_ipsum() +
  xlab("Differences between predicted and measured FEV1FVC")

G21_Boys <- data.frame(
  type = c( rep("G21_Boys$MD_FEV1FVC_bmodel_11", 1000), rep("G21_Boys$MD_FEV1FVC_GLI", 1000) ),
  value = c( rnorm(1000), rnorm(1000, mean=0) )
)
# Represent it
G21_Boys %>%
  ggplot( aes(x=value, fill=type)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")

#FEV1FVC_bmodel_12 Weight/BMI:   
FEV1FVC_bmodel_12 <- lm(FEV1FVC_pre ~ Peso + BMI, data = ARIA_Boys)
summary(FEV1FVC_bmodel_12)
SEL2020$FEV1FVC_bmodel_12 <- (89.42276+(-0.23652*SEL2020$Peso)+(0.47107*SEL2020$BMI))
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
#CI for the variables in FEV1FVC_bmodel_12:
confint(FEV1FVC_bmodel_12)
#Residuals for FEV1FVC_bmodel_12:
sigma(FEV1FVC_bmodel_12)/mean(ARIA_Boys$FEV1FVC_pre)
sigma(FEV1FVC_bmodel_12)/mean(G21_Boys$FEV1FVC_pre)
#Mean differences:
SEL2020$MD_FEV1FVC_bmodel_12 <- (SEL2020$FEV1FVC_bmodel_12-SEL2020$FEV1FVC_pre)
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
mean(G21_Boys$MD_FEV1FVC_bmodel_12)
summary(G21_Boys$MD_FEV1FVC_bmodel_12)
max(G21_Boys$MD_FEV1FVC_bmodel_12)-min(G21_Boys$MD_FEV1FVC_bmodel_12)
summary(G21_Boys$MD_FEV1FVC_GLI)
max(G21_Boys$MD_FEV1FVC_GLI)-min(G21_Boys$MD_FEV1FVC_GLI)
#Model evaluation:
FEV1FVC_bmodel_12_RSq_Mean_dif <- (1.374127*(1/0.02466))
FEV1FVC_bmodel_12_RSq_Mean_dif
AIC(FEV1FVC_bmodel_12)
BIC(FEV1FVC_bmodel_12)
cor.test(G21_Boys$FEV1FVC_pre, G21_Boys$FEV1FVC_bmodel_12, method = "pearson")
cor.test(G21_Boys$FEV1FVC_pre, G21_Boys$FEV1FVC_GLI, method = "pearson")
#Density and bar plots with mean differences compared to GLI:
hrbrthemes::import_roboto_condensed() 
ggplot(G21_Boys, aes(x=x) ) +
  geom_density( aes(x = MD_FEV1FVC_bmodel_12, y = ..density..), fill="#69b3a2" ) +
  geom_label( aes(x=0, y=0.25, label="MD_FEV1FVC_bmodel_12"), color="#69b3a2") +
  # Bottom
  geom_density( aes(x = MD_FEV1FVC_GLI, y = -..density..), fill= "#404080") +
  geom_label( aes(x=0, y=-0.25, label="MD_FEV1FVC_GLI"), color="#404080") +
  theme_ipsum() +
  xlab("Differences between predicted and measured FEV1FVC")

G21_Boys <- data.frame(
  type = c( rep("G21_Boys$MD_FEV1FVC_bmodel_12", 1000), rep("G21_Boys$MD_FEV1FVC_GLI", 1000) ),
  value = c( rnorm(1000), rnorm(1000, mean=0) )
)
# Represent it
G21_Boys %>%
  ggplot( aes(x=value, fill=type)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")

#FEV1FVC_bmodel_13 Weight/BMI/Age:   
FEV1FVC_bmodel_13 <- lm(FEV1FVC_pre ~ Peso + BMI + Idade, data = ARIA_Boys)
summary(FEV1FVC_bmodel_13)
SEL2020$FEV1FVC_bmodel_13 <- (84.29294+(-0.28649*SEL2020$Peso)+(0.55316*SEL2020$BMI)+(0.60735*SEL2020$Idade))
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
#CI for the variables in FEV1FVC_bmodel_13:
confint(FEV1FVC_bmodel_13)
#Residuals for FEV1FVC_bmodel_13:
sigma(FEV1FVC_bmodel_13)/mean(ARIA_Boys$FEV1FVC_pre)
sigma(FEV1FVC_bmodel_13)/mean(G21_Boys$FEV1FVC_pre)
#Mean differences:
SEL2020$MD_FEV1FVC_bmodel_13 <- (SEL2020$FEV1FVC_bmodel_13-SEL2020$FEV1FVC_pre)
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
mean(G21_Boys$MD_FEV1FVC_bmodel_13)
summary(G21_Boys$MD_FEV1FVC_bmodel_13)
max(G21_Boys$MD_FEV1FVC_bmodel_13)-min(G21_Boys$MD_FEV1FVC_bmodel_13)
summary(G21_Boys$MD_FEV1FVC_GLI)
max(G21_Boys$MD_FEV1FVC_GLI)-min(G21_Boys$MD_FEV1FVC_GLI)
#Model evaluation:
FEV1FVC_bmodel_13_RSq_Mean_dif <- (1.984041*(1/0.02934))
FEV1FVC_bmodel_13_RSq_Mean_dif
AIC(FEV1FVC_bmodel_13)
BIC(FEV1FVC_bmodel_13)
cor.test(G21_Boys$FEV1FVC_pre, G21_Boys$FEV1FVC_bmodel_13, method = "pearson")
cor.test(G21_Boys$FEV1FVC_pre, G21_Boys$FEV1FVC_GLI, method = "pearson")
#Density and bar plots with mean differences compared to GLI:
hrbrthemes::import_roboto_condensed() 
ggplot(G21_Boys, aes(x=x) ) +
  geom_density( aes(x = MD_FEV1FVC_bmodel_13, y = ..density..), fill="#69b3a2" ) +
  geom_label( aes(x=0, y=0.25, label="MD_FEV1FVC_bmodel_13"), color="#69b3a2") +
  # Bottom
  geom_density( aes(x = MD_FEV1FVC_GLI, y = -..density..), fill= "#404080") +
  geom_label( aes(x=0, y=-0.25, label="MD_FEV1FVC_GLI"), color="#404080") +
  theme_ipsum() +
  xlab("Differences between predicted and measured FEV1FVC")

G21_Boys <- data.frame(
  type = c( rep("G21_Boys$MD_FEV1FVC_bmodel_13", 1000), rep("G21_Boys$MD_FEV1FVC_GLI", 1000) ),
  value = c( rnorm(1000), rnorm(1000, mean=0) )
)
# Represent it
G21_Boys %>%
  ggplot( aes(x=value, fill=type)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")

#FEV1FVC_bmodel_14 BMI/Age:   
FEV1FVC_bmodel_14 <- lm(FEV1FVC_pre ~ BMI + Idade, data = ARIA_Boys)
summary(FEV1FVC_bmodel_14)
SEL2020$FEV1FVC_bmodel_14 <- (90.31774+(-0.08835*SEL2020$BMI)+(0.13976*SEL2020$Idade))
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
#CI for the variables in FEV1FVC_bmodel_14:
confint(FEV1FVC_bmodel_14)
#Residuals for FEV1FVC_bmodel_14:
sigma(FEV1FVC_bmodel_14)/mean(ARIA_Boys$FEV1FVC_pre)
sigma(FEV1FVC_bmodel_14)/mean(G21_Boys$FEV1FVC_pre)
#Mean differences:
SEL2020$MD_FEV1FVC_bmodel_14 <- (SEL2020$FEV1FVC_bmodel_14-SEL2020$FEV1FVC_pre)
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
mean(G21_Boys$MD_FEV1FVC_bmodel_14)
summary(G21_Boys$MD_FEV1FVC_bmodel_14)
max(G21_Boys$MD_FEV1FVC_bmodel_14)-min(G21_Boys$MD_FEV1FVC_bmodel_14)
summary(G21_Boys$MD_FEV1FVC_GLI)
max(G21_Boys$MD_FEV1FVC_GLI)-min(G21_Boys$MD_FEV1FVC_GLI)
#Model evaluation:
FEV1FVC_bmodel_14_RSq_Mean_dif <- (2.005756*(1/-0.00385))
FEV1FVC_bmodel_14_RSq_Mean_dif
AIC(FEV1FVC_bmodel_14)
BIC(FEV1FVC_bmodel_14)
cor.test(G21_Boys$FEV1FVC_pre, G21_Boys$FEV1FVC_bmodel_14, method = "pearson")
cor.test(G21_Boys$FEV1FVC_pre, G21_Boys$FEV1FVC_GLI, method = "pearson")
#Density and bar plots with mean differences compared to GLI:
hrbrthemes::import_roboto_condensed() 
ggplot(G21_Boys, aes(x=x) ) +
  geom_density( aes(x = MD_FEV1FVC_bmodel_14, y = ..density..), fill="#69b3a2" ) +
  geom_label( aes(x=0, y=0.25, label="MD_FEV1FVC_bmodel_14"), color="#69b3a2") +
  # Bottom
  geom_density( aes(x = MD_FEV1FVC_GLI, y = -..density..), fill= "#404080") +
  geom_label( aes(x=0, y=-0.25, label="MD_FEV1FVC_GLI"), color="#404080") +
  theme_ipsum() +
  xlab("Differences between predicted and measured FEV1FVC")

G21_Boys <- data.frame(
  type = c( rep("G21_Boys$MD_FEV1FVC_bmodel_14", 1000), rep("G21_Boys$MD_FEV1FVC_GLI", 1000) ),
  value = c( rnorm(1000), rnorm(1000, mean=0) )
)
# Represent it
G21_Boys %>%
  ggplot( aes(x=value, fill=type)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")

#Multiple Linear Regression FEV1FVC Girls: 

#FEV1FVC_gmodel 1 Height
FEV1FVC_gmodel_1 <- lm(FEV1FVC_pre ~ Altura_cm, data = ARIA_Girls)
summary(FEV1FVC_gmodel_1)
SEL2020$FEV1FVC_gmodel_1 <- (95.90393+(-0.03760*SEL2020$Altura_cm))
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
#CI for the variables in FEV1FVC_gmodel_1:
confint(FEV1FVC_gmodel_1)
#Residuals for FEV1FVC_gmodel_1:
sigma(FEV1FVC_gmodel_1)/mean(ARIA_Girls$FEV1FVC_pre)
sigma(FEV1FVC_gmodel_1)/mean(G21_Girls$FEV1FVC_pre)
#Mean differences:
SEL2020$MD_FEV1FVC_gmodel_1 <- (SEL2020$FEV1FVC_gmodel_1-SEL2020$FEV1FVC_pre)
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
mean(G21_Girls$MD_FEV1FVC_gmodel_1)
summary(G21_Girls$MD_FEV1FVC_gmodel_1)
max(G21_Girls$MD_FEV1FVC_gmodel_1)-min(G21_Girls$MD_FEV1FVC_gmodel_1)
summary(G21_Girls$MD_FEV1FVC_GLI)
max(G21_Girls$MD_FEV1FVC_GLI)-min(G21_Girls$MD_FEV1FVC_GLI)
#Model evaluation:
FEV1FVC_gmodel_1_RSq_Mean_dif <- (0.3836201*(1/-0.0001908))
FEV1FVC_gmodel_1_RSq_Mean_dif
AIC(FEV1FVC_gmodel_1)
BIC(FEV1FVC_gmodel_1)
cor.test(G21_Girls$FEV1FVC_pre, G21_Girls$FEV1FVC_gmodel_1, method = "pearson")
cor.test(G21_Girls$FEV1FVC_pre, G21_Girls$FEV1FVC_GLI, method = "pearson")
#Density and bar plots with mean differences compared to GLI:
hrbrthemes::import_roboto_condensed() 
ggplot(G21_Girls, aes(x=x) ) +
  geom_density( aes(x = MD_FEV1FVC_gmodel_1, y = ..density..), fill="#69b3a2" ) +
  geom_label( aes(x=0, y=0.25, label="MD_FEV1FVC_gmodel_1"), color="#69b3a2") +
  # Bottom
  geom_density( aes(x = MD_FEV1FVC_GLI, y = -..density..), fill= "#404080") +
  geom_label( aes(x=0, y=-0.25, label="MD_FEV1FVC_GLI"), color="#404080") +
  theme_ipsum() +
  xlab("Differences between predicted and measured FEV1/FVC")

G21_Girls <- data.frame(
  type = c( rep("G21_Girls$MD_FEV1FVC_gmodel_1", 1000), rep("G21_Girls$MD_FEV1FVC_GLI", 1000) ),
  value = c( rnorm(1000), rnorm(1000, mean=0) )
)
# Represent it
G21_Girls %>%
  ggplot( aes(x=value, fill=type)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")

#FEV1FVC_gmodel_2 Weight:
FEV1FVC_gmodel_2 <- lm(FEV1FVC_pre ~ Peso, data = ARIA_Girls)
summary(FEV1FVC_gmodel_2)
SEL2020$FEV1FVC_gmodel_2 <- (91.141131+(-0.009703*SEL2020$Peso))
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
#CI for the variables in FEV1FVC_gmodel_2:
confint(FEV1FVC_gmodel_2)
#Residuals for FVCFVC_gmodel_2:
sigma(FEV1FVC_gmodel_2)/mean(ARIA_Girls$FEV1FVC_pre)
sigma(FEV1FVC_gmodel_2)/mean(G21_Girls$FEV1FVC_pre)
#Mean differences:
SEL2020$MD_FEV1FVC_gmodel_2 <- (SEL2020$FEV1FVC_gmodel_2-SEL2020$FEV1FVC_pre)
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
mean(G21_Girls$MD_FEV1FVC_gmodel_2)
summary(G21_Girls$MD_FEV1FVC_gmodel_2)
max(G21_Girls$MD_FEV1FVC_gmodel_2)-min(G21_Girls$MD_FEV1FVC_gmodel_2)
summary(G21_Girls$MD_FEV1FVC_GLI)
max(G21_Girls$MD_FEV1FVC_GLI)-min(G21_Girls$MD_FEV1FVC_GLI)
#Model evaluation:
FEV1FVC_gmodel_2_RSq_Mean_dif <- (0.5403952*(1/-0.004387))
FEV1FVC_gmodel_2_RSq_Mean_dif
AIC(FEV1FVC_gmodel_2)
BIC(FEV1FVC_gmodel_2)
cor.test(G21_Girls$FEV1FVC_pre, G21_Girls$FEV1FVC_gmodel_2, method = "pearson")
cor.test(G21_Girls$FEV1FVC_pre, G21_Girls$FEV1FVC_GLI, method = "pearson")
#Density and bar plots with mean differences compared to GLI:
hrbrthemes::import_roboto_condensed() 
ggplot(G21_Girls, aes(x=x) ) +
  geom_density( aes(x = MD_FEV1FVC_gmodel_2, y = ..density..), fill="#69b3a2" ) +
  geom_label( aes(x=0, y=0.25, label="MD_FEV1FVC_gmodel_2"), color="#69b3a2") +
  # Bottom
  geom_density( aes(x = MD_FEV1FVC_GLI, y = -..density..), fill= "#404080") +
  geom_label( aes(x=0, y=-0.25, label="MD_FEV1FVC_GLI"), color="#404080") +
  theme_ipsum() +
  xlab("Differences between predicted and measured FEV1/FVC")

G21_Girls <- data.frame(
  type = c( rep("G21_Girls$MD_FEV1FVC_gmodel_2", 1000), rep("G21_Girls$MD_FEV1FVC_GLI", 1000) ),
  value = c( rnorm(1000), rnorm(1000, mean=0) )
)
# Represent it
G21_Girls %>%
  ggplot( aes(x=value, fill=type)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")

#FEV1FVC_gmodel_3 Age:
FEV1FVC_gmodel_3 <- lm(FEV1FVC_pre ~ Idade, data = ARIA_Girls)
summary(FEV1FVC_gmodel_3)
SEL2020$FEV1FVC_gmodel_3 <- (91.9970+(-0.1349*SEL2020$Idade))
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
#CI for the variables in FEV1FVC_gmodel_3:
confint(FEV1FVC_gmodel_3)
#Residuals for FEV1FVC_bmodel_3:
sigma(FEV1FVC_gmodel_3)/mean(ARIA_Girls$FEV1FVC_pre)
sigma(FEV1FVC_gmodel_3)/mean(G21_Girls$FEV1FVC_pre)
#Mean differences:
SEL2020$MD_FEV1FVC_gmodel_3 <- (SEL2020$FEV1FVC_gmodel_3-SEL2020$FEV1FVC_pre)
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
mean(G21_Girls$MD_FEV1FVC_gmodel_3)
summary(G21_Girls$MD_FEV1FVC_gmodel_3)
max(G21_Girls$MD_FEV1FVC_gmodel_3)-min(G21_Girls$MD_FEV1FVC_gmodel_3)
summary(G21_Girls$MD_FEV1FVC_GLI)
max(G21_Girls$MD_FEV1FVC_GLI)-min(G21_Girls$MD_FEV1FVC_GLI)
#Model evaluation:
FEV1FVC_gmodel_3_RSq_Mean_dif <- (0.4146095*(1/-0.004189))
FEV1FVC_gmodel_3_RSq_Mean_dif
AIC(FEV1FVC_gmodel_3)
BIC(FEV1FVC_gmodel_3)
cor.test(G21_Girls$FEV1FVC_pre, G21_Girls$FEV1FVC_gmodel_3, method = "pearson")
cor.test(G21_Girls$FEV1FVC_pre, G21_Girls$FEV1FVC_GLI, method = "pearson")
#Density and bar plots with mean differences compared to GLI:
hrbrthemes::import_roboto_condensed() 
ggplot(G21_Girls, aes(x=x) ) +
  geom_density( aes(x = MD_FEV1FVC_gmodel_3, y = ..density..), fill="#69b3a2" ) +
  geom_label( aes(x=0, y=0.25, label="MD_FEV1FVC_gmodel_3"), color="#69b3a2") +
  # Bottom
  geom_density( aes(x = MD_FEV1FVC_GLI, y = -..density..), fill= "#404080") +
  geom_label( aes(x=0, y=-0.25, label="MD_FEV1FVC_GLI"), color="#404080") +
  theme_ipsum() +
  xlab("Differences between predicted and measured FEV1/FVC")

G21_Girls <- data.frame(
  type = c( rep("G21_Girls$MD_FEV1FVC_gmodel_3", 1000), rep("G21_Girls$MD_FEV1FVC_GLI", 1000) ),
  value = c( rnorm(1000), rnorm(1000, mean=0) )
)
# Represent it
G21_Girls %>%
  ggplot( aes(x=value, fill=type)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")

#FEV1FVC_gmodel_4 BMI:
FEV1FVC_gmodel_4 <- lm(FEV1FVC_pre ~ BMI, data = ARIA_Girls)
summary(FEV1FVC_gmodel_4)
SEL2020$FEV1FVC_gmodel_4 <- (90.23349+(0.03182*SEL2020$BMI))
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
#CI for the variables in FEV1FVC_gmodel_4:
confint(FEV1FVC_gmodel_4)
#Residuals for FEV1FVC_gmodel_4:
sigma(FEV1FVC_gmodel_4)/mean(ARIA_Girls$FEV1FVC_pre)
sigma(FEV1FVC_gmodel_4)/mean(G21_Girls$FEV1FVC_pre)
#Mean differences:
SEL2020$MD_FEV1FVC_gmodel_4 <- (SEL2020$FEV1FVC_gmodel_4-SEL2020$FEV1FVC_pre)
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
mean(G21_Girls$MD_FEV1FVC_gmodel_4)
summary(G21_Girls$MD_FEV1FVC_gmodel_4)
max(G21_Girls$MD_FEV1FVC_gmodel_4)-min(G21_Girls$MD_FEV1FVC_gmodel_4)
summary(G21_Girls$MD_FEV1FVC_GLI)
max(G21_Girls$MD_FEV1FVC_GLI)-min(G21_Girls$MD_FEV1FVC_GLI)
#Model evaluation:
FEV1FVC_gmodel_4_RSq_Mean_dif <- (0.6014688*(1/-0.00408))
FEV1FVC_gmodel_4_RSq_Mean_dif
AIC(FEV1FVC_gmodel_4)
BIC(FEV1FVC_gmodel_4)
cor.test(G21_Girls$FEV1FVC_pre, G21_Girls$FEV1FVC_gmodel_4, method = "pearson")
cor.test(G21_Girls$FEV1FVC_pre, G21_Girls$FEV1FVC_GLI, method = "pearson")
#Density and bar plots with mean differences compared to GLI:
hrbrthemes::import_roboto_condensed() 
ggplot(G21_Girls, aes(x=x) ) +
  geom_density( aes(x = MD_FEV1FVC_gmodel_4, y = ..density..), fill="#69b3a2" ) +
  geom_label( aes(x=0, y=0.25, label="MD_FEV1FVC_gmodel_4"), color="#69b3a2") +
  # Bottom
  geom_density( aes(x = MD_FEV1FVC_GLI, y = -..density..), fill= "#404080") +
  geom_label( aes(x=0, y=-0.25, label="MD_FEV1FVC_GLI"), color="#404080") +
  theme_ipsum() +
  xlab("Differences between predicted and measured FEV1/FVC")

G21_Girls <- data.frame(
  type = c( rep("G21_Girls$MD_FEV1FVC_gmodel_4", 1000), rep("G21_Girls$MD_FEV1FVC_GLI", 1000) ),
  value = c( rnorm(1000), rnorm(1000, mean=0) )
)
# Represent it
G21_Girls %>%
  ggplot( aes(x=value, fill=type)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")

#FEV1FVC_gmodel_5 Height/Weight:
FEV1FVC_gmodel_5 <- lm(FEV1FVC_pre ~ Altura_cm + Peso, data = ARIA_Girls)
summary(FEV1FVC_gmodel_5)
SEL2020$FEV1FVC_gmodel_5 <- (97.20333+(-0.05285*SEL2020$Altura_cm)+(0.02274*SEL2020$Peso))
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
#CI for the variables in FEV1FVC_gmodel_5:
confint(FEV1FVC_gmodel_5)
#Residuals for FEV1FVC_gmodel_5:
sigma(FEV1FVC_gmodel_5)/mean(ARIA_Girls$FEV1FVC_pre)
sigma(FEV1FVC_gmodel_5)/mean(G21_Girls$FEV1FVC_pre)
#Mean differences:
SEL2020$MD_FEV1FVC_gmodel_5 <- (SEL2020$FEV1FVC_gmodel_5-SEL2020$FEV1FVC_pre)
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
mean(G21_Girls$MD_FEV1FVC_gmodel_5)
summary(G21_Girls$MD_FEV1FVC_gmodel_5)
max(G21_Girls$MD_FEV1FVC_gmodel_5)-min(G21_Girls$MD_FEV1FVC_gmodel_5)
summary(G21_Girls$MD_FEV1FVC_GLI)
max(G21_Girls$MD_FEV1FVC_GLI)-min(G21_Girls$MD_FEV1FVC_GLI)
#Model evaluation:
FEV1FVC_gmodel_5_RSq_Mean_dif <- (0.3972893*(1/-0.003862))
FEV1FVC_gmodel_5_RSq_Mean_dif
AIC(FEV1FVC_gmodel_5)
BIC(FEV1FVC_gmodel_5)
cor.test(G21_Girls$FEV1FVC_pre, G21_Girls$FEV1FVC_gmodel_5, method = "pearson")
cor.test(G21_Girls$FEV1FVC_pre, G21_Girls$FEV1FVC_GLI, method = "pearson")
#Density and bar plots with mean differences compared to GLI:
hrbrthemes::import_roboto_condensed() 
ggplot(G21_Girls, aes(x=x) ) +
  geom_density( aes(x = MD_FEV1FVC_gmodel_5, y = ..density..), fill="#69b3a2" ) +
  geom_label( aes(x=0, y=0.25, label="MD_FEV1FVC_gmodel_5"), color="#69b3a2") +
  # Bottom
  geom_density( aes(x = MD_FEV1FVC_GLI, y = -..density..), fill= "#404080") +
  geom_label( aes(x=0, y=-0.25, label="MD_FEV1FVC_GLI"), color="#404080") +
  theme_ipsum() +
  xlab("Differences between predicted and measured FEV1/FVC")

G21_Girls <- data.frame(
  type = c( rep("G21_Girls$MD_FEV1FVC_gmodel_5", 1000), rep("G21_Girls$MD_FEV1FVC_GLI", 1000) ),
  value = c( rnorm(1000), rnorm(1000, mean=0) )
)
# Represent it
G21_Girls %>%
  ggplot( aes(x=value, fill=type)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")


#FEV1FVC_gmodel_6 Height/Age:   
FEV1FVC_gmodel_6 <- lm(FEV1FVC_pre ~ Altura_cm + Idade, data = ARIA_Girls)
summary(FEV1FVC_gmodel_6)
SEL2020$FEV1FVC_gmodel_6 <- (95.72609+(-0.04056*SEL2020$Altura_cm)+(0.06597*SEL2020$Idade))
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
#CI for the variables in FEV1FVC_gmodel_6:
confint(FEV1FVC_gmodel_6)
#Residuals for FVCFVC_gmodel_6:
sigma(FEV1FVC_gmodel_6)/mean(ARIA_Girls$FEV1FVC_pre)
sigma(FEV1FVC_gmodel_6)/mean(G21_Girls$FEV1FVC_pre)
#Mean differences:
SEL2020$MD_FEV1FVC_gmodel_6 <- (SEL2020$FEV1FVC_gmodel_6-SEL2020$FEV1FVC_pre)
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
mean(G21_Girls$MD_FEV1FVC_gmodel_6)
summary(G21_Girls$MD_FEV1FVC_gmodel_6)
max(G21_Girls$MD_FEV1FVC_gmodel_6)-min(G21_Girls$MD_FEV1FVC_gmodel_6)
summary(G21_Girls$MD_FEV1FVC_GLI)
max(G21_Girls$MD_FEV1FVC_GLI)-min(G21_Girls$MD_FEV1FVC_GLI)
#Model evaluation:
FEV1FVC_gmodel_6_RSq_Mean_dif <- (0.4487789*(1/-0.004832))
FEV1FVC_gmodel_6_RSq_Mean_dif
AIC(FEV1FVC_gmodel_6)
BIC(FEV1FVC_gmodel_6)
cor.test(G21_Girls$FEV1FVC_pre, G21_Girls$FEV1FVC_gmodel_6, method = "pearson")
cor.test(G21_Girls$FEV1FVC_pre, G21_Girls$FEV1FVC_GLI, method = "pearson")
#Density and bar plots with mean differences compared to GLI:
hrbrthemes::import_roboto_condensed() 
ggplot(G21_Girls, aes(x=x) ) +
  geom_density( aes(x = MD_FEV1FVC_gmodel_6, y = ..density..), fill="#69b3a2" ) +
  geom_label( aes(x=0, y=0.25, label="MD_FEV1FVC_gmodel_6"), color="#69b3a2") +
  # Bottom
  geom_density( aes(x = MD_FEV1FVC_GLI, y = -..density..), fill= "#404080") +
  geom_label( aes(x=0, y=-0.25, label="MD_FEV1FVC_GLI"), color="#404080") +
  theme_ipsum() +
  xlab("Differences between predicted and measured FEV1/FVC")

G21_Girls <- data.frame(
  type = c( rep("G21_Girls$MD_FEV1FVC_gmodel_6", 1000), rep("G21_Girls$MD_FEV1FVC_GLI", 1000) ),
  value = c( rnorm(1000), rnorm(1000, mean=0) )
)
# Represent it
G21_Girls %>%
  ggplot( aes(x=value, fill=type)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")

#FEV1FVC_gmodel_7 Height/BMI:   
FEV1FVC_gmodel_7 <- lm(FEV1FVC_pre ~ Altura_cm + BMI, data = ARIA_Girls)
summary(FEV1FVC_gmodel_7)
SEL2020$FEV1FVC_gmodel_7 <- (95.45526+(-0.04021*SEL2020$Altura_cm)+(0.04399*SEL2020$BMI))
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
#CI for the variables in FEV1FVC_gmodel_7:
confint(FEV1FVC_gmodel_7)
#Residuals for FEV1FVC_gmodel_7:
sigma(FEV1FVC_gmodel_7)/mean(ARIA_Girls$FEV1_pre)
sigma(FEV1FVC_gmodel_7)/mean(G21_Girls$FEV1_pre)
#Mean differences:
SEL2020$MD_FEV1FVC_gmodel_7 <- (SEL2020$FEV1FVC_gmodel_7-SEL2020$FEV1FVC_pre)
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
mean(G21_Girls$MD_FEV1FVC_gmodel_7)
summary(G21_Girls$MD_FEV1FVC_gmodel_7)
max(G21_Girls$MD_FEV1FVC_gmodel_7)-min(G21_Girls$MD_FEV1FVC_gmodel_7)
summary(G21_Girls$MD_FEV1FVC_GLI)
max(G21_Girls$MD_FEV1FVC_GLI)-min(G21_Girls$MD_FEV1FVC_GLI)
#Model evaluation:
FEV1FVC_gmodel_7_RSq_Mean_dif <- (0.4010889*(1/-0.00373))
FEV1FVC_gmodel_7_RSq_Mean_dif
AIC(FEV1FVC_gmodel_7)
BIC(FEV1FVC_gmodel_7)
cor.test(G21_Girls$FEV1FVC_pre, G21_Girls$FEV1FVC_gmodel_7, method = "pearson")
cor.test(G21_Girls$FEV1FVC_pre, G21_Girls$FEV1FVC_GLI, method = "pearson")
#Density and bar plots with mean differences compared to GLI:
hrbrthemes::import_roboto_condensed() 
ggplot(G21_Girls, aes(x=x) ) +
  geom_density( aes(x = MD_FEV1FVC_gmodel_7, y = ..density..), fill="#69b3a2" ) +
  geom_label( aes(x=0, y=0.25, label="MD_FEV1FVC_gmodel_7"), color="#69b3a2") +
  # Bottom
  geom_density( aes(x = MD_FEV1FVC_GLI, y = -..density..), fill= "#404080") +
  geom_label( aes(x=0, y=-0.25, label="MD_FEV1FVC_GLI"), color="#404080") +
  theme_ipsum() +
  xlab("Differences between predicted and measured FEV1/FVC")

G21_Girls <- data.frame(
  type = c( rep("G21_Girls$MD_FEV1FVC_gmodel_7", 1000), rep("G21_Girls$MD_FEV1FVC_GLI", 1000) ),
  value = c( rnorm(1000), rnorm(1000, mean=0) )
)
# Represent it
G21_Girls %>%
  ggplot( aes(x=value, fill=type)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")

#FEV1FVC_gmodel_8 Height/Weight/Age:   
FEV1FVC_gmodel_8 <- lm(FEV1FVC_pre ~ Altura_cm + Peso + Idade, data = ARIA_Girls)
summary(FEV1FVC_gmodel_8)
SEL2020$FEV1FVC_gmodel_8 <- (97.01762+(-0.05691*SEL2020$Altura_cm)+(0.02333*SEL2020$Peso)+(0.08155*SEL2020$Idade))
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
#CI for the variables in FEV1FVC_gmodel_8:
confint(FEV1FVC_gmodel_8)
#Residuals for FVCFVC_gmodel_8:
sigma(FEV1FVC_gmodel_8)/mean(ARIA_Girls$FEV1FVC_pre)
sigma(FEV1FVC_gmodel_8)/mean(G21_Girls$FEV1FVC_pre)
#Mean differences:
SEL2020$MD_FEV1FVC_gmodel_8 <- (SEL2020$FEV1FVC_gmodel_8-SEL2020$FEV1FVC_pre)
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
mean(G21_Girls$MD_FEV1FVC_gmodel_8)
summary(G21_Girls$MD_FEV1FVC_gmodel_8)
max(G21_Girls$MD_FEV1FVC_gmodel_8)-min(G21_Girls$MD_FEV1FVC_gmodel_8)
summary(G21_Girls$MD_FEV1FVC_GLI)
max(G21_Girls$MD_FEV1FVC_GLI)-min(G21_Girls$MD_FEV1FVC_GLI)
#Model evaluation:
FEV1FVC_gmodel_8_RSq_Mean_dif <- (0.4778663*(1/-0.008491))
FEV1FVC_gmodel_8_RSq_Mean_dif
AIC(FEV1FVC_gmodel_8)
BIC(FEV1FVC_gmodel_8)
cor.test(G21_Girls$FEV1FVC_pre, G21_Girls$FEV1FVC_gmodel_8, method = "pearson")
cor.test(G21_Girls$FEV1FVC_pre, G21_Girls$FEV1FVC_GLI, method = "pearson")
#Density and bar plots with mean differences compared to GLI:
hrbrthemes::import_roboto_condensed() 
ggplot(G21_Girls, aes(x=x) ) +
  geom_density( aes(x = MD_FEV1FVC_gmodel_8, y = ..density..), fill="#69b3a2" ) +
  geom_label( aes(x=0, y=0.25, label="MD_FEV1FVC_gmodel_8"), color="#69b3a2") +
  # Bottom
  geom_density( aes(x = MD_FEV1FVC_GLI, y = -..density..), fill= "#404080") +
  geom_label( aes(x=0, y=-0.25, label="MD_FEV1FVC_GLI"), color="#404080") +
  theme_ipsum() +
  xlab("Differences between predicted and measured FEV1/FVC")

G21_Girls <- data.frame(
  type = c( rep("G21_Girls$MD_FEV1FVC_gmodel_8", 1000), rep("G21_Girls$MD_FEV1FVC_GLI", 1000) ),
  value = c( rnorm(1000), rnorm(1000, mean=0) )
)
# Represent it
G21_Girls %>%
  ggplot( aes(x=value, fill=type)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")

#FEV1FVC_gmodel_9 Height/Weight/BMI:   
FEV1FVC_gmodel_9 <- lm(FEV1FVC_pre ~ Altura_cm + Peso + BMI, data = ARIA_Girls)
summary(FEV1FVC_gmodel_9)
SEL2020$FEV1FVC_gmodel_9 <- (92.80296+(-0.02040*SEL2020$Altura_cm)+(-0.03513*SEL2020$Peso)+(0.10721*SEL2020$BMI))
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
#CI for the variables in FEV1FVC_gmodel_9:
confint(FEV1FVC_gmodel_9)
#Residuals for FEV1FVC_gmodel_9:
sigma(FEV1FVC_gmodel_9)/mean(ARIA_Girls$FEV1FVC_pre)
sigma(FEV1FVC_gmodel_9)/mean(G21_Girls$FEV1FVC_pre)
#Mean differences:
SEL2020$MD_FEV1FVC_gmodel_9 <- (SEL2020$FEV1FVC_gmodel_9-SEL2020$FEV1FVC_pre)
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
mean(G21_Girls$MD_FEV1FVC_gmodel_9)
summary(G21_Girls$MD_FEV1FVC_gmodel_9)
max(G21_Girls$MD_FEV1FVC_gmodel_9)-min(G21_Girls$MD_FEV1FVC_gmodel_9)
summary(G21_Girls$MD_FEV1FVC_GLI)
max(G21_Girls$MD_FEV1FVC_GLI)-min(G21_Girls$MD_FEV1FVC_GLI)
#Model evaluation:
SEL2020$FEV1FVC_gmodel_9_RSq_Mean_dif <- (0.4052339*(1/-0.008436))
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
mean(G21_Girls$FEV1FVC_gmodel_9_RSq_Mean_dif)
AIC(FEV1FVC_gmodel_9)
BIC(FEV1FVC_gmodel_9)
cor.test(G21_Girls$FEV1FVC_pre, G21_Girls$FEV1FVC_gmodel_9, method = "pearson")
cor.test(G21_Girls$FEV1FVC_pre, G21_Girls$FEV1FVC_GLI, method = "pearson")
#Density and bar plots with mean differences compared to GLI:
hrbrthemes::import_roboto_condensed() 
ggplot(G21_Girls, aes(x=x) ) +
  geom_density( aes(x = MD_FEV1FVC_gmodel_9, y = ..density..), fill="#69b3a2" ) +
  geom_label( aes(x=1, y=0.25, label="MD_FEV1FVC_gmodel_9"), color="#69b3a2") +
  # Bottom
  geom_density( aes(x = MD_FEV1FVC_GLI, y = -..density..), fill= "#404080") +
  geom_label( aes(x=1, y=-0.25, label="MD_FEV1FVC_GLI"), color="#404080") +
  theme_ipsum() +
  xlab("Differences between predicted and measured FEV1/FVC")

G21_Girls <- data.frame(
  type = c( rep("G21_Girls$MD_FEV1FVC_gmodel_9", 1000), rep("G21_Girls$MD_FEV1FVC_GLI", 1000) ),
  value = c( rnorm(1000), rnorm(1000, mean=0) )
)
# Represent it
G21_Girls %>%
  ggplot( aes(x=value, fill=type)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")


#FEV1FVC_gmodel_10 Height/Weight/BMI/Age:   
FEV1FVC_gmodel_10 <- lm(FEV1FVC_pre ~ Altura_cm + Peso + BMI + Idade, data = ARIA_Girls)
summary(FEV1FVC_gmodel_10)
SEL2020$FEV1FVC_gmodel_10 <- (93.44078+(-0.02906*SEL2020$Altura_cm)+(-0.02453*SEL2020$Peso)+(0.08837*SEL2020$BMI)+(0.05941*SEL2020$Idade))
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
#CI for the variables in FEV1FVC_gmodel_10:
confint(FEV1FVC_gmodel_10)
#Residuals for FEV1FVC_gmodel_10:
sigma(FEV1FVC_gmodel_10)/mean(ARIA_Girls$FEV1FVC_pre)
sigma(FEV1FVC_gmodel_10)/mean(G21_Girls$FEV1FVC_pre)
#Mean differences:
SEL2020$MD_FEV1FVC_gmodel_10 <- (SEL2020$FEV1FVC_gmodel_10- SEL2020$FEV1FVC_pre)
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
mean(G21_Girls$MD_FEV1FVC_gmodel_10)
summary(G21_Girls$MD_FEV1FVC_gmodel_10)
max(G21_Girls$MD_FEV1FVC_gmodel_10)-min(G21_Girls$MD_FEV1FVC_gmodel_10)
summary(G21_Girls$MD_FEV1FVC_GLI)
max(G21_Girls$MD_FEV1FVC_GLI)-min(G21_Girls$MD_FEV1FVC_GLI)
#Model evaluation:
FEV1FVC_gmodel_10_RSq_Mean_dif <- (0.4624262*(1/-0.01319))
FEV1FVC_gmodel_10_RSq_Mean_dif
AIC(FEV1FVC_gmodel_10)
BIC(FEV1FVC_gmodel_10)
cor.test(G21_Girls$FEV1FVC_pre, G21_Girls$FEV1FVC_gmodel_10, method = "pearson")
cor.test(G21_Girls$FEV1FVC_pre, G21_Girls$FEV1FVC_GLI, method = "pearson")
#Density and bar plots with mean differences compared to GLI:
hrbrthemes::import_roboto_condensed() 
ggplot(G21_Girls, aes(x=x) ) +
  geom_density( aes(x = MD_FEV1FVC_gmodel_10, y = ..density..), fill="#69b3a2" ) +
  geom_label( aes(x=0, y=0.25, label="MD_FEV1FVC_gmodel_10"), color="#69b3a2") +
  # Bottom
  geom_density( aes(x = MD_FEV1FVC_GLI, y = -..density..), fill= "#404080") +
  geom_label( aes(x=0, y=-0.25, label="MD_FEV1FVC_GLI"), color="#404080") +
  theme_ipsum() +
  xlab("Differences between predicted and measured FEV1/FVC")

G21_Girls <- data.frame(
  type = c( rep("G21_Girls$MD_FEV1FVC_gmodel_10", 1000), rep("G21_Girls$MD_FEV1FVC_GLI", 1000) ),
  value = c( rnorm(1000), rnorm(1000, mean=0) )
)
# Represent it
G21_Girls %>%
  ggplot( aes(x=value, fill=type)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")


#FEV1FVC_gmodel_11 Weight/Age:   
FEV1FVC_gmodel_11 <- lm(FEV1FVC_pre ~ Peso + Idade, data = ARIA_Girls)
summary(FEV1FVC_gmodel_11)
SEL2020$FEV1FVC_gmodel_11 <- (92.060971+(-0.007027*SEL2020$Peso)+(-0.115155*SEL2020$Idade))
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
#CI for the variables in FEV1FVC_gmodel_11:
confint(FEV1FVC_gmodel_11)
#Residuals for FEV1FVC_gmodel_11:
sigma(FEV1FVC_gmodel_11)/mean(ARIA_Girls$FEV1FVC_pre)
sigma(FEV1FVC_gmodel_11)/mean(G21_Girls$FEV1FVC_pre)
#Mean differences:
SEL2020$MD_FEV1FVC_gmodel_11 <- (SEL2020$FEV1FVC_gmodel_11-SEL2020$FEV1FVC_pre)
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
mean(G21_Girls$MD_FEV1FVC_gmodel_11)
summary(G21_Girls$MD_FEV1FVC_gmodel_11)
max(G21_Girls$MD_FEV1FVC_gmodel_11)-min(G21_Girls$MD_FEV1FVC_gmodel_11)
summary(G21_Girls$MD_FEV1FVC_GLI)
max(G21_Girls$MD_FEV1FVC_GLI)-min(G21_Girls$MD_FEV1FVC_GLI)
#Model evaluation:
FEV1FVC_gmodel_11_RSq_Mean_dif <- (0.4106956*(1/-0.008786))
FEV1FVC_gmodel_11_RSq_Mean_dif
AIC(FEV1FVC_gmodel_11)
BIC(FEV1FVC_gmodel_11)
cor.test(G21_Girls$FEV1FVC_pre, G21_Girls$FEV1FVC_gmodel_11, method = "pearson")
cor.test(G21_Girls$FEV1FVC_pre, G21_Girls$FEV1FVC_GLI, method = "pearson")
#Density and bar plots with mean differences compared to GLI:
hrbrthemes::import_roboto_condensed() 
ggplot(G21_Girls, aes(x=x) ) +
  geom_density( aes(x = MD_FEV1FVC_gmodel_11, y = ..density..), fill="#69b3a2" ) +
  geom_label( aes(x=0, y=0.25, label="MD_FEV1FVC_gmodel_11"), color="#69b3a2") +
  # Bottom
  geom_density( aes(x = MD_FEV1FVC_GLI, y = -..density..), fill= "#404080") +
  geom_label( aes(x=0, y=-0.25, label="MD_FEV1FVC_GLI"), color="#404080") +
  theme_ipsum() +
  xlab("Differences between predicted and measured FEV1/FVC")

G21_Girls <- data.frame(
  type = c( rep("G21_Girls$MD_FEV1FVC_gmodel_11", 1000), rep("G21_Girls$MD_FEV1FVC_GLI", 1000) ),
  value = c( rnorm(1000), rnorm(1000, mean=0) )
)
# Represent it
G21_Girls %>%
  ggplot( aes(x=value, fill=type)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")

#FEV1FVC_gmodel_12 Weight/BMI:   
FEV1FVC_gmodel_12 <- lm(FEV1FVC_pre ~ Peso + BMI, data = ARIA_Girls)
summary(FEV1FVC_gmodel_12)
SEL2020$FEV1FVC_gmodel_12 <- (90.07732+(-0.06927*SEL2020$Peso)+(0.16830*SEL2020$BMI))
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
#CI for the variables in FEV1FVC_gmodel_12:
confint(FEV1FVC_gmodel_12)
#Residuals for FEV1FVC_gmodel_12:
sigma(FEV1FVC_gmodel_12)/mean(ARIA_Girls$FEV1FVC_pre)
sigma(FEV1FVC_gmodel_12)/mean(G21_Girls$FEV1FVC_pre)
#Mean differences:
SEL2020$MD_FEV1FVC_gmodel_12 <- (SEL2020$FEV1FVC_gmodel_12-SEL2020$FEV1FVC_pre)
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
mean(G21_Girls$MD_FEV1FVC_gmodel_12)
summary(G21_Girls$MD_FEV1FVC_gmodel_12)
max(G21_Girls$MD_FEV1FVC_gmodel_12)-min(G21_Girls$MD_FEV1FVC_gmodel_12)
summary(G21_Girls$MD_FEV1FVC_GLI)
max(G21_Girls$MD_FEV1FVC_GLI)-min(G21_Girls$MD_FEV1FVC_GLI)
#Model evaluation:
FEV1FVC_gmodel_12_RSq_Mean_dif <- (0.4160002*(1/-0.00373))
FEV1FVC_gmodel_12_RSq_Mean_dif
AIC(FEV1FVC_gmodel_12)
BIC(FEV1FVC_gmodel_12)
cor.test(G21_Girls$FEV1FVC_pre, G21_Girls$FEV1FVC_gmodel_12, method = "pearson")
cor.test(G21_Girls$FEV1FVC_pre, G21_Girls$FEV1FVC_GLI, method = "pearson")
#Density and bar plots with mean differences compared to GLI:
hrbrthemes::import_roboto_condensed() 
ggplot(G21_Girls, aes(x=x) ) +
  geom_density( aes(x = MD_FEV1FVC_gmodel_12, y = ..density..), fill="#69b3a2" ) +
  geom_label( aes(x=0, y=0.25, label="MD_FEV1FVC_gmodel_12"), color="#69b3a2") +
  # Bottom
  geom_density( aes(x = MD_FEV1FVC_GLI, y = -..density..), fill= "#404080") +
  geom_label( aes(x=0, y=-0.25, label="MD_FEV1FVC_GLI"), color="#404080") +
  theme_ipsum() +
  xlab("Differences between predicted and measured FEV1/FVC")

G21_Girls <- data.frame(
  type = c( rep("G21_Girls$MD_FEV1FVC_gmodel_12", 1000), rep("G21_Girls$MD_FEV1FVC_GLI", 1000) ),
  value = c( rnorm(1000), rnorm(1000, mean=0) )
)
# Represent it
G21_Girls %>%
  ggplot( aes(x=value, fill=type)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")

#FEV1FVC_gmodel_13 Weight/BMI/Age:   
FEV1FVC_gmodel_13 <- lm(FEV1FVC_pre ~ Peso + BMI + Idade, data = ARIA_Girls)
summary(FEV1FVC_gmodel_13)
SEL2020$FEV1FVC_gmodel_13 <- (89.82434+(-0.07117*SEL2020$Peso)+(0.17176*SEL2020$BMI)+(0.02893*SEL2020$Idade))
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
#CI for the variables in FEV1FVC_gmodel_13:
confint(FEV1FVC_gmodel_13)
#Residuals for FEV1FVC_bmodel_13:
sigma(FEV1FVC_gmodel_13)/mean(ARIA_Girls$FEV1FVC_pre)
sigma(FEV1FVC_gmodel_13)/mean(G21_Girls$FEV1FVC_pre)
#Mean differences:
SEL2020$MD_FEV1FVC_gmodel_13 <- (SEL2020$FEV1FVC_gmodel_13-SEL2020$FEV1FVC_pre)
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
mean(G21_Girls$MD_FEV1FVC_gmodel_13)
summary(G21_Girls$MD_FEV1FVC_gmodel_13)
max(G21_Girls$MD_FEV1FVC_gmodel_13)-min(G21_Girls$MD_FEV1FVC_gmodel_13)
summary(G21_Girls$MD_FEV1FVC_GLI)
max(G21_Girls$MD_FEV1FVC_GLI)-min(G21_Girls$MD_FEV1FVC_GLI)
#Model evaluation:
FEV1FVC_gmodel_13_RSq_Mean_dif <- (0.008984878*(1/-0.008489))
FEV1FVC_gmodel_13_RSq_Mean_dif
AIC(FEV1FVC_gmodel_13)
BIC(FEV1FVC_gmodel_13)
cor.test(G21_Girls$FEV1FVC_pre, G21_Girls$FEV1FVC_gmodel_13, method = "pearson")
cor.test(G21_Girls$FEV1FVC_pre, G21_Girls$FEV1FVC_GLI, method = "pearson")
#Density and bar plots with mean differences compared to GLI:
hrbrthemes::import_roboto_condensed() 
ggplot(G21_Girls, aes(x=x) ) +
  geom_density( aes(x = MD_FEV1FVC_gmodel_13, y = ..density..), fill="#69b3a2" ) +
  geom_label( aes(x=0, y=0.25, label="MD_FEV1FVC_gmodel_13"), color="#69b3a2") +
  # Bottom
  geom_density( aes(x = MD_FEV1FVC_GLI, y = -..density..), fill= "#404080") +
  geom_label( aes(x=0, y=-0.25, label="MD_FEV1FVC_GLI"), color="#404080") +
  theme_ipsum() +
  xlab("Differences between predicted and measured FEV1/FVC")

G21_Girls <- data.frame(
  type = c( rep("G21_Girls$MD_FEV1FVC_gmodel_13", 1000), rep("G21_Girls$MD_FEV1FVC_GLI", 1000) ),
  value = c( rnorm(1000), rnorm(1000, mean=0) )
)
# Represent it
G21_Girls %>%
  ggplot( aes(x=value, fill=type)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")

#FEV1FVC_gmodel_14 BMI/Age:   
FEV1FVC_gmodel_14 <- lm(FEV1FVC_pre ~ BMI + Idade, data = ARIA_Girls)
summary(FEV1FVC_gmodel_14)
SEL2020$FEV1FVC_gmodel_14 <- (91.45205+(0.03318*SEL2020$BMI)+(-0.14176*SEL2020$Idade))
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
#CI for the variables in FEV1FVC_gmodel_14:
confint(FEV1FVC_gmodel_14)
#Residuals for FEV1FVC_gmodel_14:
sigma(FEV1FVC_gmodel_14)/mean(ARIA_Girls$FEV1FVC_pre)
sigma(FEV1FVC_gmodel_14)/mean(G21_Girls$FEV1FVC_pre)
#Mean differences:
SEL2020$MD_FEV1FVC_gmodel_14 <- (SEL2020$FEV1FVC_gmodel_14-SEL2020$FEV1FVC_pre)
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
mean(G21_Girls$MD_FEV1FVC_gmodel_14)
summary(G21_Girls$MD_FEV1FVC_gmodel_14)
max(G21_Girls$MD_FEV1FVC_gmodel_14)-min(G21_Girls$MD_FEV1FVC_gmodel_14)
summary(G21_Girls$MD_FEV1FVC_GLI)
max(G21_Girls$MD_FEV1FVC_GLI)-min(G21_Girls$MD_FEV1FVC_GLI)
#Model evaluation:
FEV1FVC_gmodel_14_RSq_Mean_dif <- (0.429456*(1/-0.008254))
FEV1FVC_gmodel_14_RSq_Mean_dif
AIC(FEV1FVC_gmodel_14)
BIC(FEV1FVC_gmodel_14)
cor.test(G21_Girls$FEV1FVC_pre, G21_Girls$FEV1FVC_gmodel_14, method = "pearson")
cor.test(G21_Girls$FEV1FVC_pre, G21_Girls$FEV1FVC_GLI, method = "pearson")
#Density and bar plots with mean differences compared to GLI:
hrbrthemes::import_roboto_condensed() 
ggplot(G21_Girls, aes(x=x) ) +
  geom_density( aes(x = MD_FEV1FVC_gmodel_14, y = ..density..), fill="#69b3a2" ) +
  geom_label( aes(x=0, y=0.25, label="MD_FEV1FVC_gmodel_14"), color="#69b3a2") +
  # Bottom
  geom_density( aes(x = MD_FEV1FVC_GLI, y = -..density..), fill= "#404080") +
  geom_label( aes(x=0, y=-0.25, label="MD_FEV1FVC_GLI"), color="#404080") +
  theme_ipsum() +
  xlab("Differences between predicted and measured FEV1/FVC")

G21_Girls <- data.frame(
  type = c( rep("G21_Girls$MD_FEV1FVC_gmodel_14", 1000), rep("G21_Girls$MD_FEV1FVC_GLI", 1000) ),
  value = c( rnorm(1000), rnorm(1000, mean=0) )
)
# Represent it
G21_Girls %>%
  ggplot( aes(x=value, fill=type)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")

#Multiple Linear Regression FEF2575 Boys: 

#FEF2575_bmodel 1 Height
FEF2575_bmodel_1 <- lm(FEF2575_pre ~ Altura_cm, data = ARIA_Boys)
summary(FEF2575_bmodel_1)
SEL2020$FEF2575_bmodel_1 <- (-1.145231 + (0.025109*SEL2020$Altura_cm))
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
#CI for the variables in FEF2575_bmodel_1:
confint(FEF2575_bmodel_1)
#Residuals for FEF2575_bmodel_1:
sigma(FEF2575_bmodel_1)/mean(ARIA_Boys$FEF2575_pre)
sigma(FEF2575_bmodel_1)/mean(G21_Boys$FEF2575_pre)
#Mean differences:
SEL2020$MD_FEF2575_bmodel_1 <- (SEL2020$FEF2575_bmodel_1-SEL2020$FEF2575_pre)
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
mean(G21_Boys$MD_FEF2575_bmodel_1)
summary(G21_Boys$MD_FEF2575_bmodel_1)
max(G21_Boys$MD_FEF2575_bmodel_1)-min(G21_Boys$MD_FEF2575_bmodel_1)
summary(G21_Boys$MD_FEF2575_GLI)
max(G21_Boys$MD_FEF2575_GLI)-min(G21_Boys$MD_FEF2575_GLI)
#Model evaluation:
FEF2575_bmodel_1_RSq_Mean_dif <- (0.1036193*(1/0.1216))
FEF2575_bmodel_1_RSq_Mean_dif
AIC(FEF2575_bmodel_1)
BIC(FEF2575_bmodel_1)
cor.test(G21_Boys$FEF2575_pre, G21_Boys$FEF2575_bmodel_1, method = "pearson")
cor.test(G21_Boys$FEF2575_pre, G21_Boys$FEF2575_GLI, method = "pearson")
#Density and bar plots with mean differences compared to GLI:
hrbrthemes::import_roboto_condensed() 
ggplot(G21_Boys, aes(x=x) ) +
  geom_density( aes(x = MD_FEF2575_bmodel_1, y = ..density..), fill="#69b3a2" ) +
  geom_label( aes(x=0, y=0.25, label="MD_FEF2575_bmodel_1"), color="#69b3a2") +
  # Bottom
  geom_density( aes(x = MD_FEF2575_GLI, y = -..density..), fill= "#404080") +
  geom_label( aes(x=0, y=-0.25, label="MD_FEF2575_GLI"), color="#404080") +
  theme_ipsum() +
  xlab("Differences between predicted and measured FEF2575")

G21_Boys <- data.frame(
  type = c( rep("G21_Boys$MD_FEF2575_bmodel_1", 1000), rep("G21_Boys$MD_FEF2575_GLI", 1000) ),
  value = c( rnorm(1000), rnorm(1000, mean=0) )
)
# Represent it
G21_Boys %>%
  ggplot( aes(x=value, fill=type)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")

#FEF2575_bmodel_2 Height/Weight:
FEF2575_bmodel_2 <- lm(FEF2575_pre ~ Peso, data = ARIA_Boys)
summary(FEF2575_bmodel_2)
SEL2020$FEF2575_bmodel_2 <- (1.704722+(0.016801*SEL2020$Peso))
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
#CI for the variables in FEF2575_bmodel_2:
confint(FEF2575_bmodel_2)
#Residuals for FEF2575_bmodel_2:
sigma(FEF2575_bmodel_2)/mean(ARIA_Boys$FEF2575_pre)
sigma(FEF2575_bmodel_2)/mean(G21_Boys$FEF2575_pre)
#Mean differences:
SEL2020$MD_FEF2575_bmodel_2 <- (SEL2020$FEF2575_bmodel_2-SEL2020$FEF2575_pre)
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
mean(G21_Boys$MD_FEF2575_bmodel_2)
summary(G21_Boys$MD_FEF2575_bmodel_2)
max(G21_Boys$MD_FEF2575_bmodel_2)-min(G21_Boys$MD_FEF2575_bmodel_2)
summary(G21_Boys$MD_FEF2575_GLI)
max(G21_Boys$MD_FEF2575_GLI)-min(G21_Boys$MD_FEF2575_GLI)
#Model evaluation:
FEF2575_bmodel_2_RSq_Mean_dif <- (0.04706876*(1/0.07224))
FEF2575_bmodel_2_RSq_Mean_dif
AIC(FEF2575_bmodel_2)
BIC(FEF2575_bmodel_2)
cor.test(G21_Boys$FEF2575_pre, G21_Boys$FEF2575_bmodel_2, method = "pearson")
cor.test(G21_Boys$FEF2575_pre, G21_Boys$FEF2575_GLI, method = "pearson")
#Density and bar plots with mean differences compared to GLI:
hrbrthemes::import_roboto_condensed() 
ggplot(G21_Boys, aes(x=x) ) +
  geom_density( aes(x = MD_FEF2575_bmodel_2, y = ..density..), fill="#69b3a2" ) +
  geom_label( aes(x=0, y=0.25, label="MD_FEF2575_bmodel_2"), color="#69b3a2") +
  # Bottom
  geom_density( aes(x = MD_FEF2575_GLI, y = -..density..), fill= "#404080") +
  geom_label( aes(x=0, y=-0.25, label="MD_FEF2575_GLI"), color="#404080") +
  theme_ipsum() +
  xlab("Differences between predicted and measured FEF2575")

G21_Boys <- data.frame(
  type = c( rep("G21_Boys$MD_FEF2575_bmodel_2", 1000), rep("G21_Boys$MD_FEF2575_GLI", 1000) ),
  value = c( rnorm(1000), rnorm(1000, mean=0) )
)
# Represent it
G21_Boys %>%
  ggplot( aes(x=value, fill=type)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")

#FEF2575_bmodel_3 Age:
FEF2575_bmodel_3 <- lm(FEF2575_pre ~ Idade, data = ARIA_Boys)
summary(FEF2575_bmodel_3)
SEL2020$FEF2575_bmodel_3 <- (0.96875+(0.14803*SEL2020$Idade))
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
#CI for the variables in FEF2575_bmodel_3:
confint(FEF2575_bmodel_3)
#Residuals for FEF2575_bmodel_3:
sigma(FEF2575_bmodel_3)/mean(ARIA_Boys$FEF2575_pre)
sigma(FEF2575_bmodel_3)/mean(G21_Boys$FEF2575_pre)
#Mean differences:
SEL2020$MD_FEF2575_bmodel_3 <- (SEL2020$FEF2575_bmodel_3-SEL2020$FEF2575_pre)
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
mean(G21_Boys$MD_FEF2575_bmodel_3)
summary(G21_Boys$MD_FEF2575_bmodel_3)
max(G21_Boys$MD_FEF2575_bmodel_3)-min(G21_Boys$MD_FEF2575_bmodel_3)
summary(G21_Boys$MD_FEF2575_GLI)
max(G21_Boys$MD_FEF2575_GLI)-min(G21_Boys$MD_FEF2575_GLI)
#Model evaluation:
FEF2575_bmodel_3_RSq_Mean_dif <- (0.166645*(1/0.04705))
FEF2575_bmodel_3_RSq_Mean_dif
AIC(FEF2575_bmodel_3)
BIC(FEF2575_bmodel_3)
cor.test(G21_Boys$FEF2575_pre, G21_Boys$FEF2575_bmodel_3, method = "pearson")
cor.test(G21_Boys$FEF2575_pre, G21_Boys$FEF2575_GLI, method = "pearson")
#Density and bar plots with mean differences compared to GLI:
hrbrthemes::import_roboto_condensed() 
ggplot(G21_Boys, aes(x=x) ) +
  geom_density( aes(x = MD_FEF2575_bmodel_3, y = ..density..), fill="#69b3a2" ) +
  geom_label( aes(x=0, y=0.25, label="MD_FEF2575_bmodel_3"), color="#69b3a2") +
  # Bottom
  geom_density( aes(x = MD_FEF2575_GLI, y = -..density..), fill= "#404080") +
  geom_label( aes(x=0, y=-0.25, label="MD_FEF2575_GLI"), color="#404080") +
  theme_ipsum() +
  xlab("Differences between predicted and measured FEF2575")

G21_Boys <- data.frame(
  type = c( rep("G21_Boys$MD_FEF2575_bmodel_3", 1000), rep("G21_Boys$MD_FEF2575_GLI", 1000) ),
  value = c( rnorm(1000), rnorm(1000, mean=0) )
)
# Represent it
G21_Boys %>%
  ggplot( aes(x=value, fill=type)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")

#FEF2575_bmodel_4 BMI:
FEF2575_bmodel_4 <- lm(FEF2575_pre ~ BMI, data = ARIA_Boys)
summary(FEF2575_bmodel_4)
SEL2020$FEF2575_bmodel_4 <- (1.787792+(0.026722*SEL2020$BMI))
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
#CI for the variables in FEF2575_bmodel_4:
confint(FEF2575_bmodel_4)
#Residuals for FEF2575_bmodel_4:
sigma(FEF2575_bmodel_4)/mean(ARIA_Boys$FEF2575_pre)
sigma(FEF2575_bmodel_4)/mean(G21_Boys$FEF2575_pre)
#Mean differences:
SEL2020$MD_FEF2575_bmodel_4 <- (SEL2020$FEF2575_bmodel_4-SEL2020$FEF2575_pre)
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
mean(G21_Boys$MD_FEF2575_bmodel_4)
summary(G21_Boys$MD_FEF2575_bmodel_4)
max(G21_Boys$MD_FEF2575_bmodel_4)-min(G21_Boys$MD_FEF2575_bmodel_4)
summary(G21_Boys$MD_FEF2575_GLI)
max(G21_Boys$MD_FEF2575_GLI)-min(G21_Boys$MD_FEF2575_GLI)
#Model evaluation:
FEF2575_bmodel_4_RSq_Mean_dif <- (0.005686822*(1/0.0259))
FEF2575_bmodel_4_RSq_Mean_dif
AIC(FEF2575_bmodel_4)
BIC(FEF2575_bmodel_4)
cor.test(G21_Boys$FEF2575_pre, G21_Boys$FEF2575_bmodel_4, method = "pearson")
cor.test(G21_Boys$FEF2575_pre, G21_Boys$FEF2575_GLI, method = "pearson")
#Density and bar plots with mean differences compared to GLI:
hrbrthemes::import_roboto_condensed() 
ggplot(G21_Boys, aes(x=x) ) +
  geom_density( aes(x = MD_FEF2575_bmodel_4, y = ..density..), fill="#69b3a2" ) +
  geom_label( aes(x=0, y=0.25, label="MD_FFEF2575_bmodel_4"), color="#69b3a2") +
  # Bottom
  geom_density( aes(x = MD_FEF2575_GLI, y = -..density..), fill= "#404080") +
  geom_label( aes(x=0, y=-0.25, label="MD_FEF2575_GLI"), color="#404080") +
  theme_ipsum() +
  xlab("Differences between predicted and measured FEF2575")

G21_Boys <- data.frame(
  type = c( rep("G21_Boys$MD_FEF2575_bmodel_4", 1000), rep("G21_Boys$MD_FEF2575_GLI", 1000) ),
  value = c( rnorm(1000), rnorm(1000, mean=0) )
)
# Represent it
G21_Boys %>%
  ggplot( aes(x=value, fill=type)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")

#FEF2575_bmodel_5 Height/Weight:
FEF2575_bmodel_5 <- lm(FEF2575_pre ~ Altura_cm + Peso, data = ARIA_Boys)
summary(FEF2575_bmodel_5)
SEL2020$FEF2575_bmodel_5 <- (-0.890250+(0.022420*SEL2020$Altura_cm)+(0.003295*SEL2020$Peso))
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
#CI for the variables in FEF2575_bmodel_5:
confint(FEF2575_bmodel_5)
#Residuals for FEF2575_bmodel_5:
sigma(FEF2575_bmodel_5)/mean(ARIA_Boys$FEF2575_pre)
sigma(FEF2575_bmodel_5)/mean(G21_Boys$FEF2575_pre)
#Mean differences:
SEL2020$MD_FEF2575_bmodel_5 <- (SEL2020$FEF2575_bmodel_5-SEL2020$FEF2575_pre)
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
mean(G21_Boys$MD_FEF2575_bmodel_5)
summary(G21_Boys$MD_FEF2575_bmodel_5)
max(G21_Boys$MD_FEF2575_bmodel_5)-min(G21_Boys$MD_FEF2575_bmodel_5)
summary(G21_Boys$MD_FEF2575_GLI)
max(G21_Boys$MD_FEF2575_GLI)-min(G21_Boys$MD_FEF2575_GLI)
#Model evaluation:
FEF2575_bmodel_5_RSq_Mean_dif <- (0.1026875*(1/0.1198))
FEF2575_bmodel_5_RSq_Mean_dif
AIC(FEF2575_bmodel_5)
BIC(FEF2575_bmodel_5)
cor.test(G21_Boys$FEF2575_pre, G21_Boys$FEF2575_bmodel_5, method = "pearson")
cor.test(G21_Boys$FEF2575_pre, G21_Boys$FEF2575_GLI, method = "pearson")
#Density and bar plots with mean differences compared to GLI:
hrbrthemes::import_roboto_condensed() 
ggplot(G21_Boys, aes(x=x) ) +
  geom_density( aes(x = MD_FEF2575_bmodel_5, y = ..density..), fill="#69b3a2" ) +
  geom_label( aes(x=0, y=0.25, label="MD_FEF2575_bmodel_5"), color="#69b3a2") +
  # Bottom
  geom_density( aes(x = MD_FEF2575_GLI, y = -..density..), fill= "#404080") +
  geom_label( aes(x=0, y=-0.25, label="MD_FEF2575_GLI"), color="#404080") +
  theme_ipsum() +
  xlab("Differences between predicted and measured FEF2575")

G21_Boys <- data.frame(
  type = c( rep("G21_Boys$MD_FEF2575_bmodel_5", 1000), rep("G21_Boys$MD_FEF2575_GLI", 1000) ),
  value = c( rnorm(1000), rnorm(1000, mean=0) )
)
# Represent it
G21_Boys %>%
  ggplot( aes(x=value, fill=type)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")


#FEF2575_bmodel_6 Height/Age:   
FEF2575_bmodel_6 <- lm(FEF2575_pre ~ Altura_cm + Idade, data = ARIA_Boys)
summary(FEF2575_bmodel_6)
SEL2020$FEF2575_bmodel_6 <- (-1.29280+(0.02236*SEL2020$Altura_cm)+(0.05933*SEL2020$Idade))
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
#CI for the variables in FEF2575_bmodel_6:
confint(FEF2575_bmodel_6)
#Residuals for FEF2575_bmodel_6:
sigma(FEF2575_bmodel_6)/mean(ARIA_Boys$FEF2575_pre)
sigma(FEF2575_bmodel_6)/mean(G21_Boys$FEF2575_pre)
#Mean differences:
SEL2020$MD_FEF2575_bmodel_6 <- (SEL2020$FEF2575_bmodel_6-SEL2020$FEF2575_pre)
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
mean(G21_Boys$MD_FEF2575_bmodel_6)
summary(G21_Boys$MD_FEF2575_bmodel_6)
max(G21_Boys$MD_FEF2575_bmodel_6)-min(G21_Boys$MD_FEF2575_bmodel_6)
summary(G21_Boys$MD_FEF2575_GLI)
max(G21_Boys$MD_FEF2575_GLI)-min(G21_Boys$MD_FEF2575_GLI)
#Model evaluation:
FEF2575_bmodel_6_RSq_Mean_dif <- (0.1618699*(1/0.125))
FEF2575_bmodel_6_RSq_Mean_dif
AIC(FEF2575_bmodel_6)
BIC(FEF2575_bmodel_6)
cor.test(G21_Boys$FEF2575_pre, G21_Boys$FEF2575_bmodel_6, method = "pearson")
cor.test(G21_Boys$FEF2575_pre, G21_Boys$FEF2575_GLI, method = "pearson")
#Density and bar plots with mean differences compared to GLI:
hrbrthemes::import_roboto_condensed() 
ggplot(G21_Boys, aes(x=x) ) +
  geom_density( aes(x = MD_FEF2575_bmodel_6, y = ..density..), fill="#69b3a2" ) +
  geom_label( aes(x=0, y=0.25, label="MD_FEF2575_bmodel_6"), color="#69b3a2") +
  # Bottom
  geom_density( aes(x = MD_FEF2575_GLI, y = -..density..), fill= "#404080") +
  geom_label( aes(x=0, y=-0.25, label="MD_FEF2575_GLI"), color="#404080") +
  theme_ipsum() +
  xlab("Differences between predicted and measured FEF2575")

G21_Boys <- data.frame(
  type = c( rep("G21_Boys$MD_FEF2575_bmodel_6", 1000), rep("G21_Boys$MD_FEF2575_GLI", 1000) ),
  value = c( rnorm(1000), rnorm(1000, mean=0) )
)
# Represent it
G21_Boys %>%
  ggplot( aes(x=value, fill=type)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")

#FEF2575_bmodel_7 Height/BMI:   
FEF2575_bmodel_7 <- lm(FEF2575_pre ~ Altura_cm + BMI, data = ARIA_Boys)
summary(FEF2575_bmodel_7)
SEL2020$FEF2575_bmodel_7 <- (-1.108677+(0.023830*SEL2020$Altura_cm)+(0.007640*SEL2020$BMI))
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
#CI for the variables in FEF2575_bmodel_7:
confint(FEF2575_bmodel_7)
#Residuals for FEF2575_bmodel_7:
sigma(FEF2575_bmodel_7)/mean(ARIA_Boys$FEF2575_pre)
sigma(FEF2575_bmodel_7)/mean(G21_Boys$FEF2575_pre)
#Mean differences:
SEL2020$MD_FEF2575_bmodel_7 <- (SEL2020$FEF2575_bmodel_7-SEL2020$FEF2575_pre)
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
mean(G21_Boys$MD_FEF2575_bmodel_7)
summary(G21_Boys$MD_FEF2575_bmodel_7)
max(G21_Boys$MD_FEF2575_bmodel_7)-min(G21_Boys$MD_FEF2575_bmodel_7)
summary(G21_Boys$MD_FEF2575_GLI)
max(G21_Boys$MD_FEF2575_GLI)-min(G21_Boys$MD_FEF2575_GLI)
#Model evaluation:
FEF2575_bmodel_7_RSq_Mean_dif <- (0.1026366*(1/0.1204))
FEF2575_bmodel_7_RSq_Mean_dif
AIC(FEF2575_bmodel_7)
BIC(FEF2575_bmodel_7)
cor.test(G21_Boys$FEF2575_pre, G21_Boys$FEF2575_bmodel_7, method = "pearson")
cor.test(G21_Boys$FEF2575_pre, G21_Boys$FEF2575_GLI, method = "pearson")
#Density and bar plots with mean differences compared to GLI:
hrbrthemes::import_roboto_condensed() 
ggplot(G21_Boys, aes(x=x) ) +
  geom_density( aes(x = MD_FEF2575_bmodel_7, y = ..density..), fill="#69b3a2" ) +
  geom_label( aes(x=0, y=0.25, label="MD_FEF2575_bmodel_7"), color="#69b3a2") +
  # Bottom
  geom_density( aes(x = MD_FEF2575_GLI, y = -..density..), fill= "#404080") +
  geom_label( aes(x=0, y=-0.25, label="MD_FEF2575_GLI"), color="#404080") +
  theme_ipsum() +
  xlab("Differences between predicted and measured FEF2575")

G21_Boys <- data.frame(
  type = c( rep("G21_Boys$MD_FEF2575_bmodel_7", 1000), rep("G21_Boys$MD_FEF2575_GLI", 1000) ),
  value = c( rnorm(1000), rnorm(1000, mean=0) )
)
# Represent it
G21_Boys %>%
  ggplot( aes(x=value, fill=type)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")

#FEF2575_bmodel_8 Height/Weight/Age:   
FEF2575_bmodel_8 <- lm(FEF2575_pre ~ Altura_cm + Peso + Idade, data = ARIA_Boys)
summary(FEF2575_bmodel_8)
SEL2020$FEF2575_bmodel_8 <- (-1.082534+(0.020305*SEL2020$Altura_cm)+(0.002648*SEL2020$Peso)+(0.057155*SEL2020$Idade))
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
#CI for the variables in FEF2575_bmodel_8:
confint(FEF2575_bmodel_8)
#Residuals for FEF2575_bmodel_8:
sigma(FEF2575_bmodel_8)/mean(ARIA_Boys$FEF2575_pre)
sigma(FEF2575_bmodel_8)/mean(G21_Boys$FEF2575_pre)
#Mean differences:
SEL2020$MD_FEF2575_bmodel_8 <- (SEL2020$FEF2575_bmodel_8-SEL2020$FEF2575_pre)
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
mean(G21_Boys$MD_FEF2575_bmodel_8)
summary(G21_Boys$MD_FEF2575_bmodel_8)
max(G21_Boys$MD_FEF2575_bmodel_8)-min(G21_Boys$MD_FEF2575_bmodel_8)
summary(G21_Boys$MD_FEF2575_GLI)
max(G21_Boys$MD_FEF2575_GLI)-min(G21_Boys$MD_FEF2575_GLI)
#Model evaluation:
FEF2575_bmodel_8_RSq_Mean_dif <- (0.1596615*(1/0.1226))
FEF2575_bmodel_8_RSq_Mean_dif
AIC(FEF2575_bmodel_8)
BIC(FEF2575_bmodel_8)
cor.test(G21_Boys$FEF2575_pre, G21_Boys$FEF2575_bmodel_8, method = "pearson")
cor.test(G21_Boys$FEF2575_pre, G21_Boys$FEF2575_GLI, method = "pearson")
#Density and bar plots with mean differences compared to GLI:
hrbrthemes::import_roboto_condensed() 
ggplot(G21_Boys, aes(x=x) ) +
  geom_density( aes(x = MD_FEF2575_bmodel_8, y = ..density..), fill="#69b3a2" ) +
  geom_label( aes(x=0, y=0.25, label="MD_FEF2575_bmodel_8"), color="#69b3a2") +
  # Bottom
  geom_density( aes(x = MD_FEF2575_GLI, y = -..density..), fill= "#404080") +
  geom_label( aes(x=0, y=-0.25, label="MD_FEF2575_GLI"), color="#404080") +
  theme_ipsum() +
  xlab("Differences between predicted and measured FEF2575")

G21_Boys <- data.frame(
  type = c( rep("G21_Boys$MD_FEF2575_bmodel_8", 1000), rep("G21_Boys$MD_FEF2575_GLI", 1000) ),
  value = c( rnorm(1000), rnorm(1000, mean=0) )
)
# Represent it
G21_Boys %>%
  ggplot( aes(x=value, fill=type)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")

#FEF2575_bmodel_9 Height/Weight/BMI:   
FEF2575_bmodel_9 <- lm(FEF2575_pre ~ Altura_cm + Peso + BMI, data = ARIA_Boys)
summary(FEF2575_bmodel_9)
SEL2020$FEF2575_bmodel_9 <- (-4.23008+(0.04636*SEL2020$Altura_cm)+(-0.04584*SEL2020$Peso)+(0.09648*SEL2020$BMI))
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
#CI for the variables in FEF2575_bmodel_9:
confint(FEF2575_bmodel_9)
#Residuals for FEF2575_bmodel_9:
sigma(FEF2575_bmodel_9)/mean(ARIA_Boys$FEF2575_pre)
sigma(FEF2575_bmodel_9)/mean(G21_Boys$FEF2575_pre)
#Mean differences:
SEL2020$MD_FEF2575_bmodel_9 <- (SEL2020$FEF2575_bmodel_9-SEL2020$FEF2575_pre)
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
mean(G21_Boys$MD_FEF2575_bmodel_9)
summary(G21_Boys$MD_FEF2575_bmodel_9)
max(G21_Boys$MD_FEF2575_bmodel_9)-min(G21_Boys$MD_FEF2575_bmodel_9)
summary(G21_Boys$MD_FEF2575_GLI)
max(G21_Boys$MD_FEF2575_GLI)-min(G21_Boys$MD_FEF2575_GLI)
#Model evaluation:
SEL2020$FEF2575_bmodel_9_RSq_Mean_dif <- (0.1040491*(1/0.1208))
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
mean(G21_Boys$FEF2575_bmodel_9_RSq_Mean_dif)
AIC(FEF2575_bmodel_9)
BIC(FEF2575_bmodel_9)
cor.test(G21_Boys$FEF2575_pre, G21_Boys$FEF2575_bmodel_9, method = "pearson")
cor.test(G21_Boys$FEF2575_pre, G21_Boys$FEF2575_GLI, method = "pearson")
#Density and bar plots with mean differences compared to GLI:
hrbrthemes::import_roboto_condensed() 
ggplot(G21_Boys, aes(x=x) ) +
  geom_density( aes(x = MD_FEF2575_bmodel_9, y = ..density..), fill="#69b3a2" ) +
  geom_label( aes(x=1, y=0.25, label="MD_FEF2575_bmodel_9"), color="#69b3a2") +
  # Bottom
  geom_density( aes(x = MD_FEF2575_GLI, y = -..density..), fill= "#404080") +
  geom_label( aes(x=1, y=-0.25, label="MD_FEF2575_GLI"), color="#404080") +
  theme_ipsum() +
  xlab("Differences between predicted and measured FEF2575")

G21_Boys <- data.frame(
  type = c( rep("G21_Boys$MD_FEF2575_bmodel_9", 1000), rep("G21_Boys$MD_FEF2575_GLI", 1000) ),
  value = c( rnorm(1000), rnorm(1000, mean=0) )
)
# Represent it
G21_Boys %>%
  ggplot( aes(x=value, fill=type)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")


#FEF2575_bmodel_10 Height/Weight/BMI/Age:   
FEF2575_bmodel_10 <- lm(FEF2575_pre ~ Altura_cm + Peso + BMI + Idade, data = ARIA_Boys)
summary(FEF2575_bmodel_10)
SEL2020$FEF2575_bmodel_10 <- (-4.31822+(0.04357*SEL2020$Altura_cm)+(-0.04500*SEL2020$Peso)+(0.09358*SEL2020$BMI)+(0.05595*SEL2020$Idade))
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
#CI for the variables in FEF2575_bmodel_10:
confint(FEF2575_bmodel_10)
#Residuals for FEF2575_bmodel_10:
sigma(FEF2575_bmodel_10)/mean(ARIA_Boys$FEF2575_pre)
sigma(FEF2575_bmodel_10)/mean(G21_Boys$FEF2575_pre)
#Mean differences:
SEL2020$MD_FEF2575_bmodel_10 <- (SEL2020$FEF2575_bmodel_10-SEL2020$FEF2575_pre)
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
mean(G21_Boys$MD_FEF2575_bmodel_10)
summary(G21_Boys$MD_FEF2575_bmodel_10)
max(G21_Boys$MD_FEF2575_bmodel_10)-min(G21_Boys$MD_FEF2575_bmodel_10)
summary(G21_Boys$MD_FEF2575_GLI)
max(G21_Boys$MD_FEF2575_GLI)-min(G21_Boys$MD_FEF2575_GLI)
#Model evaluation:
FEF2575_bmodel_10_RSq_Mean_dif <- (0.1593509*(1/0.1234))
FEF2575_bmodel_10_RSq_Mean_dif
AIC(FEF2575_bmodel_10)
BIC(FEF2575_bmodel_10)
cor.test(G21_Boys$FEF2575_pre, G21_Boys$FEF2575_bmodel_10, method = "pearson")
cor.test(G21_Boys$FEF2575_pre, G21_Boys$FEF2575_GLI, method = "pearson")
#Density and bar plots with mean differences compared to GLI:
hrbrthemes::import_roboto_condensed() 
ggplot(G21_Boys, aes(x=x) ) +
  geom_density( aes(x = MD_FEF2575_bmodel_10, y = ..density..), fill="#69b3a2" ) +
  geom_label( aes(x=0, y=0.25, label="MD_FEF2575_bmodel_10"), color="#69b3a2") +
  # Bottom
  geom_density( aes(x = MD_FEF2575_GLI, y = -..density..), fill= "#404080") +
  geom_label( aes(x=0, y=-0.25, label="MD_FEF2575_GLI"), color="#404080") +
  theme_ipsum() +
  xlab("Differences between predicted and measured FEF2575")

G21_Boys <- data.frame(
  type = c( rep("G21_Boys$MD_FEF2575_bmodel_10", 1000), rep("G21_Boys$MD_FEF2575_GLI", 1000) ),
  value = c( rnorm(1000), rnorm(1000, mean=0) )
)
# Represent it
G21_Boys %>%
  ggplot( aes(x=value, fill=type)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")


#FEF2575_bmodel_11 Weight/Age:   
FEF2575_bmodel_11 <- lm(FEF2575_pre ~ Peso + Idade, data = ARIA_Boys)
summary(FEF2575_bmodel_11)
SEL2020$FEF2575_bmodel_11 <- (0.979019+(0.013610*SEL2020$Peso)+(0.094884*SEL2020$Idade))
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
#CI for the variables in FEF2575_bmodel_11:
confint(FEF2575_bmodel_11)
#Residuals for FEF2575_bmodel_11:
sigma(FEF2575_bmodel_11)/mean(ARIA_Boys$FEF2575_pre)
sigma(FEF2575_bmodel_11)/mean(G21_Boys$FEF2575_pre)
#Mean differences:
SEL2020$MD_FEF2575_bmodel_11 <- (SEL2020$FEF2575_bmodel_11-SEL2020$FEF2575_pre)
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
mean(G21_Boys$MD_FEF2575_bmodel_11)
summary(G21_Boys$MD_FEF2575_bmodel_11)
max(G21_Boys$MD_FEF2575_bmodel_11)-min(G21_Boys$MD_FEF2575_bmodel_11)
summary(G21_Boys$MD_FEF2575_GLI)
max(G21_Boys$MD_FEF2575_GLI)-min(G21_Boys$MD_FEF2575_GLI)
#Model evaluation:
FEF2575_bmodel_11_RSq_Mean_dif <- (0.1502014*(1/0.08693))
FEF2575_bmodel_11_RSq_Mean_dif
AIC(FEF2575_bmodel_11)
BIC(FEF2575_bmodel_11)
cor.test(G21_Boys$FEF2575_pre, G21_Boys$FEF2575_bmodel_11, method = "pearson")
cor.test(G21_Boys$FEF2575_pre, G21_Boys$FEF2575_GLI, method = "pearson")
#Density and bar plots with mean differences compared to GLI:
hrbrthemes::import_roboto_condensed() 
ggplot(G21_Boys, aes(x=x) ) +
  geom_density( aes(x = MD_FEF2575_bmodel_11, y = ..density..), fill="#69b3a2" ) +
  geom_label( aes(x=0, y=0.25, label="MD_FEF2575_bmodel_11"), color="#69b3a2") +
  # Bottom
  geom_density( aes(x = MD_FEF2575_GLI, y = -..density..), fill= "#404080") +
  geom_label( aes(x=0, y=-0.25, label="MD_FEF2575_GLI"), color="#404080") +
  theme_ipsum() +
  xlab("Differences between predicted and measured FEF2575")

G21_Boys <- data.frame(
  type = c( rep("G21_Boys$MD_FEF2575_bmodel_11", 1000), rep("G21_Boys$MD_FEF2575_GLI", 1000) ),
  value = c( rnorm(1000), rnorm(1000, mean=0) )
)
# Represent it
G21_Boys %>%
  ggplot( aes(x=value, fill=type)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")

#FEF2575_bmodel_12 Weight/BMI:   
FEF2575_bmodel_12 <- lm(FEF2575_pre ~ Peso + BMI, data = ARIA_Boys)
summary(FEF2575_bmodel_12)
SEL2020$FEF2575_bmodel_12 <- (2.161182+(0.044622*SEL2020$Peso)+(-0.077320*SEL2020$BMI))
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
#CI for the variables in FEF2575_bmodel_12:
confint(FEF2575_bmodel_12)
#Residuals for FEF2575_bmodel_12:
sigma(FEF2575_bmodel_12)/mean(ARIA_Boys$FEF2575_pre)
sigma(FEF2575_bmodel_12)/mean(G21_Boys$FEF2575_pre)
#Mean differences:
SEL2020$MD_FEF2575_bmodel_12 <- (SEL2020$FEF2575_bmodel_12-SEL2020$FEF2575_pre)
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
mean(G21_Boys$MD_FEF2575_bmodel_12)
summary(G21_Boys$MD_FEF2575_bmodel_12)
max(G21_Boys$MD_FEF2575_bmodel_12)-min(G21_Boys$MD_FEF2575_bmodel_12)
summary(G21_Boys$MD_FEF2575_GLI)
max(G21_Boys$MD_FEF2575_GLI)-min(G21_Boys$MD_FEF2575_GLI)
#Model evaluation:
FEF2575_bmodel_12_RSq_Mean_dif <- (0.09414054*(1/0.1089))
FEF2575_bmodel_12_RSq_Mean_dif
AIC(FEF2575_bmodel_12)
BIC(FEF2575_bmodel_12)
cor.test(G21_Boys$FEF2575_pre, G21_Boys$FEF2575_bmodel_12, method = "pearson")
cor.test(G21_Boys$FEF2575_pre, G21_Boys$FEF2575_GLI, method = "pearson")
#Density and bar plots with mean differences compared to GLI:
hrbrthemes::import_roboto_condensed() 
ggplot(G21_Boys, aes(x=x) ) +
  geom_density( aes(x = MD_FEF2575_bmodel_12, y = ..density..), fill="#69b3a2" ) +
  geom_label( aes(x=0, y=0.25, label="MD_FEF2575_bmodel_12"), color="#69b3a2") +
  # Bottom
  geom_density( aes(x = MD_FEF2575_GLI, y = -..density..), fill= "#404080") +
  geom_label( aes(x=0, y=-0.25, label="MD_FEF2575_GLI"), color="#404080") +
  theme_ipsum() +
  xlab("Differences between predicted and measured FEF2575")

G21_Boys <- data.frame(
  type = c( rep("G21_Boys$MD_FEF2575_bmodel_12", 1000), rep("G21_Boys$MD_FEF2575_GLI", 1000) ),
  value = c( rnorm(1000), rnorm(1000, mean=0) )
)
# Represent it
G21_Boys %>%
  ggplot( aes(x=value, fill=type)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")

#FEF2575_bmodel_13 Weight/BMI/Age:   
FEF2575_bmodel_13 <- lm(FEF2575_pre ~ Peso + BMI + Idade, data = ARIA_Boys)
summary(FEF2575_bmodel_13)
SEL2020$FEF2575_bmodel_13 <- (1.62016+(0.03935*SEL2020$Peso)+(-0.06866*SEL2020$BMI)+(0.06406*SEL2020$Idade))
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
#CI for the variables in FEF2575_bmodel_13:
confint(FEF2575_bmodel_13)
#Residuals for FEF2575_bmodel_13:
sigma(FEF2575_bmodel_13)/mean(ARIA_Boys$FEF2575_pre)
sigma(FEF2575_bmodel_13)/mean(G21_Boys$FEF2575_pre)
#Mean differences:
SEL2020$MD_FEF2575_bmodel_13 <- (SEL2020$FEF2575_bmodel_13-SEL2020$FEF2575_pre)
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
mean(G21_Boys$MD_FEF2575_bmodel_13)
summary(G21_Boys$MD_FEF2575_bmodel_13)
max(G21_Boys$MD_FEF2575_bmodel_13)-min(G21_Boys$MD_FEF2575_bmodel_13)
summary(G21_Boys$MD_FEF2575_GLI)
max(G21_Boys$MD_FEF2575_GLI)-min(G21_Boys$MD_FEF2575_GLI)
#Model evaluation:
FEF2575_bmodel_13_RSq_Mean_dif <- (0.1584912*(1/0.1133))
FEF2575_bmodel_13_RSq_Mean_dif
AIC(FEF2575_bmodel_13)
BIC(FEF2575_bmodel_13)
cor.test(G21_Boys$FEF2575_pre, G21_Boys$FEF2575_bmodel_13, method = "pearson")
cor.test(G21_Boys$FEF2575_pre, G21_Boys$FEF2575_GLI, method = "pearson")
#Density and bar plots with mean differences compared to GLI:
hrbrthemes::import_roboto_condensed() 
ggplot(G21_Boys, aes(x=x) ) +
  geom_density( aes(x = MD_FEF2575_bmodel_13, y = ..density..), fill="#69b3a2" ) +
  geom_label( aes(x=0, y=0.25, label="MD_FEF2575_bmodel_13"), color="#69b3a2") +
  # Bottom
  geom_density( aes(x = MD_FEF2575_GLI, y = -..density..), fill= "#404080") +
  geom_label( aes(x=0, y=-0.25, label="MD_FEF2575_GLI"), color="#404080") +
  theme_ipsum() +
  xlab("Differences between predicted and measured FEF2575")

G21_Boys <- data.frame(
  type = c( rep("G21_Boys$MD_FEF2575_bmodel_13", 1000), rep("G21_Boys$MD_FEF2575_GLI", 1000) ),
  value = c( rnorm(1000), rnorm(1000, mean=0) )
)
# Represent it
G21_Boys %>%
  ggplot( aes(x=value, fill=type)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")

#FEF2575_bmodel_14 BMI/Age:   
FEF2575_bmodel_14 <- lm(FEF2575_pre ~ BMI + Idade, data = ARIA_Boys)
summary(FEF2575_bmodel_14)
SEL2020$FEF2575_bmodel_14 <- (0.792584+(0.019456*SEL2020$BMI)+(0.128283*SEL2020$Idade))
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
#CI for the variables in FEF2575_bmodel_14:
confint(FEF2575_bmodel_14)
#Residuals for FEF2575_bmodel_14:
sigma(FEF2575_bmodel_14)/mean(ARIA_Boys$FEF2575_pre)
sigma(FEF2575_bmodel_14)/mean(G21_Boys$FEF2575_pre)
#Mean differences:
SEL2020$MD_FEF2575_bmodel_14 <- (SEL2020$FEF2575_bmodel_14-SEL2020$FEF2575_pre)
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
mean(G21_Boys$MD_FEF2575_bmodel_14)
summary(G21_Boys$MD_FEF2575_bmodel_14)
max(G21_Boys$MD_FEF2575_bmodel_14)-min(G21_Boys$MD_FEF2575_bmodel_14)
summary(G21_Boys$MD_FEF2575_GLI)
max(G21_Boys$MD_FEF2575_GLI)-min(G21_Boys$MD_FEF2575_GLI)
#Model evaluation:
FEF2575_bmodel_14_RSq_Mean_dif <- (0.1554975*(1/0.05832))
FEF2575_bmodel_14_RSq_Mean_dif
AIC(FEF2575_bmodel_14)
BIC(FEF2575_bmodel_14)
cor.test(G21_Boys$FEF2575_pre, G21_Boys$FEF2575_bmodel_14, method = "pearson")
cor.test(G21_Boys$FEF2575_pre, G21_Boys$FEF2575_GLI, method = "pearson")
#Density and bar plots with mean differences compared to GLI:
hrbrthemes::import_roboto_condensed() 
ggplot(G21_Boys, aes(x=x) ) +
  geom_density( aes(x = MD_FEF2575_bmodel_14, y = ..density..), fill="#69b3a2" ) +
  geom_label( aes(x=0, y=0.25, label="MD_FEF2575_bmodel_14"), color="#69b3a2") +
  # Bottom
  geom_density( aes(x = MD_FEF2575_GLI, y = -..density..), fill= "#404080") +
  geom_label( aes(x=0, y=-0.25, label="MD_FEF2575_GLI"), color="#404080") +
  theme_ipsum() +
  xlab("Differences between predicted and measured FEF2575")

G21_Boys <- data.frame(
  type = c( rep("G21_Boys$MD_FEF2575_bmodel_14", 1000), rep("G21_Boys$MD_FEF2575_GLI", 1000) ),
  value = c( rnorm(1000), rnorm(1000, mean=0) )
)
# Represent it
G21_Boys %>%
  ggplot( aes(x=value, fill=type)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")

#Multiple Linear Regression FEF2575 Girls: 

#FEF2575_gmodel 1 Height
FEF2575_gmodel_1 <- lm(FEF2575_pre ~ Altura_cm, data = ARIA_Girls)
summary(FEF2575_gmodel_1)
SEL2020$FEF2575_gmodel_1 <- (-1.388017+(0.027047*SEL2020$Altura_cm))
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
#CI for the variables in FEF2575_gmodel_1:
confint(FEF2575_gmodel_1)
#Residuals for FEF2575_gmodel_1:
sigma(FEF2575_gmodel_1)/mean(ARIA_Girls$FEF2575_pre)
sigma(FEF2575_gmodel_1)/mean(G21_Girls$FEF2575_pre)
#Mean differences:
SEL2020$MD_FEF2575_gmodel_1 <- (SEL2020$FEF2575_gmodel_1-SEL2020$FEF2575_pre)
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
mean(G21_Girls$MD_FEF2575_gmodel_1)
summary(G21_Girls$MD_FEF2575_gmodel_1)
max(G21_Girls$MD_FEF2575_gmodel_1)-min(G21_Girls$MD_FEF2575_gmodel_1)
summary(G21_Girls$MD_FEF2575_GLI)
max(G21_Girls$MD_FEF2575_GLI)-min(G21_Girls$MD_FEF2575_GLI)
#Model evaluation:
FEF2575_gmodel_1_RSq_Mean_dif <- (0.06096721*(1/0.1566))
FEF2575_gmodel_1_RSq_Mean_dif
AIC(FEF2575_gmodel_1)
BIC(FEF2575_gmodel_1)
cor.test(G21_Girls$FEF2575_pre, G21_Girls$FEF2575_gmodel_1, method = "pearson")
cor.test(G21_Girls$FEF2575_pre, G21_Girls$FEF2575_GLI, method = "pearson")
#Density and bar plots with mean differences compared to GLI:
hrbrthemes::import_roboto_condensed() 
ggplot(G21_Girls, aes(x=x) ) +
  geom_density( aes(x = MD_FEF2575_gmodel_1, y = ..density..), fill="#69b3a2" ) +
  geom_label( aes(x=0, y=0.25, label="MD_FEF2575_gmodel_1"), color="#69b3a2") +
  # Bottom
  geom_density( aes(x = MD_FEF2575_GLI, y = -..density..), fill= "#404080") +
  geom_label( aes(x=0, y=-0.25, label="MD_FEF2575_GLI"), color="#404080") +
  theme_ipsum() +
  xlab("Differences between predicted and measured FEF2575")

G21_Girls <- data.frame(
  type = c( rep("G21_Girls$MD_FEF2575_gmodel_1", 1000), rep("G21_Girls$MD_FEF2575_GLI", 1000) ),
  value = c( rnorm(1000), rnorm(1000, mean=0) )
)
# Represent it
G21_Girls %>%
  ggplot( aes(x=value, fill=type)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")

#FEF2575_gmodel_2 Weight:
FEF2575_gmodel_2 <- lm(FEF2575_pre ~ Peso, data = ARIA_Girls)
summary(FEF2575_gmodel_2)
SEL2020$FEF2575_gmodel_2 <- (1.324668+(0.028157*SEL2020$Peso))
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
#CI for the variables in FEF2575_gmodel_2:
confint(FEF2575_gmodel_2)
#Residuals for FEF2575_gmodel_2:
sigma(FEF2575_gmodel_2)/mean(ARIA_Girls$FEF2575_pre)
sigma(FEF2575_gmodel_2)/mean(G21_Girls$FEF2575_pre)
#Mean differences:
SEL2020$MD_FEF2575_gmodel_2 <- (SEL2020$FEF2575_gmodel_2-SEL2020$FEF2575_pre)
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
mean(G21_Girls$MD_FEF2575_gmodel_2)
summary(G21_Girls$MD_FEF2575_gmodel_2)
max(G21_Girls$MD_FEF2575_gmodel_2)-min(G21_Girls$MD_FEF2575_gmodel_2)
summary(G21_Girls$MD_FEF2575_GLI)
max(G21_Girls$MD_FEF2575_GLI)-min(G21_Girls$MD_FEF2575_GLI)
#Model evaluation:
FEF2575_gmodel_2_RSq_Mean_dif <- (0.03393618*(1/0.1865))
FEF2575_gmodel_2_RSq_Mean_dif
AIC(FEF2575_gmodel_2)
BIC(FEF2575_gmodel_2)
cor.test(G21_Girls$FEF2575_pre, G21_Girls$FEF2575_gmodel_2, method = "pearson")
cor.test(G21_Girls$FEF2575_pre, G21_Girls$FEF2575_GLI, method = "pearson")
#Density and bar plots with mean differences compared to GLI:
hrbrthemes::import_roboto_condensed() 
ggplot(G21_Girls, aes(x=x) ) +
  geom_density( aes(x = MD_FEF2575_gmodel_2, y = ..density..), fill="#69b3a2" ) +
  geom_label( aes(x=0, y=0.25, label="MD_FEF2575_gmodel_2"), color="#69b3a2") +
  # Bottom
  geom_density( aes(x = MD_FEF2575_GLI, y = -..density..), fill= "#404080") +
  geom_label( aes(x=0, y=-0.25, label="MD_FEF2575_GLI"), color="#404080") +
  theme_ipsum() +
  xlab("Differences between predicted and measured FEF2575")

G21_Girls <- data.frame(
  type = c( rep("G21_Girls$MD_FEF2575_gmodel_2", 1000), rep("G21_Girls$MD_FEF2575_GLI", 1000) ),
  value = c( rnorm(1000), rnorm(1000, mean=0) )
)
# Represent it
G21_Girls %>%
  ggplot( aes(x=value, fill=type)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")

#FEF2575_gmodel_3 Age:
FEF2575_gmodel_3 <- lm(FEF2575_pre ~ Idade, data = ARIA_Girls)
summary(FEF2575_gmodel_3)
SEL2020$FEF2575_gmodel_3 <- (0.84439+(0.16294*SEL2020$Idade))
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
#CI for the variables in FEF2575_gmodel_3:
confint(FEF2575_gmodel_3)
#Residuals for FEF2575_bmodel_3:
sigma(FEF2575_gmodel_3)/mean(ARIA_Girls$FEF2575_pre)
sigma(FEF2575_gmodel_3)/mean(G21_Girls$FEF2575_pre)
#Mean differences:
SEL2020$MD_FEF2575_gmodel_3 <- (SEL2020$FEF2575_gmodel_3-SEL2020$FEF2575_pre)
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
mean(G21_Girls$MD_FEF2575_gmodel_3)
summary(G21_Girls$MD_FEF2575_gmodel_3)
max(G21_Girls$MD_FEF2575_gmodel_3)-min(G21_Girls$MD_FEF2575_gmodel_3)
summary(G21_Girls$MD_FEF2575_GLI)
max(G21_Girls$MD_FEF2575_GLI)-min(G21_Girls$MD_FEF2575_GLI)
#Model evaluation:
FEF2575_gmodel_3_RSq_Mean_dif <- (0.1191158*(1/0.04836))
FEF2575_gmodel_3_RSq_Mean_dif
AIC(FEF2575_gmodel_3)
BIC(FEF2575_gmodel_3)
cor.test(G21_Girls$FEF2575_pre, G21_Girls$FEF2575_gmodel_3, method = "pearson")
cor.test(G21_Girls$FEF2575_pre, G21_Girls$FEF2575_GLI, method = "pearson")
#Density and bar plots with mean differences compared to GLI:
hrbrthemes::import_roboto_condensed() 
ggplot(G21_Girls, aes(x=x) ) +
  geom_density( aes(x = MD_FEF2575_gmodel_3, y = ..density..), fill="#69b3a2" ) +
  geom_label( aes(x=0, y=0.25, label="MD_FEF2575_gmodel_3"), color="#69b3a2") +
  # Bottom
  geom_density( aes(x = MD_FEF2575_GLI, y = -..density..), fill= "#404080") +
  geom_label( aes(x=0, y=-0.25, label="MD_FEF2575_GLI"), color="#404080") +
  theme_ipsum() +
  xlab("Differences between predicted and measured FEF2575")

G21_Girls <- data.frame(
  type = c( rep("G21_Girls$MD_FEF2575_gmodel_3", 1000), rep("G21_Girls$MD_FEF2575_GLI", 1000) ),
  value = c( rnorm(1000), rnorm(1000, mean=0) )
)
# Represent it
G21_Girls %>%
  ggplot( aes(x=value, fill=type)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")

#FEF2575_gmodel_4 BMI:
FEF2575_gmodel_4 <- lm(FEF2575_pre ~ BMI, data = ARIA_Girls)
summary(FEF2575_gmodel_4)
SEL2020$FEF2575_gmodel_4 <- (1.441935+(0.045569*SEL2020$BMI))
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
#CI for the variables in FEF2575_gmodel_4:
confint(FEF2575_gmodel_4)
#Residuals for FEF2575_gmodel_4:
sigma(FEF2575_gmodel_4)/mean(ARIA_Girls$FEF2575_pre)
sigma(FEF2575_gmodel_4)/mean(G21_Girls$FEF2575_pre)
#Mean differences:
SEL2020$MD_FEF2575_gmodel_4 <- (SEL2020$FEF2575_gmodel_4-SEL2020$FEF2575_pre)
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
mean(G21_Girls$MD_FEF2575_gmodel_4)
summary(G21_Girls$MD_FEF2575_gmodel_4)
max(G21_Girls$MD_FEF2575_gmodel_4)-min(G21_Girls$MD_FEF2575_gmodel_4)
summary(G21_Girls$MD_FEF2575_GLI)
max(G21_Girls$MD_FEF2575_GLI)-min(G21_Girls$MD_FEF2575_GLI)
#Model evaluation:
FEF2575_gmodel_4_RSq_Mean_dif <- (-0.04832888*(1/0.08526))
FEF2575_gmodel_4_RSq_Mean_dif
AIC(FEF2575_gmodel_4)
BIC(FEF2575_gmodel_4)
cor.test(G21_Girls$FEF2575_pre, G21_Girls$FEF2575_gmodel_4, method = "pearson")
cor.test(G21_Girls$FEF2575_pre, G21_Girls$FEF2575_GLI, method = "pearson")
#Density and bar plots with mean differences compared to GLI:
hrbrthemes::import_roboto_condensed() 
ggplot(G21_Girls, aes(x=x) ) +
  geom_density( aes(x = MD_FEF2575_gmodel_4, y = ..density..), fill="#69b3a2" ) +
  geom_label( aes(x=0, y=0.25, label="MD_FEF2575_gmodel_4"), color="#69b3a2") +
  # Bottom
  geom_density( aes(x = MD_FEF2575_GLI, y = -..density..), fill= "#404080") +
  geom_label( aes(x=0, y=-0.25, label="MD_FEF2575_GLI"), color="#404080") +
  theme_ipsum() +
  xlab("Differences between predicted and measured FEF2575")

G21_Girls <- data.frame(
  type = c( rep("G21_Girls$MD_FEF2575_gmodel_4", 1000), rep("G21_Girls$MD_FEF2575_GLI", 1000) ),
  value = c( rnorm(1000), rnorm(1000, mean=0) )
)
# Represent it
G21_Girls %>%
  ggplot( aes(x=value, fill=type)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")

#FEF2575_gmodel_5 Height/Weight:
FEF2575_gmodel_5 <- lm(FEF2575_pre ~ Altura_cm + Peso, data = ARIA_Girls)
summary(FEF2575_gmodel_5)
SEL2020$FEF2575_gmodel_5 <- (-0.265034+(0.013860*SEL2020$Altura_cm)+(0.019650*SEL2020$Peso))
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
#CI for the variables in FEF2575_gmodel_5:
confint(FEF2575_gmodel_5)
#Residuals for FEF2575_gmodel_5:
sigma(FEF2575_gmodel_5)/mean(ARIA_Girls$FEF2575_pre)
sigma(FEF2575_gmodel_5)/mean(G21_Girls$FEF2575_pre)
#Mean differences:
SEL2020$MD_FEF2575_gmodel_5 <- (SEL2020$FEF2575_gmodel_5-SEL2020$FEF2575_pre)
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
mean(G21_Girls$MD_FEF2575_gmodel_5)
summary(G21_Girls$MD_FEF2575_gmodel_5)
max(G21_Girls$MD_FEF2575_gmodel_5)-min(G21_Girls$MD_FEF2575_gmodel_5)
summary(G21_Girls$MD_FEF2575_GLI)
max(G21_Girls$MD_FEF2575_GLI)-min(G21_Girls$MD_FEF2575_GLI)
#Model evaluation:
FEF2575_gmodel_5_RSq_Mean_dif <- (0.0716321*(1/0.2077 ))
FEF2575_gmodel_5_RSq_Mean_dif
AIC(FEF2575_gmodel_5)
BIC(FEF2575_gmodel_5)
cor.test(G21_Girls$FEF2575_pre, G21_Girls$FEF2575_gmodel_5, method = "pearson")
cor.test(G21_Girls$FEF2575_pre, G21_Girls$FEF2575_GLI, method = "pearson")
#Density and bar plots with mean differences compared to GLI:
hrbrthemes::import_roboto_condensed() 
ggplot(G21_Girls, aes(x=x) ) +
  geom_density( aes(x = MD_FEF2575_gmodel_5, y = ..density..), fill="#69b3a2" ) +
  geom_label( aes(x=0, y=0.25, label="MD_FEF2575_gmodel_5"), color="#69b3a2") +
  # Bottom
  geom_density( aes(x = MD_FEF2575_GLI, y = -..density..), fill= "#404080") +
  geom_label( aes(x=0, y=-0.25, label="MD_FEF2575_GLI"), color="#404080") +
  theme_ipsum() +
  xlab("Differences between predicted and measured FEF2575")

G21_Girls <- data.frame(
  type = c( rep("G21_Girls$MD_FEF2575_gmodel_5", 1000), rep("G21_Girls$MD_FEF2575_GLI", 1000) ),
  value = c( rnorm(1000), rnorm(1000, mean=0) )
)
# Represent it
G21_Girls %>%
  ggplot( aes(x=value, fill=type)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")


#FEF2575_gmodel_6 Height/Age:   
FEF2575_gmodel_6 <- lm(FEF2575_pre ~ Altura_cm + Idade, data = ARIA_Girls)
summary(FEF2575_gmodel_6)
SEL2020$FEF2575_gmodel_6 <- (-1.48855+(0.02537*SEL2020$Altura_cm)+(0.03730*SEL2020$Idade))
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
#CI for the variables in FEF2575_gmodel_6:
confint(FEF2575_gmodel_6)
#Residuals for FEF2575_gmodel_6:
sigma(FEF2575_gmodel_6)/mean(ARIA_Girls$FEF2575_pre)
sigma(FEF2575_gmodel_6)/mean(G21_Girls$FEF2575_pre)
#Mean differences:
SEL2020$MD_FEF2575_gmodel_6 <- (SEL2020$FEF2575_gmodel_6-SEL2020$FEF2575_pre)
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
mean(G21_Girls$MD_FEF2575_gmodel_6)
summary(G21_Girls$MD_FEF2575_gmodel_6)
max(G21_Girls$MD_FEF2575_gmodel_6)-min(G21_Girls$MD_FEF2575_gmodel_6)
summary(G21_Girls$MD_FEF2575_GLI)
max(G21_Girls$MD_FEF2575_GLI)-min(G21_Girls$MD_FEF2575_GLI)
#Model evaluation:
FEF2575_gmodel_6_RSq_Mean_dif <- (0.09735112*(1/-0.004832))
FEF2575_gmodel_6_RSq_Mean_dif
AIC(FEF2575_gmodel_6)
BIC(FEF2575_gmodel_6)
cor.test(G21_Girls$FEF2575_pre, G21_Girls$FEF2575_gmodel_6, method = "pearson")
cor.test(G21_Girls$FEF2575_pre, G21_Girls$FEF2575_GLI, method = "pearson")
#Density and bar plots with mean differences compared to GLI:
hrbrthemes::import_roboto_condensed() 
ggplot(G21_Girls, aes(x=x) ) +
  geom_density( aes(x = MD_FEF2575_gmodel_6, y = ..density..), fill="#69b3a2" ) +
  geom_label( aes(x=0, y=0.25, label="MD_FEF2575_gmodel_6"), color="#69b3a2") +
  # Bottom
  geom_density( aes(x = MD_FEF2575_GLI, y = -..density..), fill= "#404080") +
  geom_label( aes(x=0, y=-0.25, label="MD_FEF2575_GLI"), color="#404080") +
  theme_ipsum() +
  xlab("Differences between predicted and measured FEF2575")

G21_Girls <- data.frame(
  type = c( rep("G21_Girls$MD_FEF2575_gmodel_6", 1000), rep("G21_Girls$MD_FEF2575_GLI", 1000) ),
  value = c( rnorm(1000), rnorm(1000, mean=0) )
)
# Represent it
G21_Girls %>%
  ggplot( aes(x=value, fill=type)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")

#FEF2575_gmodel_7 Height/BMI:   
FEF2575_gmodel_7 <- lm(FEF2575_pre ~ Altura_cm + BMI, data = ARIA_Girls)
summary(FEF2575_gmodel_7)
SEL2020$FEF2575_gmodel_7 <- (-1.776229+(0.024783*SEL2020$Altura_cm)+(0.038067*SEL2020$BMI))
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
#CI for the variables in FEF2575_gmodel_7:
confint(FEF2575_gmodel_7)
#Residuals for FEF2575_gmodel_7:
sigma(FEF2575_gmodel_7)/mean(ARIA_Girls$FEF2575_pre)
sigma(FEF2575_gmodel_7)/mean(G21_Girls$FEF2575_pre)
#Mean differences:
SEL2020$MD_FEF2575_gmodel_7 <- (SEL2020$FEF2575_gmodel_7-SEL2020$FEF2575_pre)
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
mean(G21_Girls$MD_FEF2575_gmodel_7)
summary(G21_Girls$MD_FEF2575_gmodel_7)
max(G21_Girls$MD_FEF2575_gmodel_7)-min(G21_Girls$MD_FEF2575_gmodel_7)
summary(G21_Girls$MD_FEF2575_GLI)
max(G21_Girls$MD_FEF2575_GLI)-min(G21_Girls$MD_FEF2575_GLI)
#Model evaluation:
FEF2575_gmodel_7_RSq_Mean_dif <- (0.07536896*(1/0.2146))
FEF2575_gmodel_7_RSq_Mean_dif
AIC(FEF2575_gmodel_7)
BIC(FEF2575_gmodel_7)
cor.test(G21_Girls$FEF2575_pre, G21_Girls$FEF2575_gmodel_7, method = "pearson")
cor.test(G21_Girls$FEF2575_pre, G21_Girls$FEF2575_GLI, method = "pearson")
#Density and bar plots with mean differences compared to GLI:
hrbrthemes::import_roboto_condensed() 
ggplot(G21_Girls, aes(x=x) ) +
  geom_density( aes(x = MD_FEF2575_gmodel_7, y = ..density..), fill="#69b3a2" ) +
  geom_label( aes(x=0, y=0.25, label="MD_FEF2575_gmodel_7"), color="#69b3a2") +
  # Bottom
  geom_density( aes(x = MD_FEF2575_GLI, y = -..density..), fill= "#404080") +
  geom_label( aes(x=0, y=-0.25, label="MD_FEF2575_GLI"), color="#404080") +
  theme_ipsum() +
  xlab("Differences between predicted and measured FEF2575")

G21_Girls <- data.frame(
  type = c( rep("G21_Girls$MD_FEF2575_gmodel_7", 1000), rep("G21_Girls$MD_FEF2575_GLI", 1000) ),
  value = c( rnorm(1000), rnorm(1000, mean=0) )
)
# Represent it
G21_Girls %>%
  ggplot( aes(x=value, fill=type)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")

#FEF2575_gmodel_8 Height/Weight/Age:   
FEF2575_gmodel_8 <- lm(FEF2575_pre ~ Altura_cm + Peso + Idade, data = ARIA_Girls)
summary(FEF2575_gmodel_8)
SEL2020$FEF2575_gmodel_8 <- (-0.380399+(0.011338*SEL2020$Altura_cm)+(0.020021*SEL2020$Peso)+(0.050657*SEL2020$Idade))
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
#CI for the variables in FEF2575_gmodel_8:
confint(FEF2575_gmodel_8)
#Residuals for FEF2575_gmodel_8:
sigma(FEF2575_gmodel_8)/mean(ARIA_Girls$FEF2575_pre)
sigma(FEF2575_gmodel_8)/mean(G21_Girls$FEF2575_pre)
#Mean differences:
SEL2020$MD_FEF2575_gmodel_8 <- (SEL2020$FEF2575_gmodel_8-SEL2020$FEF2575_pre)
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
mean(G21_Girls$MD_FEF2575_gmodel_8)
summary(G21_Girls$MD_FEF2575_gmodel_8)
max(G21_Girls$MD_FEF2575_gmodel_8)-min(G21_Girls$MD_FEF2575_gmodel_8)
summary(G21_Girls$MD_FEF2575_GLI)
max(G21_Girls$MD_FEF2575_GLI)-min(G21_Girls$MD_FEF2575_GLI)
#Model evaluation:
FEF2575_gmodel_8_RSq_Mean_dif <- (0.1218456*(1/0.2079))
FEF2575_gmodel_8_RSq_Mean_dif
AIC(FEF2575_gmodel_8)
BIC(FEF2575_gmodel_8)
cor.test(G21_Girls$FEF2575_pre, G21_Girls$FEF2575_gmodel_8, method = "pearson")
cor.test(G21_Girls$FEF2575_pre, G21_Girls$FEF2575_GLI, method = "pearson")
#Density and bar plots with mean differences compared to GLI:
hrbrthemes::import_roboto_condensed() 
ggplot(G21_Girls, aes(x=x) ) +
  geom_density( aes(x = MD_FEF2575_gmodel_8, y = ..density..), fill="#69b3a2" ) +
  geom_label( aes(x=0, y=0.25, label="MD_FEF2575_gmodel_8"), color="#69b3a2") +
  # Bottom
  geom_density( aes(x = MD_FEF2575_GLI, y = -..density..), fill= "#404080") +
  geom_label( aes(x=0, y=-0.25, label="MD_FEF2575_GLI"), color="#404080") +
  theme_ipsum() +
  xlab("Differences between predicted and measured FEF2575")

G21_Girls <- data.frame(
  type = c( rep("G21_Girls$MD_FEF2575_gmodel_8", 1000), rep("G21_Girls$MD_FEF2575_GLI", 1000) ),
  value = c( rnorm(1000), rnorm(1000, mean=0) )
)
# Represent it
G21_Girls %>%
  ggplot( aes(x=value, fill=type)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")

#FEF2575_gmodel_9 Height/Weight/BMI:   
FEF2575_gmodel_9 <- lm(FEF2575_pre ~ Altura_cm + Peso + BMI, data = ARIA_Girls)
summary(FEF2575_gmodel_9)
SEL2020$FEF2575_gmodel_9 <- (-4.13299+(0.04239*SEL2020$Altura_cm)+(-0.03122*SEL2020$Peso)+(0.09424*SEL2020$BMI))
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
#CI for the variables in FEF2575_gmodel_9:
confint(FEF2575_gmodel_9)
#Residuals for FEF2575_gmodel_9:
sigma(FEF2575_gmodel_9)/mean(ARIA_Girls$FEF2575_pre)
sigma(FEF2575_gmodel_9)/mean(G21_Girls$FEF2575_pre)
#Mean differences:
SEL2020$MD_FEF2575_gmodel_9 <- (SEL2020$FEF2575_gmodel_9-SEL2020$FEF2575_pre)
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
mean(G21_Girls$MD_FEF2575_gmodel_9)
summary(G21_Girls$MD_FEF2575_gmodel_9)
max(G21_Girls$MD_FEF2575_gmodel_9)-min(G21_Girls$MD_FEF2575_gmodel_9)
summary(G21_Girls$MD_FEF2575_GLI)
max(G21_Girls$MD_FEF2575_GLI)-min(G21_Girls$MD_FEF2575_GLI)
#Model evaluation:
SEL2020$FEF2575_gmodel_9_RSq_Mean_dif <- (0.07945132*(1/0.2149))
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
mean(G21_Girls$FEF2575_gmodel_9_RSq_Mean_dif)
AIC(FEF2575_gmodel_9)
BIC(FEF2575_gmodel_9)
cor.test(G21_Girls$FEF2575_pre, G21_Girls$FEF2575_gmodel_9, method = "pearson")
cor.test(G21_Girls$FEF2575_pre, G21_Girls$FEF2575_GLI, method = "pearson")
#Density and bar plots with mean differences compared to GLI:
hrbrthemes::import_roboto_condensed() 
ggplot(G21_Girls, aes(x=x) ) +
  geom_density( aes(x = MD_FEF2575_gmodel_9, y = ..density..), fill="#69b3a2" ) +
  geom_label( aes(x=1, y=0.25, label="MD_FEF2575_gmodel_9"), color="#69b3a2") +
  # Bottom
  geom_density( aes(x = MD_FEF2575_GLI, y = -..density..), fill= "#404080") +
  geom_label( aes(x=1, y=-0.25, label="MD_FEF2575_GLI"), color="#404080") +
  theme_ipsum() +
  xlab("Differences between predicted and measured FEF2575")

G21_Girls <- data.frame(
  type = c( rep("G21_Girls$MD_FEF2575_gmodel_9", 1000), rep("G21_Girls$MD_FEF2575_GLI", 1000) ),
  value = c( rnorm(1000), rnorm(1000, mean=0) )
)
# Represent it
G21_Girls %>%
  ggplot( aes(x=value, fill=type)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")


#FEF2575_gmodel_10 Height/Weight/BMI/Age:   
FEF2575_gmodel_10 <- lm(FEF2575_pre ~ Altura_cm + Peso + BMI + Idade, data = ARIA_Girls)
summary(FEF2575_gmodel_10)
SEL2020$FEF2575_gmodel_10 <- (-3.81755+(0.03811*SEL2020$Altura_cm)+(-0.02597*SEL2020$Peso)+(0.08492*SEL2020$BMI)+(0.02938*SEL2020$Idade))
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
#CI for the variables in FEF2575_gmodel_10:
confint(FEF2575_gmodel_10)
#Residuals for FEF2575_gmodel_10:
sigma(FEF2575_gmodel_10)/mean(ARIA_Girls$FEF2575_pre)
sigma(FEF2575_gmodel_10)/mean(G21_Girls$FEF2575_pre)
#Mean differences:
SEL2020$MD_FEF2575_gmodel_10 <- (SEL2020$FEF2575_gmodel_10- SEL2020$FEF2575_pre)
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
mean(G21_Girls$MD_FEF2575_gmodel_10)
summary(G21_Girls$MD_FEF2575_gmodel_10)
max(G21_Girls$MD_FEF2575_gmodel_10)-min(G21_Girls$MD_FEF2575_gmodel_10)
summary(G21_Girls$MD_FEF2575_GLI)
max(G21_Girls$MD_FEF2575_GLI)-min(G21_Girls$MD_FEF2575_GLI)
#Model evaluation:
FEF2575_gmodel_10_RSq_Mean_dif <- (0.1083663*(1/0.2124))
FEF2575_gmodel_10_RSq_Mean_dif
AIC(FEF2575_gmodel_10)
BIC(FEF2575_gmodel_10)
cor.test(G21_Girls$FEF2575_pre, G21_Girls$FEF2575_gmodel_10, method = "pearson")
cor.test(G21_Girls$FEF2575_pre, G21_Girls$FEF2575_GLI, method = "pearson")
#Density and bar plots with mean differences compared to GLI:
hrbrthemes::import_roboto_condensed() 
ggplot(G21_Girls, aes(x=x) ) +
  geom_density( aes(x = MD_FEF2575_gmodel_10, y = ..density..), fill="#69b3a2" ) +
  geom_label( aes(x=0, y=0.25, label="MD_FEF2575_gmodel_10"), color="#69b3a2") +
  # Bottom
  geom_density( aes(x = MD_FEF2575_GLI, y = -..density..), fill= "#404080") +
  geom_label( aes(x=0, y=-0.25, label="MD_FEF2575_GLI"), color="#404080") +
  theme_ipsum() +
  xlab("Differences between predicted and measured FEF2575")

G21_Girls <- data.frame(
  type = c( rep("G21_Girls$MD_FEF2575_gmodel_10", 1000), rep("G21_Girls$MD_FEF2575_GLI", 1000) ),
  value = c( rnorm(1000), rnorm(1000, mean=0) )
)
# Represent it
G21_Girls %>%
  ggplot( aes(x=value, fill=type)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")


#FEF2575_gmodel_11 Weight/Age:   
FEF2575_gmodel_11 <- lm(FEF2575_pre ~ Peso + Idade, data = ARIA_Girls)
summary(FEF2575_gmodel_11)
SEL2020$FEF2575_gmodel_11 <- (0.607018+(0.026069*SEL2020$Peso)+(0.089843*SEL2020$Idade))
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
#CI for the variables in FEF2575_gmodel_11:
confint(FEF2575_gmodel_11)
#Residuals for FEF2575_gmodel_11:
sigma(FEF2575_gmodel_11)/mean(ARIA_Girls$FEF2575_pre)
sigma(FEF2575_gmodel_11)/mean(G21_Girls$FEF2575_pre)
#Mean differences:
SEL2020$MD_FEF2575_gmodel_11 <- (SEL2020$FEF2575_gmodel_11-SEL2020$FEF2575_pre)
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
mean(G21_Girls$MD_FEF2575_gmodel_11)
summary(G21_Girls$MD_FEF2575_gmodel_11)
max(G21_Girls$MD_FEF2575_gmodel_11)+min(G21_Girls$FEF2575_gmodel_11)
summary(G21_Girls$FEF2575_GLI)
max(G21_Girls$MD_FEF2575_GLI)-min(G21_Girls$MD_FEF2575_GLI)
#Model evaluation:
FEF2575_gmodel_11_RSq_Mean_dif <- (0.1351207*(1/0.1978))
FEF2575_gmodel_11_RSq_Mean_dif
AIC(FEF2575_gmodel_11)
BIC(FEF2575_gmodel_11)
cor.test(G21_Girls$FEF2575_pre, G21_Girls$FEF2575_gmodel_11, method = "pearson")
cor.test(G21_Girls$FEF2575_pre, G21_Girls$FEF2575_GLI, method = "pearson")
#Density and bar plots with mean differences compared to GLI:
hrbrthemes::import_roboto_condensed() 
ggplot(G21_Girls, aes(x=x) ) +
  geom_density( aes(x = MD_FEF2575_gmodel_11, y = ..density..), fill="#69b3a2" ) +
  geom_label( aes(x=0, y=0.25, label="MD_FEF2575_gmodel_11"), color="#69b3a2") +
  # Bottom
  geom_density( aes(x = MD_FEF2575_GLI, y = -..density..), fill= "#404080") +
  geom_label( aes(x=0, y=-0.25, label="MD_FEF2575_GLI"), color="#404080") +
  theme_ipsum() +
  xlab("Differences between predicted and measured FEF2575")

G21_Girls <- data.frame(
  type = c( rep("G21_Girls$MD_FEF2575_gmodel_11", 1000), rep("G21_Girls$MD_FEF2575_GLI", 1000) ),
  value = c( rnorm(1000), rnorm(1000, mean=0) )
)
# Represent it
G21_Girls %>%
  ggplot( aes(x=value, fill=type)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")

#FEF2575_gmodel_12 Weight/BMI:   
FEF2575_gmodel_12 <- lm(FEF2575_pre ~ Peso + BMI, data = ARIA_Girls)
summary(FEF2575_gmodel_12)
SEL2020$FEF2575_gmodel_12 <- (1.531524+(0.039741*SEL2020$Peso)+(-0.032725*SEL2020$BMI))
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
#CI for the variables in FEF2575_gmodel_12:
confint(FEF2575_gmodel_12)
#Residuals for FEF2575_gmodel_12:
sigma(FEF2575_gmodel_12)/mean(ARIA_Girls$FEF2575_pre)
sigma(FEF2575_gmodel_12)/mean(G21_Girls$FEF2575_pre)
#Mean differences:
SEL2020$MD_FEF2575_gmodel_12 <- (SEL2020$FEF2575_gmodel_12-SEL2020$FEF2575_pre)
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
mean(G21_Girls$MD_FEF2575_gmodel_12)
summary(G21_Girls$MD_FEF2575_gmodel_12)
max(G21_Girls$MD_FEF2575_gmodel_12)-min(G21_Girls$MD_FEF2575_gmodel_12)
summary(G21_Girls$MD_FEF2575_GLI)
max(G21_Girls$MD_FEF2575_GLI)-min(G21_Girls$MD_FEF2575_GLI)
#Model evaluation:
FEF2575_gmodel_12_RSq_Mean_dif <- (0.05818562*(1/0.1968))
FEF2575_gmodel_12_RSq_Mean_dif
AIC(FEF2575_gmodel_12)
BIC(FEF2575_gmodel_12)
cor.test(G21_Girls$FEF2575_pre, G21_Girls$FEF2575_gmodel_12, method = "pearson")
cor.test(G21_Girls$FEF2575_pre, G21_Girls$FEF2575_GLI, method = "pearson")
#Density and bar plots with mean differences compared to GLI:
hrbrthemes::import_roboto_condensed() 
ggplot(G21_Girls, aes(x=x) ) +
  geom_density( aes(x = MD_FEF2575_gmodel_12, y = ..density..), fill="#69b3a2" ) +
  geom_label( aes(x=0, y=0.25, label="MD_FEF2575_gmodel_12"), color="#69b3a2") +
  # Bottom
  geom_density( aes(x = MD_FEF2575_GLI, y = -..density..), fill= "#404080") +
  geom_label( aes(x=0, y=-0.25, label="MD_FEF2575_GLI"), color="#404080") +
  theme_ipsum() +
  xlab("Differences between predicted and measured FEF2575")

G21_Girls <- data.frame(
  type = c( rep("G21_Girls$MD_FEF2575_gmodel_12", 1000), rep("G21_Girls$MD_FEF2575_GLI", 1000) ),
  value = c( rnorm(1000), rnorm(1000, mean=0) )
)
# Represent it
G21_Girls %>%
  ggplot( aes(x=value, fill=type)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")

#FEF2575_gmodel_13 Weight/BMI/Age:   
FEF2575_gmodel_13 <- lm(FEF2575_pre ~ Peso + BMI + Idade, data = ARIA_Girls)
summary(FEF2575_gmodel_13)
SEL2020$FEF2575_gmodel_13 <- (0.925210+(0.035195*SEL2020$Peso)+(-0.024435*SEL2020$BMI)+(0.069344*SEL2020$Idade))
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
#CI for the variables in FEF2575_gmodel_13:
confint(FEF2575_gmodel_13)
#Residuals for FEF2575_bmodel_13:
sigma(FEF2575_gmodel_13)/mean(ARIA_Girls$FEF2575_pre)
sigma(FEF2575_gmodel_13)/mean(G21_Girls$FEF2575_pre)
#Mean differences:
SEL2020$MD_FEF2575_gmodel_13 <- (SEL2020$FEF2575_gmodel_13-SEL2020$FEF2575_pre)
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
mean(G21_Girls$MD_FEF2575_gmodel_13)
summary(G21_Girls$MD_FEF2575_gmodel_13)
max(G21_Girls$MD_FEF2575_gmodel_13)-min(G21_Girls$MD_FEF2575_gmodel_13)
summary(G21_Girls$MD_FEF2575_GLI)
max(G21_Girls$MD_FEF2575_GLI)-min(G21_Girls$MD_FEF2575_GLI)
#Model evaluation:
FEF2575_gmodel_13_RSq_Mean_dif <- (0.1301388*(1/0.2011))
FEF2575_gmodel_13_RSq_Mean_dif
AIC(FEF2575_gmodel_13)
BIC(FEF2575_gmodel_13)
cor.test(G21_Girls$FEF2575_pre, G21_Girls$FEF2575_gmodel_13, method = "pearson")
cor.test(G21_Girls$FEF2575_pre, G21_Girls$FEF2575_GLI, method = "pearson")
#Density and bar plots with mean differences compared to GLI:
hrbrthemes::import_roboto_condensed() 
ggplot(G21_Girls, aes(x=x) ) +
  geom_density( aes(x = MD_FEF2575_gmodel_13, y = ..density..), fill="#69b3a2" ) +
  geom_label( aes(x=0, y=0.25, label="MD_FEF2575_gmodel_13"), color="#69b3a2") +
  # Bottom
  geom_density( aes(x = MD_FEF2575_GLI, y = -..density..), fill= "#404080") +
  geom_label( aes(x=0, y=-0.25, label="MD_FEF2575_GLI"), color="#404080") +
  theme_ipsum() +
  xlab("Differences between predicted and measured FEF2575")

G21_Girls <- data.frame(
  type = c( rep("G21_Girls$MD_FEF2575_gmodel_13", 1000), rep("G21_Girls$MD_FEF2575_GLI", 1000) ),
  value = c( rnorm(1000), rnorm(1000, mean=0) )
)
# Represent it
G21_Girls %>%
  ggplot( aes(x=value, fill=type)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")

#FEF2575_gmodel_14 BMI/Age:   
FEF2575_gmodel_14 <- lm(FEF2575_pre ~ BMI + Idade, data = ARIA_Girls)
summary(FEF2575_gmodel_14)
SEL2020$FEF2575_gmodel_14 <- (0.120286+(0.044092*SEL2020$BMI)+(0.153756*SEL2020$Idade))
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
#CI for the variables in FEF2575_gmodel_14:
confint(FEF2575_gmodel_14)
#Residuals for FEF2575_gmodel_14:
sigma(FEF2575_gmodel_14)/mean(ARIA_Girls$FEF2575_pre)
sigma(FEF2575_gmodel_14)/mean(G21_Girls$FEF2575_pre)
#Mean differences:
SEL2020$MD_FEF2575_gmodel_14 <- (SEL2020$FEF2575_gmodel_14-SEL2020$FEF2575_pre)
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
mean(G21_Girls$MD_FEF2575_gmodel_14)
summary(G21_Girls$MD_FEF2575_gmodel_14)
max(G21_Girls$MD_FEF2575_gmodel_14)-min(G21_Girls$MD_FEF2575_gmodel_14)
summary(G21_Girls$MD_FEF2575_GLI)
max(G21_Girls$MD_FEF2575_GLI)-min(G21_Girls$MD_FEF2575_GLI)
#Model evaluation:
FEF2575_gmodel_14_RSq_Mean_dif <- (0.1382315*(1/0.1283))
FEF2575_gmodel_14_RSq_Mean_dif
AIC(FEF2575_gmodel_14)
BIC(FEF2575_gmodel_14)
cor.test(G21_Girls$FEF2575_pre, G21_Girls$FEF2575_gmodel_14, method = "pearson")
cor.test(G21_Girls$FEF2575_pre, G21_Girls$FEF2575_GLI, method = "pearson")
#Density and bar plots with mean differences compared to GLI:
hrbrthemes::import_roboto_condensed() 
ggplot(G21_Girls, aes(x=x) ) +
  geom_density( aes(x = MD_FEF2575_gmodel_14, y = ..density..), fill="#69b3a2" ) +
  geom_label( aes(x=0, y=0.25, label="MD_FEF2575_gmodel_14"), color="#69b3a2") +
  # Bottom
  geom_density( aes(x = MD_FEF2575_GLI, y = -..density..), fill= "#404080") +
  geom_label( aes(x=0, y=-0.25, label="MD_FEF2575_GLI"), color="#404080") +
  theme_ipsum() +
  xlab("Differences between predicted and measured FEF2575")

G21_Girls <- data.frame(
  type = c( rep("G21_Girls$MD_FEF2575_gmodel_14", 1000), rep("G21_Girls$MD_FEF2575_GLI", 1000) ),
  value = c( rnorm(1000), rnorm(1000, mean=0) )
)
# Represent it
G21_Girls %>%
  ggplot( aes(x=value, fill=type)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")

#Improved models for FEF2575:

#Improved FEF2575_gmodel_15:
FEF2575_gmodel_15 <- lm(FEF2575_pre ~ Altura_cm + Peso + BMI + FVC_pre, data = ARIA_Girls)
summary(FEF2575_gmodel_15)
SEL2020$FEF2575_gmodel_15 <- (-1.23855+(0.01240*SEL2020$Altura_cm)+(-0.01284*SEL2020$Peso)+(0.04417*SEL2020$BMI)+(0.74267*SEL2020$FVC_pre))
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
#CI for the variables in FEF2575_gmodel_15:
confint(FEF2575_gmodel_15)
#Residuals for FEF2575_gmodel_15:
sigma(FEF2575_gmodel_15)/mean(ARIA_Girls$FEF2575_pre)
sigma(FEF2575_gmodel_15)/mean(G21_Girls$FEF2575_pre)
#Mean differences:
SEL2020$MD_FEF2575_gmodel_15 <- (SEL2020$FEF2575_gmodel_15-SEL2020$FEF2575_pre)
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
mean(G21_Girls$MD_FEF2575_gmodel_15)
summary(G21_Girls$MD_FEF2575_gmodel_15)
max(G21_Girls$MD_FEF2575_gmodel_15)-min(G21_Girls$MD_FEF2575_gmodel_15)
summary(G21_Girls$MD_FEF2575_GLI)
max(G21_Girls$MD_FEF2575_GLI)-min(G21_Girls$MD_FEF2575_GLI)
#Model evaluation:
SEL2020$FEF2575_gmodel_15_RSq_Mean_dif <- (0.1244337*(1/0.298))
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
mean(G21_Girls$FEF2575_gmodel_15_RSq_Mean_dif)
AIC(FEF2575_gmodel_15)
BIC(FEF2575_gmodel_15)
cor.test(G21_Girls$FEF2575_pre, G21_Girls$FEF2575_gmodel_15, method = "pearson")
cor.test(G21_Girls$FEF2575_pre, G21_Girls$FEF2575_GLI, method = "pearson")
#Density and bar plots with mean differences compared to GLI:
hrbrthemes::import_roboto_condensed() 
ggplot(G21_Girls, aes(x=x) ) +
  geom_density( aes(x = MD_FEF2575_gmodel_15, y = ..density..), fill="#69b3a2" ) +
  geom_label( aes(x=0, y=0.25, label="MD_FEF2575_gmodel_15"), color="#69b3a2") +
  # Bottom
  geom_density( aes(x = MD_FEF2575_GLI, y = -..density..), fill= "#404080") +
  geom_label( aes(x=0, y=-0.25, label="MD_FEF2575_GLI"), color="#404080") +
  theme_ipsum() +
  xlab("Differences between predicted and measured FEF2575")

G21_Girls <- data.frame(
  type = c( rep("G21_Girls$MD_FEF2575_gmodel_15", 1000), rep("G21_Girls$MD_FEF2575_GLI", 1000) ),
  value = c( rnorm(1000), rnorm(1000, mean=0) )
)
# Represent it
G21_Girls %>%
  ggplot( aes(x=value, fill=type)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")


#Improved FEF2575_bmodel_15:
FEF2575_bmodel_15 <- lm(FEF2575_pre ~ Altura_cm + Idade + FVC_pre, data = ARIA_Boys)
summary(FEF2575_bmodel_15)
SEL2020$FEF2575_bmodel_15 <- (0.4493680+(-0.0001922*SEL2020$Altura_cm)+(0.0510957*SEL2020$Idade)+(0.6790825*SEL2020$FVC_pre))
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
#CI for the variables in FEF2575_bmodel_15:
confint(FEF2575_bmodel_15)
#Residuals for FEF2575_bmodel_15:
sigma(FEF2575_bmodel_15)/mean(ARIA_Boys$FEF2575_pre)
sigma(FEF2575_bmodel_15)/mean(G21_Boys$FEF2575_pre)
#Mean differences:
SEL2020$MD_FEF2575_bmodel_15 <- (SEL2020$FEF2575_bmodel_15-SEL2020$FEF2575_pre)
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")
mean(G21_Boys$MD_FEF2575_bmodel_15)
summary(G21_Boys$MD_FEF2575_bmodel_15)
max(G21_Boys$MD_FEF2575_bmodel_15)-min(G21_Boys$MD_FEF2575_bmodel_15)
summary(G21_Boys$MD_FEF2575_GLI)
max(G21_Boys$MD_FEF2575_GLI)-min(G21_Boys$MD_FEF2575_GLI)
#Model evaluation:
FEF2575_bmodel_15_RSq_Mean_dif <- (0.2233394*(1/0.2234))
FEF2575_bmodel_15_RSq_Mean_dif
AIC(FEF2575_bmodel_15)
BIC(FEF2575_bmodel_15)
cor.test(G21_Boys$FEF2575_pre, G21_Boys$FEF2575_bmodel_15, method = "pearson")
cor.test(G21_Boys$FEF2575_pre, G21_Boys$FEF2575_GLI, method = "pearson")
#Density and bar plots with mean differences compared to GLI:
hrbrthemes::import_roboto_condensed() 
ggplot(G21_Boys, aes(x=x) ) +
  geom_density( aes(x = MD_FEF2575_bmodel_15, y = ..density..), fill="#69b3a2" ) +
  geom_label( aes(x=0, y=0.25, label="MD_FEF2575_bmodel_15"), color="#69b3a2") +
  # Bottom
  geom_density( aes(x = MD_FEF2575_GLI, y = -..density..), fill= "#404080") +
  geom_label( aes(x=0, y=-0.25, label="MD_FEF2575_GLI"), color="#404080") +
  theme_ipsum() +
  xlab("Differences between predicted and measured FEF2575")

G21_Boys <- data.frame(
  type = c( rep("G21_Boys$MD_FEF2575_bmodel_15", 1000), rep("G21_Boys$MD_FEF2575_GLI", 1000) ),
  value = c( rnorm(1000), rnorm(1000, mean=0) )
)
# Represent it
G21_Boys %>%
  ggplot( aes(x=value, fill=type)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")

#Repor datasets:
ARIA <- subset(SEL2020, Project == "ARIA")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")
G21 <- subset(SEL2020, Project == "G21")
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")

#FEV1/FVC intervals girls:
mean(ARIA_Girls$FEV1FVC_pre)
sd(ARIA_Girls$FEV1FVC_pre)

#FEV1/FVC intervals boys:
mean(ARIA_Boys$FEV1FVC_pre)
sd(ARIA_Boys$FEV1FVC_pre)


#Normality test/graphical visualization:
#Idade:
shapiro.test(ARIA$Idade)
shapiro.test(G21$Idade)

ggdensity(ARIA$Idade, 
          main = "Density plot of Age",
          xlab = "ARIA")
ggqqplot(ARIA$FVC_pre)

ggdensity(G21$Idade, 
          main = "Density plot of Age",
          xlab = "G21")
ggqqplot(G21$Idade)

qplot(sample = Idade, data = SEL2020, color=Project)

#Peso:
shapiro.test(ARIA$Peso)
shapiro.test(G21$Peso)

ggdensity(ARIA$Peso, 
          main = "Density plot of Weight",
          xlab = "ARIA")
ggqqplot(ARIA$Peso)

ggdensity(G21$Peso, 
          main = "Density plot of Weight",
          xlab = "G21")
ggqqplot(G21$Peso)

qplot(sample = Peso, data = SEL2020, color=Project)

#Altura:
shapiro.test(ARIA$Altura_cm)
shapiro.test(G21$Altura_cm)

ggdensity(ARIA$Altura_cm, 
          main = "Density plot of Height",
          xlab = "ARIA")
ggqqplot(ARIA$Altura_cm)

ggdensity(G21$Altura_cm, 
          main = "Density plot of Height",
          xlab = "G21")
ggqqplot(G21$Altura_cm)

qplot(sample = Altura_cm, data = SEL2020, color=Project)

#BMI:
shapiro.test(ARIA$BMI)
shapiro.test(G21$BMI)

ggdensity(ARIA$BMI, 
          main = "Density plot of BMI",
          xlab = "ARIA")
ggqqplot(ARIA$BMI)

ggdensity(G21$BMI, 
          main = "Density plot of BMI",
          xlab = "G21")
ggqqplot(G21$BMI)

qplot(sample = BMI, data = SEL2020, color=Project)

#FVC:
shapiro.test(ARIA$FVC_pre)
shapiro.test(G21$FVC_pre)

ggdensity(ARIA$FVC_pre, 
          main = "Density plot of FVC",
          xlab = "ARIA")
ggqqplot(ARIA$FVC_pre)

ggdensity(G21$FVC_pre, 
          main = "Density plot of FVC",
          xlab = "G21")
ggqqplot(G21$FVC_pre)

qplot(sample = FVC_pre, data = SEL2020, color=Project)

#FEV1:
shapiro.test(ARIA$FEV1_pre)
shapiro.test(G21$FEV1_pre)

ggdensity(ARIA$FEV1_pre, 
          main = "Density plot of FEV1",
          xlab = "ARIA")
ggqqplot(ARIA$FEV1_pre)

ggdensity(G21$FEV1_pre, 
          main = "Density plot of FEV1",
          xlab = "G21")
ggqqplot(G21$FEV1_pre)

qplot(sample = FEV1_pre, data = SEL2020, color=Project)

#FEV1/FVC:
shapiro.test(ARIA$FEV1FVC_pre)
shapiro.test(G21$FEV1FVC_pre)

ggdensity(ARIA$FEV1FVC_pre, 
          main = "Density plot of FEV1/FVC",
          xlab = "ARIA")
ggqqplot(ARIA$FEV1FVC_pre)

ggdensity(G21$FEV1FVC_pre, 
          main = "Density plot of FEV1/FVC",
          xlab = "G21")
ggqqplot(G21$FEV1FVC_pre)

qplot(sample = FEV1FVC_pre, data = SEL2020, color=Project)

#FEF2575:
shapiro.test(ARIA$FEF2575_pre)
shapiro.test(G21$FEF2575_pre)

ggdensity(ARIA$FEF2575_pre, 
          main = "Density plot of FEF2575",
          xlab = "ARIA")
ggqqplot(ARIA$FEF2575_pre)

ggdensity(G21$FEF2575_pre, 
          main = "Density plot of FEF2575",
          xlab = "G21")
ggqqplot(G21$FEF2575_pre)

qplot(sample = FEF2575_pre, data = SEL2020, color=Project)

#Bland-Altman plot to access degree of agreement between measured and predicted values:

#FVC Boys
  #ARIA Equations:
ggplot(data = G21_Boys) + geom_point(mapping = aes(x = FVC_pre, y = MD_FVC_bmodel_8))

blandr.statistics ( G21_Boys$FEV1_pre , G21_Boys$FVC_bmodel_8 , sig.level=0.95 )
blandr.draw( G21_Boys$FVC_pre , G21_Boys$FVC_bmodel_8, plotter="rplot", 
             plotTitle="Bland-Altman plot for measured FVC
and FVC predicted from ARIA Boys",
             normalLow, normalHigh)

  #GLI Equations:
ggplot(data = G21_Boys) + geom_point(mapping = aes(x = FVC_pre, y = MD_FVC_GLI))

blandr.statistics ( G21_Boys$FVC_pre , G21_Boys$FVC_GLI , sig.level=0.95 )
blandr.draw( G21_Boys$FVC_pre , G21_Boys$FVC_GLI, plotter="rplot", 
             plotTitle="Bland-Altman plot for measured FVC
and FVC predicted from GLI Boys",
             normalLow, normalHigh)

#FVC Girls:
  
#ARIA Equations:
ggplot(data = G21_Girls) + geom_point(mapping = aes(x = FVC_pre, y = MD_FVC_gmodel_9))

blandr.statistics ( G21_Girls$FVC_pre , G21_Girls$FVC_gmodel_9 , sig.level=0.95 )
blandr.draw( G21_Girls$FVC_pre , G21_Girls$FVC_gmodel_9, plotter="rplot", 
             plotTitle="Bland-Altman plot for measured FVC
and FVC predicted from ARIA Girls",
             normalLow, normalHigh)

#GLI Equations:
ggplot(data = G21_Girls) + geom_point(mapping = aes(x = FVC_pre, y = MD_FVC_GLI))

blandr.statistics ( G21_Girls$FVC_pre , G21_Girls$FVC_GLI , sig.level=0.95 )
blandr.draw( G21_Girls$FVC_pre , G21_Girls$FVC_GLI, plotter="rplot", 
             plotTitle="Bland-Altman plot for measured FVC
and FVC predicted from GLI Girls",
             normalLow, normalHigh)

#Bland-Altman FEV1:

#FEV1 Boys
#ARIA Equations:
ggplot(data = G21_Boys) + geom_point(mapping = aes(x = FEV1_pre, y = MD_FEV1_bmodel_6))

blandr.statistics ( G21_Boys$FEV1_pre , G21_Boys$FEV1_bmodel_6 , sig.level=0.95 )
blandr.draw( G21_Boys$FEV1_pre , G21_Boys$FEV1_bmodel_6, plotter="rplot", 
             plotTitle="Bland-Altman plot for measured FEV1
and FEV1 predicted from ARIA Boys",
             normalLow, normalHigh)

#GLI Equations:
ggplot(data = G21_Boys) + geom_point(mapping = aes(x = FEV1_pre, y = MD_FEV1_GLI))

blandr.statistics ( G21_Boys$FEV1_pre , G21_Boys$FEV1_GLI , sig.level=0.95 )
blandr.draw( G21_Boys$FEV1_pre , G21_Boys$FEV1_GLI, plotter="rplot", 
             plotTitle="Bland-Altman plot for measured FEV1
and FEV1 predicted from GLI Boys",
             normalLow, normalHigh)

#FEV1 Girls:

#ARIA Equations:
ggplot(data = G21_Girls) + geom_point(mapping = aes(x = FEV1_pre, y = MD_FEV1_gmodel_10))

blandr.statistics ( G21_Girls$FEV1_pre , G21_Girls$FEV1_gmodel_10 , sig.level=0.95 )
blandr.draw( G21_Girls$FEV1_pre , G21_Girls$FEV1_gmodel_10, plotter="rplot", 
             plotTitle="Bland-Altman plot for measured FEV1
and FEV1 predicted from ARIA Girls",
             normalLow, normalHigh)

#GLI Equations:
ggplot(data = G21_Girls) + geom_point(mapping = aes(x = FEV1_pre, y = MD_FEV1_GLI))

blandr.statistics ( G21_Girls$FEV1_pre , G21_Girls$FEV1_GLI , sig.level=0.95 )
blandr.draw( G21_Girls$FEV1_pre , G21_Girls$FEV1_GLI, plotter="rplot", 
             plotTitle="Bland-Altman plot for measured FEV1
and FEV1 predicted from GLI Girls",
             normalLow, normalHigh)

#Bland-Altman FEF2575:

#FEF2575 Boys
#ARIA Equations:
ggplot(data = G21_Boys) + geom_point(mapping = aes(x = FEF2575_pre, y = MD_FEF2575_bmodel_15))

blandr.statistics ( G21_Boys$FEF2575_pre , G21_Boys$FEF2575_bmodel_15 , sig.level=0.95 )
blandr.draw( G21_Boys$FEF2575_pre , G21_Boys$FEF2575_bmodel_15, plotter="rplot", 
             plotTitle="Bland-Altman plot for measured FEF2575
and FEF2575 predicted from ARIA Boys",
             normalLow, normalHigh)

#GLI Equations:
ggplot(data = G21_Boys) + geom_point(mapping = aes(x = FEF2575_pre, y = MD_FEF2575_GLI))

blandr.statistics ( G21_Boys$FEF2575_pre , G21_Boys$FEF2575_GLI , sig.level=0.95 )
blandr.draw( G21_Boys$FEF2575_pre , G21_Boys$FEF2575_GLI, plotter="rplot", 
             plotTitle="Bland-Altman plot for measured FEF2575
and FEF2575 predicted from GLI Boys",
             normalLow, normalHigh)

#FEF2575 Girls:

#ARIA Equations:
ggplot(data = G21_Girls) + geom_point(mapping = aes(x = FEF2575_pre, y = MD_FEF2575_gmodel_15))

blandr.statistics ( G21_Girls$FEF2575_pre , G21_Girls$FEF2575_gmodel_15 , sig.level=0.95 )
blandr.draw( G21_Girls$FEF2575_pre , G21_Girls$FEF2575_bmodel_15, plotter="rplot", 
             plotTitle="Bland-Altman plot for measured FEF2575
and FEF2575 predicted from ARIA Girls",
             normalLow, normalHigh)

#GLI Equations:
ggplot(data = G21_Girls) + geom_point(mapping = aes(x = FEF2575_pre, y = MD_FEF2575_GLI))

blandr.statistics ( G21_Girls$FEF2575_pre , G21_Girls$FEF2575_GLI , sig.level=0.95 )
blandr.draw( G21_Girls$FEF2575_pre , G21_Girls$FEF2575_GLI, plotter="rplot", 
             plotTitle="Bland-Altman plot for measured FEF2575
and FEF2575 predicted from GLI Girls",
             normalLow, normalHigh)

#Graphical Comparisons of Lung Function parameters between groups:

#FVC:
ggplot(data=SEL2020, aes(x=FVC_pre, group=Project, fill=Project)) + 
  geom_density(adjust=1.5, alpha=.4) + 
  theme_ipsum() + 
  scale_color_manual(name = "Project", values = c( "0" = "grey89", "1" = "grey48"), 
                     labels = c("0"="ARIA", "1"="G21"))

SEL2020$Project <- factor(SEL2020$Project , levels=c("ARIA", "G21"))
boxplot(SEL2020$FVC_pre ~ SEL2020$Project , ylab="FVC (L)" , 
    xlab="FVC distribution by Project", horizontal=FALSE, axes=TRUE ,range=0)


SEL2020 %>%
  ggplot( aes(x=Project, y=FVC_pre, fill=NULL)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.1, alpha=0.2) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=20)
  ) +
  ggtitle("FVC Distribution by Project") +
  xlab("")

SEL2020 %>%
  ggplot( aes(x=Project, y=FVC_pre, fill=Project)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("FVC Distribution by Project") +
  xlab("Project")

SEL2020 %>%
  ggplot( aes(x=Project, y=FVC_pre, fill=Project)) +
  geom_violin() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("FVC Distribution by Project") +
  xlab("")

#FEV1:
ggplot(data=SEL2020, aes(x=FEV1_pre, group=Project, fill=Project)) + 
  geom_density(adjust=1.5, alpha=.4) + 
  theme_ipsum() + 
  scale_color_manual(name = "Project", values = c( "0" = "grey90", "1" = "grey0"), 
                     labels = c("0"="ARIA", "1"="G21"))

SEL2020 %>%
  ggplot( aes(x=Project, y=FEV1_pre, fill=NULL)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.1, alpha=0.2) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=20)
  ) +
  ggtitle("FEV1 Distribution by Project") +
  xlab("")

SEL2020 %>%
  ggplot( aes(x=Project, y=FEV1_pre, fill=Project)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("FEV1 Distribution by Project") +
  xlab("Project")

SEL2020 %>%
  ggplot( aes(x=Project, y=FEV1_pre, fill=Project)) +
  geom_violin() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("FEV1 Distribution by Project") +
  xlab("")


#FEF2575:
ggplot(data=SEL2020, aes(x=FEF2575_pre, group=Project, fill=Project)) + 
  geom_density(adjust=1.5, alpha=.4) + 
  theme_ipsum() + 
  scale_color_manual(name = "Project", values = c( "0" = "grey90", "1" = "grey0"), 
                     labels = c("0"="ARIA", "1"="G21"))

SEL2020 %>%
  ggplot( aes(x=Project, y=FEF2575_pre, fill=NULL)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.1, alpha=0.2) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=20)
  ) +
  ggtitle("FEF2575 Distribution by Project") +
  xlab("")

SEL2020 %>%
  ggplot( aes(x=Project, y=FEF2575_pre, fill=Project)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("FEF2575 Distribution by Project") +
  xlab("Project")

SEL2020 %>%
  ggplot( aes(x=Project, y=FEF2575_pre, fill=Project)) +
  geom_violin() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("FEF2575 Distribution by Project") +
  xlab("")

#FEV1/FVC:
ggplot(data=SEL2020, aes(x=FEV1FVC_pre, group=Project, fill=Project)) + 
  geom_density(adjust=1.5, alpha=.4) + 
  theme_ipsum() + 
  scale_color_manual(name = "Project", values = c( "0" = "grey90", "1" = "grey0"), 
                     labels = c("0"="ARIA", "1"="G21"))

SEL2020 %>%
  ggplot( aes(x=Project, y=FEV1FVC_pre, fill=NULL)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.1, alpha=0.2) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=19)
  ) +
  ggtitle("FEV1FVC Distribution by Project") +
  xlab("")

SEL2020 %>%
  ggplot( aes(x=Project, y=FEV1FVC_pre, fill=Project)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=20)
  ) +
  ggtitle("FEV1FVC Distribution by Project") +
  xlab("Project")

SEL2020 %>%
  ggplot( aes(x=Project, y=FEV1FVC_pre, fill=Project)) +
  geom_violin() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("FEV1FVC Distribution by Project") +
  xlab("")

#Age:
ggplot(data=SEL2020, aes(x=Idade, group=Project, fill=Project)) + 
  geom_density(adjust=1.5, alpha=.4) + 
  theme_ipsum() + 
  scale_color_manual(name = "Project", values = c( "0" = "grey90", "1" = "grey0"), 
                     labels = c("0"="ARIA", "1"="G21"))

SEL2020 %>%
  ggplot( aes(x=Project, y=Idade, fill=NULL)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.1, alpha=0.2) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Age Distribution by Project") +
  xlab("")

SEL2020 %>%
  ggplot( aes(x=Project, y=Idade, fill=Project)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Age Distribution by Project") +
  xlab("Project")

SEL2020 %>%
  ggplot( aes(x=Project, y=Idade, fill=Project)) +
  geom_violin() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=20)
  ) +
  ggtitle("Age Distribution by Project") +
  xlab("")

#Height:
ggplot(data=SEL2020, aes(x=Altura_cm, group=Project, fill=Project)) + 
  geom_density(adjust=1.5, alpha=.4) + 
  theme_ipsum() + 
  scale_color_manual(name = "Project", values = c( "0" = "grey90", "1" = "grey0"), 
                     labels = c("0"="ARIA", "1"="G21"))

SEL2020 %>%
  ggplot( aes(x=Project, y=Altura_cm, fill=NULL)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.1, alpha=0.2) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=20)
  ) +
  ggtitle("Height Distribution by Project") +
  xlab("")

SEL2020 %>%
  ggplot( aes(x=Project, y=Altura_cm, fill=Project)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Height Distribution by Project") +
  xlab("Project")

SEL2020 %>%
  ggplot( aes(x=Project, y=Altura_cm, fill=Project)) +
  geom_violin() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Height Distribution by Project") +
  xlab("")


#Weight:
ggplot(data=SEL2020, aes(x=Peso, group=Project, fill=Project)) + 
  geom_density(adjust=1.5, alpha=.4) + 
  theme_ipsum() + 
  scale_color_manual(name = "Project", values = c( "0" = "grey90", "1" = "grey0"), 
                     labels = c("0"="ARIA", "1"="G21"))

SEL2020 %>%
  ggplot( aes(x=Project, y=Peso, fill=NULL)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.1, alpha=0.2) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=20)
  ) +
  ggtitle("Weight Distribution by Project") +
  xlab("")

SEL2020 %>%
  ggplot( aes(x=Project, y=Peso, fill=Project)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Weight Distribution by Project") +
  xlab("Project")

SEL2020 %>%
  ggplot( aes(x=Project, y=Peso, fill=Project)) +
  geom_violin() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Weight Distribution by Project") +
  xlab("")

#BMI:
ggplot(data=SEL2020, aes(x=BMI, group=Project, fill=Project)) + 
  geom_density(adjust=1.5, alpha=.4) + 
  theme_ipsum() + 
  scale_color_manual(name = "Project", values = c( "0" = "grey90", "1" = "grey0"), 
                     labels = c("0"="ARIA", "1"="G21"))

SEL2020 %>%
  ggplot( aes(x=Project, y=BMI, fill=NULL)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.1, alpha=0.2) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=20)
  ) +
  ggtitle("BMI Distribution by Project") +
  xlab("")

SEL2020 %>%
  ggplot( aes(x=Project, y=BMI, fill=Project)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("BMI Distribution by Project") +
  xlab("Project")

SEL2020 %>%
  ggplot( aes(x=Project, y=BMI, fill=Project)) +
  geom_violin() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("BMI Distribution by Project") +
  xlab("")

#Correlation matrix:
install.packages("GGally")
library(GGally)
library(readxl)
corr_matrix <- read_excel("~/Desktop/datasets_er/corr_matrix.xlsx")
View(corr_matrix)

ggpairs(corr_matrix, title="Correlogram matrix between Lung Function parameters and anthropometric data") 
data(corr_matrix)
ggpairs(corr_matrix, ggplot2::aes(colour=Gender)) 


install.packages("corrplot")
library(corrplot)

library(readxl)
corr_matrix_boys <- read_excel("~/Desktop/datasets_er/corr_matrix_boys.xlsx")
View(corr_matrix_boys)

corr_boys<-cor(corr_matrix_boys)
head(round(corr_boys,2))
corrplot(corr_boys)
corrplot(corr_boys, tl.col = "black", tl.srt = 45)
corrplot(corr_boys, method="number")
corrplot(corr_boys, type="lower", method="number")
corrplot(corr_boys, type="upper")


col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(corr_boys, method="color",  col=col(200),
         diag=FALSE, type="upper", 
         title=" ", addCoef.col = "black")
         
library(readxl)
corr_matrix_girls <- read_excel("~/Desktop/datasets_er/corr_matrix_girls.xlsx")
View(corr_matrix_girls) 

corr_girls<-cor(corr_matrix_girls)
head(round(corr_girls,2))
corrplot(corr_girls, type="lower", method="number")
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(corr_girls, method="color",  col=col(200),
         diag=FALSE, type="upper", 
         title=" ", addCoef.col = "black")


#Boxplot to compare genders in ARIA:
#FVC:
View(ARIA)
ARIA$Gender <- factor(ARIA$Gender , levels=c("Male", "Female"))

boxplot(ARIA$FVC_pre ~ ARIA$Gender , ylab="FVC (L)" , 
        xlab="FVC distribution by Gender", horizontal=FALSE, axes=TRUE ,range=0)

#FEV1:
boxplot(ARIA$FEV1_pre ~ ARIA$Gender , ylab="FEV1 (L)" , 
        xlab="FEV1 distribution by Gender", horizontal=FALSE, axes=TRUE ,range=0)

#FEF2575:
boxplot(ARIA$FEF2575_pre ~ ARIA$Gender , ylab="FEF25/75 (L/s)" , 
        xlab="FEF25/75 distribution by Gender", horizontal=FALSE, axes=TRUE ,range=0)

#FEV1/FVC:
boxplot(ARIA$FEV1FVC_pre ~ ARIA$Gender , ylab="FEV1/FVC" , 
        xlab="FEV1/FVC distribution by Gender", horizontal=FALSE, axes=TRUE ,range=0)


#Models comparisson:
#Conversion to z-scores:
View(G21_Boys)
View(G21_Girls)

G21_Boys$FVC_z_score<-scale(G21_Boys$FVC_pre, center = TRUE, scale = TRUE)
G21_Boys$FEV1_z_score<-scale(G21_Boys$FEV1_pre, center = TRUE, scale = TRUE)
G21_Boys$FEF2575_z_score<-scale(G21_Boys$FEF2575_pre, center = TRUE, scale = TRUE)

G21_Boys$FVC_GLI_z_score<-scale(G21_Boys$FVC_GLI, center = TRUE, scale = TRUE)
G21_Boys$FEV1_GLI_z_score<-scale(G21_Boys$FEV1_GLI, center = TRUE, scale = TRUE)
G21_Boys$FEF2575_GLI_z_score<-scale(G21_Boys$FEF2575_GLI, center = TRUE, scale = TRUE)

G21_Boys$FVC_bmodel_8_z_score<-scale(G21_Boys$FVC_bmodel_8, center = TRUE, scale = TRUE)
G21_Boys$FEV1_bmodel_6_z_score<-scale(G21_Boys$FEV1_bmodel_6, center = TRUE, scale = TRUE)
G21_Boys$FEF2575_bmodel_15_z_score<-scale(G21_Boys$FEF2575_bmodel_15, center = TRUE, scale = TRUE)

G21_Girls$FVC_z_score<-scale(G21_Girls$FVC_pre, center = TRUE, scale = TRUE)
G21_Girls$FEV1_z_score<-scale(G21_Girls$FEV1_pre, center = TRUE, scale = TRUE)
G21_Girls$FEF2575_z_score<-scale(G21_Girls$FEF2575_pre, center = TRUE, scale = TRUE)

G21_Girls$FVC_GLI_z_score<-scale(G21_Girls$FVC_GLI, center = TRUE, scale = TRUE)
G21_Girls$FEV1_GLI_z_score<-scale(G21_Girls$FEV1_GLI, center = TRUE, scale = TRUE)
G21_Girls$FEF2575_GLI_z_score<-scale(G21_Girls$FEF2575_GLI, center = TRUE, scale = TRUE)

G21_Girls$FVC_gmodel_9_z_score<-scale(G21_Girls$FVC_gmodel_9, center = TRUE, scale = TRUE)
G21_Girls$FEV1_gmodel_10_z_score<-scale(G21_Girls$FEV1_gmodel_10, center = TRUE, scale = TRUE)
G21_Girls$FEF2575_gmodel_15_z_score<-scale(G21_Girls$FEF2575_gmodel_15, center = TRUE, scale = TRUE)

write.csv(G21_Boys,"~/Desktop/G21_Boys_s.csv", row.names = TRUE)
write.csv(G21_Girls,"~/Desktop/G21_Girls_s.csv", row.names = TRUE)

#Import datasets:

library(readxl)

G21_Boys_s <- read_excel("~/Desktop/datasets_er/G21_Boys_s.xlsx")
View(G21_Boys_s)

G21_Girls_s <- read_excel("~/Desktop/datasets_er/G21_Girls_s.xlsx")
View(G21_Girls_s)

library(ggplot2)
library(hrbrthemes)
library(dplyr)
library(tidyr)
library(viridis)


#Boys
#FVC:
ggplot(data=G21_Boys_s, aes(x=FVC_z_score, group=Source, fill=Source)) +
  geom_density(adjust=1.5, alpha=.4) + theme_ipsum()

ggplot(G21_Boys_s, aes(x=Source, y=FVC_z_score, fill=Source)) + 
  geom_violin()

ggplot(data=G21_Boys_s, aes(x=FVC, group=Source, fill=Source)) +
  geom_density(adjust=1.5, alpha=.4) +
  theme_ipsum()

ggplot(data=G21_Boys_s, aes(x=FVC, group=Source, fill=Source)) +
  geom_density(adjust=1.5) +
  theme_ipsum() +
  facet_wrap(~Source) +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    axis.ticks.x=element_blank()
  )

install.packages("ggridges")
library(ggridges)

ggplot(G21_Boys_s, aes(x = FVC, y = Source)) +
  geom_density_ridges(aes(fill = Source)) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800", "#FC4E07"))

ggplot(G21_Boys_s, aes(x = FVC_z_score, y = Source)) +
  geom_density_ridges(aes(fill = Source)) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800", "#FC4E07"))

#FEV1:
ggplot(data=G21_Boys_s, aes(x=FEV1_z_score, group=Source, fill=Source)) +
  geom_density(adjust=1.5, alpha=.4) + theme_ipsum()

ggplot(G21_Boys_s, aes(x=Source, y=FEV1_z_score, fill=Source)) + 
  geom_violin()

ggplot(G21_Boys_s, aes(x = FEV1, y = Source)) +
  geom_density_ridges(aes(fill = Source)) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800", "#FC4E07"))

ggplot(G21_Boys_s, aes(x = FEV1_z_score, y = Source)) +
  geom_density_ridges(aes(fill = Source)) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800", "#FC4E07"))

#FEF2575:
ggplot(data=G21_Boys_s, aes(x=FEF2575_z_score, group=Source, fill=Source)) +
  geom_density(adjust=1.5, alpha=.4) + theme_ipsum()

ggplot(G21_Boys_s, aes(x=Source, y=FEF2575_z_score, fill=Source)) + 
  geom_violin()

ggplot(G21_Boys_s, aes(x = FEF2575, y = Source)) +
  geom_density_ridges(aes(fill = Source)) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800", "#FC4E07"))

ggplot(G21_Boys_s, aes(x = FEF2575_z_score, y = Source)) +
  geom_density_ridges(aes(fill = Source)) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800", "#FC4E07"))

#Girls
#FVC:
ggplot(data=G21_Girls_s, aes(x=FVC_z_score, group=Source, fill=Source)) +
  geom_density(adjust=1.5, alpha=.4) + theme_ipsum()

ggplot(G21_Girls_s, aes(x=Source, y=FVC_z_score, fill=Source)) + 
  geom_violin()

ggplot(G21_Girls_s, aes(x = FVC_z_score, y = Source)) +
  geom_density_ridges(aes(fill = Source)) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800", "#FC4E07"))

#FEV1:
ggplot(data=G21_Girls_s, aes(x=FEV1_z_score, group=Source, fill=Source)) +
  geom_density(adjust=1.5, alpha=.4) + theme_ipsum()

ggplot(G21_Girls_s, aes(x=Source, y=FEV1_z_score, fill=Source)) + 
  geom_violin()

ggplot(G21_Girls_s, aes(x = FEV1_z_score, y = Source)) +
  geom_density_ridges(aes(fill = Source)) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800", "#FC4E07"))

#FEF2575:
ggplot(data=G21_Girls_s, aes(x=FEF2575_z_score, group=Source, fill=Source)) +
  geom_density(adjust=1.5, alpha=.4) + theme_ipsum()

ggplot(G21_Girls_s, aes(x=Source, y=FEF2575_z_score, fill=Source)) + 
  geom_violin()

ggplot(G21_Girls_s, aes(x = FEF2575_z_score, y = Source)) +
  geom_density_ridges(aes(fill = Source)) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800", "#FC4E07"))

#FEV1/FVC evaluation:
View(SEL2020)

G21_Boys$FEV1FVC_GLI_model<-(G21_Boys$FEV1FVC_pre*100)/G21_Boys$FEV1FVC_GLI
G21_Girls$FEV1FVC_GLI_model<-(G21_Girls$FEV1FVC_pre*100)/G21_Girls$FEV1FVC_GLI
G21_Boys$FEV1FVC_ARIA_model<-(G21_Boys$FEV1FVC_pre*100)/(89.96)
G21_Girls$FEV1FVC_ARIA_model<-(G21_Girls$FEV1FVC_pre*100)/(90.81)

summary(G21_Boys$FEV1FVC_GLI_model)
summary(G21_Boys$FEV1FVC_ARIA_model)

summary(G21_Girls$FEV1FVC_GLI_model)
summary(G21_Girls$FEV1FVC_ARIA_model)

sum(G21_Boys$FEV1FVC_GLI_model < 80)
sum(G21_Boys$FEV1FVC_ARIA_model < 80)

sum(G21_Girls$FEV1FVC_GLI_model < 80)
sum(G21_Girls$FEV1FVC_ARIA_model < 80)

#CC measured and predicted values:
#Boys:
#FVC:
ggplot(G21_Boys_s, aes(x=FVC_measured, y=FVC, color=Source)) + 
  geom_point(size=1) + geom_smooth(method=lm) +
  theme_ipsum()

#FEV1:
ggplot(G21_Boys_s, aes(x=FEV1_measured, y=FEV1, color=Source)) + 
  geom_point(size=1) + geom_smooth(method=lm) +
  theme_ipsum()

#FEF2575:
ggplot(G21_Boys_s, aes(x=FEF2575_measured, y=FEF2575, color=Source)) + 
  geom_point(size=1) + geom_smooth(method=lm) +
  theme_ipsum()

#Girls:
#FVC:
ggplot(G21_Girls_s, aes(x=FVC_measured, y=FVC, color=Source)) + 
  geom_point(size=1) + geom_smooth(method=lm) +
  theme_ipsum()

#FEV1:
ggplot(G21_Girls_s, aes(x=FEV1_measured, y=FEV1, color=Source)) + 
  geom_point(size=1) + geom_smooth(method=lm) +
  theme_ipsum()

#FEF2575:
ggplot(G21_Girls_s, aes(x=FEF2575_measured, y=FEF2575, color=Source)) + 
  geom_point(size=1) + geom_smooth(method=lm) +
  theme_ipsum()

#Corr Matrix:
#Boys:
#FVC:
G21_Boys_s_corr_matrix_fvc <- read_excel("~/Desktop/datasets_er/G21_Boys_s_corr_matrix_fvc.xlsx")
ggpairs(G21_Boys_s_corr_matrix_fvc, title=" Correlation Matrix between measured FVC predicted 
        values from GLI and ARIA for boys") 
 
#FEV1:
G21_Boys_s_corr_matrix_fev1 <- read_excel("~/Desktop/datasets_er/G21_Boys_s_corr_matrix_fev1.xlsx")
ggpairs(G21_Boys_s_corr_matrix_fev1, title=" Correlation Matrix between measured FEV1 predicted 
        values from GLI and ARIA for boys") 

#FEF2575:
G21_Boys_s_corr_matrix_fef2575 <- read_excel("~/Desktop/datasets_er/G21_Boys_s_corr_matrix_fef2575.xlsx")
ggpairs(G21_Boys_s_corr_matrix_fef2575, title=" Correlation Matrix between measured FEF2575 predicted 
        values from GLI and ARIA for boys") 

#Girls:
#FVC:
G21_Girls_s_corr_matrix_fvc <- read_excel("~/Desktop/datasets_er/G21_Girls_s_corr_matrix_fvc.xlsx")
ggpairs(G21_Girls_s_corr_matrix_fvc, title=" Correlation Matrix between measured FVC predicted 
        values from GLI and ARIA for girls") 

#FEV1:
G21_Girls_s_corr_matrix_fev1 <- read_excel("~/Desktop/datasets_er/G21_Girls_s_corr_matrix_fev1.xlsx")
ggpairs(G21_Girls_s_corr_matrix_fev1, title=" Correlation Matrix between measured FEV1 predicted 
        values from GLI and ARIA for girls") 

#FEF2575:
G21_Girls_s_corr_matrix_fef2575 <- read_excel("~/Desktop/datasets_er/G21_Girls_s_corr_matrix_fef2575.xlsx")
ggpairs(G21_Girls_s_corr_matrix_fef2575, title=" Correlation Matrix between measured FEF2575 predicted 
        values from GLI and ARIA for girls") 
