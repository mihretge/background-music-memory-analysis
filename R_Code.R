
# ===========================================================================================================
# 1. COMPLETELY RANDOMIZED DESIGN: using One way Anova model to test the effect of background music on memory        performance using a 24 card memory Puzzel Game using Misses as a primary endpoint(response)
# ===========================================================================================================

# Clear workspace
rm(list=ls())

# Load needed packages
library(readxl)
library(Hmisc)

# Read Excel file
data <- read_excel("C:/Users/mihre/Downloads/Math 4730 Data Collected Combined.xlsx")

# Check data
print(data)
names(data)

music = factor(data$Order, levels = c("N","S","L"))
misses = data$Misses

# Create dataset
misses.data = data.frame(music = music, misses = misses)

# Display dataset
print(misses.data)

# Summary by the background music type
summary(misses ~ music, data = misses.data)

# (1) Effects Model (ANOVA)
# y_ij = μ + τ_i + ε_ij

m1.aov = aov(misses ~ music, data = misses.data)

# Treatment effects
model.tables(m1.aov)

# Hypothesis test
summary(m1.aov)


# (2) Means Model (Linear Model)
# The means model: y_ij = μ_i + ε_ij

m1.lm = lm(misses ~ music, data = misses.data)

summary(m1.lm)

# ANOVA table
anova(m1.lm)

# To test the effect of the treatment
# The ANOVA table helps test the hypothesis:
# Null Hypothesis (H0): τ_1 = τ_2 = τ_3 = τ_4 = 0  (No effect of background music on memory performance)
# Alternative Hypothesis (H1): At least one τ_i ≠ 0 (background music has an effect on memory performance)

# Extract ANOVA table
anova_table <- summary(m1.aov)[[1]]

F0 <- anova_table["music", "F value"]
pvalue <- anova_table["music", "Pr(>F)"]

alpha <- 0.05

# Decision
cat("Decision:", ifelse(pvalue < alpha, "Reject H0", "Do not reject H0"), "\n")

# Model Adequacy Checking

res = m1.aov$residuals
fitted = m1.aov$fitted.values

par(mfrow=c(1,2))

# Normality check
qqnorm(res)
qqline(res)

# in the qq plot we can see that the points roughly follow the straight line

# Residuals vs fitted
plot(fitted, res)

# in the fitted vs residual plot, points are randomly scattred, so all our assumptions are satisified


# ===========================================================================================================
# 2. COMPLETELY RANDOMIZED DESIGN: using One way Anova model to test the effect of background music on memory        performance using a 24 card memory Puzzel Game using Moves as a secondary endpoint(response)
# ===========================================================================================================

moves = data$Moves
moves.data = data.frame(music = music, moves = moves)

summary(moves ~ music, data = moves.data)

m2.aov = aov(moves ~ music, data = moves.data)
model.tables(m2.aov)
summary(m2.aov)

m2.lm = lm(moves ~ music, data = moves.data)
summary(m2.lm)
anova(m2.lm)

# To test the effect of the treatment
# The ANOVA table helps test the hypothesis:
# Null Hypothesis (H0): τ_1 = τ_2 = τ_3 = τ_4 = 0  (No effect of background music on memory performance)
# Alternative Hypothesis (H1): At least one τ_i ≠ 0 (background music has an effect on memory performance)

# Extract ANOVA table
anova_table <- summary(m2.aov)[[1]]

F1 <- anova_table["music", "F value"]
pvalue1 <- anova_table["music", "Pr(>F)"]

alpha <- 0.05

# Decision
cat("Decision:", ifelse(pvalue < alpha, "Reject H0", "Do not reject H0"), "\n")


# Model Adequacy Checking for moves 

res = m2.aov$residuals
fitted = m2.aov$fitted.values

par(mfrow=c(1,2))

# Normality check
qqnorm(res)
qqline(res)

# in the qq plot we can see that the points roughly follow the straight line

# Residuals vs fitted
plot(fitted, res)

# in the fitted vs residual plot, points are randomly scattred, so all our assumptions are satisified


# Estimating sample size:
# MSE from ANOVA is used as an estimate for variance.
# Effect size: f = d / sqrt(MSE)

library(pwr)

# Number of groups
a <- 3

# Extracting variance from ANOVA (Misses model)
anova_table <- summary(m1.aov)[[1]]
sigma2hat <- anova_table["Residuals", "Mean Sq"]

D <- 4 # Maximum mean difference to detect

# Computing effect size f
effect.size <- D / sqrt(sigma2hat)

pwr.anova.test(k=3, f=effect.size, power=0.99, sig.level = 0.05)  # Compute required sample size

# sample size n is 21, which means we need 21*3 = 63 total data points which is less than what we have(90)
# for the effect size f = 0.6121169 our sample size is more than enough, our sample size can sufficiently detect a difference of size 4(D) with a 99% power.

# ==================================================================================
# 3. RANDOMIZED BLOCK DESIGN: using misses the primary endpoint
# ==================================================================================

# Response, treatment, and block respectively 
misses = data$Misses
music = factor(data$Order, levels = c("N","S","L"))      # treatment label
participant = factor(data$Participants)                  # block label

RCBD_dataframe <- data.frame(misses,
                             factor(music),
                             factor(participant))

# (1) Statistical model
# y_{ij} = mu + tau_i + beta_j + e_{ij}
# for i = 1,2,3 and j = 1,...,30

# (2) Hypotheses for treatment effect
# H0: tau_1 = tau_2 = tau_3 = 0
# vs H1: tau_i is not 0 for at least one i

# (3) ANOVA table for RCBD
m1 = aov(misses ~ factor(music) + factor(participant), data = RCBD_dataframe)
summary(m1)

# (a) Decision for treatment effect
anova_table <- summary(m1)[[1]]
F0 <- anova_table["factor(music)", "F value"]
pvalue <- anova_table["factor(music)", "Pr(>F)"]
alpha <- 0.05

cat("Decision:", ifelse(pvalue < alpha, "Reject H0", "Do not reject H0"), "\n")

# to check whether blocking seem to be necessary, 
MS_block <- anova_table["factor(participant)", "Mean Sq"]
MSE <- anova_table["Residuals", "Mean Sq"]
cat("MS(blocks) =", MS_block, "\n")
cat("MSE =", MSE, "\n")

# to check whether blocking seem to be necessary, we can Compare MS(blocks) with MSE, form our results we get MS(blocks) = 64.95824 >>> MSE = 31.57433, so yes blocking was indeed necessary.


# (4) we can also Compare the results (SSE) from RCBD with completely randomized design
m2 = aov(misses ~ factor(music), data = RCBD_dataframe)
summary(m2)

# CRD SSE = 3715.1
# RCBD we have , SSE = 1831.3 and SSBlock = 1883.8
# eventhough we still reject the null hypothesis and say that music is not significant, we noticed that the p value in RCBD (0.0916) is lower than the p value in CRD (0.1646), which tells us that RCBD reduces the blocking effect by reducing the variation between participants.

# ==================================================================================
# 4. RANDOMIZED BLOCK DESIGN: using moves the secondary endpoint
# ==================================================================================

moves = data$Moves
m1_moves = aov(moves ~ factor(music) + factor(participant), data = RCBD_dataframe)
summary(m1_moves)

# (a) Decision for treatment effect
anova_table <- summary(m1_moves)[[1]]
F0 <- anova_table["factor(music)", "F value"]
pvalue <- anova_table["factor(music)", "Pr(>F)"]
alpha <- 0.05

cat("Decision:", ifelse(pvalue < alpha, "Reject H0", "Do not reject H0"), "\n")

# The Decision is still the same the type of music background has no significance on memory performance using either endpoints 

# ==================================================================================
# 5.FACTORIAL DESIGN: 3 Factor factorial design (factors: music, age, gender)
# ==================================================================================
# required data
music  <- factor(data$Order, levels = c("N","S","L"))

gender <- factor(data$Gender,
                 levels = c("F","M"),
                 labels = c("Female","Male"))

age <- factor(data$Age,
              levels = c("Y","O"),
              labels = c("Young","Old"))

# Response
response <- data$Misses

# Three-factor factorial design

model1 <- aov(response ~ music * gender * age)
summary(model1)

# The 3 factor interaction, music and gender are not significant since the p values for each are greater than 0.05, but age is significant since our p value = 0.0191 is less than 0.05.


# ==================================================================================
# 6.FACTORIAL DESIGN + RBCD: 3 Factor factorial design (factors: music, age, gender) and RBCD, taking
#   participants as a block to reduce between partcipant variation
# ==================================================================================


# to reduce the variation between participant to partipant, we have coupled factorial design and RBCD 
model2 <- aov(Misses ~ music * gender * age + factor(Participants), data = data)
summary(model2)

# The 3 factor interaction, music and gender are not significant since the p values for each are greater than 0.05, but age is still significant since our p value = 0.00828 is less than 0.05.

# Since our results from the factorial design suggested that age is a significant factor we can test which age group is performing well in the memory puzzel game
TukeyHSD(aov(Misses ~ age, data = data))

# from this result we get that participants that are old has 3.417863 more misses than the young participants on   average.

# ==================================================================================
# 7.FACTORIAL DESIGN,FACTORIAL DESIGN + RBCD: using moves the secondary endpoint
# ==================================================================================
response <- data$Moves

# Factorial design
model_moves <- aov(response ~ music * gender * age)
summary(model_moves)

#factorial design + RCBD
model_moves1 <- aov(response ~ music * gender * age + factor(Participants), data = data)
summary(model_moves1)

# even taking the secondary endpoint, moves suggests that only age is significant



