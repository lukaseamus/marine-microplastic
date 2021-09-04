##################################################################
##### Title: Microplastic emission from the abrasion of rope #####
##### Author: Luka Seamus Wright                             #####
##################################################################

### Data preparation ####
## Load data ####
frag <- read.csv("~/PATH/fragments.csv")

## Rename variables ####
rope <- factor(frag$rope)
age <- frag$age # age (yr)
poly <- factor(frag$polymer, levels = c("Polysteel", "Polypropylene"))
m <- frag$mass*20000 # mass (μg m-1 hauled)
f <- frag$fragments/50 # fragments (n m-1 hauled)

steel <- frag[1:10,] # create subset of main dataframe
dia <- steel$diameter # diameter (mm)
wear <- factor(pi*dia*10) # wear surface (cm2 m-1)
sm <- steel$mass*20000
sf <- steel$fragments/50

### Data exploration ####
par(mfrow = c(1,2))
plot(f ~ age) # clearly more fragments in older rope
plot(m ~ age) # clearly larger mass in older rope

boxplot(sm ~ wear)
boxplot(sf ~ wear)

#### Data analysis ####
### The effect of wear surface ####
## Mass ####
m1 <- lm(sm ~ wear)

# test model fit
par(mfrow = c(1,2))
plot(resid(m1) ~ fitted(m1)) # heterogenous
abline(0, 0)
qqnorm(resid(m1))
qqline(resid(m1)) # normal

# improve homogeneity with generalised least squares
require(nlme)
m2 <- gls(sm ~ wear, weights = varIdent(form = ~1|wear))

# test model fit
plot(m2)
plot(resid(m2, type = "normalized") ~ fitted(m2, type = "normalized"))
abline(0, 0) # homogenous

qqnorm(resid(m2, type = "normalized"))
qqline(resid(m2, type = "normalized")) # normal

# interpret optimal model
require(car)
Anova(m2, type = 2)
# Response: sm
#      Df  Chisq Pr(>Chisq)    
# wear  1 19.041   1.28e-05 ***

## Fragments ####
m3 <- lm(sf ~ wear)

# test model fit
plot(resid(m3) ~ fitted(m3)) # heterogenous
abline(0, 0)
qqnorm(resid(m1))
qqline(resid(m1)) # normal

# improve homogeneity with generalised least squares
m4 <- gls(sf ~ wear, weights = varIdent(form = ~1|wear))

# test model fit
plot(m4)
plot(resid(m4, type = "normalized") ~ fitted(m4, type = "normalized"))
abline(0, 0) # homogenous

qqnorm(resid(m4, type = "normalized"))
qqline(resid(m4, type = "normalized")) # normal

# interpret optimal model
Anova(m4, type = 2)
# Response: sf
#      Df  Chisq Pr(>Chisq)   
# wear  1 6.7211   0.009528 **

### Mass ####
m5 <- lm(m ~ rope)

# test model fit
plot(resid(m5) ~ fitted(m5)) # heterogenous
abline(0, 0)
qqnorm(resid(m5))
qqline(resid(m5)) # non-normal but balanced

# improve homogeneity with generalised least squares
m6 <- gls(m ~ rope, weights = varIdent(form = ~1|rope))

# test model fit
plot(m6)
plot(resid(m6, type = "normalized") ~ fitted(m6, type = "normalized"))
abline(0, 0) # homogenous

qqnorm(resid(m6, type = "normalized"))
qqline(resid(m6, type = "normalized")) # normal

# interpret optimal model
Anova(m6, type = 2)
# Response: m
#      Df  Chisq Pr(>Chisq)    
# rope  4 369.78  < 2.2e-16 ***

summary(m6)
#                Value Std.Error   t-value p-value
# (Intercept)   13.360   1.27115 10.510172  0.0000 
# ropeB         -2.560   2.41988 -1.057904  0.3002
# ropeC         -1.760   3.06200 -0.574788  0.5706
# ropeD        233.240  17.68898 13.185610  0.0000 ***
# ropeE       1038.532  74.74991 13.893421  0.0000 ***

rope <- factor(rope, levels = c("B", "C", "D", "E", "A"))
m6 <- gls(m ~ rope, weights = varIdent(form = ~1|rope))
summary(m6)
#                Value Std.Error   t-value p-value
# (Intercept)   10.800   2.05913  5.244944  0.0000
# ropeC          0.800   3.46410  0.230940  0.8192
# ropeD        235.800  17.76300 13.274784  0.0000 ***
# ropeE       1041.092  74.76746 13.924399  0.0000 ***

rope <- factor(rope, levels = c("C", "D", "E", "A", "B"))
m6 <- gls(m ~ rope, weights = varIdent(form = ~1|rope))
summary(m6)
#                Value Std.Error   t-value p-value
# (Intercept)   11.600   2.78568  4.164157  0.0003
# ropeD        235.000  17.86181 13.156562  0.0000 ***
# ropeE       1040.292  74.79100 13.909321  0.0000 ***

rope <- factor(rope, levels = c("D", "E", "A", "B", "C"))
m6 <- gls(m ~ rope, weights = varIdent(form = ~1|rope))
summary(m6)
#                Value Std.Error   t-value p-value
# (Intercept)  246.600  17.64325  13.97702       0
# ropeE        805.292  76.79334  10.48648       0 ***

rope <- factor(rope, levels = c("A", "B", "C", "D", "E"))

### Fragments ####
m7 <- lm(f ~ rope)

# test model fit
plot(resid(m7) ~ fitted(m7)) # heterogenous
abline(0, 0)
qqnorm(resid(m7))
qqline(resid(m7)) # non-normal but balanced

# improve homogeneity with generalised least squares
m8 <- gls(f ~ rope, weights = varIdent(form = ~1|rope))

# test model fit
plot(m8)
plot(resid(m8, type = "normalized") ~ fitted(m8, type = "normalized"))
abline(0, 0) # homogenous

qqnorm(resid(m8, type = "normalized"))
qqline(resid(m8, type = "normalized")) # normal
par(mfrow = c(1, 1))

# interpret optimal model
Anova(m8, type = 2)
# Response: f
#      Df  Chisq Pr(>Chisq)    
# rope  4 404.78  < 2.2e-16 ***

summary(m8)
#                Value Std.Error   t-value p-value
# (Intercept)  39.9880   3.05190 13.102647  0.0000
# ropeB       -25.5188   4.11393 -6.203014  0.0000 ***
# ropeC       -17.5290   6.19701 -2.828626  0.0091 **
# ropeD       679.7317  51.58333 13.177352  0.0000 ***
# ropeE       727.4066  54.61078 13.319836  0.0000 ***

rope <- factor(rope, levels = c("B", "C", "D", "E", "A"))
m8 <- gls(f ~ rope, weights = varIdent(form = ~1|rope))
summary(m8)
#                Value Std.Error   t-value p-value
# (Intercept)  14.4692   2.75868  5.244964  0.0000
# ropeC         7.9897   6.05799  1.318877  0.1992
# ropeD       705.2505  51.56681 13.676441  0.0000 ***
# ropeE       752.9254  54.59518 13.791060  0.0000 ***

rope <- factor(rope, levels = c("C", "D", "E", "A", "B"))
m8 <- gls(f ~ rope, weights = varIdent(form = ~1|rope))
summary(m8)
#                Value Std.Error   t-value p-value
# (Intercept)  22.4589   5.39341  4.164144  0.0003
# ropeD       697.2608  51.77465 13.467222  0.0000 ***
# ropeE       744.9357  54.79153 13.595817  0.0000 ***

rope <- factor(rope, levels = c("D", "E", "A", "B", "C"))
m8 <- gls(f ~ rope, weights = varIdent(form = ~1|rope))
summary(m8)
#                 Value Std.Error    t-value p-value
# (Intercept)  719.7197  51.49297  13.977048  0.0000
# ropeE         47.6749  74.99700   0.635691  0.5308

rope <- factor(rope, levels = c("A", "B", "C", "D", "E"))

#### Data visualisation ####
## Descriptive statistics ####
require(psych)
Mstat <- describeBy(m, rope, quant = c(.25,.75), mat = T)
Mstat$polymer <- factor(c("Polysteel", rep("Polypropylene",4)))
Mstat$age <- factor(c(0, 0, 1, 2, 10))
Fstat <- describeBy(f, rope, quant = c(.25,.75), mat = T)
Fstat$polymer <- factor(c("Polysteel", rep("Polypropylene",4)))
Fstat$age <- factor(c(0, 0, 1, 2, 10))

SMstat <- describeBy(sm, wear, mat = T)
SFstat <- describeBy(sf, wear, mat = T)

### Set base theme ####
require(ggplot2)
mytheme <- theme(panel.background = element_blank(),
                 panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 panel.border = element_blank(),
                 plot.margin = unit(c(.2, .4, .2, .2),"cm"),
                 axis.line = element_line(),
                 axis.title = element_text(size = 15),
                 axis.title.x = element_blank(),
                 axis.text = element_text(size = 12, colour = "black"),
                 axis.ticks.length = unit(.25, "cm"),
                 axis.ticks = element_line(colour = "black"),
                 legend.key = element_blank(),
                 legend.text = element_text(size = 12),
                 legend.text.align = 0,
                 legend.title = element_text(size = 12, face = "bold"),
                 text = element_text(family = "Helvetica"))


## Wear surface
mwp <- ggplot(SMstat, aes(group1, mean)) +
  geom_col(fill = c("#9AB9CC", "#2e4a5b"), width = 0.5) +
  geom_errorbar(aes(ymin = mean - se*qnorm(0.975), ymax = mean + se*qnorm(0.975)), 
                width = 0.1) +
  annotate("text", x = 1.5, y = 16.56, label = "***", size = 4.2) +
  labs(x = expression("Wear surface (cm"^2*" m"^-1*")"),
       y = expression("Microplastic mass ("*mu*"g m"^-1*")")) +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_discrete(labels = c("377", "503")) +
  coord_cartesian(ylim = c(5, 20)) +
  mytheme
mwp

fwp <- ggplot(SFstat, aes(group1, mean)) +
  geom_col(fill = c("#9AB9CC", "#2e4a5b"), width = 0.5) +
  geom_errorbar(aes(ymin = mean - se*qnorm(0.975), ymax = mean + se*qnorm(0.975)), 
                width = 0.1) +
  annotate("text", x = 1.5, y = 46.17446, label = "**", size = 4.2) +
  labs(x = expression("Wear surface (cm"^2*" m"^-1*")"),
       y = expression("Microplastic fragemnts (m"^-1*")")) +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_discrete(labels = c("377", "503")) +
  coord_cartesian(ylim = c(20, 60)) +
  mytheme
fwp


require(cowplot)
wp <- plot_grid(fwp, mwp, labels = "auto", label_size = 15)
ggdraw(add_sub(wp, expression("Wear surface (cm"^2*" m"^-1*")"),
               size = 15, hjust = 0.38, vjust = 0)) # 5 x 5 in

# Mass
mp <- ggplot(Mstat, aes(group1, mean)) +
  annotate("segment", x = Inf, y = Inf, xend = -Inf, yend = Inf, lwd = 1) +
  geom_col(aes(fill = age), width = 0.5) +
  geom_errorbar(aes(ymin = mean - se*qnorm(0.975), ymax = mean + se*qnorm(0.975)), 
                width = 0.1) +
  scale_fill_manual(values = c("#2e4a5b", "#48748F", "#6E9BB6", "#9AB9CC"),
                    guide = "none") +
  geom_text(aes(group1, mean + se*qnorm(0.975) + 45, label = c(rep("a",3),"b","c")), 
            size = 4.2, check_overlap = T) +
  geom_text(aes(group1, 1450, label = n), size = 4, check_overlap = T) +
  facet_grid(~factor(polymer, levels = c("Polysteel", "Polypropylene")), 
             scales = "free", space = "free") +
  ylab(expression("Abraded microplastic mass ("*mu*"g m"^-1*")")) +
  xlab("Rope age (years)") +
  scale_y_continuous(expand = c(0,0), breaks = seq(0, 1500, by = 250)) +
  scale_x_discrete(labels = c(0, 1, 2, 10)) +
  coord_cartesian(ylim = c(0, 1500)) +
  mytheme +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 12, hjust = 0),
        panel.spacing = unit(.5, "cm"))
mp

# Fragments
fp <- ggplot(Fstat, aes(group1, mean)) +
  annotate("segment", x = Inf, y = Inf, xend = -Inf, yend = Inf, lwd = 1) +
  geom_col(aes(fill = age), width = 0.5) +
  geom_errorbar(aes(ymin = mean - se*qnorm(0.975), ymax = mean + se*qnorm(0.975)), 
                width = 0.1) +
  scale_fill_manual(values = c("#2e4a5b", "#48748F", "#6E9BB6", "#9AB9CC"),
                    guide = "none") +
  geom_text(aes(group1, mean + se*qnorm(0.975) + 40), label = c("a","b","b","c","c"), 
                size = 4.2, check_overlap = T) +
  geom_text(aes(group1, 1210, label = n), size = 4, check_overlap = T) +
  facet_grid(~factor(polymer, levels = c("Polysteel", "Polypropylene")), 
             scales = "free", space = "free") +
  ylab(expression("Abraded microplastic fragments (m"^-1*")")) +
  xlab("Rope age (years)") +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_discrete(labels = c(0, 1, 2, 10)) +
  coord_cartesian(ylim = c(0, 1250)) +
  mytheme +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 12, hjust = 0),
        panel.spacing = unit(.5, "cm"))
fp


# Combined
fmp <- plot_grid(fp, mp, labels = "auto", label_size = 15)
ggdraw(add_sub(fmp, expression("Rope age (years)"),
               size = 15, hjust = 0.38, vjust = 0)) # 5 x 10 in

###### Clean up ####
detach(package:nlme)
detach(package:car)
detach(package:psych)
detach(package:cowplot)
detach(package:ggplot2)
rm(list = ls())
graphics.off()
cat("\014")
