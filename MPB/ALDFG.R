################################################
##### Title: Prevalent fishing litter may  #####
##### contribute to microplastic pollution #####
##### Author: Luka Seamus Wright           #####
################################################

#### Site map ####
### Required packages ####
require(rworldmap)
require(rgdal)
require(ggplot2)
require(cowplot)

### Set base theme ####
mytheme <- theme(panel.background = element_blank(),
                 panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 panel.border = element_blank(),
                 axis.line = element_line(),
                 axis.title = element_text(size = 15),
                 axis.text = element_text(size = 12, colour = "black"),
                 axis.ticks.length = unit(.25, "cm"),
                 axis.ticks = element_line(colour = "black"),
                 legend.key = element_blank(),
                 legend.text = element_text(size = 12),
                 legend.text.align = 0,
                 legend.title = element_text(size = 12, face = "bold"),
                 text = element_text(family = "Helvetica Neue"))

### Load data ####
map.data <- getMap(resolution = "high") # coarse map
seas <- read.csv("~/Desktop/Plymouth University/IMLRU/ALDFG/Data/seas.csv")
map.data.fine <- readOGR("~/Desktop/Plymouth University/IMLRU/ALDFG/Data/gadm36_GBR_shp/gadm36_GBR_0.shp") # detailed map
sites <- read.csv("~/Desktop/Plymouth University/IMLRU/ALDFG/Data/coordinates.csv")
site.names <- sites[c(2,4,6,8,10,12),]

### Plot UK map ####
UKmap <- ggplot() + 
  coord_map(xlim = c(-12, 4), ylim = c(48, 62)) +
  geom_polygon(data = seas, mapping = aes(x = long, y = lat, group = sea, fill = litter*1440),
               colour = "#ffffff", size = 0.2) +
  geom_polygon(data = map.data, mapping = aes(x = long, y = lat, group = group),
               fill = "#5b5b5e", colour = "#5b5b5e", size = 0.001) +
  geom_rect(aes(xmin = -3, xmax = -6, ymin = 49.5, ymax = 52), 
            fill = NA, colour = "#000000", size = 0.5, linejoin = "mitre") +
  scale_fill_gradient(low = "#ffebc2", high = "#be3819",
                      guide = guide_colorbar(title = expression("ALDFG (pers"^-1*" m"^-1*" d"^-1*")"),
                                             ticks.colour = "#000000", ticks.linewidth = 1.2)) +
  scale_x_continuous(breaks = seq(-12, 4, by = 4), expand = c(0,0)) +
  scale_y_continuous(breaks = seq(48, 62, by = 2), expand = c(0,0)) +
  mytheme +
  theme(axis.title = element_blank(),  # modify base theme
        axis.line = element_blank(),
        panel.border = element_rect(fill = NA, size = 1),
        legend.background = element_blank(),
        legend.position = c(0.33, 0.82))

UKmap


### Plot SW map ####
SWmap <- ggplot() + 
  coord_map(xlim = c(-6, -3), ylim = c(49.5, 52)) +
  geom_polygon(aes(x = c(-6, -6, -3, -3), y = c(49.5, 51, 51, 49.5)), fill = "#cae4f0") +
  geom_polygon(aes(x = c(-5.266, -7, -7, -6, -6, -5, -5, -4.8, -2.71, -1.5, -5.11), 
                   y = c(50, 50, 50.5, 50.5, 51, 51, 51.63, 51.69, 52.77, 51.2, 50.28)), fill = "#e1f0e8") +
  geom_polygon(data = map.data.fine, mapping = aes(x = long, y = lat, group = group),
               fill = "#5b5b5e", colour = "#5b5b5e", size = 0.001) +
  geom_point(data = sites, mapping = aes(x = long, y = lat), size = .5) +
  geom_line(data = sites, mapping = aes(x = long, y = lat, group = site)) +
  geom_label(data = site.names, mapping = aes(x = long, y = lat, label = site),
             label.r = unit(0, "lines"), label.size = .5, size = 4.2) +
  annotate("text", label = c("Bristol Channel (f)", "Western English Channel (e)"), 
           x = c(-5.875, -3.125), y = c(50.38, 50.1), colour = c("#7dbe9c", "#53a7ce"), angle = 90, size = 4.2) +
  scale_x_continuous(breaks = seq(-7,-1, by = 1), expand = c(0,0)) +
  scale_y_continuous(breaks = seq(49.5, 52, by = 0.5), position = "right", expand = c(0,0)) +
  mytheme +
  theme(axis.title = element_blank(),  # modify base theme
        axis.line = element_blank(),
        panel.border = element_rect(fill = NA, size = 1))

SWmap


### Combined map ####
map <- plot_grid(UKmap, SWmap, labels = "auto",
                 rel_widths = c(0.838, 1))
map # print (dimensions: 5.5 x 7.5 in)

#### Part 1: Count data ####
### Data preparation ####
## Load data ####
count <- read.csv("~/Desktop/Plymouth University/IMLRU/ALDFG/Data/count.csv")

## Rename variables ####
c.region <- count$region
c.type <- factor(count$type, levels = c("Twisted", "Braided", "Filament"))
c <- count$count/100 # count m-1

### Data exploration ####
## Normality and homogeneity ####
hist(c) # highly right skewed
boxplot(c ~ c.type * c.region) # heterogenous

#### Data analysis ####
m1 <- lm(c ~ c.type * c.region)
drop1(m1, test = "F") # remove interacion
m1 <- lm(c ~ c.type + c.region)

# test model fit
par(mfrow = c(2,2))
plot(m1, col = c.type) 
# variance is a lot smaller for Twisted than Filament

par(mfrow = c(1,2))
hist(resid(m1))
qqnorm(resid(m1))
qqline(resid(m1)) # right-skewed
par(mfrow = c(1,1))

# try fitting gamma distribution
require(fitdistrplus)
norm <- fitdist(c, "norm")
gamma <- fitdist(c, "gamma")

cdfcomp(list(norm, gamma), 
        legendtext = c("Normal", "Gamma"), 
        fitlty = 1)
gofstat(list(norm, gamma), 
        fitnames = c("Normal", "Gamma"))
# according to fitdistrplus, gamma fits a lot better

m2 <- glm(c ~ c.type * c.region, family = Gamma(link = "log"))
drop1(m2, test = "Chisq") # remove interaction
m2 <- glm(c ~ c.type + c.region, family = Gamma(link = "log"))

# test model fit
par(mfrow = c(2,2))
plot(m2, col = c.type) # variance is relatively homogenous

par(mfrow = c(1,2))
hist(resid(m2)) 
qqnorm(resid(m2))
qqline(resid(m2)) # normal
par(mfrow = c(1,1))

# interpret model
require(car)
Anova(m2, type = 2)
# Response: c
#          LR Chisq Df Pr(>Chisq)    
# c.type     5.9903  2    0.05003 .  
# c.region  19.4114  1  1.054e-05 ***


#### Data visualisation ####
## Descriptive statistics ####
require(psych)
cstat <- describeBy(c, list(c.type, c.region), mat = T)
cstat$group1 <- factor(cstat$group1, levels = c("Twisted", "Braided", "Filament"))
crstat <- describeBy(c, c.region, mat = T)
names(crstat)[names(crstat) == "group1"] <- "group2"

## Plot ####
cp <- ggplot(cstat, mapping = aes(group1, mean, fill = group2, colour = group2)) +
  geom_col(position = "dodge", width = 0.85) +
  geom_errorbar(aes(ymin = mean, ymax = mean + se),
                width = .1, lwd = .4,
                position = position_dodge(width = 0.85)) +
  geom_point(aes(group1, median), size = 2, 
             position = position_dodge(width = 0.85)) +
  geom_rug(crstat, mapping = aes(1, mean),
           size = 1, sides = "l", length = unit(.24, "cm")) +
  geom_text(aes(group1, mean + se + 0.08, label = n), 
            position = position_dodge(width = 0.85),
            size = 4.2, colour = "#000000") +
  scale_fill_manual(values = c("#e1f0e8", "#cae4f0"),
                    guide = guide_legend(title = "Coast")) +
  scale_colour_manual(values = c("#7dbe9c", "#53a7ce"),
                      guide = guide_legend(title = "Coast")) +
  ylab(expression("Eulittoral abundance (m"^-1*")")) +
  scale_y_continuous(labels = c(0, 0.5, 1, 1.5, 2, 2.5), expand = c(0,0)) +
  coord_cartesian(ylim = c(0, 2.5)) +
  mytheme +
  theme(axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.background = element_blank(),
        legend.position = c(0.22, 0.92))

cp # print (dimensions: 5 x 3 in)

##### Part 2: Dimensions data ####
#### Data preparation ####
### Load data ####
dims <- read.csv("~/Desktop/Plymouth University/IMLRU/ALDFG/Data/dimensions.csv")

### Rename variables ####
d.type <- factor(dims$type, levels = c("Twisted", "Braided", "Filament"))
d.region <- dims$region
lm <- dims$length # in mm
w <- dims$width # in mm
v <- pi*((0.5*w)^2)*lm*0.001 # in cm^3
l <- lm/10 # in cm

#### Data exploration ####
### Volume ####
hist(v) # highly right skewed
boxplot(v ~ d.type * d.region) # very heterogenous variance

### Length ####
hist(l) # right skewed
boxplot(l ~ d.type * d.region) # heterogenous variance

#### Data analysis ####
### Volume ####
m3 <- lm(v ~ d.type * d.region)
drop1(m3, test = "F") # retain interaction

# test model fit
par(mfrow = c(2,2))
plot(m3, col = d.type) # variance is larger for Twisted than Filament

par(mfrow = c(1,2))
hist(resid(m3)) 
qqnorm(resid(m3))
qqline(resid(m3)) # residuals are non-normal
par(mfrow = c(1,1))

# try fitting gamma distribution
norm <- fitdist(v, "norm")
gamma <- fitdist(v, "gamma")

cdfcomp(list(norm, gamma), 
        legendtext = c("Normal", "Gamma"), 
        fitlty = 1)
gofstat(list(norm, gamma), 
        fitnames = c("Normal", "Gamma"))
# according to fitdistrplus, gamma fits much better

m4 <- glm(v ~ d.type * d.region, family = Gamma(link = "log"), maxit = 100)
drop1(m4, test = "Chisq") # retain interaction

# test model fit
par(mfrow = c(2,2))
plot(m4, col = d.type) 
# homogeneity is better between rope types

par(mfrow = c(1,2))
hist(resid(m4)) 
qqnorm(resid(m4))
qqline(resid(m4)) # relatively normal
par(mfrow = c(1,1))

# interpret model
Anova(m4, type = 3)
# Response: v
#                 LR Chisq Df Pr(>Chisq)    
# d.type           134.161  2  < 2.2e-16 ***
# d.region          28.310  1  1.034e-07 ***
# d.type:d.region   19.718  2  5.227e-05 ***

d.type <- factor(d.type, levels = c("Braided", "Twisted", "Filament"))
m4 <- glm(v ~ d.type * d.region, family = Gamma(link = "log"), maxit = 100)
Anova(m4, type = 3) 
# Response: v
#                 LR Chisq Df Pr(>Chisq)    
# d.type           134.161  2  < 2.2e-16 ***
# d.region           1.110  1      0.292    
# d.type:d.region   19.718  2  5.227e-05 ***

d.type <- factor(d.type, levels = c("Filament", "Twisted", "Braided"))
m4 <- glm(v ~ d.type * d.region, family = Gamma(link = "log"), maxit = 100)
Anova(m4, type = 3) 
# Response: v
#                 LR Chisq Df Pr(>Chisq)    
# d.type           134.161  2  < 2.2e-16 ***
# d.region           1.049  1     0.3057    
# d.type:d.region   19.718  2  5.227e-05 ***

d.region <- factor(d.region, levels = c("South", "North"))
m4 <- glm(v ~ d.type * d.region, family = Gamma(link = "log"), maxit = 100)
Anova(m4, type = 3) 
# Response: v
#                 LR Chisq Df Pr(>Chisq)    
# d.type           100.693  2  < 2.2e-16 ***
# d.region           1.049  1     0.3057    
# d.type:d.region   19.718  2  5.227e-05 ***

# pairwise contrasts
require(emmeans)
emmeans(m4, pairwise ~ d.type * d.region)
# contrast                        estimate    SE  df z.ratio p.value
# Filament South - Twisted South    -4.337 0.307 Inf -14.148 <.0001 
# Filament South - Braided South    -3.543 0.321 Inf -11.038 <.0001 #
# Filament South - Filament North    0.394 0.381 Inf   1.035 0.9063 
# Filament South - Twisted North    -6.495 0.553 Inf -11.749 <.0001 
# Filament South - Braided North    -3.910 0.391 Inf -10.004 <.0001 
# Twisted South - Braided South      0.794 0.261 Inf   3.047 0.0280 #
# Twisted South - Filament North     4.731 0.331 Inf  14.280 <.0001 
# Twisted South - Twisted North     -2.158 0.520 Inf  -4.150 0.0005 
# Twisted South - Braided North      0.427 0.343 Inf   1.245 0.8146 
# Braided South - Filament North     3.937 0.345 Inf  11.422 <.0001 
# Braided South - Twisted North     -2.952 0.529 Inf  -5.583 <.0001 
# Braided South - Braided North     -0.367 0.356 Inf  -1.031 0.9077 
# Filament North - Twisted North    -6.889 0.567 Inf -12.151 <.0001 
# Filament North - Braided North    -4.304 0.411 Inf -10.484 <.0001 #
# Twisted North - Braided North      2.585 0.574 Inf   4.505 0.0001 #

d.type <- factor(d.type, levels = c("Twisted", "Braided", "Filament"))
d.region <- factor(d.region, levels = c("North", "South"))

### Length ####
m5 <- lm(l ~ d.type * d.region)
drop1(m5, test = "F") # retain interaction

# test model fit
par(mfrow = c(2,2))
plot(m5, col = d.type) 
# variance is slightly different between rope types

par(mfrow = c(1,2))
hist(resid(m5)) 
qqnorm(resid(m5))
qqline(resid(m5)) # residuals are right-skewed
par(mfrow = c(1,1))

# try fitting gamma distribution
norm <- fitdist(l, "norm")
gamma <- fitdist(l, "gamma")

cdfcomp(list(norm, gamma), 
        legendtext = c("Normal", "Gamma"), 
        fitlty = 1)
gofstat(list(norm, gamma), 
        fitnames = c("Normal", "Gamma"))
# according to fitdistrplus, gamma fits better

m6 <- glm(l ~ d.type * d.region, family = Gamma(link = "log"))
drop1(m6, test = "Chisq") # retain interaction

# test model fit
par(mfrow = c(2,2))
plot(m6, col = d.type) # variance is more homogenous

par(mfrow = c(1,2))
hist(resid(m6)) 
qqnorm(resid(m6))
qqline(resid(m6)) # residuals are quite normal
par(mfrow = c(1,1))

# interpret model
Anova(m6, type = 3) 
# Response: l
#                 LR Chisq Df Pr(>Chisq)    
# d.type            18.687  2  8.754e-05 ***
# d.region          13.471  1  0.0002423 ***
# d.type:d.region   12.325  2  0.0021070 **

d.type <- factor(d.type, levels = c("Braided", "Twisted", "Filament"))
m6 <- glm(l ~ d.type * d.region, family = Gamma(link = "log"))
Anova(m6, type = 3) 
# Response: l
#                 LR Chisq Df Pr(>Chisq)    
# d.type           18.6868  2  8.754e-05 ***
# d.region          1.5327  1   0.215709    
# d.type:d.region  12.3250  2   0.002107 ** 

d.type <- factor(d.type, levels = c("Filament", "Twisted", "Braided"))
m6 <- glm(l ~ d.type * d.region, family = Gamma(link = "log"))
Anova(m6, type = 3) 
# Response: l
#                 LR Chisq Df Pr(>Chisq)    
# d.type           18.6868  2  8.754e-05 ***
# d.region          1.4731  1   0.224865    
# d.type:d.region  12.3250  2   0.002107 ** 
  
d.region <- factor(d.region, levels = c("South", "North"))
m6 <- glm(l ~ d.type * d.region, family = Gamma(link = "log"))
Anova(m6, type = 3) 
# Response: l
#                 LR Chisq Df Pr(>Chisq)    
# d.type            46.098  2  9.773e-11 ***
# d.region           1.473  1   0.224865    
# d.type:d.region   12.325  2   0.002107 ** 

# pairwise contrasts
emmeans(m6, pairwise ~ d.type * d.region)
# contrast                        estimate    SE  df z.ratio p.value
# Braided North - Filament North    -0.755 0.252 Inf -3.000  0.0323 #
# Braided North - Twisted North     -1.393 0.352 Inf -3.963  0.0010 #
# Braided North - Braided South      0.266 0.218 Inf  1.218  0.8282 
# Braided North - Filament South    -1.040 0.239 Inf -4.341  0.0002 
# Braided North - Twisted South     -0.367 0.210 Inf -1.748  0.5000 
# Filament North - Twisted North    -0.639 0.347 Inf -1.839  0.4408 #
# Filament North - Braided South     1.020 0.211 Inf  4.830  <.0001 
# Filament North - Filament South   -0.285 0.233 Inf -1.222  0.8263 
# Filament North - Twisted South     0.387 0.203 Inf  1.908  0.3968 
# Twisted North - Braided South      1.659 0.324 Inf  5.121  <.0001 
# Twisted North - Filament South     0.354 0.339 Inf  1.044  0.9030 
# Twisted North - Twisted South      1.026 0.319 Inf  3.220  0.0162 
# Braided South - Filament South    -1.305 0.197 Inf -6.636  <.0001 #
# Braided South - Twisted South     -0.633 0.160 Inf -3.965  0.0010 #
# Filament South - Twisted South     0.672 0.188 Inf  3.580  0.0046 #

d.type <- factor(d.type, levels = c("Twisted", "Braided", "Filament"))
d.region <- factor(d.region, levels = c("North", "South"))

#### Data visualisation ####
### Volume ####
## Descriptive statistics ####
vstat <- describeBy(v, list(d.type, d.region), mat = T)
vstat$group1 <- factor(vstat$group1, levels = c("Twisted", "Braided", "Filament"))
vrstat <- describeBy(v, d.region, mat = T)
names(vrstat)[names(vrstat) == "group1"] <- "group2"

## Plot ####
# needs to be split in two because of scale break
vpl <- ggplot(vstat, mapping = aes(group1, mean, fill = group2, colour = group2)) +
  geom_col(position = "dodge", width = 0.85) +
  geom_errorbar(aes(ymin = mean, ymax = mean + se),
                width = .1, lwd = .4,
                position = position_dodge(width = 0.85)) +
  geom_point(aes(group1, median, colour = group2), size = 2, 
             position = position_dodge(width = 0.85)) +
  geom_rug(vrstat, mapping = aes(1, mean),
           size = 1, sides = "l", length = unit(.24, "cm")) +
  geom_text(aes(group1, mean + se + 0.45, label = n), 
            position = position_dodge(width = 0.85),
            size = 4.2, colour = "#000000") +
  scale_fill_manual(values = c("#e1f0e8", "#cae4f0"),
                    guide = guide_legend(title = "Coast")) +
  scale_colour_manual(values = c("#7dbe9c", "#53a7ce"),
                      guide = guide_legend(title = "Coast")) +
  ylab(expression("Volume (cm"^3*")")) +
  scale_y_continuous(labels = c(0, 2.5, 5, 7.5, 10), breaks = seq(0, 10, by = 2.5),
                     expand = c(0,0)) +
  coord_cartesian(ylim = c(0, 11)) +
  mytheme +
  theme(axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "none")

vpl # print (dimensions: 4 x 3 in)

vpu <- ggplot(vstat, mapping = aes(group1, mean, fill = group2, colour = group2)) +
  geom_col(position = "dodge", width = 0.85) +
  geom_errorbar(aes(ymin = mean, ymax = mean + se),
                width = .1, lwd = .4,
                position = position_dodge(width = 0.85)) +
  geom_point(aes(group1, median, colour = group2), size = 2, 
             position = position_dodge(width = 0.85)) +
  geom_rug(vrstat, mapping = aes(1, mean),
           size = 1, sides = "l", length = unit(.24, "cm")) +
  geom_text(aes(group1, mean + se + 6, label = n), 
            position = position_dodge(width = 0.85),
            size = 4.2, colour = "#000000") +
  scale_fill_manual(values = c("#e1f0e8", "#cae4f0"),
                    guide = guide_legend(title = "Coast")) +
  scale_colour_manual(values = c("#7dbe9c", "#53a7ce"),
                      guide = guide_legend(title = "Coast")) +
  ylab(expression("Volume (cm"^3*")")) +
  scale_y_continuous(breaks = c(seq(60, 100, by = 40)), 
                     labels = c("6.0", ""), expand = c(0,0)) +
  coord_cartesian(ylim = c(50, 100)) +
  mytheme +
  theme(axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "none")

vpu # print (dimensions: 1.5 x 3 in)


### Length ####
## Descriptive statistics ####
lstat <- describeBy(l, list(d.type, d.region), mat = T)
lstat$group1 <- factor(lstat$group1, levels = c("Twisted", "Braided", "Filament"))
lrstat <- describeBy(l, d.region, mat = T)
names(lrstat)[names(lrstat) == "group1"] <- "group2"

## Plot ####
lp <- ggplot(lstat, mapping = aes(group1, mean, fill = group2, colour = group2)) +
  geom_col(position = "dodge", width = 0.85) +
  geom_errorbar(aes(ymin = mean, ymax = mean + se),
                width = .1, lwd = .4,
                position = position_dodge(width = 0.85)) +
  geom_point(aes(group1, median, colour = group2), size = 2, 
             position = position_dodge(width = 0.85)) +
  geom_rug(lrstat, mapping = aes(1, mean),
           size = 1, sides = "l", length = unit(.24, "cm")) +
  geom_text(aes(group1, mean + se + 3, label = n), 
            position = position_dodge(width = 0.85),
            size = 4.2, colour = "#000000") +
  scale_fill_manual(values = c("#e1f0e8", "#cae4f0"),
                    guide = guide_legend(title = "Coast")) +
  scale_colour_manual(values = c("#7dbe9c", "#53a7ce"),
                      guide = guide_legend(title = "Coast")) +
  ylab(expression("Length (cm)")) +
  scale_y_continuous(expand = c(0,0)) +
  coord_cartesian(ylim = c(0, 100)) +
  mytheme +
  theme(axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "none")

lp # print (dimensions: 5 x 3 in)


##### Part 3: Filament data ####
#### Data preparation ####
### Load data ####
filament <- read.csv("~/Desktop/Plymouth University/IMLRU/ALDFG/Data/filaments.csv")
filament <- filament[-74,] # remove outlier

### Rename variables ####
f.region <- filament$region
f.type <- factor(filament$type, levels = c("Twisted", "Braided"))
f <- filament$filaments

#### Data exploration ####
### Normality and homogeneity ####
hist(f) # highly right skewed
boxplot(f ~ f.type * f.region) # heterogenous variance


#### Data analysis ####
m7 <- lm(f ~ f.type * f.region)
drop1(m7, test = "F") # retain interaction

# test model fit
par(mfrow = c(2,2))
plot(m7, col = f.type) # variance is larger for Twisted than Braided

par(mfrow = c(1,2))
hist(resid(m7)) # residuals are right-skewed
qqnorm(resid(m7))
qqline(resid(m7))
par(mfrow = c(1,1))

# try fitting different distribution
norm <- fitdist(f, "norm")
gamma <- fitdist(f, "gamma")

cdfcomp(list(norm, gamma), 
        legendtext = c("Normal", "Gamma"), 
        fitlty = 1)
gofstat(list(norm, gamma), 
        fitnames = c("Normal", "Gamma"))
# according to fitdistrplus, gamma fits better

m8 <- glm(f ~ f.type * f.region, family = Gamma(link = "log"))
drop1(m8, test = "Chisq") # retain interaction

# test model fit
par(mfrow = c(2,2))
plot(m8, col = f.type) # homogeneity is better

par(mfrow = c(1,2))
hist(resid(m8)) 
qqnorm(resid(m8))
qqline(resid(m8)) # normal 
par(mfrow = c(1,1))

# interpret model
Anova(m8, type = 3)
# Response: f
#                 LR Chisq Df Pr(>Chisq)    
# f.type            8.6313  1   0.003304 ** 
# f.region         19.1623  1  1.201e-05 ***
# f.type:f.region   6.3303  1   0.011870 *  
  
f.type <- factor(f.type, levels = c("Braided", "Twisted"))
m8 <- glm(f ~ f.type * f.region, family = Gamma(link = "log"))
Anova(m8, type = 3)
# Response: f
#                 LR Chisq Df Pr(>Chisq)   
# f.type            8.6313  1   0.003304 **
# f.region          0.3992  1   0.527515   
# f.type:f.region   6.3303  1   0.011870 * 
  
f.region <- factor(f.region, levels = c("South", "North"))
m8 <- glm(f ~ f.type * f.region, family = Gamma(link = "log"))
Anova(m8, type = 3)
# Response: f
#                 LR Chisq Df Pr(>Chisq)  
# f.type            0.0067  1    0.93460  
# f.region          0.3992  1    0.52752  
# f.type:f.region   6.3303  1    0.01187 *
  
# pairwise contrasts
emmeans(m8, pairwise ~ f.type * f.region)
# contrast                      estimate    SE  df z.ratio p.value
# Braided North - Twisted North  -0.7140 0.246 Inf -2.903  0.0193 
# Braided North - Braided South   0.1269 0.202 Inf  0.629  0.9227 
# Braided North - Twisted South   0.1398 0.182 Inf  0.767  0.8693 
# Twisted North - Braided South   0.8409 0.228 Inf  3.687  0.0013 
# Twisted North - Twisted South   0.8538 0.211 Inf  4.045  0.0003 
# Braided South - Twisted South   0.0129 0.157 Inf  0.082  0.9998 

f.type <- factor(f.type, levels = c("Twisted", "Braided"))
f.region <- factor(f.region, levels = c("North", "South"))

#### Data visualisation ####
## Descriptive statistics ####
fstat <- describeBy(f, list(f.type, f.region), mat = T)
fstat$group1 <- factor(fstat$group1, levels = c("Twisted", "Braided"))
frstat <- describeBy(f, f.region, mat = T)
names(frstat)[names(frstat) == "group1"] <- "group2"

## Plot ####
fp <- ggplot(fstat, mapping = aes(group1, mean, fill = group2, colour = group2)) +
  geom_col(position = "dodge", width = 0.85) +
  geom_errorbar(aes(ymin = mean, ymax = mean + se),
                width = .1, lwd = .4,
                position = position_dodge(width = 0.85)) +
  geom_point(aes(group1, median, colour = group2), size = 2, 
             position = position_dodge(width = 0.85)) +
  geom_rug(frstat, mapping = aes(1, mean),
           size = 1, sides = "l", length = unit(.24, "cm")) +
  geom_text(aes(group1, mean + se + 3, label = n), 
            position = position_dodge(width = 0.85),
            size = 4.2, colour = "#000000") +
  scale_fill_manual(values = c("#e1f0e8", "#cae4f0"),
                    guide = guide_legend(title = "Coast")) +
  scale_colour_manual(values = c("#7dbe9c", "#53a7ce"),
                      guide = guide_legend(title = "Coast")) +
  ylab(expression("Filaments (rope"^-1*")")) +
  scale_y_continuous(expand = c(0,0), labels = c(0, 25, 50, 75, "1.0")) +
  coord_cartesian(ylim = c(0, 100)) +
  mytheme +
  theme(axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "none")

fp # print (dimensions: 5 x 2.3 in)


##### Combined plot ####
abc <- plot_grid(cp, fp, lp, ncol = 3, labels = "auto", label_size = 15,
                 label_fontfamily = "Helvetica Neue",
                 rel_widths = c(1, 2.3/3, 1))

abc # save as 5 x 8.22 in


##### Estimate means and medians ####
#### Descriptive statistics ####
ctstat <- describeBy(c, c.type, quant = c(0.25, 0.75), mat = T)
lmtstat <- describeBy(lm, d.type, quant = c(0.25, 0.75), mat = T) # note this is length in mm
vtstat <- describeBy(v, d.type, quant = c(0.25, 0.75), mat = T)
ftstat <- describeBy(f, f.type, quant = c(0.25, 0.75), mat = T)

ctstat2 <- describeBy(c, list(c.type, c.region), quant = c(0.25, 0.75), mat = T)
lmtstat2 <- describeBy(lm, list(d.type, d.region), quant = c(0.25, 0.75), mat = T) # note this is length in mm
vtstat2 <- describeBy(v, list(d.type, d.region), quant = c(0.25, 0.75), mat = T)
ftstat2 <- describeBy(f, list(f.type, f.region), quant = c(0.25, 0.75), mat = T)

#### Extract useful stats ####
ctstat <- ctstat[,c(2,5,7,10:11,16:17)]
lmtstat <- lmtstat[,c(2,5,7,10:11,16:17)]
vtstat <- vtstat[,c(2,5,7,10:11,16:17)]
ftstat <- ftstat[,c(2,5,7,10:11,16:17)]
ftstat <- tibble::add_row(ftstat, group1 = "Filament", 
                          mean = 1, median = 1, Q0.25 = 1, Q0.75 = 1,
                          min = 1, max = 1, .after = 2)

ctstat2 <- ctstat2[,c(2:3,6,8,11:12,17:18)]
lmtstat2 <- lmtstat2[,c(2:3,6,8,11:12,17:18)]
vtstat2 <- vtstat2[,c(2:3,6,8,11:12,17:18)]
ftstat2 <- ftstat2[,c(2:3,6,8,11:12,17:18)]
# rownames(ftstat2) <- NULL
ftstat2 <- tibble::add_row(ftstat2, group1 = "Filament", 
                           group2 = "North",
                           mean = 1, median = 1, 
                           Q0.25 = 1, Q0.75 = 1,
                           min = 1, max = 1, .after = 2)
ftstat2 <- tibble::add_row(ftstat2, group1 = "Filament", 
                           group2 = "South",
                           mean = 1, median = 1, 
                           Q0.25 = 1, Q0.75 = 1,
                           min = 1, max = 1, .after = 5)

#### Combine in one dataframe ####
plastic <- data.frame(cbind(ctstat, ftstat[,2:7], lmtstat[,2:7], vtstat[,2:7]))
plastic2 <- data.frame(cbind(ctstat2, ftstat2[,3:8], lmtstat2[,3:8], vtstat2[,3:8]))

#### Rename columns ####
names(plastic)[names(plastic) == "group1"] <- "Type"
names(plastic)[names(plastic) == "mean"] <- "Count.mean"
names(plastic)[names(plastic) == "median"] <- "Count.median"
names(plastic)[names(plastic) == "Q0.25"] <- "Count.25"
names(plastic)[names(plastic) == "Q0.75"] <- "Count.75"
names(plastic)[names(plastic) == "min"] <- "Count.min"
names(plastic)[names(plastic) == "max"] <- "Count.max"

names(plastic)[names(plastic) == "mean.1"] <- "Filament.mean"
names(plastic)[names(plastic) == "median.1"] <- "Filament.median"
names(plastic)[names(plastic) == "Q0.25.1"] <- "Filament.25"
names(plastic)[names(plastic) == "Q0.75.1"] <- "Filament.75"
names(plastic)[names(plastic) == "min.1"] <- "Filament.min"
names(plastic)[names(plastic) == "max.1"] <- "Filament.max"

names(plastic)[names(plastic) == "mean.2"] <- "Length.mean"
names(plastic)[names(plastic) == "median.2"] <- "Length.median"
names(plastic)[names(plastic) == "Q0.25.2"] <- "Length.25"
names(plastic)[names(plastic) == "Q0.75.2"] <- "Length.75"
names(plastic)[names(plastic) == "min.2"] <- "Length.min"
names(plastic)[names(plastic) == "max.2"] <- "Length.max"

names(plastic)[names(plastic) == "mean.3"] <- "Volume.mean"
names(plastic)[names(plastic) == "median.3"] <- "Volume.median"
names(plastic)[names(plastic) == "Q0.25.3"] <- "Volume.25"
names(plastic)[names(plastic) == "Q0.75.3"] <- "Volume.75"
names(plastic)[names(plastic) == "min.3"] <- "Volume.min"
names(plastic)[names(plastic) == "max.3"] <- "Volume.max"

str(plastic)

names(plastic2)[names(plastic2) == "group1"] <- "Type"
names(plastic2)[names(plastic2) == "group2"] <- "Coast"
names(plastic2)[names(plastic2) == "mean"] <- "Count.mean"
names(plastic2)[names(plastic2) == "median"] <- "Count.median"
names(plastic2)[names(plastic2) == "Q0.25"] <- "Count.25"
names(plastic2)[names(plastic2) == "Q0.75"] <- "Count.75"
names(plastic2)[names(plastic2) == "min"] <- "Count.min"
names(plastic2)[names(plastic2) == "max"] <- "Count.max"

names(plastic2)[names(plastic2) == "mean.1"] <- "Filament.mean"
names(plastic2)[names(plastic2) == "median.1"] <- "Filament.median"
names(plastic2)[names(plastic2) == "Q0.25.1"] <- "Filament.25"
names(plastic2)[names(plastic2) == "Q0.75.1"] <- "Filament.75"
names(plastic2)[names(plastic2) == "min.1"] <- "Filament.min"
names(plastic2)[names(plastic2) == "max.1"] <- "Filament.max"

names(plastic2)[names(plastic2) == "mean.2"] <- "Length.mean"
names(plastic2)[names(plastic2) == "median.2"] <- "Length.median"
names(plastic2)[names(plastic2) == "Q0.25.2"] <- "Length.25"
names(plastic2)[names(plastic2) == "Q0.75.2"] <- "Length.75"
names(plastic2)[names(plastic2) == "min.2"] <- "Length.min"
names(plastic2)[names(plastic2) == "max.2"] <- "Length.max"

names(plastic2)[names(plastic2) == "mean.3"] <- "Volume.mean"
names(plastic2)[names(plastic2) == "median.3"] <- "Volume.median"
names(plastic2)[names(plastic2) == "Q0.25.3"] <- "Volume.25"
names(plastic2)[names(plastic2) == "Q0.75.3"] <- "Volume.75"
names(plastic2)[names(plastic2) == "min.3"] <- "Volume.min"
names(plastic2)[names(plastic2) == "max.3"] <- "Volume.max"

str(plastic2)

### Estimate total volume per metre ####
plastic$Vmean <- with(plastic, Count.mean * Volume.mean)
plastic$Vmedian <- with(plastic, Count.median * Volume.median)
plastic$V25 <- with(plastic, Count.25 * Volume.25)
plastic$V75 <- with(plastic, Count.75 * Volume.75)
plastic$Vmin <- with(plastic, Count.min * Volume.min)
plastic$Vmax <- with(plastic, Count.max * Volume.max)

plastic2$Vmean <- with(plastic2, Count.mean * Volume.mean)
plastic2$Vmedian <- with(plastic2, Count.median * Volume.median)
plastic2$V25 <- with(plastic2, Count.25 * Volume.25)
plastic2$V75 <- with(plastic2, Count.75 * Volume.75)
plastic2$Vmin <- with(plastic2, Count.min * Volume.min)
plastic2$Vmax <- with(plastic2, Count.max * Volume.max)

### Calculate potential microplastic per metre ####
plastic$Mmean <- with(plastic, Count.mean * Filament.mean * Length.mean/5) # 5 mm is the maximum size of microplastic
plastic$Mmedian <- with(plastic, Count.median * Filament.median * Length.median/5)
plastic$M25 <- with(plastic, Count.25 * Filament.25 * Length.25/5)
plastic$M75 <- with(plastic, Count.75 * Filament.75 * Length.75/5)
plastic$Mmin <- with(plastic, Count.min * Filament.min * Length.min/5)
plastic$Mmax <- with(plastic, Count.max * Filament.max * Length.max/5)

plastic2$Mmean <- with(plastic2, Count.mean * Filament.mean * Length.mean/5) # 5 mm is the maximum size of microplastic
plastic2$Mmedian <- with(plastic2, Count.median * Filament.median * Length.median/5)
plastic2$M25 <- with(plastic2, Count.25 * Filament.25 * Length.25/5)
plastic2$M75 <- with(plastic2, Count.75 * Filament.75 * Length.75/5)
plastic2$Mmin <- with(plastic2, Count.min * Filament.min * Length.min/5)
plastic2$Mmax <- with(plastic2, Count.max * Filament.max * Length.max/5)

#### Extract useful data ####
plastic <- plastic[,c(1, 26:37)]
head(plastic)

plastic2 <- plastic2[,c(1:2, 27:38)]
head(plastic2)

##### Estimate variance ####
variance <- data.frame(type = c.type, count = c)
variance <- variance[order(variance$type),]

variance2 <- data.frame(type = c.type, region = c.region, count = c)
variance2 <- variance2[order(variance2$type),]
variance2 <- variance2[order(variance2$region),]

# estimated variance is based on the variable with the smallest sample size and
# largest variance

### Add means and medians ####
variance$filaments.mean <- rep(ftstat[,2], each = 6)
variance$filaments.median <- rep(ftstat[,3], each = 6)
variance$length.mean <- rep(lmtstat[,2], each = 6)
variance$length.median <- rep(lmtstat[,3], each = 6)
variance$volume.mean <- rep(vtstat[,2], each = 6)
variance$volume.median <- rep(vtstat[,3], each = 6)

variance2$filaments.mean <- rep(ftstat2[,3], each = 3)
variance2$filaments.median <- rep(ftstat2[,4], each = 3)
variance2$length.mean <- rep(lmtstat2[,3], each = 3)
variance2$length.median <- rep(lmtstat2[,4], each = 3)
variance2$volume.mean <- rep(vtstat2[,3], each = 3)
variance2$volume.median <- rep(vtstat2[,4], each = 3)

### Estimate total volume per metre ####
variance$Vmean <- with(variance, count * volume.mean)
variance$Vmedian <- with(variance, count * volume.median)

variance2$Vmean <- with(variance2, count * volume.mean)
variance2$Vmedian <- with(variance2, count * volume.median)

### Estimate potential microplastic per metre ####
variance$Mmean <- with(variance, count * filaments.mean * (length.mean/5))
variance$Mmedian <- with(variance, count * filaments.median * (length.median/5))

variance2$Mmean <- with(variance2, count * filaments.mean * (length.mean/5))
variance2$Mmedian <- with(variance2, count * filaments.median * (length.median/5))

### Calculate 95% CI and IQR ####
## Volume ####
statVmean <- with(variance, describeBy(Vmean, type, mat = T))
statVmedian <- with(variance, describeBy(Vmedian, type, quant = c(.25,.75), mat = T))

statVmean$lower <- with(statVmean, mean - se * 1.96) # add CI bounds
statVmean$upper <- with(statVmean, mean + se * 1.96)

statVmean2 <- with(variance2, describeBy(Vmean, list(type, region), mat = T))
statVmedian2 <- with(variance2, describeBy(Vmedian, list(type, region), quant = c(.25,.75), mat = T))

statVmean2$lower <- with(statVmean2, mean - se * 1.96) # add CI bounds
statVmean2$upper <- with(statVmean2, mean + se * 1.96)

## Microplastic ####
statMmean <- with(variance, describeBy(Mmean, type, mat = T))
statMmedian <- with(variance, describeBy(Mmedian, type, quant = c(.25,.75), mat = T))

statMmean$lower <- with(statMmean, mean - se * 1.96) # add CI bounds
statMmean$upper <- with(statMmean, mean + se * 1.96)

statMmean2 <- with(variance2, describeBy(Mmean, list(type, region), mat = T))
statMmedian2 <- with(variance2, describeBy(Mmedian, list(type, region), quant = c(.25,.75), mat = T))

statMmean2$lower <- with(statMmean, mean - se * 1.96) # add CI bounds
statMmean2$upper <- with(statMmean, mean + se * 1.96)

### Master data frame ####
master <- cbind(plastic[,c(1:3, 8:9)], statVmean[,16:17], statVmedian[,16:17], statMmean[,16:17], statMmedian[,16:17])

master2 <- cbind(plastic2[,c(1:4, 9:10)], statVmean2[,17:18], statVmedian2[,17:18], statMmean2[,17:18], statMmedian2[,17:18])

## Rename columns ####
colnames(master)[6] <- "Vlower"
colnames(master)[7] <- "Vupper"
colnames(master)[8] <- "V25"
colnames(master)[9] <- "V75"
colnames(master)[10] <- "Mlower"
colnames(master)[11] <- "Mupper"
colnames(master)[12] <- "M25"
colnames(master)[13] <- "M75"

colnames(master2)[7] <- "Vlower"
colnames(master2)[8] <- "Vupper"
colnames(master2)[9] <- "V25"
colnames(master2)[10] <- "V75"
colnames(master2)[11] <- "Mlower"
colnames(master2)[12] <- "Mupper"
colnames(master2)[13] <- "M25"
colnames(master2)[14] <- "M75"

#### Create .csv file ####
write.csv(master, file = "~/Desktop/Plymouth University/IMLRU/ALDFG/Data/master.csv", 
          row.names = F) # save overall differences between ALDFG types

write.csv(master2, file = "~/Desktop/Plymouth University/IMLRU/ALDFG/Data/master2.csv", 
          row.names = F) # save differences between ALDFG types and coasts


##### Compare to fishing activity ####
#### Load fishery data ####
fishing <- data.frame(region = c(rep("North",13), rep("South",13)),
                      year = 2006:2018,
                      catch = c(8077.2, 8167.03, 9414.25, 7916.69, 8187.5,
                                9977.03, 10215.2, 11168.37, 9024.42, 8769.62,
                                10872.53, 8279.2, 9170.053,
                                32765.39, 34395.78, 34524.48, 32644.06, 37697.2,
                                36640.89, 44648.3, 40862.67, 37735.67, 36768.38,
                                40689.54, 39141.36, 33955.099))
# these data are catch (t) using gear associated with each ALDFG type 
# data were extracted from ICES and MMO (Table S1)
with(fishing, plot(catch ~ year, pch = region)) 
# the marked difference between regions is apparent across years

#### Calculate means ± standard errors ####
fish.stat <- with(fishing, describeBy(catch, region, mat = T))

fish.stat$Vmean <- with(statVmean2, c(mean[1] + mean[2] + mean[3],
                                      mean[4] + mean[5] + mean[6]))
fish.stat$Mmean <- with(statMmean2, c(mean[1] + mean[2] + mean[3],
                                      mean[4] + mean[5] + mean[6]))

fish.stat$Vse <- with(statVmean2, c(sqrt(se[1]^2 + se[2]^2 + se[3]^2),
                                    sqrt(se[4]^2 + se[5]^2 + se[6]^2)))
fish.stat$Mse <- with(statMmean2, c(sqrt(se[1]^2 + se[2]^2 + se[3]^2),
                                    sqrt(se[4]^2 + se[5]^2 + se[6]^2)))

abundance <- describeBy(c, c.region, mat = T)
fish.stat$Amean <- abundance$mean
fish.stat$Ase <- abundance$se

#### Plot ####
t <- 2500/3

fish.plot <- ggplot(fish.stat) +
                geom_line(aes(mean, Mmean), size = .8) +
                geom_line(aes(mean, Amean*t), size = .8) +
                geom_errorbarh(aes(y = Mmean, xmin = mean - se, xmax = mean + se),
                               height = 0, size = .8) +
                geom_errorbarh(aes(y = Amean*t, xmin = mean - se, xmax = mean + se),
                               height = 0, size = .8) +
                geom_pointrange(aes(mean, Mmean, ymin = Mmean - Mse, ymax = Mmean + Mse, 
                                    fill = group1), shape = 21, size = .8) +
                geom_pointrange(aes(mean, Amean*t, ymin = Amean*t - Ase*t, ymax = Amean*t + Ase*t, 
                                fill = group1), shape = 22, size = .8) +
                scale_fill_manual(values = c("#ffffff","#000000"),
                                  guide = guide_legend(title = "Coast")) +
                ylab(expression("Potential microplastic fragments (m"^-1*")")) +
                xlab(expression("Total catch (t yr"^-1*")")) +
                scale_y_continuous(expand = c(0, 0),
                                   sec.axis = sec_axis(~./t, name = expression("ALDFG abundance (m"^-1*")"),
                                                       breaks = seq(0, 3, by = 0.5))) +
                scale_x_continuous(expand = c(0, 0), breaks = seq(5000, 40000, by = 5000)) +
                coord_cartesian(ylim = c(0, 2500), xlim = c(5000, 40000)) +
                mytheme +
                theme(legend.position = c(0.09, 0.9),
                      plot.margin = unit(c(.2, .6, .2, .2),"cm"))
                
fish.plot # dimensions: 5 x 8



###### Clean up ####
detach(package:psych)
detach(package:rgdal)
detach(package:rworldmap)
detach(package:fitdistrplus)
detach(package:car)
detach(package:emmeans)
detach(package:ggplot2)
detach(package:cowplot)
rm(list = ls())
graphics.off()
cat("\014")

