# install.packages(c("pscl", "haven", "dplyr", "ggplot2", "moments", "digest"))

library(pscl)
library(haven)
library(dplyr)
library(ggplot2)
library(moments)

dat1 = read_sav("./Study Files for OSF/Study 1-VDT-BPAQ/VDT BPAQ-SF Study 1.sav")
dat2 = read_sav("./Study Files for OSF/Study 2-VDT-BPAQ and MCSD Scale/VDT-BPAQ-Social Desirability Study 2.sav")
dat3 = read_sav("./Study Files for OSF/Study 3-VDT State Hostility/VDT-Hostility Study 3.sav")
dat4 = read_sav("./Study Files for OSF/Study 4-VDT-RATVS/VDT-RATV Study 4.sav")
dat5 = read_sav("./Study Files for OSF/Study 5-VDT Vignettes/VDT Study 5.sav")
dat6 = read_sav("./Study Files for OSF/Study 6-VDT In-Person/VDT Study 6.sav")

# WIP: Make functions to reduce clutter
# poisplot = function()
# loesplot = function()

# Study 1 ----
dat1$Sex.f = as.factor(dat1$Sex)
hist(dat1$Pins); table(dat1$Pins)

m1 = glm(Pins ~ ATotal, data = dat1, family = "poisson")
summary(m1)
hist(m1$residuals) # Despite poisson regression, residuals still badly non-normal
ggplot(dat1, aes(x = ATotal, y = Pins, col = Sex.f)) +
  geom_point(alpha = .5, position = position_jitter(width = .05)) +
  geom_smooth() +
  ggtitle("Study 1: Pins and Trait Aggression \n Loess curve")
ggplot(dat1, aes(x = ATotal, y = Pins, col = Sex.f)) +
  geom_point(alpha = .5, position = position_jitter(width = .05)) +
  geom_smooth(method = "glm", method.args = list(family = "poisson")) +
  ggtitle("Study 1: Pins and Trait Aggression \n Poisson regression")


# Try a zero-inflated negative binomial instead?
m1.1 = zeroinfl(Pins ~ ATotal, data = dat1, dist = "negbin")
summary(m1.1)
hist(m1.1$residuals); kurtosis(m1.1$residuals); skewness(m1.1$residuals) 
# still doesn't look great,
# highly leptokurtic and strong right skew

# Study 2 ----
dat2$Sex.f = as.factor(dat2$Sex)
hist(dat2$PinToal); table(dat2$PinToal)
hist(dat2$PinsBack); table(dat2$PinsBack)
hist(dat2$PinsFront); table(dat2$PinsFront)

m2 = glm(PinToal ~ BPAQTotal, data = dat2, family = "poisson")
summary(m2)
m2.c = glm(PinToal ~ BPAQ_Center + Sex * soc_desirability_center * BPAQ_Center, 
           data = dat2, 
           family = "poisson")
summary(m2.c)
hist(m2$residuals); kurtosis(m2$residuals); skewness(m2$residuals)
ggplot(dat2[!is.na(dat2$Sex.f),], aes(x = BPAQTotal, y = PinToal, col = Sex.f)) +
  geom_point(alpha = .5, position = position_jitter(width = .05)) +
  geom_smooth() +
  ggtitle("Study 2: Pins and Buss-Perry Aggression \n Loess regression")
ggplot(dat2[!is.na(dat2$Sex.f),], aes(x = BPAQTotal, y = PinToal, col = Sex.f)) +
  geom_point(alpha = .5, position = position_jitter(width = .05)) +
  geom_smooth(method = "glm", method.args = list(family = "poisson")) +
  ggtitle("Study 2: Pins and Buss-Perry Aggression \n Poisson regression")

m2.1 = glm(PinsBack ~ BPAQTotal, data = dat2, family = "poisson")
summary(m2.1)
hist(m2.1$residuals); kurtosis(m2.1$residuals); skewness(m2.1$residuals)
ggplot(dat2[!is.na(dat2$Sex.f),], aes(x = BPAQTotal, y = PinsBack, col = Sex.f)) +
  geom_point(alpha = .5, position = position_jitter(width = .05)) +
  geom_smooth() +
  ggtitle("Study 2: Pins and Buss-Perry Aggression \n Loess regression")
ggplot(dat2[!is.na(dat2$Sex.f),], aes(x = BPAQTotal, y = PinsBack, col = Sex.f)) +
  geom_point(alpha = .5, position = position_jitter(width = .05)) +
  geom_smooth(method = "glm", method.args = list(family = "poisson")) +
  ggtitle("Study 2: Pins and Buss-Perry Aggression \n Poisson regression")

m2.2 = glm(PinsFront ~ BPAQTotal, data = dat2, family = "poisson")
summary(m2.2)
hist(m2.2$residuals); kurtosis(m2.2$residuals); skewness(m2.2$residuals)
ggplot(dat2[!is.na(dat2$Sex.f),], aes(x = BPAQTotal, y = PinsFront, col = Sex.f)) +
  geom_point(alpha = .5, position = position_jitter(width = .05)) +
  geom_smooth() +
  ggtitle("Study 2: Pins and Buss-Perry Aggression \n Loess regression")
ggplot(dat2[!is.na(dat2$Sex.f),], aes(x = BPAQTotal, y = PinsFront, col = Sex.f)) +
  geom_point(alpha = .5, position = position_jitter(width = .05)) +
  geom_smooth(method = "glm", method.args = list(family = "poisson")) +
  ggtitle("Study 2: Pins and Buss-Perry Aggression \n Poisson regression")

# Zero-infl models?
m2.3 = zeroinfl(Pins ~ ATotal, data = dat1, dist = "negbin")
summary(m2.3)
hist(m2.3$residuals); kurtosis(m2.3$residuals); skewness(m2.3$residuals) 
m2.4 = zeroinfl(Pins ~ ATotal, data = dat1, dist = "negbin")
summary(m2.4)
hist(m2.4$residuals); kurtosis(m2.4$residuals); skewness(m2.4$residuals) 
m2.5 = zeroinfl(Pins ~ ATotal, data = dat1, dist = "negbin")
summary(m2.5)
hist(m2.5$residuals); kurtosis(m2.5$residuals); skewness(m2.5$residuals) 
