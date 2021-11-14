library(ggResidpanel)
library(emmeans)
library(lme4)

load("totalflowers_df.Rdata")
table(flowers$no_flowers)
table(flowers$year)

#Poisson GLM
mod <- glmer(no_flowers ~ year * treatment + (1|plot), data = flowers, family = "poisson")
summary(mod)
joint_tests(mod)

flowers.emm <- emmeans(mod, specs = c("year", "treatment"), type = "response"); flowers.emm
pairs(flowers.emm, by = "year", infer = T, reverse = T)