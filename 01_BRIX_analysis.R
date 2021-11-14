library(lme4)
library(ggResidpanel)
library(emmeans)

load("analysis_df.Rdata")
  #remove censored BRIX values and column with volume values
brix.df <- analysis.df[complete.cases(analysis.df$BRIX), -7]; rm(analysis.df)

#interaction model
mod <- lmer(BRIX ~ year * sp * treatment + (1|plot), data = brix.df)
resid_panel(mod)
summary(mod)

joint_tests(mod) #Type III ANOVA

brix.emm <- emmeans(mod, specs = c("treatment", "sp", "year")); brix.emm
pairs(brix.emm, by = c("sp", "year"), infer = T, reverse = T)

