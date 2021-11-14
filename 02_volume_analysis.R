library(ggResidpanel)
library(emmeans)
library(lme4)

load("analysis_df.Rdata")

# Make dfs
volume.df.zeros <- analysis.df
volume.df.zeros$vol.ind <- ifelse(analysis.df$volume == 0, 0, 1)
volume.df.nonzero <- analysis.df[which(analysis.df$volume != 0),]; rm(analysis.df)

# Binomal model for presence/absence
mod1 <- glmer(vol.ind ~ year * sp * treatment + (1|plot), data = volume.df.zeros, family = binomial(link = "logit"))
  # get the model to converge
ss <- getME(mod1,c("theta","fixef"))
mod.zero.int <- update(mod1, start = ss, control = glmerControl(optCtrl = list(maxfun=2e4)))

summary(mod.zero.int)
joint_tests(mod.zero.int)

vol.zero.emm <- emmeans(mod.zero.int, specs = c("treatment", "sp", "year"), type = "response")
vol.zero.emm
pairs(vol.zero.emm, by = c("sp", "year"), infer = T, reverse = T)

# Loglinear model for volume
mod.nonzero.int <- lmer(log(volume) ~ year * sp * treatment + (1|plot), data = volume.df.nonzero)
resid_panel(mod.nonzero.int)

summary(mod.nonzero.int)
joint_tests(mod.nonzero.int)

vol.nonzero.emm <- emmeans(mod.nonzero.int, specs = c("treatment", "sp", "year"), type = "response")
vol.nonzero.emm
pairs(vol.nonzero.emm, by = c("sp", "year"), infer = T, reverse = T)
