library(ggplot2)
library(reshape2)

load("analysis_df.Rdata")
load("totalflowers_df.Rdata")

my_cols <- c("#008080", "#bc5449")

brix.df <- analysis.df[complete.cases(analysis.df$BRIX), -7]
volume.df.zeros <- analysis.df
volume.df.zeros$vol.ind <- ifelse(analysis.df$volume == 0, 0, 1)
volume.df.nonzero <- analysis.df[which(analysis.df$volume != 0),]

rm(analysis.df)

# Pres/Ab
ns <- as.data.frame(with(volume.df.zeros, table(treatment, year, sp)))
ns$Present <- aggregate(volume.df.zeros$vol.ind, by = list(volume.df.zeros$treatment, volume.df.zeros$year, volume.df.zeros$sp), sum)$x
ns$Absent <- ns$Freq - ns$Present
ns$sp_year <- paste(ns$sp, ns$year)

plot_df <- melt(ns, id.vars = c("sp_year", "treatment"), measure.vars = c("Present", "Absent"))
plot_df$sp <- unlist(lapply(strsplit(plot_df$sp_year, split = " "), '[[', 1))
plot_df$year <- unlist(lapply(strsplit(plot_df$sp_year, split = " "), '[[', 2))
plot_df$variable <- droplevels(plot_df$variable)
plot_df$year_pa <- paste(plot_df$year, plot_df$variable, sep = " ")
plot_df$year_pa <- factor(plot_df$year_pa, levels = c("2015 Present",
                                   "2016 Present",
                                   "2015 Absent",
                                   "2016 Absent"))
plot_df$sp_year <- as.factor(plot_df$sp_year)

ggplot(plot_df, aes(x = year_pa, y = value, fill = treatment)) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_fill_manual(values = my_cols) +
  facet_wrap(~sp) +
  labs(fill = "Treatment") +
  xlab("Species-Year") + ylab("Count") +
  theme_bw() +
  theme(aspect.ratio = 1, legend.position = "none")

# Volume
ggplot(data = volume.df.nonzero,
       aes(x = year, y = log(volume), fill = treatment)) +
  geom_boxplot() +
  scale_fill_manual(values = my_cols) +
  facet_wrap(~ sp) +
  labs(fill = "Treatment") + xlab("") + theme_bw() +
  theme(aspect.ratio = 1, legend.position = "none")

# BRIX
ggplot(data = brix.df,
       aes(x = year, y = BRIX, fill = treatment)) +
  geom_boxplot() +
  scale_fill_manual(values = my_cols) +
  facet_wrap(~ sp) +
  labs(fill = "Treatment") + xlab("") + theme_bw() +
  theme(aspect.ratio = 1, legend.position = "none")

# Number of flowers
flowers$sp <- "Bsagittata"

ggplot(data = flowers,
       aes(x = year, y = no_flowers, fill = treatment)) +
  geom_boxplot() +
  facet_wrap(~sp) +
  scale_fill_manual(values = my_cols) +
  labs(fill = "Treatment") + xlab("") + ylab("number of flowers") +
  theme_bw() +
  theme(aspect.ratio = 1)

