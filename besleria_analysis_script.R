library(readxl)
library(rstatix)
data <- read_xlsx("besleria_summary_matrix.xlsx")
str(data)

# Elevation
boxplot(elev_mean ~ distribution, data=data)
boxplot(elev_range ~ distribution, data=data)
wilcox.test(elev_mean ~ distribution, data=data)
wilcox.test(elev_range ~ distribution, data=data)
wilcox_effsize(data, elev_mean ~ distribution)
wilcox_effsize(data, elev_range ~ distribution)

# Thermal range
boxplot(thermal_range ~ distribution, data=data)
boxplot(tempmed_sd ~ distribution, data=data)
boxplot(tempmed_cv ~ distribution, data=data)
wilcox.test(thermal_range ~ distribution, data=data)
wilcox.test(tempmed_sd ~ distribution, data=data)
wilcox.test(tempmed_cv ~ distribution, data=data)
wilcox_effsize(data, thermal_range ~ distribution)
wilcox_effsize(data, tempmed_sd ~ distribution)
wilcox_effsize(data, tempmed_cv ~ distribution)

# Precipitation
boxplot(prec_mean ~ distribution, data=data)
boxplot(prec_range ~ distribution, data=data)
wilcox.test(prec_mean ~ distribution, data=data)
wilcox.test(prec_range ~ distribution, data=data)
wilcox_effsize(data, prec_mean ~ distribution)
wilcox_effsize(data, prec_range ~ distribution)

# Habitat tolerance
boxplot(tolerance_sum ~ distribution, data=data)
wilcox.test(tolerance_sum ~ distribution, data=data)
wilcox_effsize(data, tolerance_sum ~ distribution)

# Climate stability index
data$temp_sd_z <- scale(data$tempmed_sd)
data$thermal_range_z <- scale(data$thermal_range)
data$prec_range_z <- scale(data$prec_range)
data$stability_index <-
  -data$temp_sd_z -
  data$thermal_range_z -
  data$prec_range_z
boxplot(stability_index ~ distribution, data=data)
wilcox.test(stability_index ~ distribution, data=data)
wilcox_effsize(data, stability_index ~ distribution)

# Integrated index of environmental breadth
data$env_breadth <-
  data$elev_range +
  data$thermal_range +
  data$prec_range +
  data$dist_range
boxplot(env_breadth ~ distribution, data=data)
wilcox.test(env_breadth ~ distribution, data=data)
wilcox_effsize(data, env_breadth ~ distribution)

# EOO conversion
data$log_eoo_km2 <- log10(data$eoo_km2)
plot(data$log_eoo_km2, data$tolerance_sum,
     pch = 19,
     xlab = "log(eoo_km2)",
     ylab = "Tolerance sum")

# Main regression model
model1 <- lm(tolerance_sum ~ log_eoo_km2, data=data)
summary(model1)
par(mfrow = c(2,2))
plot(model1)
cor.test(data$log_eoo_km2, data$tolerance_sum, method = "spearman")

# Relationship to environmental variables
cor.test(data$log_eoo_km2, data$env_breadth, method = "spearman")
cor.test(data$log_eoo_km2, data$elev_range, method = "spearman")
cor.test(data$log_eoo_km2, data$thermal_range, method = "spearman")
cor.test(data$log_eoo_km2, data$prec_range, method = "spearman")
cor.test(data$log_eoo_km2, data$tempmed_sd, method = "spearman")
cor.test(data$log_eoo_km2, data$tempmed_cv, method = "spearman")
cor.test(data$log_eoo_km2, data$tempmax_sd, method = "spearman")
cor.test(data$log_eoo_km2, data$tempmax_cv, method = "spearman")
cor.test(data$log_eoo_km2, data$tempmin_sd, method = "spearman")
cor.test(data$log_eoo_km2, data$tempmin_cv, method = "spearman")

# Leave-one-out
library(boot)
boot_fn <- function(data, index) {
  coef(lm(tolerance_sum ~ log_eoo_km2, data=data[index,]))
}
boot(data, boot_fn, R = 1000)

# Complementary regression models
model2 <- lm(log_eoo_km2 ~ elev_range + thermal_range + prec_range, data=data)
summary(model2)
car::vif(model2)
library(boot)
boot_fn <- function(data, index) {coef(lm(log_eoo_km2 ~ elev_range + thermal_range + prec_range, data=data[index,]))}
boot(data, boot_fn, R = 1000)

library(car)
vif(lm(tolerance_sum ~ elev_mean + prec_range + thermal_range, data=data))
model3 <- lm(tolerance_sum ~ elev_mean + prec_range + thermal_range, data=data)
summary(model3)
car::vif(model3)
library(boot)
boot_fn <- function(data, index) {coef(lm(tolerance_sum ~ elev_mean + prec_range + thermal_range, data=data[index,]))}
boot(data, boot_fn, R = 1000)

# Graphical display
library(ggplot2)
x11()
ggplot(data, aes(x = log_eoo_km2, y = tolerance_sum)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = TRUE) +
  theme_classic() +
  labs(
    x = "log (EOO [km²])",
    y = "Habitat tolerance",
    title = "Relationship between geographic range size and habitat tolerance")

# PCoA
library(cluster)
library(dplyr)
vars_env <- data[, c(
  "elev_mean", "dist_mean", "tempmed_mean", "tempmax_mean", "tempmin_mean", "prec_mean", "thermal_range")]
vars_scaled <- scale(vars_env)
dist_matrix <- daisy(vars_scaled, metric = "gower")
print(as.matrix(dist_matrix)[1:13, 1:13])
library(vegan)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(ggrepel)
pcoa <- cmdscale(dist_matrix, k = 2, eig = TRUE)
axes_samples <- as.data.frame(pcoa$points)
colnames(axes_samples) <- c("Axis1", "Axis2")
axes_samples$species <- data$species
variables_env <- envfit(pcoa, data[, c("elev_mean", "dist_mean", "tempmed_mean", "tempmax_mean", "tempmin_mean", "prec_mean", "thermal_range")], perm = 999)
variables_env_df <- as.data.frame(variables_env$vectors$arrows * variables_env$vectors$r)
variables_env_df$Variables <- rownames(variables_env_df)
x11()
p <- ggplot() +
  geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
  geom_vline(xintercept = 0, color = "darkgray", linewidth = 0.5) +
  geom_text_repel(
    data = axes_samples,
    aes(x = Axis1, y = Axis2, label = species, color = species),
    size = 4,
    fontface = "bold",
    alpha = 0.9,
    box.padding = 0.3,
    point.padding = 0.1,
    max.overlaps = Inf,
    segment.color = NA) +
  geom_text_repel(
    data = variables_env_df,
    aes(x = Dim1, y = Dim2, label = Variables),
    size = 4,
    fontface = "bold",
    color = "black",
    box.padding = 0.3,
    max.overlaps = Inf,
    segment.color = NA
  ) +
  scale_color_manual(values = c(
    "aure" = "darkgreen", "brev" = "darkgreen", "diab" = "darkgreen", "disc" = "darkgreen",
    "flum" = "darkgreen", "gran" = "darkgreen", "long" = "darkgreen", "maca" = "darkgreen",
    "mela" = "darkgreen", "meri" = "darkgreen", "sell" = "darkgreen", "flav" = "darkorange", "laxi" = "darkorange")) +
  labs(x = "", y = "") +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid = element_blank())
print(p)

# PERMANOVA
library(vegan)
adonis2(dist_matrix ~ biogeo_group, data=data, permutations=999)
