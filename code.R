# Packages -----------------
library(tidyverse)
library(readxl)
library(vegan)
library(betapart)
library(adespatial)
library(betareg)
library(cowplot)
library(ggridges)
library(viridisLite) 
library(stringr)

# Getting data ready --------------
data = read_excel("raw_data.xlsx")
com = data[,12:31]
data$category = as.factor(data$category)
levels(data$category)

data$`maximum_length (m)` = as.numeric(data$`maximum_length (m)`)
data$`maximum_width (m)` = as.numeric(data$`maximum_width (m)` )
data$`maximum_depth (cm)` = as.numeric(data$`maximum_depth (cm)`)
data$depth_m = data$`maximum_depth (cm)` / 100
V = (2/3) * pi * data$`maximum_length (m)`* data$`maximum_width (m)`  * data$depth_m 
data$Volume <- V

road = data %>% 
  filter(category == "Road ditches")
pools = data %>% 
  filter(category == "Temporary pools")
road_com = road[,12:31]
pool_com = pools[,12:31]
road_pa = decostand(road_com, method = "pa")
pool_pa = decostand(pool_com, method = "pa")

# PERMANOVA & NMDS ----------------
data$CAT <- interaction(data$Period, data$category)
levels(data$CAT)
levels(data$CAT) <- c(
  "Ditches - Dry",  # Dry Period.Road ditches
  "Ditches - Wet",  # Wet Period.Road ditches
  "Pools - Dry",  # Dry Period.Temporary pools
  "Pools - Wet"   # Wet Period.Temporary pools
)

data$CAT <- factor(data$CAT, levels = c("Pools - Wet", "Pools - Dry", 
                                        "Ditches - Wet", "Ditches - Dry"))

distancia <- vegdist(com, method = "bray")

resultado_permanova <- adonis2(distancia ~ data$CAT, permutations = 999)
print(resultado_permanova)

dispersao <- betadisper(distancia, data$CAT)
anova(dispersao)  

nmds_result <- metaMDS(com, distance = "bray", k = 2, trymax = 200)
nmds_scores <- as.data.frame(scores(nmds_result, display = "sites"))
nmds_scores$CAT <- data$CAT

stress_value <- round(nmds_result$stress, 3)

hull_data <- nmds_scores %>%
  group_by(CAT) %>%
  slice(chull(NMDS1, NMDS2))

## Figure 2 ----------------
NMDS_plot = ggplot(nmds_scores, aes(x = NMDS1, y = NMDS2)) +
  geom_polygon(data = hull_data, aes(fill = CAT, group = CAT), 
               alpha = 0.3, color = "black", show.legend = F,
               linewidth = 0.7) +
  geom_point(aes(fill = CAT), shape = 21, size = 5, color = "black",
             show.legend = F, alpha = 1, stroke = 0.6) +
  theme_bw(base_size = 20)+
  scale_fill_manual(values = c("#009E73", "#E69F00", "#0072B2", "#D55E00"))+
  facet_wrap(~CAT)+
  scale_x_continuous(limits = c(-3.8, 3),
                     breaks = seq(-3, 3, by = 1.5))+
  scale_y_continuous(limits = c(-3, 2.3),
                     breaks = seq(-3, 2, by = 1.5))


ggsave("NMDS.tiff", NMDS_plot)

## Presence-absence PERMANOVA -------------------
com_pa = decostand(com, method = "pa")

distancia_pa <- vegdist(com, method = "jaccard", binary = TRUE)

resultado_permanova_pa <- adonis2(
  distancia_pa ~ data$CAT,
  permutations = 999
)
print(resultado_permanova_pa)

dispersao_pa <- betadisper(distancia_pa, data$CAT)
anova(dispersao_pa)

nmds_result <- metaMDS(com_pa, distance = "jaccard", k = 2, trymax = 200)
nmds_scores <- as.data.frame(scores(nmds_result, display = "sites"))
nmds_scores$CAT <- data$CAT

stress_value <- round(nmds_result$stress, 3)

hull_data <- nmds_scores %>%
  group_by(CAT) %>%
  slice(chull(NMDS1, NMDS2))


# Beta diversity  -------------------------------

TP_WP_df = data %>% 
  filter(CAT == "Pools - Wet")

TP_WP_com = decostand(TP_WP_df[,12:31], method = "pa")
str(TP_WP_com)

TP_WP_beta = beta.multi(TP_WP_com,  index.family = "sorensen")
TP_WP_beta #Beta total = 0.86, turnover = 0.75, nestedness = 0.11

TP_DP_df = data %>% 
  filter(CAT == "Pools - Dry")

TP_DP_com = decostand(TP_DP_df[,12:31], method = "pa")
str(TP_DP_com)

TP_DP_beta = beta.multi(TP_DP_com,  index.family = "sorensen")
TP_DP_beta #Beta total = 0.89, turnover = 0.71, nestedness = 0.18

RD_WP_df = data %>% 
  filter(CAT == "Ditches - Wet")

RD_WP_com = decostand(RD_WP_df[,12:31], method = "pa")
str(RD_WP_com)

RD_WP_beta = beta.multi(RD_WP_com,  index.family = "sorensen")
RD_WP_beta #Beta total = 0.87, turnover = 0.71, nestedness = 0.16

RD_DP_df = data %>% 
  filter(CAT == "Ditches - Dry")

RD_DP_com = decostand(RD_DP_df[,12:31], method = "pa")
str(RD_DP_com)

RD_DP_beta = beta.multi(RD_DP_com,  index.family = "sorensen")
RD_DP_beta #Beta total = 0.89, turnover = 0.82, nestedness = 0.07


beta_df <- data.frame(
  CAT = c("Pools - Wet", "Pools - Dry", "Ditches - Wet", "Ditches - Dry"),
  Beta_total = c(0.86, 0.89, 0.87, 0.89),
  Turnover = c(0.75, 0.71, 0.71, 0.82),
  Nestedness = c(0.11, 0.18, 0.16, 0.07)
)

beta_long <- beta_df %>%
  pivot_longer(cols = c("Turnover", "Nestedness"),
               names_to = "Component",
               values_to = "Value")

beta_long$CAT = as.factor(beta_long$CAT)
levels(beta_long$CAT)
beta_long$CAT<- factor(beta_long$CAT, 
                       levels = c("Pools - Wet", "Pools - Dry", 
                                  "Ditches - Wet", "Ditches - Dry"))

## Figure 3 ----------------
betadiv_barplot = ggplot(beta_long, aes(x = CAT, y = Value, fill = Component)) +
  geom_bar(stat = "identity", color = "black", alpha = 0.9, size = 0.6) +
  theme_bw(base_size = 18) +
  scale_fill_manual(values = c(
    "Nestedness" = "#8C510A",     # marrom terroso escuro
    "Turnover" = "#BF812D"    # marrom-claro dourado
  ))+
  labs(x = "Category", y = "Beta diversity", fill = "Component") +
  scale_y_continuous(limits = c(0,1), expand = c(0,0))+
  labs(x = NULL)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("betadiv_barplot.tiff", betadiv_barplot)

# Pairwise beta diversity ----------------
TP_WP_pair <- beta.pair(TP_WP_com, index.family = "sorensen")

TP_WP_pair_df <- data.frame(round(as.numeric(TP_WP_pair$beta.sor), 2),
                                 round(as.numeric(TP_WP_pair$beta.sim), 2),
                                 round(as.numeric(TP_WP_pair$beta.sne), 2))
colnames(TP_WP_pair_df) <- c("Sorensen", "Simpson", "Aninhamento")
head(TP_WP_pair_df)

TP_DP_pair <- beta.pair(TP_DP_com, index.family = "sorensen")

TP_DP_pair_df <- data.frame(round(as.numeric(TP_DP_pair$beta.sor), 2),
                            round(as.numeric(TP_DP_pair$beta.sim), 2),
                            round(as.numeric(TP_DP_pair$beta.sne), 2))
colnames(TP_DP_pair_df) <- c("Sorensen", "Simpson", "Aninhamento")
head(TP_DP_pair_df)

RD_WP_pair <- beta.pair(RD_WP_com, index.family = "sorensen")

RD_WP_pair_df <- data.frame(round(as.numeric(RD_WP_pair$beta.sor), 2),
                            round(as.numeric(RD_WP_pair$beta.sim), 2),
                            round(as.numeric(RD_WP_pair$beta.sne), 2))
colnames(RD_WP_pair_df) <- c("Sorensen", "Simpson", "Aninhamento")
head(RD_WP_pair_df)

RD_DP_pair <- beta.pair(RD_DP_com, index.family = "sorensen")

RD_DP_pair_df <- data.frame(round(as.numeric(RD_DP_pair$beta.sor), 2),
                            round(as.numeric(RD_DP_pair$beta.sim), 2),
                            round(as.numeric(RD_DP_pair$beta.sne), 2))
colnames(RD_DP_pair_df) <- c("Sorensen", "Simpson", "Aninhamento")
head(RD_DP_pair_df)

# Partial Mantel -----------------------
dms_to_decimal <- function(dms) {
  deg  <- as.numeric(str_extract(dms, "^[0-9]+"))
  min  <- as.numeric(str_extract(dms, "(?<=°)[0-9]+"))
  sec  <- as.numeric(str_extract(dms, "(?<=')[0-9.]+"))
  sign <- ifelse(grepl("[SW]", dms), -1, 1)
  sign * (deg + min/60 + sec/3600)
}

get_coord_dist <- function(df) {
  lat <- str_extract(df$coordinates, "^[^ ]+")
  lon <- str_extract(df$coordinates, "(?<= )[^ ]+$")
  
  lat_dec <- sapply(lat, dms_to_decimal)
  lon_dec <- sapply(lon, dms_to_decimal)
  
  coords_dec <- cbind(lon_dec, lat_dec)
  dist(coords_dec)
}

run_partial_mantel <- function(df, pair_df, predictor, response) {
  
  # matriz ambiental
  pred_vec <- as.numeric(df[[predictor]])
  pred_dist <- vegdist(pred_vec, method = "euclidean")
  
  # matriz espacial
  coord_dist <- get_coord_dist(df)
  
  # matriz beta
  beta_mat <- pair_df[[response]]
  
  res <- mantel.partial(
    beta_mat,
    pred_dist,
    as.dist(coord_dist),
    method = "pearson",
    permutations = 999
  )
  
  data.frame(
    predictor = predictor,
    response  = response,
    r         = res$statistic,
    p         = res$signif
  )
}

scenarios <- list(
  TP_WP = list(df = TP_WP_df, pair = TP_WP_pair),
  TP_DP = list(df = TP_DP_df, pair = TP_DP_pair),
  RD_WP = list(df = RD_WP_df, pair = RD_WP_pair),
  RD_DP = list(df = RD_DP_df, pair = RD_DP_pair)
)

predictors <- c("Volume", "Temp", "DO", "pH", "stream_distance")
responses  <- c("beta.sim", "beta.sne")


results <- do.call(
  rbind,
  lapply(names(scenarios), function(scn) {
    
    df   <- scenarios[[scn]]$df
    pair <- scenarios[[scn]]$pair
    
    do.call(
      rbind,
      lapply(predictors, function(pred) {
        do.call(
          rbind,
          lapply(responses, function(resp) {
            
            out <- run_partial_mantel(df, pair, pred, resp)
            out$scenario <- scn
            out
            
          })
        )
      })
    )
  })
)

View(results)

# Figure 4 ---------------------
pairwise_df <- function(df, pair, predictor, response){
  xdist <- as.matrix(vegdist(as.numeric(df[[predictor]]), method = "euclidean"))
  ydist <- as.matrix(pair[[response]])
  
  keep <- upper.tri(xdist, diag = FALSE)
  
  tibble(
    x = xdist[keep],
    y = ydist[keep]
  )
}

# A) Pools - Wet: turnover ~ stream_distance
dat_A <- pairwise_df(TP_WP_df, TP_WP_pair, "stream_distance", "beta.sim")

# B) Pools - Dry: nestedness ~ Volume
dat_B <- pairwise_df(TP_DP_df, TP_DP_pair, "Volume", "beta.sne")

# C) Ditches - Wet: turnover ~ pH
dat_C <- pairwise_df(RD_WP_df, RD_WP_pair, "pH", "beta.sim")

# D) Ditches - Dry: nestedness ~ Volume  (NOVO)
dat_D <- pairwise_df(RD_DP_df, RD_DP_pair, "Volume", "beta.sne")

plot_panel <- function(dat, ylab, xlab, fill_col){
  ggplot(dat, aes(x = x, y = y)) +
    geom_point(size = 6, shape = 21, fill = fill_col, alpha = 0.8, stroke = 0.6) +
    geom_smooth(method = lm, se = FALSE, color = "black", linetype = "dashed", linewidth = 1.2) +
    labs(y = ylab, x = xlab) +
    theme_bw(base_size = 18) +
    scale_y_continuous(limits = c(0,1), breaks = seq(0, 1, by = 0.25))
}


fig_A <- plot_panel(
  dat_A,
  ylab = "Turnover (Pools – Wet)",
  xlab = "Differences in distance to nearest stream (m)",
  fill_col = "#009E73"
)

fig_B <- plot_panel(
  dat_B,
  ylab = "Nestedness (Pools – Dry)",
  xlab = expression("Differences in volume (m"^3*")"),
  fill_col = "#E69F00"
)

fig_C <- plot_panel(
  dat_C,
  ylab = "Turnover (Ditches – Wet)",
  xlab = "Differences in pH",
  fill_col = "#0072B2"
)

fig_D <- plot_panel(
  dat_D,
  ylab = "Nestedness (Ditches – Dry)",
  xlab = expression("Differences in volume (m"^3*")"),
  fill_col = "#D55E00"
)

fig_complete <- plot_grid(fig_A, fig_B, fig_C, fig_D, nrow = 2, labels = "AUTO")

ggsave("Figure_4.jpg", fig_complete, width = 12, height = 9, dpi = 300)


# Figure S1 ----------------------------------------
data$month = as.factor(data$month)
levels(data$month)
data$Volume
data$category
data <- data %>%
  mutate(
    month = factor(month, levels = month.name),           
    category = factor(category, levels = c("Temporary pools", "Road ditches"))
  )


sum_month <- data %>%
  group_by(category, month) %>%
  summarise(
    n        = dplyr::n(),
    vol_mean = mean(Volume, na.rm = TRUE),
    vol_sd   = sd(Volume, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  complete(
    category,
    month = factor(month.name, levels = month.name),
    fill = list(n = 0, vol_mean = NA_real_, vol_sd = NA_real_)
  )


fig_s1 = ggplot(sum_month, aes(x = month, y = vol_mean)) +
  geom_errorbar(aes(ymin = vol_mean - vol_sd, ymax = vol_mean + vol_sd),
                width = 0.2, size = 0.8, na.rm = TRUE) +
  geom_point(size = 2, na.rm = TRUE) +
  geom_line(aes(group = 1), linewidth = 0.5, na.rm = TRUE) +
  facet_wrap(~ category, ncol = 1, scales = "free_y") +
  labs(
    x = NULL,
    y = expression("Mean ± SD of Volume (m"^3*")")
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

fig_s1

ggsave("fig_s1.jpg", fig_s1)
