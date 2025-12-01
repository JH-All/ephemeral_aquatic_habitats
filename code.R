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

# Mantel Volume ---------------------
vol_dist_TP_WP = vegdist(TP_WP_df$Volume, method = "euclidean")
dados_dis_vol_tp_wp = data.frame(vol_dist_TP_WP, TP_WP_pair_df)

mantel_turnover <- mantel(TP_WP_pair$beta.sim, vol_dist_TP_WP, 
                          method = "pearson", permutations = 999)
mantel_turnover

mantel_nestedness <- mantel(TP_WP_pair$beta.sne,  vol_dist_TP_WP,
                            method = "pearson", permutations = 999)
mantel_nestedness

vol_dist_TP_DP = vegdist(TP_DP_df$Volume, method = "euclidean")
dados_dis_vol_tp_dp = data.frame(vol_dist_TP_DP, TP_DP_pair_df)

mantel_turnover <- mantel(TP_DP_pair$beta.sim, vol_dist_TP_DP, 
                          method = "pearson", permutations = 999)
mantel_turnover

mantel_nestedness <- mantel(TP_DP_pair$beta.sne,  vol_dist_TP_DP,
                            method = "pearson", permutations = 999)
mantel_nestedness

vol_dist_RD_WP = vegdist(RD_WP_df$Volume, method = "euclidean")
dados_dis_vol_rd_wp = data.frame(vol_dist_RD_WP, RD_WP_pair_df)

mantel_turnover <- mantel(RD_WP_pair$beta.sim, vol_dist_RD_WP, 
                          method = "pearson", permutations = 999)
mantel_turnover

mantel_nestedness <- mantel(RD_WP_pair$beta.sne,  vol_dist_RD_WP,
                            method = "pearson", permutations = 999)
mantel_nestedness

vol_dist_RD_DP = vegdist(RD_DP_df$Volume, method = "euclidean")
dados_dis_vol_rd_dp = data.frame(vol_dist_RD_DP, RD_DP_pair_df)

mantel_turnover <- mantel(RD_DP_pair$beta.sim, vol_dist_RD_DP, 
                          method = "pearson", permutations = 999)
mantel_turnover

mantel_nestedness <- mantel(RD_DP_pair$beta.sne,  vol_dist_RD_DP,
                            method = "pearson", permutations = 999)
mantel_nestedness

# Mantel Temperature ---------------------
class(TP_WP_df$Temp)
TP_WP_df$Temp = as.numeric(TP_WP_df$Temp)
temp_dist_TP_WP = vegdist(TP_WP_df$Temp, method = "euclidean")
dados_dis_temp_tp_wp = data.frame(temp_dist_TP_WP, TP_WP_pair_df)
mantel_turnover <- mantel(TP_WP_pair$beta.sim, temp_dist_TP_WP, 
                          method = "pearson", permutations = 999)
mantel_turnover
mantel_nestedness <- mantel(TP_WP_pair$beta.sne,  temp_dist_TP_WP,
                            method = "pearson", permutations = 999)
mantel_nestedness
TP_DP_df$Temp = as.numeric(TP_DP_df$Temp)
temp_dist_TP_DP = vegdist(TP_DP_df$Temp, method = "euclidean")
dados_dis_temp_tp_dp = data.frame(temp_dist_TP_DP, TP_DP_pair_df)
mantel_turnover <- mantel(TP_DP_pair$beta.sim, temp_dist_TP_DP, 
                          method = "pearson", permutations = 999)
mantel_turnover
mantel_nestedness <- mantel(TP_DP_pair$beta.sne,  temp_dist_TP_DP,
                            method = "pearson", permutations = 999)
mantel_nestedness
RD_WP_df$Temp = as.numeric(RD_WP_df$Temp)
temp_dist_RD_WP = vegdist(RD_WP_df$Temp, method = "euclidean")
dados_dis_temp_rd_wp = data.frame(temp_dist_RD_WP, RD_WP_pair_df)
mantel_turnover <- mantel(RD_WP_pair$beta.sim, temp_dist_RD_WP, 
                          method = "pearson", permutations = 999)
mantel_turnover
mantel_nestedness <- mantel(RD_WP_pair$beta.sne,  temp_dist_RD_WP,
                            method = "pearson", permutations = 999)
mantel_nestedness
RD_DP_df$Temp = as.numeric(RD_DP_df$Temp)
temp_dist_RD_DP = vegdist(RD_DP_df$Temp, method = "euclidean")
dados_dis_temp_rd_dp = data.frame(temp_dist_RD_DP, RD_DP_pair_df)
mantel_turnover <- mantel(RD_DP_pair$beta.sim, temp_dist_RD_DP, 
                          method = "pearson", permutations = 999)
mantel_turnover

mantel_nestedness <- mantel(RD_DP_pair$beta.sne,  temp_dist_RD_DP,
                            method = "pearson", permutations = 999)
mantel_nestedness

# Mantel Dissolved Oxygen ----------------
class(TP_WP_df$DO)
TP_WP_df$DO = as.numeric(TP_WP_df$DO)
DO_dist_TP_WP = vegdist(TP_WP_df$DO, method = "euclidean")
dados_dis_DO_tp_wp = data.frame(DO_dist_TP_WP, TP_WP_pair_df)
mantel_turnover <- mantel(TP_WP_pair$beta.sim, DO_dist_TP_WP, 
                          method = "pearson", permutations = 999)
mantel_turnover
mantel_nestedness <- mantel(TP_WP_pair$beta.sne,  DO_dist_TP_WP,
                            method = "pearson", permutations = 999)
mantel_nestedness
TP_DP_df$DO = as.numeric(TP_DP_df$DO)
DO_dist_TP_DP = vegdist(TP_DP_df$DO, method = "euclidean")
dados_dis_DO_tp_dp = data.frame(DO_dist_TP_DP, TP_DP_pair_df)
mantel_turnover <- mantel(TP_DP_pair$beta.sim, DO_dist_TP_DP, 
                          method = "pearson", permutations = 999)
mantel_turnover
mantel_nestedness <- mantel(TP_DP_pair$beta.sne,  DO_dist_TP_DP,
                            method = "pearson", permutations = 999)
mantel_nestedness
RD_WP_df$DO = as.numeric(RD_WP_df$DO)
DO_dist_RD_WP = vegdist(RD_WP_df$DO, method = "euclidean")
dados_dis_DO_rd_wp = data.frame(DO_dist_RD_WP, RD_WP_pair_df)
mantel_turnover <- mantel(RD_WP_pair$beta.sim, DO_dist_RD_WP, 
                          method = "pearson", permutations = 999)
mantel_turnover
mantel_nestedness <- mantel(RD_WP_pair$beta.sne,  DO_dist_RD_WP,
                            method = "pearson", permutations = 999)
mantel_nestedness
RD_DP_df$DO = as.numeric(RD_DP_df$DO)
DO_dist_RD_DP = vegdist(RD_DP_df$DO, method = "euclidean")
dados_dis_DO_rd_dp = data.frame(DO_dist_RD_DP, RD_DP_pair_df)
mantel_turnover <- mantel(RD_DP_pair$beta.sim, DO_dist_RD_DP, 
                          method = "pearson", permutations = 999)
mantel_turnover

mantel_nestedness <- mantel(RD_DP_pair$beta.sne, DO_dist_RD_DP,
                            method = "pearson", permutations = 999)
mantel_nestedness

# Mantel pH ----------------
class(TP_WP_df$pH)
TP_WP_df$pH = as.numeric(TP_WP_df$pH)
pH_dist_TP_WP = vegdist(TP_WP_df$pH, method = "euclidean")
dados_dis_pH_tp_wp = data.frame(pH_dist_TP_WP, TP_WP_pair_df)
mantel_turnover <- mantel(TP_WP_pair$beta.sim, pH_dist_TP_WP, 
                          method = "pearson", permutations = 999)
mantel_turnover
mantel_nestedness <- mantel(TP_WP_pair$beta.sne,  pH_dist_TP_WP,
                            method = "pearson", permutations = 999)
mantel_nestedness
TP_DP_df$pH= as.numeric(TP_DP_df$pH)
pH_dist_TP_DP = vegdist(TP_DP_df$pH, method = "euclidean")
dados_dis_pH_tp_dp = data.frame(pH_dist_TP_DP, TP_DP_pair_df)
mantel_turnover <- mantel(TP_DP_pair$beta.sim, pH_dist_TP_DP, 
                          method = "pearson", permutations = 999)
mantel_turnover
mantel_nestedness <- mantel(TP_DP_pair$beta.sne,  pH_dist_TP_DP,
                            method = "pearson", permutations = 999)
mantel_nestedness
RD_WP_df$pH = as.numeric(RD_WP_df$pH)
pH_dist_RD_WP = vegdist(RD_WP_df$pH, method = "euclidean")
dados_dis_pH_rd_wp = data.frame(pH_dist_RD_WP, RD_WP_pair_df)
mantel_turnover <- mantel(RD_WP_pair$beta.sim, pH_dist_RD_WP, 
                          method = "pearson", permutations = 999)
mantel_turnover
mantel_nestedness <- mantel(RD_WP_pair$beta.sne,  DO_dist_RD_WP,
                            method = "pearson", permutations = 999)
mantel_nestedness
RD_DP_df$pH = as.numeric(RD_DP_df$pH)
pH_dist_RD_DP = vegdist(RD_DP_df$pH, method = "euclidean")
dados_dis_pH_rd_dp = data.frame(pH_dist_RD_DP, RD_DP_pair_df)
mantel_turnover <- mantel(RD_DP_pair$beta.sim, pH_dist_RD_DP, 
                          method = "pearson", permutations = 999)
mantel_turnover

mantel_nestedness <- mantel(RD_DP_pair$beta.sne, pH_dist_RD_DP,
                            method = "pearson", permutations = 999)
mantel_nestedness

# Mantel Distance to nearest stream ------------
class(TP_WP_df$river_distance)
TP_WP_df$stream_distance = as.numeric(TP_WP_df$stream_distance)
stream_dist_TP_WP = vegdist(TP_WP_df$stream_distance, method = "euclidean")
dados_dis_stream_tp_wp = data.frame(stream_dist_TP_WP, TP_WP_pair_df)
mantel_turnover <- mantel(TP_WP_pair$beta.sim, stream_dist_TP_WP, 
                          method = "pearson", permutations = 999)
mantel_turnover
mantel_nestedness <- mantel(TP_WP_pair$beta.sne,  stream_dist_TP_WP,
                            method = "pearson", permutations = 999)
mantel_nestedness
TP_DP_df$stream_distance = as.numeric(TP_DP_df$stream_distance)
stream_dist_TP_DP = vegdist(TP_DP_df$stream_distance, method = "euclidean")
dados_dis_stream_tp_dp = data.frame(stream_dist_TP_DP, TP_DP_pair_df)
mantel_turnover <- mantel(TP_DP_pair$beta.sim, stream_dist_TP_DP, 
                          method = "pearson", permutations = 999)
mantel_turnover
mantel_nestedness <- mantel(TP_DP_pair$beta.sne,stream_dist_TP_DP,
                            method = "pearson", permutations = 999)
mantel_nestedness
RD_WP_df$stream_distance = as.numeric(RD_WP_df$stream_distance)
stream_dist_RD_WP = vegdist(RD_WP_df$stream_distance, method = "euclidean")
dados_dis_stream_rd_wp = data.frame(stream_dist_RD_WP, RD_WP_pair_df)
mantel_turnover <- mantel(RD_WP_pair$beta.sim, stream_dist_RD_WP, 
                          method = "pearson", permutations = 999)
mantel_turnover
mantel_nestedness <- mantel(RD_WP_pair$beta.sne,  stream_dist_RD_WP,
                            method = "pearson", permutations = 999)
mantel_nestedness
RD_DP_df$stream_distance = as.numeric(RD_DP_df$stream_distance)
stream_dist_RD_DP = vegdist(RD_DP_df$stream_distance, method = "euclidean")
dados_dis_stream_rd_dp = data.frame(stream_dist_RD_DP, RD_DP_pair_df)
mantel_turnover <- mantel(RD_DP_pair$beta.sim, stream_dist_RD_DP, 
                          method = "pearson", permutations = 999)
mantel_turnover

mantel_nestedness <- mantel(RD_DP_pair$beta.sne, stream_dist_RD_DP,
                            method = "pearson", permutations = 999)
mantel_nestedness

# Figure 4 ---------------------
fig_a = dados_dis_stream_tp_wp %>% 
  ggplot(aes(x = stream_dist_TP_WP, y = Simpson))+
  geom_point(size = 6, shape = 21, fill = "#009E73", alpha = 0.8,
             stroke = 0.6) +
  geom_smooth(method = lm, se = FALSE, color = "black",
              linetype = "dashed", linewidth = 1.2)+
  labs(y = "Turnover (Pools - Wet)", 
       x = "Differences in distance to nearest stream (m)")+
  theme_bw(base_size = 18)+
  scale_y_continuous(limits = c(0,1), breaks = seq(0, 1, by = 0.25))


fig_b = dados_dis_stream_rd_dp %>% 
  ggplot(aes(x = stream_dist_RD_DP, y = Simpson))+
  geom_point(size = 6, shape = 21, fill = "#D55E00", alpha = 0.8,
             stroke = 0.6) +
  geom_smooth(method = lm, se = FALSE, color = "black",
              linetype = "dashed", linewidth = 1.2)+
  labs(y = "Turnover (Ditches - Dry)", 
       x = "Differences in distance to nearest stream (m)")+
  theme_bw(base_size = 18)+
  scale_y_continuous(limits = c(0,1), breaks = seq(0, 1, by = 0.25))

fig_c = dados_dis_vol_tp_dp %>% 
  ggplot(aes(x = vol_dist_TP_DP, y = Aninhamento))+
  geom_point(size = 6, shape = 21, fill = "#E69F00", alpha = 0.8,
             stroke = 0.6) +
  geom_smooth(method = lm, se = FALSE, color = "black",
              linetype = "dashed", linewidth = 1.2)+
  labs(y = "Nestedness (Pools - Dry)", x = expression("Differences in volume (m"^3*")"))+
  theme_bw(base_size = 18)+
  scale_y_continuous(limits = c(0,1), breaks = seq(0, 1, by = 0.25))

fig_d = dados_dis_pH_rd_wp %>% 
  ggplot(aes(x = pH_dist_RD_WP, y = Simpson))+
  geom_point(size = 6, shape = 21, fill = "#0072B2", alpha = 0.8,
             stroke = 0.6) +
  geom_smooth(method = lm, se = FALSE, color = "black",
              linetype = "dashed", linewidth = 1.2)+
  labs(y = "Turnover (Ditches - Wet)", 
       x = "Differences in pH")+
  theme_bw(base_size = 18)+
  scale_y_continuous(limits = c(0,1), breaks = seq(0, 1, by = 0.25))

fig_complete = plot_grid(fig_a, fig_c, fig_d, fig_b, nrow = 2, 
                         labels = "AUTO")

ggsave("fig_complete.jpg", fig_complete, width = 12, height = 8)


#   Figure 5 -----------------

## TP_WP Species plot ----------------------------
long_df_tp_wp <- TP_WP_df %>%
  pivot_longer(cols = 12:31, names_to = "species", values_to = "abundance") %>%
  mutate(
    species         = fct_reorder(species, as.numeric(abundance), 
                                  .fun = median, na.rm = TRUE),
    abundance       = as.numeric(abundance),
    stream_distance = as.numeric(stream_distance)
  ) %>%
  filter(!is.na(abundance), !is.na(stream_distance), abundance > 0)

fig_5a = ggplot(long_df_tp_wp, aes(x = stream_distance, y = species,
                                   size = abundance)) +
  geom_point(alpha = 0.9, position = position_jitter(height = 0.1, width = 0),
             shape = 21,fill = "#009E73", color = "black",
             stroke = 0.6)+
  labs(y = NULL, x = "Distance to the nearest stream (m)",
       size = "Abundance")+
  theme_bw(base_size = 18)+
  theme(
    axis.text.y = element_text(face = "italic")
  )+
  scale_x_continuous(limits = c(0,100))

fig_5a

## TP_DP Species plot ----------------------------
long_df_tp_dp <- TP_DP_df %>%
  pivot_longer(cols = 12:31, names_to = "species", values_to = "abundance") %>%
  mutate(
    species         = fct_reorder(species, as.numeric(abundance), 
                                  .fun = median, na.rm = TRUE),
    abundance       = as.numeric(abundance),
    Volume = as.numeric(Volume)
  ) %>%
  filter(!is.na(abundance), !is.na(Volume), abundance > 0)

fig_5b = ggplot(
  long_df_tp_dp %>%
    mutate(highlight = ifelse(species == "Nannostomus beckfordi", "highlight", "other")),
  aes(x = Volume, y = species, size = abundance, fill = highlight)
) +
  geom_point(
    alpha = 0.9,
    position = position_jitter(height = 0.1, width = 0),
    shape = 21, color = "black", stroke = 0.6
  ) +
  scale_fill_manual(
    values = c("highlight" = "darkgray", "other" = "#E69F00"),
    guide = "none"
  ) +
  labs(x = "Volume (m³)", y = NULL, size = "Abundance") +
  guides(
    size = guide_legend(
      override.aes = list(
        fill = "#E69F00", 
        shape = 21,
        color = "black"
      )
    )
  )+
  theme_bw(base_size = 18)+
  theme(
    axis.text.y = element_text(face = "italic")
  )+
  scale_x_continuous(limits = c(0,15))

fig_5b

## RD_WP Species plot ----------------------------
long_df_rd_wp <- RD_WP_df %>%
  pivot_longer(cols = 12:31, names_to = "species", values_to = "abundance") %>%
  mutate(
    species         = fct_reorder(species, as.numeric(abundance), 
                                  .fun = median, na.rm = TRUE),
    abundance       = as.numeric(abundance),
    pH = as.numeric(pH)
  ) %>%
  filter(!is.na(abundance), !is.na(pH), abundance > 0)

fig_5c = ggplot(
  long_df_rd_wp %>%
    mutate(
      highlight = ifelse(
        species %in% c("Nannostomus beckfordi", "Poecilia reticulata",
                       "Hoplosternum littorale"),
        "highlight", "other"
      )
    ),
  aes(x = pH, y = species, size = abundance, fill = highlight)
)+
  geom_point(
    alpha = 0.9,
    position = position_jitter(height = 0.1, width = 0),
    shape = 21, color = "black", stroke = 0.6
  ) +
  scale_fill_manual(
    values = c("highlight" = "darkgray", "other" = "#0072B2"),
    guide = "none"
  ) +
  labs(x = "pH", y = NULL, size = "Abundance") +
  guides(
    size = guide_legend(
      override.aes = list(
        fill = "#0072B2", 
        shape = 21,
        color = "black"
      )
    )
  )+
  theme_bw(base_size = 18)+
  theme(
    axis.text.y = element_text(face = "italic")
  )+
  scale_x_continuous(limits = c(2.5,7))

fig_5c

## RD_DP Species plot ----------------------------
long_df_rd_dp <- RD_DP_df %>%
  pivot_longer(cols = 12:31, names_to = "species", values_to = "abundance") %>%
  mutate(
    species         = fct_reorder(species, as.numeric(abundance), 
                                  .fun = median, na.rm = TRUE),
    abundance       = as.numeric(abundance),
    stream_distance = as.numeric(stream_distance)
  ) %>%
  filter(!is.na(abundance), !is.na(stream_distance), abundance > 0)

fig_5d = ggplot(
  long_df_rd_dp %>%
    mutate(
      highlight = ifelse(
        species %in% c("Nannostomus beckfordi", "Poecilia reticulata"),
        "highlight", "other"
      )
    ),
  aes(x = stream_distance, y = species, size = abundance, fill = highlight)
)+
  geom_point(
    alpha = 0.9,
    position = position_jitter(height = 0.1, width = 0),
    shape = 21, color = "black", stroke = 0.6
  ) +
  scale_fill_manual(
    values = c("highlight" = "darkgray", "other" ="#D55E00"),
    guide = "none"
  ) +
  labs(x = "Distance to the nearest stream (m)", y = NULL, size = "Abundance") +
  guides(
    size = guide_legend(
      override.aes = list(
        fill = "#D55E00", 
        shape = 21,
        color = "black"
      )
    )
  )+
  theme_bw(base_size = 18)+
  theme(
    axis.text.y = element_text(face = "italic")
  )+
  scale_x_continuous(limits = c(0,800))

fig_5d

figure5 = plot_grid(fig_5a, fig_5b, fig_5c, fig_5d, nrow = 2, labels = "AUTO")

ggsave("fig_5.jpg", figure5, width = 15, height = 12)

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
