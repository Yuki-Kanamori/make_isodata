
# packages ----------------------------------------------------------------
require(tidyverse)
require(heatwaveR)
require(ggrepel)
library(glmmTMB)


# directory ----------------------------------------------------------------
dir_sst = "/Users/Yuki/Library/CloudStorage/Dropbox/SST/all/"
dir_iso = "/Users/Yuki/Library/CloudStorage/Dropbox/isodata/doto/distribution/"


# data --------------------------------------------------------------------
## sst
setwd(dir = dir_sst)
sst_ap = read.csv("sst_APMZ.csv", fileEncoding = "CP932") %>% mutate(shore = "AP")
sst_mz = read.csv("sst_APMZ.csv", fileEncoding = "CP932") %>% mutate(shore = "MZ")
sst_mb = read.csv("sst_MB.csv", fileEncoding = "CP932") %>% mutate(shore = "MB")
sst_nn = read.csv("sst_NN.csv", fileEncoding = "CP932") %>% mutate(shore = "NN")
sst_mc = read.csv("sst_MC.csv", fileEncoding = "CP932") %>% mutate(shore = "MC")
head(sst_mj)

m_sst = rbind(sst_ap, sst_mz, sst_mb, sst_nn, sst_mc) %>% group_by(year, shore) %>% summarize(m_sst = mean(SST, na.rm = T))
fig_sst = rbind(sst_ap, sst_mz, sst_mb, sst_nn, sst_mc) %>% group_by(year) %>% summarize(m_sst = mean(SST))

g = ggplot(fig_sst, aes(x = year, y = m_sst))
p = geom_point()
l = geom_line()
th = theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8))
labs = labs(x = "Year", y = "Yearly mean SST")
g+p+l+labs+theme_bw()+th

## iso
setwd(dir = dir_iso)
iso = read.csv("doto07C.csv", fileEncoding = "CP932")
unique(iso$species)
target = iso %>% filter(species == "フクロフノリ" | species == "クロバギンナンソウ") %>% 
  mutate(count = 1) %>% group_by(year, species, plot) %>% summarize(freq = sum(count)/200) %>% 
  mutate(shore = str_sub(plot, 1, 2))

list = data.frame(plot = unique(target$plot), year = rep(2002:2025, each = 25)) %>% mutate(shore = str_sub(plot, 1, 2))
list2 = rbind(list %>% mutate(species = "フクロフノリ"), list %>% mutate(species = "クロバギンナンソウ"))

target = left_join(list2, target, by = c("year", "plot", "species", "shore"))
target[is.na(target)] = 0

target2 = left_join(target, m_sst, by = c("year", "shore"))


## mob
mob = read.csv("/Users/Yuki/Library/CloudStorage/Dropbox/isodata/doto/distribution/doto07C_mob.csv", fileEncoding = "CP932")
sp_list = data.frame(species = unique(mob$species))
write.csv(sp_list, "sp_list_mob_diet.csv", fileEncoding = "CP932", row.names = FALSE)



# mhws --------------------------------------------------------------------
sst_ap = sst_ap %>% mutate(temp = paste(year, month, day, sep = "-")) %>% mutate(t = as.Date(temp)) %>% select(-temp) %>% rename(temp = SST)
sst_mb = sst_mb %>% mutate(temp = paste(year, month, day, sep = "-")) %>% mutate(t = as.Date(temp)) %>% select(-temp) %>% rename(temp = SST)
sst_mc = sst_mc %>% mutate(temp = paste(year, month, day, sep = "-")) %>% mutate(t = as.Date(temp)) %>% select(-temp) %>% rename(temp = SST)
sst_nn = sst_nn %>% mutate(temp = paste(year, month, day, sep = "-")) %>% mutate(t = as.Date(temp)) %>% select(-temp) %>% rename(temp = SST)
all_sst = rbind(sst_ap, sst_mb, sst_mc, sst_nn)
head(all_sst)

(lonlat_shore = all_sst %>% distinct(shore, .keep_all = TRUE))
#   lat      ymd     lon temp year month day shore          t
# 1 42.875 19820101 144.625  9.3 1982     1   1    MJ 1982-01-01
# 2 43.125 19820101 144.875  8.9 1982     1   1    OR 1982-01-01
# 3 43.125 19820101 144.875  8.9 1982     1   1    AG 1982-01-01
# 4 42.875 19820101 144.875  9.7 1982     1   1    AK 1982-01-01
# 5 43.125 19820101 145.125  9.4 1982     1   1    KG 1982-01-01

shore = c("AP", "MB", "MC", "NN")


res_events <- NULL

for (i in unique(all_sst$shore)) {
  
  temp_i <- all_sst %>%
    filter(shore == i) %>%
    select(t, temp) %>%     # heatwaveR に必要な列だけ
    arrange(t)
  
  # climatology（熱波/寒波共通で使う）
  clim <- ts2clm(
    data = temp_i,
    climatologyPeriod = c("1982-01-01", "1999-12-31")
  )
  
  # --- 熱波 ---
  res_hw <- detect_event(clim)  # coldSpells=FALSE がデフォ
  cate_hw <- category(res_hw, S = FALSE, name = "sanriku") %>%
    select(event_no, category, season)
  
  ev_hw <- left_join(res_hw$event, cate_hw, by = "event_no") %>%
    mutate(shore = i, event_type = "MHW")
  
  # --- 寒波（coldSpells） ---
  res_cs <- detect_event(clim, coldSpells = TRUE)
  cate_cs <- category(res_cs, S = FALSE, name = "sanriku") %>%
    select(event_no, category, season)
  
  ev_cs <- left_join(res_cs$event, cate_cs, by = "event_no") %>%
    mutate(shore = i, event_type = "MCS")
  
  res_events <- bind_rows(res_events, ev_hw, ev_cs)
}

count(res_events, event_type)
head(res_events)

summary(res_events)
res_events = res_events %>% mutate(year = str_sub(date_peak, 1, 4))

res_year <- res_events %>%
  group_by(year, event_type) %>%
  summarise(
    duration_mean = mean(duration, na.rm = TRUE),
    intensity_mean = mean(intensity_mean, na.rm = TRUE),
    intensity_cumulative = mean(intensity_cumulative, na.rm = TRUE),
    .groups = "drop"
  )

res_year2 <- res_year %>%
  mutate(
    year = as.integer(year),
    event_type = factor(event_type, levels = c("MHW", "MCS"))
  ) %>%
  arrange(event_type, year)

ggplot(res_year2,
       aes(x = year, y = intensity_cumulative)) +
  geom_line(color = "black", linewidth = 1) +
  geom_point(size = 2) +
  facet_wrap(~ event_type, ncol = 1,
             labeller = labeller(
               event_type = c(MHW = "Heatwaves", MCS = "Cold spells")
             )) +
  labs(
    x = "Year"
  ) +
  theme_bw()

res_year_shore <- res_events %>%
  group_by(year, shore, event_type) %>%
  summarise(
    intensity_mean = mean(intensity_mean, na.rm = TRUE),
    .groups = "drop"
  )

res_year_shore2 <- res_year_shore %>%
  mutate(
    year = as.integer(year),
    event_type = factor(event_type, levels = c("MHW", "MCS"))
  ) %>%
  arrange(event_type, year)

ggplot(res_year_shore2,
       aes(x = year, y = intensity_mean, color = shore)) +
  geom_line() +
  facet_wrap(~ event_type, ncol = 1) +
  theme_bw()



# 生物量と平均水温のデータにmhwsの強度*日数の値を付与する
# 2) shore × year × event_type で intensity_cumulative を集計
#    ここでは「その年に起きたイベントの intensity_cumulative を合計」しています
evt_year <- res_events %>%
  group_by(shore, year, event_type) %>%
  summarise(
    intensity_cumulative_sum = sum(intensity_cumulative, na.rm = TRUE),
    .groups = "drop"
  )

# 3) MHW/MCS を列にする（wide）
evt_wide <- evt_year %>%
  tidyr::pivot_wider(
    names_from  = event_type,
    values_from = intensity_cumulative_sum,
    names_prefix = "icumu_",
    values_fill = 0
  )
# できる列例：icumu_MHW, icumu_MCS
evt_wide2 = evt_wide %>%
  mutate(year = as.integer(year)) %>%
  filter(between(year, 2002, 2025))

df_mz = evt_wide2 %>% filter(shore == "AP") %>% mutate(shore = "MZ")
evt_wide2 = rbind(evt_wide2, df_mz)

target3 <- left_join(target2, evt_wide2, by = c("shore", "year"))


setwd(dir_iso)
write.csv(target3, "df_boosting_doto.csv", fileEncoding = "CP932", row.names = FALSE)



# 環境変化の作図 -----------------------------------------------------------------
df_fig = target3 %>% filter(species == "フクロフノリ") %>% select(year, m_sst, icumu_MHW, icumu_MCS) %>%
  mutate(
    icumu_MHW = replace_na(icumu_MHW, 0),
    icumu_MCS = replace_na(icumu_MCS, 0)
  ) %>%
  gather(key = type, value = value, 2:4)  %>% group_by(year, type) %>% summarise(value = mean(value, na.rm = T))

fig = ggplot(df_fig,
       aes(x = year, y = value)) +
  geom_line(color = "black", linewidth = 1) +
  geom_point(size = 2) +
  facet_wrap(~ type, ncol = 3,
             labeller = labeller(
               event_type = c(m_sst = "平均水温", icumu_MHW = "熱波", icumu_MCS = "寒波")
             ), scale = "free") +
  labs(
    x = "Year"
  ) +
  theme_bw(base_family = "HiraKakuPro-W3")

ggsave(filename = "trend_akkeshi_env.png", plot = fig, units = "in", width = 11.69, height = 8.27)




# トレンド ---------------------------------------------------------------------
fig_target = target3 %>% group_by(year, species) %>% summarize(mean = mean(freq), sd = sd(freq))

g = ggplot(fig_target, aes(x = year, y = mean))
p = geom_point()
l = geom_line()
f = facet_wrap(~ species, ncol = 2)
g+p+l+f+theme_bw(base_family = "HiraKakuPro-W3")

hline_df <- fig_target %>%
  group_by(species) %>%
  summarise(
    min = min(mean, na.rm = TRUE),
    max = max(mean, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    low  = min + (max - min) * 1/3,
    high = min + (max - min) * 2/3
  ) %>%
  pivot_longer(
    cols = c(low, high),
    names_to = "level",
    values_to = "yintercept"
  )

g = ggplot(fig_target, aes(x = year, y = mean))
p = geom_point()
l = geom_line()
f = facet_wrap(~ species, ncol = 2)
h <- geom_hline(
  data = hline_df,
  aes(yintercept = yintercept),
  linetype = "dashed",
  color = "red",
  linewidth = 0.4,
  inherit.aes = FALSE
)
fig = g+p+l+h+f+theme_bw(base_family = "HiraKakuPro-W3")
ggsave(filename = "trend_akkeshi.png", plot = fig, units = "in", width = 11.69, height = 8.27)



# glm ---------------------------------------------------------------------
target3$freq_beta <- (target3$freq * (nrow(target3) - 1) + 0.5) / nrow(target3)

target3_scaled <- target3
vars <- c("year", "m_sst","icumu_MCS","icumu_MHW")
target3_scaled[vars] <- scale(target3_scaled[vars])

funo = target3 %>% filter(species == "フクロフノリ")
kuro = target3 %>% filter(species == "クロバギンナンソウ")

glmm_funo <- glmmTMB(
  freq_beta ~ year + m_sst + I(m_sst^2) + icumu_MCS + icumu_MHW +
    (1 | shore),
  family = beta_family(),
  data = funo
)
summary(glmm_funo)


glmm_kuro <- glmmTMB(
  freq_beta ~ year + m_sst + I(m_sst^2) + icumu_MCS + icumu_MHW +
    (1 | shore),
  family = beta_family(),
  data = kuro
)
summary(glmm_kuro)
