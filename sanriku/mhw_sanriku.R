
# packages ----------------------------------------------------------------
require(tidyverse)
require(heatwaveR)
require(ggrepel)


# directory ----------------------------------------------------------------
dir_sst = "/Users/Yuki/Library/CloudStorage/Dropbox/SST/all/"
dir_iso = "/Users/Yuki/Dropbox/isodata/sanriku/07/"


# data --------------------------------------------------------------------
## sst
setwd(dir = dir_sst)
sst_mj = read.csv("sst_MJ.csv", fileEncoding = "CP932") %>% mutate(shore = "MJ")
sst_or = read.csv("sst_OR.csv", fileEncoding = "CP932") %>% mutate(shore = "OR")
sst_ag = read.csv("sst_AG.csv", fileEncoding = "CP932") %>% mutate(shore = "AG")
sst_ak = read.csv("sst_AK.csv", fileEncoding = "CP932") %>% mutate(shore = "AK")
sst_kg = read.csv("sst_KG.csv", fileEncoding = "CP932") %>% mutate(shore = "KG")
head(sst_mj)

m_sst = rbind(sst_mj, sst_or, sst_ag, sst_ak, sst_kg) %>% group_by(year, shore) %>% summarize(m_sst = mean(SST))
fig_sst = rbind(sst_mj, sst_or, sst_ag, sst_ak, sst_kg) %>% group_by(year) %>% summarize(m_sst = mean(SST))

g = ggplot(fig_sst, aes(x = year, y = m_sst))
p = geom_point()
l = geom_line()
th = theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8))
labs = labs(x = "Year", y = "Yearly mean SST")
g+p+l+labs+theme_bw()+th


## iso
setwd(dir = dir_iso)
c = read.csv("sanriku07C.csv", fileEncoding = "CP932")
ext = read.csv("sanriku07C_Ext.csv", fileEncoding = "CP932")


# 旧天然区と延長データから水産有用種のデータを抽出する
matu_c = c %>% filter(species == "イガイ" | species == "マツモ" | species == "座マツモ" | species == "ヒメテングサ" | species == "マコンブ" | species == "チガイソ" | species == "ホソメコンブ" | species == "ムラサキイガイ" | species == "ワカメ" | species == "フクロフノリ" | species == "ヒジキ" | species == "マボヤ" | species == "ツノマタ属spp" | species == "ツノマタ" | species == "イボツノマタ" | species == "マルバツノマタ")
unique(matu_c$species)
matu_ext = ext %>% filter(species == "イガイ" | species == "マツモ" | species == "座マツモ" | species == "ヒメテングサ" | species == "マコンブ" | species == "チガイソ" | species == "ホソメコンブ" | species == "ムラサキイガイ" | species == "ワカメ" | species == "フクロフノリ" | species == "ヒジキ" | species == "マボヤ" | species == "ツノマタ" | species == "イボツノマタ" | species == "マルバツノマタ")

# surveyという列を新しく作成し，旧天然区ならC・延長ならExtと入れる
matu_c = matu_c %>% mutate(survey = "C")
matu_ext = matu_ext %>% mutate(survey = "Ext")

# データを縦向きに統合する
matu = rbind(matu_c, matu_ext) %>% select(-column, -row) %>% mutate(count = 1)


### 移動性
c_mob = read.csv("sanriku07C_mob.csv", fileEncoding = "CP932") %>% mutate(survey = "C") %>% filter(species == "クボガイ")
ext_mob = read.csv("sanriku07C_Ext_mob.csv", fileEncoding = "CP932") %>% mutate(survey = "Ext") %>% filter(species == "クボガイ")

matu2 = rbind(c_mob, ext_mob) %>% mutate(plot = str_sub(plot, 1, 3)) %>% select(-height, -row)

matu = rbind(matu, matu2)

# 種名を変更する
sp = data.frame(species = unique(matu$species))
sp$species2 = c("イガイ類", "マツモ", "テングサ類", "ヒジキ","フクロフノリ", "イガイ類", "マツモ", "ツノマタ類", "ツノマタ類", "ツノマタ類", "コンブ類", "ワカメ", "コンブ類", "ツノマタ類", "マボヤ", "コンブ類", "クボガイ")
matu = matu %>% left_join(sp, by = "species") 
matu$species = NULL
matu = matu %>% rename(species = species2)

# プロットごと・年ごとに生物量を集計する
matu2 = matu %>% group_by(year, plot, species) %>% count()

temp = data.frame(plot = unique(matu2$plot), year = rep(2002:2025, each = 25))

matu3 = NULL
for(i in unique(matu2$species)){
  df_temp = matu2 %>% filter(species == i)
  temp2 = left_join(temp, df_temp, by = c("year", "plot")) %>% mutate(species = paste(i))
  matu3 = rbind(matu3, temp2)
}
matu3[is.na(matu3)] = 0

# 2011年以降は調査範囲が広くなっているので，単純にイワフジツボが出現した点の数で比べることができない
# イワフジツボが出現した点の数を調査点の数で割ることにする
# 2010年まではMAX200点，2011年からはMAX400点
matu3 = matu3 %>% mutate(grid = ifelse(year < 2011, 200, 400))
matu3 = matu3 %>% mutate(freq = n/grid)

# 生物量の年平均値を求める
# trend_matu = matu2 %>% group_by(year, species) %>% summarize(abundance = mean(freq))
# head(trend_matu)

matu3 = matu3 %>% mutate(shore = str_sub(plot, 1, 2))
head(matu3)
unique(matu3$plot)




# sstと生物量データを統合する
df = left_join(matu3, m_sst, by = c("year", "shore")) 
summary(df)


# mhws --------------------------------------------------------------------
# test
sst_mj = sst_mj %>% mutate(temp = paste(year, month, day, sep = "-")) %>% mutate(t = as.Date(temp)) %>% select(-temp) %>% rename(temp = SST)
sst_or = sst_or %>% mutate(temp = paste(year, month, day, sep = "-")) %>% mutate(t = as.Date(temp)) %>% select(-temp) %>% rename(temp = SST)
sst_ag = sst_ag %>% mutate(temp = paste(year, month, day, sep = "-")) %>% mutate(t = as.Date(temp)) %>% select(-temp) %>% rename(temp = SST)
sst_ak = sst_ak %>% mutate(temp = paste(year, month, day, sep = "-")) %>% mutate(t = as.Date(temp)) %>% select(-temp) %>% rename(temp = SST)
sst_kg = sst_kg %>% mutate(temp = paste(year, month, day, sep = "-")) %>% mutate(t = as.Date(temp)) %>% select(-temp) %>% rename(temp = SST)
all_sst = rbind(sst_mj, sst_or, sst_ag, sst_ak, sst_kg)
head(all_sst)

(lonlat_shore = all_sst %>% distinct(shore, .keep_all = TRUE))
#   lat      ymd     lon temp year month day shore          t
# 1 42.875 19820101 144.625  9.3 1982     1   1    MJ 1982-01-01
# 2 43.125 19820101 144.875  8.9 1982     1   1    OR 1982-01-01
# 3 43.125 19820101 144.875  8.9 1982     1   1    AG 1982-01-01
# 4 42.875 19820101 144.875  9.7 1982     1   1    AK 1982-01-01
# 5 43.125 19820101 145.125  9.4 1982     1   1    KG 1982-01-01

shore = c("MJ", "OR", "AG", "AK", "KG")

# res_mhw = NULL
# for(i in shore){
#   temp = all_sst %>% filter(shore == i)
#   
#   res = detect_event(ts2clm(data = temp,
#                             climatologyPeriod = c("1982-01-01", "2023-12-31")))
#   cate = category(res, S = FALSE, name = "sanriku") %>% select(event_no, category, season)
#   df_event = left_join(res[["event"]], cate, by = "event_no") %>% mutate(shore = paste(i))
#   
#   res_mhw = rbind(res_mhw, df_event)
# }
# unique(res_mhw$shore)

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

df2 <- left_join(df, evt_wide2, by = c("shore", "year"))


# # 説明変数の標準化
# summary(df2)
# m_sst2 = data.frame(m_sst2 = scale(df$m_sst))
# sum2 = data.frame(sum2 = scale(df$sum))
# 
# df2 = cbind(df, m_sst2, sum2) %>% as.data.frame()
# head(df2)

setwd(dir_iso)
write.csv(df2, "df_boosting_sanriku.csv", fileEncoding = "CP932", row.names = FALSE)



# 環境トレンド ------------------------------------------------------------------
df_fig = df2 %>% filter(species == "フクロフノリ") %>% select(year, m_sst, icumu_MHW, icumu_MCS) %>%
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

ggsave(filename = "trend_sanriku_env.png", plot = fig, units = "in", width = 11.69, height = 8.27)




# 資源状況 --------------------------------------------------------------------
jp_font <- "HiraKakuPro-W3"

## 1) 種×年：平均 dens ± SE
df_year <- df2 %>%
  group_by(species, year) %>%
  summarise(
    mean_dens = mean(freq, na.rm = TRUE),
    sd_dens   = sd(freq, na.rm = TRUE),
    n         = sum(is.finite(freq)),
    se_dens   = ifelse(n > 1, sd_dens / sqrt(n), 0),
    .groups = "drop"
  ) %>%
  mutate(species = as.character(species))  # ★facet紐付けを確実にする

## 2) 赤破線：その種の「mean_dens」の min/max を3等分（2本）
df_lines <- df_year %>%
  group_by(species) %>%
  summarise(
    dens_min = min(mean_dens, na.rm = TRUE),
    dens_max = max(mean_dens, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    yint1 = dens_min + (dens_max - dens_min) / 3,
    yint2 = dens_min + 2 * (dens_max - dens_min) / 3
  ) %>%
  pivot_longer(c(yint1, yint2), names_to = "which", values_to = "yint") %>%
  mutate(species = as.character(species))  # ★facet紐付けを確実にする

## 3) 念のためチェック（yint は必ず min-max の間）
stopifnot(all(df_lines$yint >= rep(df_lines %>% distinct(species, dens_min) %>% arrange(species) %>% pull(dens_min), each = 2) - 1e-8))
stopifnot(all(df_lines$yint <= rep(df_lines %>% distinct(species, dens_max) %>% arrange(species) %>% pull(dens_max), each = 2) + 1e-8))

## 4) 作図
fig <- ggplot(df_year, aes(x = year, y = mean_dens)) +
  geom_ribbon(aes(ymin = mean_dens - se_dens,
                  ymax = mean_dens + se_dens),
              alpha = 0.2) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.8) +
  
  ## ★speciesを持つdf_linesを使う（facetに正しく紐付く）
  geom_hline(
    data = df_lines,
    aes(yintercept = yint),
    color = "red",
    linetype = "dashed",
    linewidth = 0.7
  ) +
  
  facet_wrap(~ species, ncol = 2, scales = "free_y") +
  labs(x = "年", y = "資源量 ± SE", title = "") +
  theme_bw(base_family = jp_font)

print(fig)

## 5) 縦長で保存
ggsave(
  filename = "dens_yearly_facet_vertical.png",
  plot = fig,
  width = 8,
  height = 14,
  units = "in",
  dpi = 300
)




# 資源の動向 -------------------------------------------------------------------
library(broom)

## 1) 直近5年
latest_year <- max(df2$year, na.rm = TRUE)

df_recent <- df2 %>%
  filter(year >= latest_year - 4)

## 2) 種ごとに回帰（安全な方法）

reg_results <- df_recent %>%
  group_by(species) %>%
  nest() %>%
  mutate(
    model = map(data, ~ lm(freq ~ year, data = .x)),
    tidy  = map(model, broom::tidy)
  ) %>%
  unnest(tidy) %>%
  ungroup()

## 3) year の係数のみ

beta_year <- reg_results %>%
  filter(term == "year") %>%
  select(species, estimate, std.error, statistic, p.value) %>%
  arrange(desc(estimate))

print(beta_year)





# 各生物 --------------------------------------------
unique(df2$species) # "テングサ類" "フクロフノリ" "マツモ" "イガイ類" "ヒジキ" "コンブ類" "ワカメ" 

# マツモ
matu = df2 %>% filter(species == "マツモ") %>% mutate(m_sst2_2 = (m_sst2)^2, sum2_2 = (sum2)^2)
summary(matu)

res_matu = NULL
for(i in unique(matu$plot)){
  temp2 = matu %>% filter(plot == i)
  res = lm(freq ~ m_sst2 + m_sst2_2 + sum2 + sum2_2, data = temp2)
  # res2 = data.frame(res[["coefficients"]], plot = paste(i), species = "マツモ")
  eff = data.frame(eff = res[["coefficients"]][["m_sst2"]]+res[["coefficients"]][["m_sst2_2"]]+res[["coefficients"]][["sum2"]]+res[["coefficients"]][["sum2_2"]], plot = paste(i), species = "マツモ")
  res_matu = rbind(res_matu, eff)
}
res = lm(freq ~ m_sst2 + m_sst2_2 + sum2 + sum2_2 , data = matu)
res = lm(freq ~ m_sst2 + m_sst2_2 , data = matu)
summary(res)

summary(matu)
t_matu = matu %>% filter(between(year, 2019, 2023))

res_t_matu = NULL
for(i in unique(matu$plot)){
  temp2 = t_matu %>% filter(plot == i)
  res = lm(freq*100 ~ year, data = temp2)
  # res2 = data.frame(res[["coefficients"]], plot = paste(i), species = "マツモ")
  eff = data.frame(eff = res[["coefficients"]][["year"]], species = "マツモ")
  res_t_matu = rbind(res_t_matu, eff)
}
res_t_matu
res_matu
summary(lm(freq*100 ~ year, data = t_matu))

# res_matu2 = data.frame(m_env = mean(res_matu$eff), se_env = sd(res_matu$eff)/5,
#                        m_cov = mean(res_t_matu$eff), se_cov = sd(res_t_matu$eff)/5)
  


# フノリ
funo = df2 %>% filter(species == "フクロフノリ") %>% mutate(m_sst2_2 = (m_sst2)^2, sum2_2 = (sum2)^2)
res_funo = NULL
for(i in unique(matu$plot)){
  temp2 = funo %>% filter(plot == i)
  res = lm(freq ~ m_sst2 + m_sst2_2 + sum2 + sum2_2, data = temp2)
  # res2 = data.frame(res[["coefficients"]], plot = paste(i), species = "マツモ")
  eff = data.frame(eff = res[["coefficients"]][["m_sst2"]]+res[["coefficients"]][["m_sst2_2"]]+res[["coefficients"]][["sum2"]]+res[["coefficients"]][["sum2_2"]], plot = paste(i), species = "フノリ")
  res_funo = rbind(res_funo, eff)
}
res = lm(freq ~ m_sst2 + m_sst2_2 + sum2 + sum2_2 , data = funo)
summary(res)

t_funo = funo %>% filter(between(year, 2019, 2023))
res_t_funo = NULL
for(i in unique(matu$plot)){
  temp2 = t_funo %>% filter(plot == i)
  res = lm(freq*100 ~ year, data = temp2)
  # res2 = data.frame(res[["coefficients"]], plot = paste(i), species = "マツモ")
  eff = data.frame(eff = res[["coefficients"]][["year"]], species = "フノリ")
  res_t_funo = rbind(res_t_funo, eff)
}
res_t_funo
summary(lm(freq*100 ~ year, data = t_funo))


# コンブ類
kel = df2 %>% filter(species == "コンブ類") %>% mutate(m_sst2_2 = (m_sst2)^2, sum2_2 = (sum2)^2)
res_kel = NULL
for(i in unique(matu$plot)){
  temp2 = kel %>% filter(plot == i)
  res = lm(freq ~ m_sst2 + m_sst2_2 + sum2 + sum2_2, data = temp2)
  # res2 = data.frame(res[["coefficients"]], plot = paste(i), species = "マツモ")
  eff = data.frame(eff = res[["coefficients"]][["m_sst2"]]+res[["coefficients"]][["m_sst2_2"]]+res[["coefficients"]][["sum2"]]+res[["coefficients"]][["sum2_2"]], plot = paste(i), species = "コンブ類")
  res_kel = rbind(res_kel, eff)
}
res = lm(freq ~ m_sst2 + m_sst2_2 + sum2 + sum2_2 , data = kel)
summary(res)

t_kel = kel %>% filter(between(year, 2019, 2023))

res_t_kel = NULL
for(i in unique(matu$plot)){
  temp2 = t_kel %>% filter(plot == i)
  res = lm(freq*100 ~ year, data = temp2)
  # res2 = data.frame(res[["coefficients"]], plot = paste(i), species = "マツモ")
  eff = data.frame(eff = res[["coefficients"]][["year"]], species = "コンブ類")
  res_t_kel = rbind(res_t_kel, eff)
}
res_t_kel
summary(lm(freq*100 ~ year, data = t_kel))


# ワカメ
wak = df2 %>% filter(species == "ワカメ") %>% mutate(m_sst2_2 = (m_sst2)^2, sum2_2 = (sum2)^2)
res_wak = NULL
for(i in unique(matu$plot)){
  temp2 = wak %>% filter(plot == i)
  res = lm(freq ~ m_sst2 + m_sst2_2 + sum2 + sum2_2, data = temp2)
  # res2 = data.frame(res[["coefficients"]], plot = paste(i), species = "マツモ")
  eff = data.frame(eff = res[["coefficients"]][["m_sst2"]]+res[["coefficients"]][["m_sst2_2"]]+res[["coefficients"]][["sum2"]]+res[["coefficients"]][["sum2_2"]], plot = paste(i), species = "ワカメ")
  res_wak = rbind(res_wak, eff)
}
res = lm(freq ~ m_sst2 + m_sst2_2 + sum2 + sum2_2 , data = wak)
summary(res)

t_wak = wak %>% filter(between(year, 2019, 2023))

res_t_wak = NULL
for(i in unique(matu$plot)){
  temp2 = t_wak %>% filter(plot == i)
  res = lm(freq*100 ~ year, data = temp2)
  # res2 = data.frame(res[["coefficients"]], plot = paste(i), species = "マツモ")
  eff = data.frame(eff = res[["coefficients"]][["year"]], species = "ワカメ")
  res_t_wak = rbind(res_t_wak, eff)
}
res_t_wak
summary(lm(freq*100 ~ year, data = t_wak))


# テングサ類
ten = df2 %>% filter(species == "テングサ類") %>% mutate(m_sst2_2 = (m_sst2)^2, sum2_2 = (sum2)^2)
res_ten = NULL
for(i in unique(matu$plot)){
  temp2 = ten %>% filter(plot == i)
  res = lm(freq ~ m_sst2 + m_sst2_2 + sum2 + sum2_2, data = temp2)
  # res2 = data.frame(res[["coefficients"]], plot = paste(i), species = "マツモ")
  eff = data.frame(eff = res[["coefficients"]][["m_sst2"]]+res[["coefficients"]][["m_sst2_2"]]+res[["coefficients"]][["sum2"]]+res[["coefficients"]][["sum2_2"]], plot = paste(i), species = "テングサ類")
  res_ten = rbind(res_ten, eff)
}
res = lm(freq ~ m_sst2 + m_sst2_2 + sum2 + sum2_2 , data = ten)
summary(res)

t_ten = ten %>% filter(between(year, 2019, 2023))

res_t_ten = NULL
for(i in unique(matu$plot)){
  temp2 = t_ten %>% filter(plot == i)
  res = lm(freq*100 ~ year, data = temp2)
  # res2 = data.frame(res[["coefficients"]], plot = paste(i), species = "マツモ")
  eff = data.frame(eff = res[["coefficients"]][["year"]], species = "テングサ")
  res_t_ten = rbind(res_t_ten, eff)
}
res_t_ten
summary(lm(freq*100 ~ year, data = t_ten))


# イガイ類
iga = df2 %>% filter(species == "イガイ類") %>% mutate(m_sst2_2 = (m_sst2)^2, sum2_2 = (sum2)^2)
res_iga = NULL
for(i in unique(matu$plot)){
  temp2 = iga %>% filter(plot == i)
  res = lm(freq ~ m_sst2 + m_sst2_2 + sum2 + sum2_2, data = temp2)
  # res2 = data.frame(res[["coefficients"]], plot = paste(i), species = "マツモ")
  eff = data.frame(eff = res[["coefficients"]][["m_sst2"]]+res[["coefficients"]][["m_sst2_2"]]+res[["coefficients"]][["sum2"]]+res[["coefficients"]][["sum2_2"]], plot = paste(i), species = "イガイ類")
  res_iga = rbind(res_iga, eff)
}
res = lm(freq ~ m_sst2 + m_sst2_2 + sum2 + sum2_2 , data = iga)
summary(res)

t_iga = iga %>% filter(between(year, 2019, 2023))

res_t_iga = NULL
for(i in unique(matu$plot)){
  temp2 = t_iga %>% filter(plot == i)
  res = lm(freq*100 ~ year, data = temp2)
  # res2 = data.frame(res[["coefficients"]], plot = paste(i), species = "マツモ")
  eff = data.frame(eff = res[["coefficients"]][["year"]], species = "イガイ")
  res_t_iga = rbind(res_t_iga, eff)
}
res_t_iga
summary(lm(freq*100 ~ year, data = t_iga))

# ヒジキ
hiji = df2 %>% filter(species == "ヒジキ") %>% mutate(m_sst2_2 = (m_sst2)^2, sum2_2 = (sum2)^2)
res_hiji = NULL
for(i in unique(matu$plot)){
  temp2 = hiji %>% filter(plot == i)
  res = lm(freq ~ m_sst2 + m_sst2_2 + sum2 + sum2_2, data = temp2)
  # res2 = data.frame(res[["coefficients"]], plot = paste(i), species = "マツモ")
  eff = data.frame(eff = res[["coefficients"]][["m_sst2"]]+res[["coefficients"]][["m_sst2_2"]]+res[["coefficients"]][["sum2"]]+res[["coefficients"]][["sum2_2"]], plot = paste(i), species = "ヒジキ")
  res_hiji = rbind(res_hiji, eff)
}
res = lm(freq ~ m_sst2 + m_sst2_2 + sum2 + sum2_2 , data = hiji)
summary(res)

summary(hiji)
t_hiji = hiji %>% filter(between(year, 2019, 2023))

res_t_hiji = NULL
for(i in unique(matu$plot)){
  temp2 = t_hiji %>% filter(plot == i)
  res = lm(freq*100 ~ year, data = temp2)
  # res2 = data.frame(res[["coefficients"]], plot = paste(i), species = "マツモ")
  eff = data.frame(eff = res[["coefficients"]][["year"]], species = "ヒジキ")
  res_t_hiji = rbind(res_t_hiji, eff)
}
res_t_hiji
summary(lm(freq*100 ~ year, data = t_hiji))



# 三陸プロット ------------------------------------------------------------------
res_matu3 = cbind(res_matu, res_t_matu %>% select(-species) %>% rename(eff2 = eff))
res_funo3 = cbind(res_funo, res_t_funo %>% select(-species) %>% rename(eff2 = eff))
res_ten3 = cbind(res_ten, res_t_ten %>% select(-species) %>% rename(eff2 = eff))
res_iga3 = cbind(res_iga, res_t_iga %>% select(-species) %>% rename(eff2 = eff))
res_wak3 = cbind(res_wak, res_t_wak %>% select(-species) %>% rename(eff2 = eff))
res_kel3 = cbind(res_kel, res_t_kel %>% select(-species) %>% rename(eff2 = eff))
res_hiji3 = cbind(res_hiji, res_t_hiji %>% select(-species) %>% rename(eff2 = eff))

sanp = rbind(res_matu3, res_funo3, res_ten3, res_iga3, res_wak3, res_kel3, res_hiji3)

g = ggplot(sanp, aes(x = eff, y = eff2, color = species))
p = geom_point()
g+p+theme_bw(base_family = "HiraKakuPro-W3")

sanp2 = sanp %>% group_by(species) %>% summarize(mean = mean(eff), mean2 = mean(eff2), se = sd(eff)/5, se2 = sd(eff2)/5)
summary(sanp2)
g = ggplot(sanp2, aes(x = mean2, y = mean))
p = geom_point(size = 3)
b = geom_errorbar(aes(ymin = mean-se, ymax = mean+se)) 
b2 = geom_errorbarh(aes(xmin = mean2-se2, xmax = mean2+se2))
labs = labs(x = "資源量の傾向（増減）", y = "温暖化と海洋熱波からの影響")
fig = g+p+b+b2+labs+
  geom_rect(xmin = 0.0, xmax = 1.2, ymin = -1.5, ymax = 0.0, fill = 'orange', alpha = 0.05)+
  geom_rect(xmin = -1.3, xmax = 0.0, ymin = -1.5, ymax = 0.0, fill = 'red', alpha = 0.05)+
  geom_rect(xmin = -1.3, xmax = 0.0, ymin = 0.0, ymax = 1.3, fill = 'yellow', alpha = 0.05)+
  geom_rect(xmin = 0.0, xmax = 1.2, ymin = 0.0, ymax = 1.3, fill = 'green', alpha = 0.05)+
  coord_cartesian(xlim = c(-1.2, 0.1), ylim = c(-0.005, 0.02))+
  # geom_text_repel(label = sanp2$species, family = "HiraKakuPro-W3", size = 5)+
  theme_bw(base_family = "HiraKakuPro-W3")
setwd(dir = "/Users/Yuki/Dropbox/isodata/sanriku/")
ggsave(filename = "sanrikuplot.png", plot = fig, units = "in", width = 11.69, height = 8.27)

