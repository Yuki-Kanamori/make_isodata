require(tidyverse)


# directory ---------------------------------------------------------------
dir_save = "/Users/Yuki/Dropbox/isodata/sanriku/07/"


# data --------------------------------------------------------------------
sessile_c = read.csv("sanriku07C.csv", fileEncoding = "CP932")
sessile_c_ext = read.csv("sanriku07C_Ext.csv", fileEncoding = "CP932")

mobile_c = read.csv("sanriku07C_mob.csv", fileEncoding = "CP932")
mobile_c_ext = read.csv("sanriku07C_Ext_mob.csv", fileEncoding = "CP932")


# ワカメ，コンブ類，マツモ，ヒジキ，テングサ類，ツノマタ類，フクロフノリ，クボガイ，イガイ，ホヤ類


# 固着性 ---------------------------------------------------------------------
unique(sessile_c$species)
get1 = c("ワカメ", "ホソメコンブ", "マコンブ", "コンブsp", "座マツモ", "マツモ", "ヒジキ", "ヒメテングサ", "コトジツノマタ", "ツノマタ属spp", "ツノマタ", "イボツノマタ", "マルバツノマタ", "座フクロフノリ", "フクロフノリ", "マボヤ")
get_plot = c("MJ", "AG")

s_c = sessile_c %>% filter(species %in% get1, str_detect(plot, paste(get_plot, collapse = "|"))) %>% 
  mutate(plot = ifelse(plot == "AG1C(新種あり)", "AG1C", plot)) %>% 
  mutate(plot = str_sub(plot, 1, 3)) %>% 
  group_by(year, plot, species) %>% count() %>% mutate(freq = n/200)

s_ce = sessile_c_ext %>% filter(species %in% get1, str_detect(plot, paste(get_plot, collapse = "|"))) %>% 
  mutate(plot = str_sub(plot, 1, 3)) %>% 
  group_by(year, plot, species) %>% count() %>% mutate(freq = n/400)

df_s = rbind(s_c, s_ce)

unique(df_s$plot)

tag = data.frame(year = rep(2002:2025, each = 10), plot = unique(df_s$plot))

# ワカメ
wakame = left_join(tag, df_s %>% filter(species == "ワカメ"), by = c("year", "plot")) %>% mutate(species = "ワカメ")
wakame[is.na(wakame)] = 0
min = min(wakame$freq)
max = max(wakame$freq)
(low = min+(max-min)*1/3)
(high = min+(max-min)*2/3)

g = ggplot(df, aes(x = year, y = mean))
p = geom_point()
l = geom_line()
f = facet_wrap(~ species, scale = "free")
g+p+l+f+theme_bw(base_family = "HiraKakuPro-W3")+geom_hline(yintercept = c(low, high), linetype = "dashed", color = "red")


# コンブ類
konbu = left_join(tag, df_s %>% filter(species %in% c("ホソメコンブ", "マコンブ", "コンブsp")), by = c("year", "plot")) %>% mutate(species = "コンブ")
konbu[is.na(konbu)] = 0

# マツモ
matsumo = left_join(tag, df_s %>% filter(species %in% c("座マツモ", "マツモ")), by = c("year", "plot")) %>% mutate(species = "マツモ")
matsumo[is.na(matsumo)] = 0

# ヒジキ
hijiki = left_join(tag, df_s %>% filter(species %in% c("ヒジキ")), by = c("year", "plot")) %>% mutate(species = "ヒジキ")
hijiki[is.na(hijiki)] = 0

# テングサ
tengusa = left_join(tag, df_s %>% filter(species %in% c("ヒメテングサ")), by = c("year", "plot")) %>% mutate(species = "テングサ")
tengusa[is.na(tengusa)] = 0

# ツノマタ
tsunomata = left_join(tag, df_s %>% filter(species %in% c("コトジツノマタ", "ツノマタ属spp", "ツノマタ", "イボツノマタ", "マルバツノマタ")), by = c("year", "plot")) %>% mutate(species = "ツノマタ")
tsunomata[is.na(tsunomata)] = 0

# フノリ
funori = left_join(tag, df_s %>% filter(species %in% c("座フクロフノリ", "フクロフノリ")), by = c("year", "plot")) %>% mutate(species = "フノリ")
funori[is.na(funori)] = 0

# ホヤ
hoya = left_join(tag, df_s %>% filter(species %in% c("マボヤ")), by = c("year", "plot")) %>% mutate(species = "ホヤ")
hoya[is.na(hoya)] = 0

# イガイ
igai = left_join(tag, df_s %>% filter(species %in% c("イガイ")), by = c("year", "plot")) %>% mutate(species = "イガイ")
igai[is.na(igai)] = 0


# 移動性 ---------------------------------------------------------------------
m_c = mobile_c %>% filter(species == "クボガイ", str_detect(plot, paste(get_plot, collapse = "|"))) %>% 
  mutate(plot = ifelse(plot == "AG1C(新種あり)", "AG1C", plot)) %>% 
  mutate(plot = str_sub(plot, 1, 3)) %>% 
  group_by(year, plot, species) %>% count() %>% mutate(freq = n/0.5)

m_ce = mobile_c_ext %>% filter(species == "クボガイ", str_detect(plot, paste(get_plot, collapse = "|"))) %>% 
  group_by(year, plot, species) %>% count() %>% mutate(freq = n/1)

df_m = rbind(m_c, m_ce)

unique(df_m$plot)

# クボガイ
kubo = left_join(tag, df_m %>% filter(species %in% c("クボガイ")), by = c("year", "plot")) %>% mutate(species = "クボガイ")
kubo[is.na(kubo)] = 0


# 作図と水準 ----------------------------------------------------------------------
df = rbind(wakame, konbu, matsumo, hijiki, tengusa, tsunomata, funori, hoya, kubo) %>% group_by(year, species) %>% summarize(mean = mean(freq), sd = sd(freq))

hline_df <- df %>%
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

g = ggplot(df, aes(x = year, y = mean))
p = geom_point()
l = geom_line()
f = facet_wrap(~ species, scale = "free")
h <- geom_hline(
  data = hline_df,
  aes(yintercept = yintercept),
  linetype = "dashed",
  color = "grey40",
  linewidth = 0.4,
  inherit.aes = FALSE
)
fig = g+p+l+h+f+theme_bw(base_family = "HiraKakuPro-W3")
ggsave(filename = "trend.png", plot = fig, units = "in", width = 11.69, height = 8.27)


# トレンド --------------------------------------------------------------------
trend_5y <- df %>%
  filter(year >= max(year, na.rm = TRUE) - 4) %>%   # 直近5年（例: 2021–2025）
  group_by(species) %>%
  summarise(
    n_years = n_distinct(year),
    model   = list(lm(mean ~ year, data = pick(year, mean))),
    .groups = "drop"
  ) %>%
  mutate(
    tidied = map(model, ~ broom::tidy(.x)),
    slope  = map_dbl(tidied, ~ .x$estimate[.x$term == "year"]),
    intercept = map_dbl(tidied, ~ .x$estimate[.x$term == "(Intercept)"]),
    p_value   = map_dbl(tidied, ~ .x$p.value[.x$term == "year"]),
    r2        = map_dbl(model, ~ summary(.x)$r.squared),
    direction = case_when(
      slope > 0 ~ "positive",
      slope < 0 ~ "negative",
      TRUE ~ "flat"
    )
  ) %>%
  select(species, n_years, slope, direction, p_value, r2)

trend_5y






