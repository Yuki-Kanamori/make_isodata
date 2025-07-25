
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
matu_c = c %>% filter(species == "イガイ" | species == "マツモ" | species == "座マツモ" | species == "ヒメテングサ" | species == "マコンブ" | species == "チガイソ" | species == "ホソメコンブ" | species == "ムラサキイガイ" | species == "ワカメ" | species == "フクロフノリ" | species == "ヒジキ")
unique(matu_c$species)
matu_ext = ext %>% filter(species == "イガイ" | species == "マツモ" | species == "座マツモ" | species == "ヒメテングサ" | species == "マコンブ" | species == "チガイソ" | species == "ホソメコンブ" | species == "ムラサキイガイ" | species == "ワカメ" | species == "フクロフノリ" | species == "ヒジキ")

# surveyという列を新しく作成し，旧天然区ならC・延長ならExtと入れる
matu_c = matu_c %>% mutate(survey = "C")
matu_ext = matu_ext %>% mutate(survey = "Ext")

# データを縦向きに統合する
matu = rbind(matu_c, matu_ext)

# 種名を変更する
sp = data.frame(species = unique(matu$species))
sp$species2 = c("イガイ類", "マツモ", "テングサ類", "ヒジキ","フクロフノリ", "イガイ類", "マツモ", "コンブ類", "ワカメ", "コンブ類", "コンブ類")
matu = matu %>% left_join(sp, by = "species") 
matu$species = NULL
matu = matu %>% rename(species = species2)

# プロットごと・年ごとに生物量を集計する
matu2 = matu %>% group_by(year, plot, species) %>% count()

temp = data.frame(plot = unique(matu2$plot), year = rep(2002:2023, each = 25))

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

shore = c("MJ", "OR", "AG", "AK", "KG")

res_mhw = NULL
for(i in shore){
  temp = all_sst %>% filter(shore == i)
  
  res = detect_event(ts2clm(data = temp,
                            climatologyPeriod = c("1982-01-01", "2023-12-31")))
  cate = category(res, S = FALSE, name = "sanriku") %>% select(event_no, category, season)
  df_event = left_join(res[["event"]], cate, by = "event_no") %>% mutate(shore = paste(i))
  
  res_mhw = rbind(res_mhw, df_event)
}
unique(res_mhw$shore)
summary(res_mhw)
res_mhw = res_mhw %>% mutate(year = str_sub(date_peak, 1, 4))

d = res_mhw %>% select(duration, shore, year) %>% rename(value = duration) %>% mutate(cate = "duration")
i = res_mhw %>% select(intensity_mean, shore, year) %>% rename(value = intensity_mean) %>% mutate(cate = "intensity_mean")
c = res_mhw %>% select(intensity_cumulative, shore, year) %>% rename(value = intensity_cumulative) %>% mutate(cate = "intensity_cumulative")
dic = rbind(d, i, c) 
summary(dic)
fig_dic = dic %>% group_by(cate, year) %>% summarize(mean = mean(value))
fig_dic$cate = factor(fig_dic$cate, levels = c("duration", "intensity_mean", "intensity_cumulative"))
levels(fig_dic$cate)

g = ggplot(fig_dic, aes(x = year, y = mean, group = cate))
p = geom_point()
l = geom_line()
f = facet_wrap(~ cate, scales = "free", ncol = 3)
labs = labs(x = "Year", y = "")
th = theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8))
fig = g+p+l+f+labs+theme_bw()+th
setwd(dir = "/Users/Yuki/Dropbox/isodata/sanriku/")
ggsave(filename = "mhw.pdf", plot = fig, units = "in", width = 11.69, height = 8.27)

# 日数と強度の掛け算だけ
g = ggplot(fig_dic %>% filter(cate == "intensity_cumulative") %>% mutate(year = as.numeric(year)), aes(x = year, y = mean, group = cate))
p = geom_point()
l = geom_line()
labs = labs(x = "Year", y = "MHWs")
th = theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8))
fig = g+p+l+labs+theme_bw()+th+scale_x_continuous(breaks = seq(1980, 2024, 10))
setwd(dir = "/Users/Yuki/Dropbox/isodata/sanriku/")
ggsave(filename = "mhw_intensity_cumulative.pdf", plot = fig, units = "in", width = 11.69, height = 8.27)


# 生物量と平均水温のデータにmhwsの強度*日数の値を付与する
head(c); head(df)
summary(c)
c2 = c %>% group_by(year, shore) %>% summarize(sum = sum(value)) %>% mutate(year = as.numeric(year))
summary(c2)
temp = data.frame(shore = shore, year = rep(1985:2024, each = 5))
c3 = left_join(temp, c2, by = c("year", "shore"))
c3[is.na(c3)] = 0
df = left_join(df, c3, by = c("year", "shore"))

# 説明変数の標準化
summary(df)
m_sst2 = data.frame(m_sst2 = scale(df$m_sst))
sum2 = data.frame(sum2 = scale(df$sum))

df2 = cbind(df, m_sst2, sum2) %>% as.data.frame()
head(df2)



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

