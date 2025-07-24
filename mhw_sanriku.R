
# packages ----------------------------------------------------------------
require(tidyverse)
require(heatwaveR)


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


# 旧天然区と延長データからイワフジツボのデータを抽出する
matu_c = c %>% filter(species == "イガイ" | species == "マツモ" | species == "座マツモ" | species == "ヒメテングサ" | species == "マコンブ" | species == "チガイソ" | species == "ホソメコンブ" | species == "ムラサキイガイ" | species == "ワカメ" | species == "フクロフノリ")
unique(matu_c$species)

# イワフジツボだけが入っているかを確認する
matu_ext = ext %>% filter(species == "イガイ" | species == "マツモ" | species == "座マツモ" | species == "ヒメテングサ" | species == "マコンブ" | species == "チガイソ" | species == "ホソメコンブ" | species == "ムラサキイガイ" | species == "ワカメ" | species == "フクロフノリ")

# surveyという列を新しく作成し，旧天然区ならC・延長ならExtと入れる
matu_c = matu_c %>% mutate(survey = "C")
matu_ext = matu_ext %>% mutate(survey = "Ext")

# データを縦向きに統合する
matu = rbind(matu_c, matu_ext)

# 種名を変更する
sp = data.frame(species = unique(matu$species))
sp$species2 = c("イガイ類", "マツモ", "テングサ類", "フクロフノリ", "イガイ類", "マツモ", "コンブ類", "ワカメ", "コンブ類", "コンブ類")
matu = matu %>% left_join(sp, by = "species") 
matu$species = NULL
matu = matu %>% rename(species = species2)

# プロットごと・年ごとにイワフジツボの生物量を集計する
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
f = facet_wrap(~ cate, scales = "free", ncol = 2)
labs = labs(x = "Year", y = "")
th = theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8))
g+p+l+f+labs+theme_bw()+th


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
unique(df2$species) # "テングサ類" "フクロフノリ" "マツモ" "イガイ類" "コンブ類" "ワカメ" 

# マツモ
matu = df2 %>% filter(species == "マツモ") %>% mutate(m_sst2_2 = (m_sst2)^2, sum2_2 = (sum2)^2)
summary(matu)
res = lm(freq ~ m_sst2 + m_sst2_2 + sum2 + sum2_2 , data = matu)
summary(res)


# フノリ
funo = df2 %>% filter(species == "フクロフノリ") %>% mutate(m_sst2_2 = (m_sst2)^2, sum2_2 = (sum2)^2)
res = lm(freq ~ m_sst2 + m_sst2_2 + sum2 + sum2_2 , data = funo)
summary(res)

# コンブ類
kel = df2 %>% filter(species == "コンブ類") %>% mutate(m_sst2_2 = (m_sst2)^2, sum2_2 = (sum2)^2)
res = lm(freq ~ m_sst2 + m_sst2_2 + sum2 + sum2_2 , data = kel)
summary(res)

# ワカメ
wak = df2 %>% filter(species == "ワカメ") %>% mutate(m_sst2_2 = (m_sst2)^2, sum2_2 = (sum2)^2)
res = lm(freq ~ m_sst2 + m_sst2_2 + sum2 + sum2_2 , data = wak)
summary(res)

# イガイ類
ten = df2 %>% filter(species == "テングサ類") %>% mutate(m_sst2_2 = (m_sst2)^2, sum2_2 = (sum2)^2)
res = lm(freq ~ m_sst2 + m_sst2_2 + sum2 + sum2_2 , data = ten)
summary(res)

# 