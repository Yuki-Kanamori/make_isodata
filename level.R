
# packages ----------------------------------------------------------------
require(tidyverse)
require(ggplot2)


# directory -------------------------------------------------------------
dir = "/Users/Yuki/Dropbox/isodata/sanriku/07/"


# data --------------------------------------------------------------------
c = read.csv("sanriku07C.csv", fileEncoding = "CP932")
ext = read.csv("sanriku07C_Ext.csv", fileEncoding = "CP932")


# check -------------------------------------------------------------------
summary(c)
summary(ext)

head(c)
head(ext)

unique(c$plot)
length(unique(c$plot))

unique(c$species)
unique(ext$species)


# example -----------------------------------------------------------------
# 山田町（明神MJと荒神AG）におけるイワフジツボの生物量の時間変化を調べる
#

# 旧天然区と延長データからイワフジツボのデータを抽出する
iwa_c = c %>% filter(species == "イワフジツボ")
iwa_ext = ext %>% filter(species == "イワフジツボ")

# surveyという列を新しく作成し，旧天然区ならC・延長ならExtと入れる
iwa_c = iwa_c %>% mutate(survey = "C")
iwa_ext = iwa_ext %>% mutate(survey = "Ext")

# データを縦向きに統合する
iwa = rbind(iwa_c, iwa_ext)

# プロットごと・年ごとにイワフジツボの生物量を集計する
iwa2 = iwa %>% group_by(year, plot) %>% count()

# 
# 2011年以降は調査範囲が広いので，
iwa2 = iwa2 %>% mutate(grid = ifelse(year < 2011, 200, 400))
iwa2 = iwa2 %>% mutate(freq = n/grid)

# shore（海岸）という列を新しく作成し，MJとAGのデータを抽出する
iwa2 = iwa2 %>% mutate(shore = str_sub(plot, 1, 2))
iwa_y = iwa2 %>% filter(shore == "MJ" | shore == "AG")
unique(iwa_y$shore) # 明神と荒神のデータが抽出できているかを確認

# 生物量の年平均値を求める
trend_iwa = iwa_y %>% group_by(year) %>% summarize(abundance = mean(freq))

# 作図
g = ggplot(data = trend_iwa, aes(x = year, y = abundance))
p = geom_point()
l = geom_line()
labs = labs(x = "Year", y = "Abundance", title = "Iwafujitsubo")
g+p+l+labs+theme_bw()


# example -----------------------------------------------------------------
# 山田町（明神MJと荒神AG）におけるイワフジツボの生物量の水準を調べる
#

# 生物量の最小値と最大値を求める
min = min(trend_iwa$abundance)
(max = max(trend_iwa$abundance))

# 低位・中位・高位の基準値を求める
(low = min+(max-min)*1/3)
(high = min+(max-min)*2/3)
tail(trend_iwa)

# 作図
# 低位・中位・高位の補助線を入れる
g = ggplot(data = trend_iwa, aes(x = year, y = abundance))
p = geom_point()
l = geom_line()
labs = labs(x = "Year", y = "Abundance", title = "Iwafujitsubo")
g+p+l+geom_hline(yintercept = c(low, high), linetype = "dashed", color = "red")+labs+theme_bw()

# 図の保存
fig = g+p+l+geom_hline(yintercept = c(low, high), linetype = "dashed", color = "red")+labs+theme_bw()
setwd(dir)
ggsave(filename = "trend_iwa.pdf", plot = fig, units = "in", width = 11.69, height = 8.27)
