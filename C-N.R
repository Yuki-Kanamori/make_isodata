# packages ----------------------------------------------------------------
require(tidyverse)
require(ggplot2)


# directory -------------------------------------------------------------
dir = "/Users/Yuki/Dropbox/isodata/sanriku/07/"


# data --------------------------------------------------------------------
setwd(dir)
c = read.csv("sanriku07C.csv", fileEncoding = "CP932")
ext = read.csv("sanriku07C_Ext.csv", fileEncoding = "CP932")


# check -------------------------------------------------------------------
summary(c)
summary(ext)

head(c, 10)
head(ext)

unique(c$plot)
length(unique(c$plot))

unique(ext$plot)
length(unique(ext$plot))

unique(c$species)
unique(ext$species)


# example -----------------------------------------------------------------
# step1: マツモの生物量の時間変化を調べる
#

# 旧天然区と延長データからイワフジツボのデータを抽出する
# iwa_c = c %>% filter(species == "イワフジツボ")
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

# 2011年以降は調査範囲が広くなっているので，単純にイワフジツボが出現した点の数で比べることができない
# イワフジツボが出現した点の数を調査点の数で割ることにする
# 2010年まではMAX200点，2011年からはMAX400点
matu2 = matu2 %>% mutate(grid = ifelse(year < 2011, 200, 400))
matu2 = matu2 %>% mutate(freq = n/grid)

# 生物量の年平均値を求める
trend_matu = matu2 %>% group_by(year, species) %>% summarize(abundance = mean(freq))
head(trend_matu)

# 作図
g = ggplot(data = trend_matu, aes(x = year, y = abundance*100))
p = geom_point()
l = geom_line()
f = facet_wrap(~ species, scale = "free")
labs = labs(x = "Year", y = "Abundance (Coverage, %)", title = "")
fig = g+p+l+f+labs+theme_bw(base_family = "HiraKakuPro-W3")
setwd(dir)
ggsave(filename = "trend.png", plot = fig, units = "in", width = 11.69, height = 8.27)
