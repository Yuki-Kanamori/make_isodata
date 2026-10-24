require(tidyverse)
require(stringr)
require(fuzzyjoin)

# make dataframe ----------------------------------------------------------
setwd("/Users/Yuki/Dropbox/SST/all")
dir = "/Users/Yuki/Dropbox/SST/all"
widths = rep(3, each = 1440)


fl = list.files(path = dir, full.names = T, pattern = "txt") #読み込むファイル名のリスト化
length(fl)


length(fl)
fl[1]
fl[length(fl)]

#####データの緯度経度を指定する#####
#MGDSSTのlat
lat = data.frame(seq(89.875, -89.875, -0.25))
colnames(lat) = "lat"
lat$tag = rep(1:720)
#欲しい緯度範囲
lat2 = lat[lat$lat >= 24.75 & lat$lat <= 48.75, ]


#MGDSSTのlon
lon = data.frame(seq(359.875, 0, -0.25))
colnames(lon) = "lon"
lon$tag = rep(1:1440)
#欲しい経度範囲
lon2 = lon[lon$lon >= 122.25 & lon$lon <= 150.25, ] # 小さくした


# ディレクトリの作成 ---------------------------------------------------------------
# for(i in 1982:2024){
#   setwd(dir = "/Users/Yuki/Dropbox/SST/all/japan/")
#   dir.create(paste(i))
# }



# データフレームの作成 --------------------------------------------------------------
setwd("/Users/Yuki/Dropbox/SST/all")
for(i in 1:1){
  df = read_fwf(file = fl[i], fwf_widths(col_names = T, widths = widths), skip = 1)
  df2 = df[min(lat2$tag):max(lat2$tag), min(lon2$tag):max(lon2$tag)]
  colnames(df2) = lon2$lon
  df2$lat = rep(lat2$lat)
  df2$ymd = rep(str_sub(fl[i], (nchar(fl[i])-11), (nchar(fl[i])-4)), each = nrow(df2))
  
  year = str_sub(fl[i], -12, -9)
  # dir_write = paste("/Users/Yuki/Dropbox/SST/all/japan/", year, "/", sep = "", collapse ="")
  # setwd(dir_write)
  # write.csv(df2, paste0(str_sub(fl[i], -25, -5), "_japan.csv"), row.names = FALSE)
}

df3 = df2 %>% tidyr::gather(key = lon, value = SST, -c(ymd, lat))
sst_lonlat = df3 %>% select(lon, lat) %>% mutate(lon = as.numeric(lon))

setwd("/Users/Yuki/MGDSST/MHWs")
plot = read.csv("sanriku_lonlat.csv")
# plot = plot[c(1, 6, 11, 16, 21), ]
summary(plot)
summary(sst_lonlat)

near = NULL
for(i in 1:nrow(plot)){
  temp_plot = plot[i, ]
  pairs = temp_plot %>%
    geo_left_join(sst_lonlat, max_dist = 50, unit = "km", distance_col = "distance") %>% filter(distance == min(distance))
  near = rbind(near, pairs)
}
# write.csv(near, "near.csv", row.names = FALSE)

mj = near %>% filter(plot == "MJ1") %>% mutate(lon = lon.y, lat = lat.y) %>% select(lon, lat)
or = near %>% filter(plot == "OR1") %>% mutate(lon = lon.y, lat = lat.y) %>% select(lon, lat)
ag = near %>% filter(plot == "AG1") %>% mutate(lon = lon.y, lat = lat.y) %>% select(lon, lat)
ak = near %>% filter(plot == "AK1") %>% mutate(lon = lon.y, lat = lat.y) %>% select(lon, lat)
kg = near %>% filter(plot == "KG1") %>% mutate(lon = lon.y, lat = lat.y) %>% select(lon, lat)


# sstデータ ------------------------------------------------------------------
dir_sst = "/Users/Yuki/Library/CloudStorage/Dropbox/SST/all/"
setwd(dir = dir_sst)

sst1 = read_csv("sst_1982-1991.csv")
mj1 = sst1 %>% filter(lon == mj$lon, lat == mj$lat)
or1 = sst1 %>% filter(lon == or$lon, lat == or$lat)
ag1 = sst1 %>% filter(lon == ag$lon, lat == ag$lat)
ak1 = sst1 %>% filter(lon == ak$lon, lat == ak$lat)
kg1 = sst1 %>% filter(lon == kg$lon, lat == kg$lat)

sst2 = read_csv("sst_1992-2001.csv")
mj2 = sst2 %>% filter(lon == mj$lon, lat == mj$lat)
or2 = sst2 %>% filter(lon == or$lon, lat == or$lat)
ag2 = sst2 %>% filter(lon == ag$lon, lat == ag$lat)
ak2 = sst2 %>% filter(lon == ak$lon, lat == ak$lat)
kg2 = sst2 %>% filter(lon == kg$lon, lat == kg$lat)

sst3 = read_csv("sst_2002-2011.csv")
mj3 = sst3 %>% filter(lon == mj$lon, lat == mj$lat)
or3 = sst3 %>% filter(lon == or$lon, lat == or$lat)
ag3 = sst3 %>% filter(lon == ag$lon, lat == ag$lat)
ak3 = sst3 %>% filter(lon == ak$lon, lat == ak$lat)
kg3 = sst3 %>% filter(lon == kg$lon, lat == kg$lat)

sst4 = read_csv("sst_2012-2021.csv")
mj4 = sst4 %>% filter(lon == mj$lon, lat == mj$lat)
or4 = sst4 %>% filter(lon == or$lon, lat == or$lat)
ag4 = sst4 %>% filter(lon == ag$lon, lat == ag$lat)
ak4 = sst4 %>% filter(lon == ak$lon, lat == ak$lat)
kg4 = sst4 %>% filter(lon == kg$lon, lat == kg$lat)

sst5 = read_csv("sst_2022-2024.csv")
mj5 = sst5 %>% filter(lon == mj$lon, lat == mj$lat)
or5 = sst5 %>% filter(lon == or$lon, lat == or$lat)
ag5 = sst5 %>% filter(lon == ag$lon, lat == ag$lat)
ak5 = sst5 %>% filter(lon == ak$lon, lat == ak$lat)
kg5 = sst5 %>% filter(lon == kg$lon, lat == kg$lat)

sst6 = read_csv("sst_2025.csv")
mj6 = sst6 %>% filter(lon == mj$lon, lat == mj$lat)
or6 = sst6 %>% filter(lon == or$lon, lat == or$lat)
ag6 = sst6 %>% filter(lon == ag$lon, lat == ag$lat)
ak6 = sst6 %>% filter(lon == ak$lon, lat == ak$lat)
kg6 = sst6 %>% filter(lon == kg$lon, lat == kg$lat)

sst7 = read_csv("sst_2026.csv")
mj7 = sst7 %>% filter(lon == mj$lon, lat == mj$lat)
or7 = sst7 %>% filter(lon == or$lon, lat == or$lat)
ag7 = sst7 %>% filter(lon == ag$lon, lat == ag$lat)
ak7 = sst7 %>% filter(lon == ak$lon, lat == ak$lat)
kg7 = sst7 %>% filter(lon == kg$lon, lat == kg$lat)


sst_MJ = rbind(mj1, mj2, mj3, mj4, mj5, mj6, mj7)
summary(sst_MJ)
sst_OR = rbind(or1, or2, or3, or4, or5, or6, or7)
summary(sst_OR)
sst_AG = rbind(ag1, ag2, ag3, ag4, ag5, ag6, ag7)
summary(sst_AG)
sst_AK = rbind(ak1, ak2, ak3, ak4, ak5, ak6, ak7)
summary(sst_AK)
sst_KG = rbind(kg1, kg2, kg3, kg4, kg5, kg6, kg7)
summary(sst_KG)


write.csv(sst_MJ, "sst_MJ.csv", row.names = FALSE)
write.csv(sst_OR, "sst_OR.csv", row.names = FALSE)
write.csv(sst_AG, "sst_AG.csv", row.names = FALSE)
write.csv(sst_AK, "sst_AK.csv", row.names = FALSE)
write.csv(sst_KG, "sst_KG.csv", row.names = FALSE)
