require(tidyverse)
require(stringr)


# download the MGDSST data ------------------------------------------------
#####1982-2017#####
#最新版とそれ以前のデータでは，ファイル名が異なる


###古いデータ###
#カレンダーの作成
time = seq(as.Date("2023-01-01"), as.Date("2024-12-31"), by="days")


time2 = str_c(str_sub(time, 1, 4), str_sub(time, 6, 7), str_sub(time, 9, 10))
time2[1]


for(i in 1:length(time2)){
  url = paste("https://www.data.jma.go.jp/gmd/goos/data/pub/JMA-product/mgd_sst_glb_D/", substr(time2[i], 1, 4), "/re_mgd_sst_glb_D", time2[i], ".txt.gz", sep = "")
  file = paste("/Users/Yuki/Library/CloudStorage/Dropbox/SST/all/mgd_sst_glb_D", time2[i], ".txt.gz", sep = "")
  download.file(url, file)
}



###最新のデータ###
#カレンダーの作成
time = seq(as.Date("2026-01-01"), as.Date("2026-10-22"), by="days")


time2 = str_c(str_sub(time, 1, 4), str_sub(time, 6, 7), str_sub(time, 9, 10))
time2[1]


for(i in 1:length(time2)){
  url = paste("https://www.data.jma.go.jp/gmd/goos/data/pub/JMA-product/mgd_sst_glb_D/", substr(time2[i], 1, 4), "/mgd_sst_glb_D", time2[i], ".txt.gz", sep = "")
  file = paste("/Users/Yuki/Library/CloudStorage/Dropbox/SST/all/mgd_sst_glb_D", time2[i], ".txt.gz", sep = "")
  download.file(url, file)
}




# .gzデータの解凍 ---------------------------------------------------------------
# ターミナルで
Yuki@YukinoMacBook-Pro MGDSST % cd /Users/Yuki/Dropbox/SST/all
# Yuki@YukinoMacBook-Pro all % find ./ -type f -name "*.gz" -exec gunzip {} \;
Yuki@YukinoMacBook-Pro all % find ./ -type f -name "*.gz" -exec gunzip -f {} \;




# make dataframe ----------------------------------------------------------
setwd("/Users/Yuki/Dropbox/SST/all")
dir = "/Users/Yuki/Dropbox/SST/all"
widths = rep(3, each = 1440)


fl = list.files(path = dir, full.names = T, pattern = "txt") #読み込むファイル名のリスト化
length(fl)


length(fl)
fl[1]


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


#####データフレームの作成#####
fl[3652]
df_8291 = c()
for(i in 1:3652){
  df = read_fwf(file = fl[i], fwf_widths(col_names = T, widths = widths), skip = 1)
  df2 = df[min(lat2$tag):max(lat2$tag), min(lon2$tag):max(lon2$tag)]
  colnames(df2) = lon2$lon
  df2$lat = rep(lat2$lat)
  df2$ymd = rep(str_sub(fl[i], (nchar(fl[i])-11), (nchar(fl[i])-4)), each = nrow(df2))
  
  df_8291 = rbind(df_8291, df2)
}
all = df_8291 %>% tidyr::gather(key = lon, value = SST, -c(ymd, lat))
all = all %>% dplyr::mutate(year = str_sub(all$ymd, 1, 4), month = str_sub(all$ymd, 5, 6), day = str_sub(all$ymd, 7, 8), lon = as.numeric(str_sub(all$lon, 1, 10)), SST = SST/10) %>% dplyr::filter(SST != 99.9)
write.csv(all, "sst_1982-1991.csv", fileEncoding = "CP932", row.names = FALSE)


fl[3652*2+1]
df_9201 = c()
for(i in 3653:7305){
  df = read_fwf(file = fl[i], fwf_widths(col_names = T, widths = widths), skip = 1)
  df2 = df[min(lat2$tag):max(lat2$tag), min(lon2$tag):max(lon2$tag)]
  colnames(df2) = lon2$lon
  df2$lat = rep(lat2$lat)
  df2$ymd = rep(str_sub(fl[i], (nchar(fl[i])-11), (nchar(fl[i])-4)), each = nrow(df2))
  
  df_9201 = rbind(df_9201, df2)
}
all = df_9201 %>% tidyr::gather(key = lon, value = SST, -c(ymd, lat))
all = all %>% dplyr::mutate(year = str_sub(all$ymd, 1, 4), month = str_sub(all$ymd, 5, 6), day = str_sub(all$ymd, 7, 8), lon = as.numeric(str_sub(all$lon, 1, 10)), SST = SST/10) %>% dplyr::filter(SST != 99.9)
write.csv(all, "sst_1992-2001.csv", fileEncoding = "CP932", row.names = FALSE)

fl[3652*3+1]
df_0211 = c()
for(i in 7305:10957){
  df = read_fwf(file = fl[i], fwf_widths(col_names = T, widths = widths), skip = 1)
  df2 = df[min(lat2$tag):max(lat2$tag), min(lon2$tag):max(lon2$tag)]
  colnames(df2) = lon2$lon
  df2$lat = rep(lat2$lat)
  df2$ymd = rep(str_sub(fl[i], (nchar(fl[i])-11), (nchar(fl[i])-4)), each = nrow(df2))
  
  df_0211 = rbind(df_0211, df2)
}
all = df_0211 %>% tidyr::gather(key = lon, value = SST, -c(ymd, lat))
all = all %>% dplyr::mutate(year = str_sub(all$ymd, 1, 4), month = str_sub(all$ymd, 5, 6), day = str_sub(all$ymd, 7, 8), lon = as.numeric(str_sub(all$lon, 1, 10)), SST = SST/10) %>% dplyr::filter(SST != 99.9)
write.csv(all, "sst_2002-2011.csv", fileEncoding = "CP932", row.names = FALSE)

fl[3652*4+2]
df_1221 = c()
for(i in 10958:14610){
  df = read_fwf(file = fl[i], fwf_widths(col_names = T, widths = widths), skip = 1)
  df2 = df[min(lat2$tag):max(lat2$tag), min(lon2$tag):max(lon2$tag)]
  colnames(df2) = lon2$lon
  df2$lat = rep(lat2$lat)
  df2$ymd = rep(str_sub(fl[i], (nchar(fl[i])-11), (nchar(fl[i])-4)), each = nrow(df2))
  
  df_1221 = rbind(df_1221, df2)
}
all = df_1221 %>% tidyr::gather(key = lon, value = SST, -c(ymd, lat))
all = all %>% dplyr::mutate(year = str_sub(all$ymd, 1, 4), month = str_sub(all$ymd, 5, 6), day = str_sub(all$ymd, 7, 8), lon = as.numeric(str_sub(all$lon, 1, 10)), SST = SST/10) %>% dplyr::filter(SST != 99.9)
write.csv(all, "sst_2012-2021.csv", fileEncoding = "CP932", row.names = FALSE)

df_2224 = c()
for(i in 14610:length(fl)){
  df = read_fwf(file = fl[i], fwf_widths(col_names = T, widths = widths), skip = 1)
  df2 = df[min(lat2$tag):max(lat2$tag), min(lon2$tag):max(lon2$tag)]
  colnames(df2) = lon2$lon
  df2$lat = rep(lat2$lat)
  df2$ymd = rep(str_sub(fl[i], (nchar(fl[i])-11), (nchar(fl[i])-4)), each = nrow(df2))
  
  df_2224 = rbind(df_2224, df2)
}
all = df_2224 %>% tidyr::gather(key = lon, value = SST, -c(ymd, lat))
all = all %>% dplyr::mutate(year = str_sub(all$ymd, 1, 4), month = str_sub(all$ymd, 5, 6), day = str_sub(all$ymd, 7, 8), lon = as.numeric(str_sub(all$lon, 1, 10)), SST = SST/10) %>% dplyr::filter(SST != 99.9)
write.csv(all, "sst_2022-2024.csv", fileEncoding = "CP932", row.names = FALSE)

fl[14610+365*3]
df_25 = c()
for(i in 15706:length(fl)){
  df = read_fwf(file = fl[i], fwf_widths(col_names = T, widths = widths), skip = 1)
  df2 = df[min(lat2$tag):max(lat2$tag), min(lon2$tag):max(lon2$tag)]
  colnames(df2) = lon2$lon
  df2$lat = rep(lat2$lat)
  df2$ymd = rep(str_sub(fl[i], (nchar(fl[i])-11), (nchar(fl[i])-4)), each = nrow(df2))
  
  df_25 = rbind(df_25, df2)
}
all = df_25 %>% tidyr::gather(key = lon, value = SST, -c(ymd, lat))
all = all %>% dplyr::mutate(year = str_sub(all$ymd, 1, 4), month = str_sub(all$ymd, 5, 6), day = str_sub(all$ymd, 7, 8), lon = as.numeric(str_sub(all$lon, 1, 10)), SST = SST/10) %>% dplyr::filter(SST != 99.9)
write.csv(all, "sst_2025.csv", fileEncoding = "CP932", row.names = FALSE)


fl[14611+365*4]
df_26 = c()
for(i in 16072:length(fl)){
  df = read_fwf(file = fl[i], fwf_widths(col_names = T, widths = widths), skip = 1)
  df2 = df[min(lat2$tag):max(lat2$tag), min(lon2$tag):max(lon2$tag)]
  colnames(df2) = lon2$lon
  df2$lat = rep(lat2$lat)
  df2$ymd = rep(str_sub(fl[i], (nchar(fl[i])-11), (nchar(fl[i])-4)), each = nrow(df2))
  
  df_26 = rbind(df_26, df2)
}
all = df_26 %>% tidyr::gather(key = lon, value = SST, -c(ymd, lat))
all = all %>% dplyr::mutate(year = str_sub(all$ymd, 1, 4), month = str_sub(all$ymd, 5, 6), day = str_sub(all$ymd, 7, 8), lon = as.numeric(str_sub(all$lon, 1, 10)), SST = SST/10) %>% dplyr::filter(SST != 99.9)
write.csv(all, "sst_2026.csv", fileEncoding = "CP932", row.names = FALSE)






df_all = c()
for(i in 1:length(fl)){
  # df = readfiles(fl[i])
  df = read_fwf(file = fl[i], fwf_widths(col_names = T, widths = widths), skip = 1)
  df2 = df[min(lat2$tag):max(lat2$tag), min(lon2$tag):max(lon2$tag)]
  colnames(df2) = lon2$lon
  df2$lat = rep(lat2$lat)
  df2$ymd = rep(str_sub(fl[i], (nchar(fl[i])-11), (nchar(fl[i])-4)), each = nrow(df2))
  
  df_all = rbind(df_all, df2)
}
# write.csv(df_all, "df_all.csv")
# 
# df_all = read.csv("df_all.csv")
# head(df_all, 5)




summary(df_all)
all = df_all %>% tidyr::gather(key = lon, value = SST, -c(ymd, lat))
summary(all)
all = all %>% dplyr::mutate(year = str_sub(all$ymd, 1, 4), month = str_sub(all$ymd, 5, 6), day = str_sub(all$ymd, 7, 8), lon = as.numeric(str_sub(all$lon, 1, 10)), SST = SST/10) %>% dplyr::filter(SST != 99.9)
summary(all)
head(all, 5)
write.csv(all, "sst_1982-2023.csv", fileEncoding = "CP932")
