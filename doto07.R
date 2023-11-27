require(tidyverse)
require(readxl)
require(ggplot2)

dir_input = "/Users/Yuki/Dropbox/isodata/doto/C/07/"

list = list.files(dir_input)

df = NULL
for(i in 1:length(list)){
  sheet = excel_sheets(paste0(dir_input, list[i]))
  
  temp2 = NULL
  for(j in 1:length(sheet)){
    temp = read_excel(path = paste0(dir_input, list[i]),
                      range = "B4:K23",
                      sheet = sheet[j], 
                      col_names = c("c01", "c02", "c03", "c04", "c05", "c06", "c07", "c08", "c09", "c10"))
    temp = temp %>% 
      gather(key = "column", value = "species") %>% 
      mutate(row = rep(c("r01", "r02", "r03", "r04", "r05", "r06", "r07", "r08", "r09", "r10", "r11", "r12", "r13", "r14", "r15", "r16", "r17", "r18", "r19", "r20"), 10), plot = paste(sheet[j]), year = as.numeric(str_sub(list[i], 1, 4)), month = as.numeric(str_sub(list[i], 5, 6)))
    
    if(nrow(temp) != 200){
      warning("警告！")
    }else{
      temp2 = rbind(temp2, temp)
    }
  }
  df = rbind(df, temp2)
}

# データ数の確認
200*length(list)*length(sheet) - nrow(df) #110000

# 出現種の確認
unique(df$species)
df[df == "砂没"] <- NA
df[df == "NoData"] <- NA
df[df == "No Data"] <- NA
df[df == "nodata"] <- NA
df[df == "死イワフジツボ"] <- 0
df[df == "座フクロフノリ"] <- "フクロフノリ"
df[df == "座マツモ"] <- "マツモ"
df[df == "ハバノリ"] <- "セイヨウハバノリ属spp"
df[df == "ウズマキゴカイ→北海道ではスベカワウズマキゴカイ"] <- "スベカワウズマキゴカイ"
df$species = ifelse(df$species == "死イワフジツボ", 0, df$species)

head(df)
df = df %>% mutate(height = as.numeric(str_sub(row, 2, 3)))

df2 = df %>% group_by(year, plot, height, species) %>% count()


# cog ---------------------------------------------------------------------
df2$ko_temp = df2$height*df2$n
haha = df2 %>% group_by(year, plot, species) %>% summarize(haha = sum(n), na.rm = T)

head(df2); head(haha)

ko = df2 %>% group_by(year, plot, species) %>% summarize(ko = sum(ko_temp), na.rm = T)

df3 = left_join(ko, haha, by = c("year", "plot", "species"))
df3$cog = df3$ko/df3$haha

df4 = df3 %>% group_by(year, species) %>% summarize(mean_cog = mean(cog))

choice = c("イワフジツボ", "フクロフノリ", "ピリヒバ", "クロバギンナンソウ", "ネバリモ", "ツヤナシシオグサ", "マツモ", "アナアオサ", "ツヤナシシオグサ", "エンドウイトグサ")

g = ggplot(df4 %>% filter(species %in% choice), aes(x = year, y = mean_cog))
p = geom_point()
l = geom_line()
f = facet_wrap(~ species, ncol = 3, scale = "free")
g+p+l+f+theme_bw()+scale_y_reverse()




# 垂直分布 --------------------------------------------------------------
df5 = df2 %>% filter(species %in% choice) %>% select(-ko_temp) %>% group_by(year, species, height) %>% summarize(mean_n = mean(n))

g = ggplot(df5, aes(x = height, y = mean_n, group = year, color = year))
l = geom_line(size = 1)
f = facet_wrap(~ species, ncol = 3, scale = "free")
g+l+f+theme_bw()+scale_colour_gradientn(colours = c("blue", "cyan", "green", "yellow", "orange", "red", "darkred"))


time = data.frame(year = 2002:2025, time = rep(1:8, each = 3))
df6 = left_join(df2 %>% filter(species %in% choice) %>% select(-ko_temp), time, by = "year") %>% group_by(time, species, height) %>% summarize(mean_n =  mean(n))

g = ggplot(df6, aes(x = height, y = mean_n, group = time, color = time))
l = geom_line(size = 1)
f = facet_wrap(~ species, ncol = 3, scale = "free")
g+l+f+theme_bw()+scale_colour_gradientn(colours = c("blue", "cyan", "green", "yellow", "orange", "red", "darkred"))
