require(tidyverse)
require(readxl)
require(ggplot2)

dir_input = "/Users/Yuki/Dropbox/isodata/doto/C/07/"
dir_save = "/Users/Yuki/Dropbox/isodata/doto/distribution"

list = list.files(dir_input)

df = NULL
for(i in 1:length(list)){
  sheet = excel_sheets(paste0(dir_input, list[i]))
  
  temp2 = NULL
  for(j in 1:length(sheet)){
    temp = read_excel(path = paste0(dir_input, list[i]),
                      range = "AE103:AP181",
                      sheet = sheet[j], 
                      col_names = c("species", "species_code", "r01", "r02", "r03", "r04", "r05", "r06", "r07", "r08", "r09", "r10"))
    temp = temp %>% select(-species_code) %>% gather(key = "row", value = "count", -species) %>% filter(count >= 0) %>% mutate(plot = paste(sheet[j]), year = as.numeric(str_sub(list[i], 1, 4)), month = as.numeric(str_sub(list[i], 5, 6)))
    
    temp2 = rbind(temp2, temp)
  }
  df = rbind(df, temp2)
}

head(df)
df = df %>% mutate(height = as.numeric(str_sub(row, 2, 3)))

setwd(dir_save)
save(df, file = "mobile07.Rdata")

df$n_count = as.numeric(df$count)
df2 = df %>% group_by(year, plot, height, species) %>% summarize(n = sum(n_count)) %>% arrange(-n) %>% filter(n > 0)

# cog ---------------------------------------------------------------------
df2$ko_temp = df2$height*df2$n
haha = df2 %>% group_by(year, plot, species) %>% summarize(haha = sum(n), na.rm = T)

head(df2); head(haha)

ko = df2 %>% group_by(year, plot, species) %>% summarize(ko = sum(ko_temp), na.rm = T)

df3 = left_join(ko, haha, by = c("year", "plot", "species"))
df3$cog = df3$ko/df3$haha

df4 = df3 %>% group_by(year, species) %>% summarize(mean_cog = mean(cog))
unique(df4$species)

choice = c("クロタマキビ", "サラサシロガイ", "タマキビ", "シロガイ", "チヂミボラ", "ウチダヘソタマキビ")

g = ggplot(df4 %>% filter(species %in% choice), aes(x = year, y = mean_cog))
p = geom_point()
l = geom_line()
f = facet_wrap(~ species, ncol = 3, scale = "free")
fig = g+p+l+f+theme_bw()+scale_y_reverse()

setwd(dir_save)
ggsave(file = "cog_mob.png", plot = fig, units = "in", width = 11.69, height = 8.27)
