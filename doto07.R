require(tidyverse)
require(readxl)

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

head(df)
df = df %>% mutate(height = as.numeric(str_sub(row, 2, 3)))

df2 = df %>% group_by(year, plot, height, species) %>% count()


# cog ---------------------------------------------------------------------
df2$temp = 

