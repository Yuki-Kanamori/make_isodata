require(tidyverse)
require(readxl)
require(ggplot2)

dir_input = "/Users/Yuki/Library/CloudStorage/Dropbox/isodata/sanriku/07/C/"
dir_save = "/Users/Yuki/Dropbox/isodata/sanriku"

list = list.files(dir_input)

df = NULL
for(i in 1:length(list)){
  sheet = excel_sheets(paste0(dir_input, list[i]))
  
  temp2 = NULL
  for(j in 1:25){
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


# 延長 ----------------------------------------------------------------------
dir_input = "/Users/Yuki/Library/CloudStorage/Dropbox/isodata/sanriku/07/Ext/"
dir_save = "/Users/Yuki/Dropbox/isodata/sanriku"

list = list.files(dir_input)

df2 = NULL
for(i in 1:length(list)){
  sheet = excel_sheets(paste0(dir_input, list[i]))
  
  temp2 = NULL
  for(j in 1:25){
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
  df2 = rbind(df2, temp2)
}

sanriku = rbind(df, df2)
df = sanriku

# 出現種の確認
unique(df$species)
df[df == "NA"] = NA
df[df == "NotSurveyed"] = NA
df[df == "notada"] = NA
df[df == "nodata"] = NA
df[df == "NO DATA"] = NA
df[df == "no data"] = NA
df[df == "Nodata"] = NA
df[df == "NODATA"] = NA
df[df == "No Data"] = NA
df[df == "NoData"] = NA
df[df == "NO"] = NA
df[df == "?"] = NA

df[df == "スサビ"] = "スサビノリ"
df[df == "エゾカサネカンザシゴカイ"] = "エゾカサネカンザシ"
df[df == "ベニマダラ0"] = "ベニマダラ"
df[df == "ツヤナシ"] = "ツヤナシシオグサ"

head(df)
df2 = df %>% group_by(species) %>% count() %>% na.omit() %>% filter(species != 0) %>% arrange(-n)

setwd(dir_save)
write.csv(df2, "species_list.csv", fileEncoding = "CP932", row.names = FALSE)
