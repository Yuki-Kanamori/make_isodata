require(tidyverse)
require(readxl)
require(ggplot2)

dir_input = "/Users/Yuki/Dropbox/isodata/doto/C/04/"
dir_save = "/Users/Yuki/Dropbox/isodata/doto/distribution"

list = list.files(dir_input)

df = NULL
for(i in 16:length(list)){
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

# setwd(dir_save)
# save(df, file = "mobile.Rdata")

df$n_count = as.numeric(df$count)
df2 = df %>% na.omit() %>% group_by(species) %>% summarize(n = sum(n_count)) %>% arrange(-n) %>% filter(n > 0)

setwd(dir = dir_save)
write.csv(df2, "species_list_mob.csv", fileEncoding = "CP932", row.names = FALSE)
