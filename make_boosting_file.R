require(tidyverse)



# directory ---------------------------------------------------------------
dir_iso = "/Users/Yuki/Dropbox/isodata/sanriku/07/"



# data --------------------------------------------------------------------
setwd(dir_iso)
sessile = read.csv("df_boosting_sanriku.csv", fileEncoding = "CP932")

mob_ext = read.csv("sanriku07C_Ext_mob.csv", fileEncoding = "CP932") %>% filter(count != 0)
mob = read.csv("sanriku07C_mob.csv", fileEncoding = "CP932") %>% filter(count != 0)

m_list1 = data.frame(species = unique(mob_ext$species))
m_list2 = data.frame(species = unique(mob$species))
m_list = rbind(m_list1, m_list2) %>% distinct()
# write.csv(m_list, "m_list.csv", fileEncoding = "CP932", row.names = FALSE)

m_list = read.csv("m_list.csv", fileEncoding = "CP932")


mob = rbind(mob, mob_ext)
mob = left_join(m_list, mob, by = "species")

m_mob = mob %>% mutate(plot = str_sub(plot, 1, 3)) %>% group_by(plot, year, diet) %>% summarise(sum = sum(as.numeric(count))) %>% mutate(dens = ifelse(year < 2011, sum/0.5, sum/1))

igai = sessile %>% filter(species == "イガイ類")
kaisou = sessile %>% filter(species != "イガイ類")

igai = left_join(igai, m_mob %>% filter(diet == "肉食性") %>% select(-diet, -sum), by = c("year", "plot"))
igai$dens[is.na(igai$dens)] <- 0

kaisou = left_join(kaisou, m_mob %>% filter(diet == "植食性") %>% select(-diet, -sum), by = c("year", "plot"))
kaisou$dens[is.na(kaisou$dens)] <- 0

sessile2 = rbind(igai, kaisou)
sessile2[is.na(sessile2)] = 0
summary(sessile2)

setwd(dir_iso)
write.csv(sessile2, "df_boosting_sanriku2.csv", fileEncoding = "CP932", row.names = FALSE)
