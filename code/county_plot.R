library(maps)
library(dplyr)
library(ggplot2)
library(tibble)
library(tidyverse)
setwd("~/PycharmProjects/EDAV/chsi_dataset")

data(county.fips)
df = read.csv('./RELATIVEHEALTHIMPORTANCE.csv')
df2 = read.csv('./RISKFACTORSANDACCESSTOCARE.csv')
df2 = df2[df2$CHSI_State_Name!="Alaska",]
df2[df2<0] = 0

#Selecting variables
no_ex = aggregate(df2[, 7], list(df2$CHSI_State_Name), mean)
fat = aggregate(df2[, 13], list(df2$CHSI_State_Name), mean)
theme_dotplot <- theme_bw(18) +
  theme(axis.text.y = element_text(size = rel(.75)),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(size = rel(.75)),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = 0.5),
        panel.grid.minor.x = element_blank())
ggplot() + geom_point(data=fat,
                      aes(x = x,
                          y = fct_reorder(Group.1, x))) +
  ylab("") + xlab("FAT INDEX") + theme_dotplot

#Same for other variables

#Draw the map

ct = read.csv('./zcta_county_rel_10.txt', colClasses=c("character",rep("numeric",23)))

toFIPS = function(state, county) {
  state = sprintf("%02d", state)
  county = sprintf("%03d", county)
  return(as.numeric(paste0(state,county)))
}

toZIP = function(state, county, ct) {
  if (length(which(ct$STATE == state && ct$COUNTY == county)) == 0) {
    return("-1")
  }
  return(ct[which(ct$STATE == state && ct$COUNTY == county), 'ZCTA5'])
}
list = c("RHI_Infant_Mortality_Ind", "RHI_Brst_Cancer_Ind", "RHI_Col_Cancer_Ind", "RHI_CHD_Ind")

print(toZIP(1, 1, ct))
plot_df = data.frame(region = vector(length = nrow(df)), value = vector(length = nrow(df)))
for (i in 1:nrow(df)) {
  plot_df[i, "region"] = toFIPS(df[i, "State_FIPS_Code"], df[i, "County_FIPS_Code"])
  plot_df[i, "value"] = gray(abs(df[i, "RHI_Brst_Cancer_Ind"] /8)) 
}
counties <- county.fips %>% left_join(plot_df, by=c('fips'='region'))
map("county", fill=TRUE, col=counties$value)