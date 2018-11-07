library(maps)
library(dplyr)
setwd("~/PycharmProjects/EDAV/chsi_dataset")

data(county.fips)
df = read.csv('./RELATIVEHEALTHIMPORTANCE.csv')
df2 = read.csv('./RISKFACTORSANDACCESSTOCARE.csv')
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