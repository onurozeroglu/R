#Cluster

library(eurostat) 
library(ggplot2) 
library(dplyr) 
# Get data 
df_phicp <- eurostat::get_eurostat("prc_hicp_manr", time_format = "date")

Countries <- c("AT" = "Austria", "BE" = "Belgium", "BG" = "Bulgaria",
                      "CH" = "Switzerland", "CY" = "Cyprus", "CZ" = "Czech Republic", 
                      "DE" = "Germany", "DK" = "Denmark", "EA" = "Euro area", 
                      "EE" = "Estonia", "EL" = "Greece", "ES" = "Spain", 
                      "FI" = "Finland", "FR" = "France", "HR" = "Croatia", 
                      "HU" = "Hungary", "IE" = "Ireland", "IS" = "Iceland", 
                      "IT" = "Italy", "LI" = "Liechtenstein", "LT" = "Lithuania", 
                      "LU" = "Luxembourg", "LV" = "Latvia", "ME" = "Montenegro", 
                      "MK" = "North Macedonia", "MT" = "Malta", "NL" = "Netherlands", 
                      "NO" = "Norway", "PL" = "Poland", "PT" = "Portugal", 
                      "RO" = "Romania", "RS" = "Serbia", "SE" = "Sweden", 
                      "SI" = "Slovenia", "SK" = "Slovakia", "UK" = "United Kingdom") 

df_phicp$geo <- Countries[df_phicp$geo] 
df_phicp <- na.omit(df_phicp)                                 


df_phicp<-df_phicp %>% filter(df_phicp$time > '2000-02-01')
df_phicp<-df_phicp %>% filter(df_phicp$time < '2022-09-01')
df_phicp <- df_phicp[df_phicp$coicop == "CP00", ] 

ggplot(data = df_phicp , aes(x = time, y = values, color = geo)) + geom_line() +  labs(title = "HICP data over time", x = "Year", y = "HICP value") +  
  theme(legend.position = "bottom") 



df_phicp <- df_phicp %>% group_by(geo) %>% summarise(change = values[2] - values[1]) 


cs_minkowski = hclust(dist(df_phicp[ , 2],p=1.5,method = "minkowski"))
plot(cs_minkowski,  labels = df_phicp$geo, main = "Original Tree")
rect.hclust(cs_minkowski,4)
