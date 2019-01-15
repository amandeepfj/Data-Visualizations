setwd("C:/US Drive/MSBA/Data Visualization/Group Project/")
library(readxl)
sheet1 <- data.table(read_excel(path = "DataDataData.xlsx", sheet = 1))
sheet2 <- data.table(read_excel(path = "DataDataData.xlsx", sheet = 2))
sheet3 <- data.table(read_excel(path = "DataDataData.xlsx", sheet = 3))
detach("package:readxl", unload=TRUE)

# Code that creates country_names table ----------------------
library(data.table)
library(dplyr)

unique_names <- sheet1[, .N, by = (variable)][N == 1917]

country_names <- data.table(1:nrow(unique_names), unique_names[, variable]) 
setnames(country_names, old = c("V1", "V2"), new = c("DVIZ_Country_Code", "Country_Name"))

duplicatedName <- sheet1[, .N, by = (variable)][N != 1917]

dot_not_removed <- duplicatedName[, variable]
next_country_code <- nrow(unique_names)+1
country_codes <- c()
seq.to.consider <- seq(from = 1, to = length(dot_not_removed), by =2)
for (i in seq.to.consider){
  country_codes <<- c(country_codes, next_country_code)
  country_codes <<- c(country_codes, next_country_code)
  next_country_code <<- next_country_code + 1
}

dt.dup.country.names <- data.table(country_codes, dot_not_removed)
setnames(dt.dup.country.names, old = c("country_codes", "dot_not_removed"), 
         new = c("DVIZ_Country_Code", "Country_Name"))

country_names <- bind_rows(country_names, dt.dup.country.names)

add_country <- function(country_name, country_code = -1){
  if(country_code == -1){
    country_names <<- rbind(country_names, list(next_country_code, country_name))
    next_country_code <<- next_country_code + 1
  } else{
    country_names <<- rbind(country_names, list(country_code, country_name))
  }
}

# End of country name code . Use add_country_with_code to add new country

merge(sheet1, country_names, by.x = "variable", by.y = "Country_Name")[, .N, by = (DVIZ_Country_Code)][N != 1917]

merge(sheet2, country_names, by.x = "Country", by.y = "Country_Name", all.x = TRUE)[is.na(DVIZ_Country_Code)]

add_country("Bolivia", 163)
add_country("Bosnia", 164)
add_country("Brunei", 166)
add_country("CapeVerde", 168)
add_country("CuraÃ§ao", 178)
add_country("DemocraticRepublicOfTheCongo", 180)
add_country("FaroeIslands", 184)
add_country("Guinea-Bissau", 188)
add_country("Iran", 190)
add_country("Laos", 192)
add_country("Micronesia", 194)
add_country("NorthKorea", 179)
add_country("SouthKorea", 200)
add_country("SouthKorea", 200)
add_country("USVirginIsland", 228)
add_country("esternSahara", 232)
add_country("Syria", 219)
add_country("Moldova", 201)
add_country("SaintHelena,AscensionandTristandaCunha", 204)
add_country("Russia", 203)
add_country("US", 227)
add_country("Vietnam", 229)
add_country("WallisandFutuna", 231)
add_country("Venezuela", 229)
add_country("Turks&CaicosIslands", 223)
add_country("Timor-Leste", 221)
add_country("HongKong", 173)
add_country("Tanzania", 226)
add_country("Palestine", 218)
add_country("Macedonia", 220)
add_country("SãoToméandPríncipe", 210)
add_country("Taiwan")

add_country("CzechRepublic")
add_country("Guernsey")
add_country("IvoryCoast")
add_country("Jersey")
add_country("Kosovo")
add_country("Macau")
add_country("SaintBarthélemy")

add_country("SaintMartin", 213)
add_country("SintMaarten", 213)
add_country("Rest of the World")
add_country("orld", country_names[Country_Name == "Rest of the World", DVIZ_Country_Code])


#country_names[Country_Name %like% "Rest", ]
temp <- merge(sheet1, country_names, by.x = "variable", by.y = "Country_Name", all.x = TRUE)

fwrite(country_names, "country_names.csv")
fwrite(temp, "immigration_data.csv")


