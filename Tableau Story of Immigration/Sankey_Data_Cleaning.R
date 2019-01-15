setwd("C:/US Drive/MSBA/Data Visualization/Group Project/")

library(data.table)
immigrationData <- fread("Year_Wise_Immigration_Data.csv")
valid.Immi.Rows <- immigrationData[(value != "") & (!is.na(value)), ]
valid.Immi.Rows[, Year := ï..Year]
valid.Immi.Rows[, ï..Year := NULL]
sort(unique(valid.Immi.Rows[Destination %like% "dev", Destination]))
sort(unique(valid.Immi.Rows[Destination %like% "inc", Destination]))

sort(unique(valid.Immi.Rows[variable %like% "raq", variable]))

unique(valid.Immi.Rows[!(Destination %in% unique(valid.Immi.Rows[, variable]))][,Destination])

tt <- valid.Immi.Rows[, .N, by = .(variable, Year)][order(N)]

sd(tt[variable == "United.States.of.America", N])

originCountries <- c("Syrian.Arab.Republic", "Mexico", "China", "India", "Iraq")
destinationContinents <- c("AFRICA", "ASIA", "EUROPE", "LATIN AMERICA AND THE CARIBBEAN", "NORTHERN AMERICA", "OCEANIA")
destinationIncomeBased <- c("High-income countries", 
                            "Middle-income countries", "Low-income countries")
destinationDevlopmentBased <- c("More developed regions", 
                        "Less developed regions, excluding least developed countries", "Least developed countries")

sankey_chart_data <- valid.Immi.Rows[(variable %in% originCountries) &
                  (Destination %in% c(destinationContinents, destinationIncomeBased, destinationDevlopmentBased)), .(Origin = variable, Destination, Year,
                                                                                     Migrations = value)]

sankey_chart_data[, VizSide := "POO"]
POO <- data.table(sankey_chart_data)
sankey_chart_data[, VizSide := "POD"]
POD <- data.table(sankey_chart_data)
sankey_chart_data_dups <- merge(POO, POD, by = colnames(sankey_chart_data), all = TRUE)

library(xlsx)
write.xlsx(sankey_chart_data_dups, "sankey_chart_data.xlsx")

