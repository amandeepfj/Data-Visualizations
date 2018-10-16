#setwd("C:/US Amazon Drive/MSBA/Data Visualization/Assignment 1")
#dataSet <- read.csv("Clean_Data.csv", nrows=1000)
dataSet <- read.csv("Clean_Data.csv")
dataSet <- na.omit(dataSet)
dataSet["Season"] <- dataSet$ï..Season
attach(dataSet)
library(ggplot2)
library(ggthemes)

#local variables to set colors
border_point_C <- "black"
NAS_C <- "#977254"
Aircraft_C <- "#619CFF"
Carrier_C <- "#00BFBF"
Sec_Weather_C <- "#00BA38"
cols <- c("PT_Delay_Aircraft_C"=Aircraft_C, "PT_Delay_NAS_C"=NAS_C, "PT_Delay_Carrier_C"=Carrier_C,
          "PT_Delay_Security_Weather_C"=Sec_Weather_C)

pointShape <- 21
trendLineSize <- 1.25
pointSize <- 2.25
seasonFacet <- factor(Season, levels = c("Spring", "Summer", "Autumn", "Winter"))
seasonsLabels <- as_labeller(c("Spring" = "Spring", "Summer" = "Summer", "Autumn" = "Fall", "Winter" = "Winter"))
(
  #Graph creation
m <- ggplot(dataSet, aes(x = Year))+
    #Shows the trend lines
    stat_summary(fun.y=mean, geom="line", size=trendLineSize, aes(y = PT_Delay_NAS, color="PT_Delay_NAS_C")) +
    stat_summary(fun.y=mean, geom="line", size=trendLineSize, aes(y = PT_Delay_Aircraft, color="PT_Delay_Aircraft_C")) +
    stat_summary(fun.y=mean, geom="line", size=trendLineSize, aes(y = PT_Delay_Carrier, color="PT_Delay_Carrier_C")) +
    stat_summary(fun.y=mean, geom="line", size=trendLineSize, aes(y = PT_Delay_Security_Weather, color="PT_Delay_Security_Weather_C")) +
    
    #Shows the points on trend lines
    stat_summary(fun.y=mean, shape=pointShape, geom="point", size=pointSize, fill=NAS_C, color=border_point_C,
                 aes(y = PT_Delay_NAS, color="PT_Delay_NAS_C"))+
    stat_summary(fun.y=mean, shape=pointShape, geom="point", size=pointSize, fill=Aircraft_C, color=border_point_C,
                 aes(y = PT_Delay_Aircraft, color="PT_Delay_Aircraft_C"))+
    stat_summary(fun.y=mean, shape=pointShape, geom="point", size=pointSize, fill=Carrier_C, color=border_point_C,
                 aes(y = PT_Delay_Carrier, color="PT_Delay_Carrier_C"))+
    stat_summary(fun.y=mean, shape=pointShape, geom="point", size=pointSize, fill=Sec_Weather_C, color=border_point_C,
                 aes(y = PT_Delay_Security_Weather, color="PT_Delay_Security_Weather_C"))+
    
    #Creates 4 parts Summer, WInter, Fall, Spring
    facet_wrap( ~Season, nrow = 1, ncol = 4, labeller = seasonsLabels)+
    
    scale_y_continuous(name ="Percentage delays") +
    scale_x_continuous(breaks=c(2001, 2010, 2017), labels = c("2001", "2010", "2017")) +
    theme_calc()+
    theme(axis.title.x=element_blank()) +
    theme(legend.box.background = element_rect(colour = "black"), legend.background = element_blank()) +
    theme( axis.line = element_line(colour = "darkblue", 
                                    size = 1, linetype = "solid"))+
    theme(strip.background = element_rect(colour = "black", fill = "#FAFAFA"),strip.text = element_text(colour = 'black', size = 10))+
    scale_colour_manual(name="Reasons",
                        values=cols, labels = c("Aircraft","Carrier", "NAS", "Security & Weather"))+
    #Label settings
    labs(title = "In any season, Don't always blame weather for delays, Airlines are the real defaulters!", 
         subtitle = "Trend over last 14 years, have the airlines done enough to improve delay percentages?",
         caption = "Data Source : Bureau of Transportation Statistics, Airline Service Quality Performance")
)
