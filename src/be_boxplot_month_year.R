setwd("/media/dogbert/dev/scripte/R_scripts/")

# for schleife über alle 6 gebiete plots_HEG, ..., plots_AEG
file <- list.files("/media/dogbert/dev/scripte/R_scripts/", pattern=glob2rx("plots.csv"), recursive=TRUE, full.names=TRUE)
data_t <- read.csv("/media/dogbert/dev/scripte/R_scripts/plots.csv", header = TRUE, sep=",")

require(grid)
library(ggplot2)
library(reshape)

var_Name_t = 'Ta_200'

data_mean<- aggregate(Ta_200~ datetime, data_t,  FUN=mean)
data_median<- aggregate(Ta_200~ datetime, data_t,  FUN=median)
data_max<- aggregate(Ta_200~ datetime, data_t,  FUN=max)
data_min<- aggregate(Ta_200~ datetime, data_t,  FUN=min)

#https://stat.ethz.ch/education/semesters/as2012/biostat/Uebungen/Uebungen/R-in-Kurzform.pdf
#quantile(data_mean, probs = c(.25, 0.5, 0.75), na.rm = TRUE)
summary(data_t)

head(data_t, 80)
boxplot(data_t$Ta_200 ~ data_t$datetime)

ggplot(data_t, aes(data_t$datetime, data_t$Ta_200)) + 
  geom_boxplot()

data_t$month <- as.numeric(substr(as.character(data_t$datetime), 6, 7))

data_t$aggid <- paste0(data_t$plotID, "_", 
                       as.numeric(substr(as.character(data_t$datetime), 6, 7)))

monthmean <- aggregate(data_t$Ta_200, by = list(data_t$aggid), FUN = mean)
colnames(monthmean) <- c("aggid", "Ta_200_mean")

data_tn <- merge(data_t, monthmean, by = "aggid")

data_tn$Ta_200_ds <- data_tn$Ta_200 - data_tn$Ta_200_mean

data_tn$year <- as.factor(substr(as.character(data_tn$datetime), 1, 4))

data_tn$datetime_ro <- as.factor(paste0(substr(as.character(data_tn$datetime), 6, 7), "-", 
                              substr(as.character(data_tn$datetime), 1, 4)))



ggplot(data_tn, aes(datetime_ro, Ta_200_ds, fill = data_tn$year)) + 
  geom_boxplot(notch = FALSE) + 
  geom_vline(xintercept = seq(6.5, 6*12, 6), linetype="dotted") + 
  # stat_summary(fun.y=median, geom="line", aes(group=data_tn$year, colour = data_tn$year))  + 
  labs(list(x = "Month-Year", y = "Air temperature (°C, deseasoned)", fill = "Year")) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


Day <- substr(data_t$datetime, 0, 4)
#t <- colnames(data_mean) <- c('Day','Leg',var_Name_t)
#levels(data_mean$Leg) <- c("max", "mittel", "min")

data_t$Ta_200 <- round(data_t$Ta_200, 1)

p1<- (ggplot(aes(x = substr(data_t$datetime, 3, 7), y = Ta_200, group = Day, label = Ta_200), 
         data = data_t) + 
         scale_size_area()+
         xlab(NULL)+
         ylab(paste(var_Name_t,"°C") )+
         geom_boxplot(width=0.2)+
         #geom_line() + 
        # geom_point() + 
        # geom_text(vjust = -0.5,hjust=-0.1, size=3, color="black") + 
         ggtitle("Record of 6 years") +
       #  theme(plot.title=element_text(family="Arial", face="bold", size=10))+
         theme(plot.margin = unit(c(8,8,8,8), "mm"))+
         theme(title = element_text(vjust=1))+
         theme(legend.title=element_blank() ) + 
       # scale_x_discrete(labels=xaxisTitles) 
        # theme(axis.ticks = element_blank(), axis.text.x = element_blank()) # beschriftung der X Achse
         theme(axis.text=element_text(colour="black"))
)
print(p1)