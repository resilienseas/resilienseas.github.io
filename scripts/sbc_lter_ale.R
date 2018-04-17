########### SBC LTER ALE (Alegria Reef) Data Exploration

if (!require(pacman)) install.packages("pacman")
library(pacman)
p_load(
  here,
  plyr,
  tidyr,
  dplyr,
  readr,
  ggplot2,
  extrafont,
  scales)

# import fonts to use 'spectral' in graphics
# note 'spectral' fonts must be installed on the computer - have not found a workaround for importing from github yet
# font_import()

# read ALE_arag CSV from github
ale <- read_csv("data/ALE_arag.csv")

# set -9999 values to NA
ale[ale==-9999] <- NA

colnames(ale) <- c("date.time", "site", "lat", "long", "aragonite")

ale$date.time <- as.POSIXct(ale$date.time,format = "%Y-%m-%d %H:%M:%S")

# separate out date and time
# ale <- separate(ale, "date.time", c("date","time"), sep=" ")
# ale$date <- as.Date(ale$date,'%Y-%m-%d')

# remove NA for mean/std deviation calculations
ale.wo.na <- na.omit(ale)

png(filename="visualizations/ale.arag.ts.png", width = 12, height = 7, units = 'in', res = 300)
ggplot(data=ale, aes(x = date.time, y = aragonite)) +
  #geom_rect(data=ale, xmin=-Inf,
            #xmax=Inf,
            #ymin=(mean(ale.wo.na$aragonite) - sd(ale.wo.na$aragonite)),
            #ymax=(mean(ale.wo.na$aragonite) + sd(ale.wo.na$aragonite)), aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),
            #fill="grey95",alpha=0.5) +
  geom_rect(data=ale, xmin=as.POSIXct('2012-02-06'),
            xmax=as.POSIXct('2012-06-22'),
            ymin=-Inf,
            ymax=Inf, aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),
            fill="grey90") +
  geom_rect(data=ale, xmin=as.POSIXct('2013-03-29'),
            xmax=as.POSIXct('2013-10-07'),
            ymin=-Inf,
            ymax=Inf, aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),
            fill="grey90") +
  geom_line(colour = "grey20", size = 0.4, alpha = 0.5) +
  #geom_smooth(data = ale.wo.na, aes(date.time, aragonite), span = 0.1, color = "grey80", se = FALSE) +
  #geom_hline(yintercept = mean(ale.wo.na$aragonite), color="dodgerblue2", linetype = "dashed") +
  #geom_hline(yintercept = 2, color="light pink", linetype = "dashed") +
  labs(title="Ocean Acidification in the Santa Barbara Channel", subtitle = "June 2011 - December 2013\n", x="", y = "") +
  theme_linedraw() +
  theme(text=element_text(family="Spectral"), plot.title = element_text(size = 26, hjust = 0.5), plot.subtitle=element_text(size = 20, hjust=0.5), axis.title = element_text(size = 18), axis.text = element_text(size = 16), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.text.y = element_blank()) + 
        #axis.ticks.y = element_blank()) +
  #theme(plot.margin=unit(c(5,10,5,5),"mm")) +
  scale_y_reverse(limits = c(4.25, 0)) +
  scale_x_datetime(expand = c(0,0), date_breaks = "3 months", labels = date_format("%b-%y"))
  # annotate("text", x = as.POSIXct('2011-09-03'), y = 4, label = "Low Acidification", family = "Spectral") +
  # annotate("text", x = as.POSIXct('2011-09-03'), y = 0.5, label = "High Acidification", family = "Spectral") +
  # annotate("text", x = as.POSIXct('2013-07-01'), y = 4, label = "Data gaps", family = "Spectral")
dev.off()

################### Seasonal Data Subset

ale.seasonal <- subset(ale, ale$date.time >= "2011-11-01 00:00:00" & ale$date.time <= "2012-01-31 00:00:00")

ale.seasonal$threshold <- cut(ale.seasonal$aragonite,
                             breaks = c(-Inf, 1, 1.7, 2, Inf),
                             labels = c("<1", "1-1.7", "1.7-2", ">2"))

png(filename="visualizations/ale.arag.seasonal.png", width = 12, height = 7, units = 'in', res = 300)
ggplot(ale.seasonal, aes(date.time, aragonite)) +
  #geom_rect(data=ale.seasonal, xmin=-Inf,
           # xmax=Inf,
          #  ymin=2,
           # ymax=1, aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax), 
           # fill="grey20") +
  geom_line(colour = "grey85") +
  geom_point(aes(date.time, aragonite, color = threshold), size = 0.8, alpha = 0.6) +
  #geom_smooth(data = ale.seasonal, aes(date, aragonite), se = FALSE) +
  scale_color_manual(values=c("red", "pink2", "mistyrose", "dodgerblue4")) +
  labs(title="Seasonal Ocean Acidification in the Santa Barbara Channel", subtitle = "November 2012 - January 2013\n", x="", y = "") +
  #theme_classic() +
  theme(text=element_text(family="Spectral"), plot.title = element_text(size = 24, hjust = 0.5), plot.subtitle = element_text(size = 18, hjust = 0.5), axis.title = element_text(size = 18), axis.text = element_text(size = 14), legend.text=element_text(size=14), legend.title = element_text(size=14), legend.key = element_rect(fill = "grey70")) +
  theme(panel.background = element_rect(fill = "grey75",
                                        colour = "grey75"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(), legend.position = "none") +
    scale_x_datetime(date_breaks = "1 month", labels = date_format("%b")) +
  scale_y_reverse(limits = c(2.6, 0.75))
dev.off()

############# Weeky visualization

ale.march <- subset(ale, ale$date.time >= "2013-03-02 00:00:00" & ale$date.time <= "2013-03-08 00:00:00")

png(filename="visualizations/ale.arag.week.png", width = 12, height = 7, units = 'in', res = 300)
ggplot(ale.march, aes(date.time, aragonite)) +
  geom_line(colour = "grey20", alpha = 0.8) +
  geom_hline(yintercept = 1.7, color="pink2", linetype = "dashed", size = 1) +
  labs(title="Weekly Ocean Acidification in the Santa Barbara Channel", subtitle = "March 2013\n",x="", y = "") +
  theme_linedraw() +
  theme(text=element_text(family="Spectral"), plot.title = element_text(size = 24, hjust = 0.5), plot.subtitle=element_text(size = 18, hjust=0.5), axis.title = element_text(size = 18), axis.text = element_text(size = 14), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  scale_x_datetime(date_breaks = "1 day", labels = date_format("%m/%d")) +
  scale_y_reverse(limits = c(3.3, 1.3)) +
  annotate("text", x = as.POSIXct('2013-03-03'), y = 1.62, label = "Biologically-critical threshold", family = "Spectral", size = 6)
  #annotate("text", x = as.POSIXct('2013-03-05'), y = 1.15, label = "1.36", family = "Spectral", vjust = 0.4, hjust = -0.6)
dev.off()