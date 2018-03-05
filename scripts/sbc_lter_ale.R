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
font_import()

# read ALE_arag CSV from github
ale <- read_csv("data/ALE_arag.csv")

# set -9999 values to NA
ale[ale==-9999] <- NA

colnames(ale) <- c("date.time", "site", "lat", "long", "aragonite")

# separate out date and time
ale <- separate(ale, "date.time", c("date","time"), sep=" ")
ale$date <- as.Date(ale$date,'%Y-%m-%d')

# remove NA for mean/std deviation calculations
ale.wo.na <- na.omit(ale)

png(filename="visualizations/ale.arag.ts.png", width = 12, height = 7, units = 'in', res = 300)
ggplot(data=ale, aes(date, aragonite)) +
  geom_rect(data=ale, xmin=-Inf,
            xmax=Inf,
            ymin=(mean(ale.wo.na$aragonite) - sd(ale.wo.na$aragonite)),
            ymax=(mean(ale.wo.na$aragonite) + sd(ale.wo.na$aragonite)), aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),
            fill="grey90",alpha=0.5) +
  geom_line(colour = "grey30", size = 0.4, alpha = 0.8) +
  geom_hline(yintercept = mean(ale.wo.na$aragonite), color="dodgerblue", linetype = "dashed") +
  labs(title="Santa Barbara Channel Aragonite Saturation State\n",x="\nDate", y = "Aragonite Saturation State\n") +
  theme_classic() +
  theme(text=element_text(family="Spectral"), plot.title = element_text(size = 24, hjust = 0.5), axis.title = element_text(size = 18), axis.text = element_text(size = 14)) +
  theme(plot.margin=unit(c(5,10,5,5),"mm")) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 4.15)) +
  scale_x_date(expand = c(0,0), date_breaks = "3 months", labels = date_format("%b. '%y"))
dev.off()

#annotate(geom = "text", x = as.Date("2012-02-03"), y = 3.5,label= "Mean = 2.327\nSD = 0.364",fontface = "bold") +
