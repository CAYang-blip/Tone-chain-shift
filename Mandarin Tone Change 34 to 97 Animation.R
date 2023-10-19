## 1934 to 1997 Mandarin Tone Change Animation

#libraries
library(tidyverse)
library(gganimate)
library(Rcpp)
library(gifski)
library(transformr)

# set to where files are stored
setwd("Mandarin tone change/Animation data")

### Obata 1934 ###
Obatas <- paste("Obata_T", c(1:4), ".csv", sep = "")
obata <- data.frame(matrix(ncol = 3, nrow =0))

for (obat in Obatas){
  ob_temp <- read.csv(obat)
  obata <- rbind(obata, ob_temp)
}
obata$Tone <- as.factor(obata$Tone)

# Normalization of the times
obata <- obata %>% 
  group_by(Tone)%>%
  mutate(Time = Time - min(Time))%>%
  ungroup()

obata <- obata %>%
  mutate(T_int = max(Time)-min(Time)) %>%
  mutate(Time = (Time - min(Time))/T_int)

# Convert Hz to Chao Numbers
lgmax <- log(max(obata$Pitch,na.rm = TRUE))
lgmin <- log(min(obata$Pitch,na.rm = TRUE))
lgdif <- lgmax - lgmin
r_mean = mean(obata$Pitch)

obata <- obata %>% 
  group_by(Tone) %>%
  mutate(Semi_Tone = 12 * log(Pitch/r_mean) / log(2)) %>%
  mutate(Chao_Number = 5* (log(Pitch)- lgmin)/lgdif) %>%
  select(-c(Semi_Tone, T_int))

# plot data
ggplot(data = filter(obata, Tone ==2)) +
  geom_smooth(mapping = aes(Time, Chao_Number), color = "blue")+
  theme_bw()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.title.y=element_blank(), 
        axis.ticks.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        # explicitly set the horizontal lines (or they will disappear too)
        panel.grid.major.y = element_line( size=.1, color="grey" ), 
        legend.position = "none")+
  ylim(1,5)

ggsave("T2_cit_34.png", dpi=300)

### Xu 1997 ###
Xus_97 <- paste("Xu_T", c(1:4), ".csv", sep = "")
xu97 <- data.frame(matrix(ncol = 3, nrow =0))

for (xu in Xus_97){
  xu_temp <- read.csv(xu)
  xu97 <- rbind(xu97, xu_temp)
}

xu97$Tone <- as.factor(xu97$Tone)

# Normalization of the times
xu97 <- xu97 %>% 
  group_by(Tone)%>%
  mutate(Time = Time - min(Time))%>%
  ungroup()

xu97 <- xu97 %>%
  mutate(T_int = max(Time)-min(Time)) %>%
  mutate(Time = (Time - min(Time))/T_int)%>%
  select(-c(T_int))


# Convert Hz to Chao Numbers
lgmax <- log(max(xu97$Pitch,na.rm = TRUE))
lgmin <- log(min(xu97$Pitch,na.rm = TRUE))
lgdif <- lgmax - lgmin

r_mean = mean(xu97$Pitch)

xu97 <- xu97 %>% 
  group_by(Tone) %>%
  mutate(Semi_Tone = 12 * log(Pitch/r_mean) / log(2)) %>%
  mutate(Chao_Number = 5* (log(Pitch)- lgmin)/lgdif)%>%
  ungroup()%>%
  select(-c(Semi_Tone))

#plot data
ggplot(data = xu97, aes(Time, Chao_Number, color = Tone)) +
  geom_smooth()+
  ggtitle("xu97")

##### Create Animation Plot #######

#combine data frames for animation
xu97$Year = 97
obata$Year = 34
full_df <- rbind(xu97, obata)

# this expression selects which tones you wish to display,
# to change it simply place a '|' between each desired tone (e.g.: "1|3|4")
toneExpr <- "1|2|3|4" 
plotData <- full_df[grepl(toneExpr, full_df$Tone),]

# create base animation
animPlot <- ggplot(data = plotData, aes(x = Time, y = Chao_Number, color = Tone)) +
  geom_line(stat = "smooth", method = "loess",
            linewidth = 5,
            alpha = 0.6) +
  scale_color_manual(values = c("red", "yellow","grey","brown"))+
  theme_bw() +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    plot.title = element_text(size = 14),
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 16)
  ) +
  ylim(-.5, 5.5)+
  transition_time(Year)

# add more custom settings
TAnim <<- animate(animPlot, height = 500, width = 800, fps = 
                    10, duration = 10, start_pause = 20, 
                  end_pause = 20,  rewind = F)

TAnim #display plot in R
anim_save("Mandarin34to97_color.gif", res=300) # save plot with desired title, end with .gif
