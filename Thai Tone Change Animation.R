## 1911 to 2006 Thai Tone Change Animation
##code written by Tyler Braito

#libraries
library(tidyverse)
library(gganimate)
library(Rcpp)
library(gifski)
library(transformr)

# set working directory to where files are stored
setwd("Thai tone change")

### 1911 ###
thai.1911.tones <- paste("Thai T", c(1:5), " 1911.csv", sep = "")
thai.1911 <- data.frame(matrix(ncol = 3, nrow =0))

for (tone11 in thai.1911.tones){
  thai_temp <- read.csv(tone11)
  thai.1911 <- rbind(thai.1911, thai_temp)
}

thai.1911$Tone <- as.factor(thai.1911$Tone)
colnames(thai.1911) <- c("Time", "Chao_Number", "Tone")


# thai.1911 <- thai.1911 %>% 
#   group_by(Tone) %>%
#   mutate(T_int = max(Time)-min(Time)) %>%
#   mutate(Time = (Time - min(Time))/T_int) %>%
#   select(-c("T_int"))

ggplot(data = thai.1911) +
  geom_smooth(aes(Time, Chao_Number, color = Tone))+
  ggtitle("thai.1911")

### 1962 ### 
thai.1962.tones <- paste("Thai T", c(1:5), " 1962.csv", sep = "")
thai.1962 <- data.frame(matrix(ncol = 3, nrow =0))

for (tone62 in thai.1962.tones){
  thai_temp <- read.csv(tone62)
  thai.1962 <- rbind(thai.1962, thai_temp)
}

thai.1962$Tone <- as.factor(thai.1962$Tone)

# Convert Hz to Chao Numbers
lgmax <- log(max(thai.1962$Pitch,na.rm = TRUE))
lgmin <- log(min(thai.1962$Pitch,na.rm = TRUE))
lgdif <- lgmax - lgmin
r_mean = mean(thai.1962$Pitch)

thai.1962 <- thai.1962 %>% 
  group_by(Tone) %>%
  mutate(Semi_Tone = 12 * log(Pitch/r_mean) / log(2)) %>%
  mutate(Chao_Number = 5* (log(Pitch)- lgmin)/lgdif) %>%
  select(-c(Semi_Tone, Pitch))

# Restandarize Time based on average per tone
averages <- c(404, 413, 343, 352, 371)
max_value <- max(averages)
relative_times <- averages / max_value
ToneGroup <- data.frame(Tone = 1:5, TimeScaled = relative_times)
ToneGroup$Tone <- as.factor(ToneGroup$Tone)

thai.1962 <- thai.1962 %>%
  left_join(ToneGroup, by = "Tone") %>%
  group_by(Tone) %>%
  mutate(Int_T = max(Time) - min(Time)) %>%
  mutate(Time = (Time - min(Time)) * TimeScaled)%>%
  select(-c(Int_T, TimeScaled))

#Alternative is to normalize based on scale of 0 to 1 for all tones
thai.1962 <- thai.1962 %>% 
  group_by(Tone) %>%
  mutate(T_int = max(Time)-min(Time)) %>%
  mutate(Time = (Time - min(Time))/T_int) %>%
  select(-T_int)

ggplot(data = thai.1962) +
  geom_smooth(aes(Time, Chao_Number, color = Tone))+
  ggtitle("thai.1962")

### 2006 ### 
thai.2006.tones <- paste("Thai T", c(1:5), " 2006.csv", sep = "")
thai.2006 <- data.frame(matrix(ncol = 3, nrow =0))

for (tone06 in thai.2006.tones){
  thai_temp <- read.csv(tone06)
  thai.2006 <- rbind(thai.2006, thai_temp)
}

thai.2006$Tone <- as.factor(thai.2006$Tone)

# Convert Hz to Chao Numbers
lgmax <- log(max(thai.2006$Pitch,na.rm = TRUE))
lgmin <- log(min(thai.2006$Pitch,na.rm = TRUE))
lgdif <- lgmax - lgmin
r_mean = mean(thai.2006$Pitch)

thai.2006 <- thai.2006 %>% 
  group_by(Tone) %>%
  mutate(Semi_Tone = 12 * log(Pitch/r_mean) / log(2)) %>%
  mutate(Chao_Number = 5* (log(Pitch)- lgmin)/lgdif) %>%
  select(-c(Semi_Tone))

# thai.2006 <- thai.2006 %>% 
#   group_by(Tone) %>%
#   mutate(T_int = max(Time)-min(Time)) %>%
#   mutate(Time = (Time - min(Time))/T_int) %>%
#   select(-c("T_int"))

ggplot(data = thai.2006) +
  geom_smooth(aes(Time, Chao_Number, color = Tone))+
  ggtitle("thai.2006")


### 2023 ###
thai.2023.tones <- paste("Thai T", c(1:5), " 2023.csv", sep = "")
thai.2023 <- data.frame(matrix(ncol = 3, nrow =0))

for (tone23 in thai.2023.tones){
  thai_temp <- read.csv(tone23)
  thai.2023 <- rbind(thai.2023, thai_temp)
}
View(thai.2023)
thai.2023$Tone <- as.factor(thai.2023$Tone)

#Alternative is to normalize based on scale of 0 to 1 for all tones
thai.2023 <- thai.2023 %>% 
  group_by(Tone) %>%
  mutate(T_int = max(Time)-min(Time)) %>%
  mutate(Time = (Time - min(Time))/T_int) %>%
  select(-T_int)

ggplot(data = thai.2023) +
  geom_smooth(aes(Time, Chao_Number, color = Tone))+
  ggtitle("thai.2023")


##### Create Animation Plot #######


#create transition time
thai.2006$Year = 2006
thai.1911$Year = 1911
thai.1962$Year = 1962
thai.2023$Year = 2023

full_df <- rbind(thai.1962, thai.2023) #selected desired dataframes

# this expression selects which tones you wish to display,
# to change it simply place a '|' between each desired tone (e.g.: "2|3|5")
toneExpr <- "4" 
plotData <- full_df[grepl(toneExpr, full_df$Tone),]

# create base animation
animPlot <- ggplot(data = plotData, aes(x = Time, y = Chao_Number, color = Tone)) +
  geom_line(stat = "smooth", method = "loess",
            linewidth = 5,
            alpha = 0.6) +
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
    legend.position = "none"
    #legend.text = element_text(size = 16),
    #legend.title = element_text(size = 16)
  ) +
  ylim(-.5, 5.5)+
  transition_time(Year)

# add more custom settings
TAnim <<- animate(animPlot, height = 500, width = 500, fps = 
                    10, duration = 10, start_pause = 20, 
                  end_pause = 20,  rewind = F)

TAnim #display plot in R

anim_save("High_1962_2023.gif", res=144) # save plot with desired title, end with .gif
