##Amanda Zhou
##Chalkbeat 2018
##AP 2001-2017 Snapshot

library(readxl)
library(tidyverse)
library(sysfonts)
library(showtext)

df2 <- data.frame(Type = NA, Score = NA, Count = NA, Year = NA)


for(i in 2001:2004){
  df <- read_xls(path = paste0("AP/data/national-summary-", i, ".xls"), sheet = 3, range = "A5:C75")
  names(df) <- c("Type", "Score", "Count")
  df$Score[seq(7,70,7)] <- "Mean Score"
  df$Type[df$Score == "Mean Score"] <- NA
  df$Type <- rep(df$Type[!is.na(df$Type)], each = 7)
  df$Year <-  i
  
  df2 <- bind_rows(df, df2)
}


for(i in 2005:2011){
  df <- read_xls(path = paste0("AP/data/national-summary-", i, ".xls"), range = "A5:C75")
  names(df) <- c("Type", "Score", "Count")
  df$Score[seq(7,70,7)] <- "Mean Score"
  df$Type[df$Score == "Mean Score"] <- NA
  df$Type <- rep(df$Type[!is.na(df$Type)], each = 7)
  df$Year <-  i
  
  df2 <- bind_rows(df, df2)
}

for(i in c(2012:2014, 2016:2017)){
  df <- read_xls(path = paste0("AP/data/national-summary-", i, ".xls"), range = "B6:D76")
  names(df) <- c("Type", "Score", "Count")
  df$Score[seq(7,70,7)] <- "Mean Score"
  df$Type[df$Score == "Mean Score"] <- NA
  df$Type <- rep(head(unique(df$Type[!is.na(df$Type)]), 10), each = 7)
  df$Year <-  i
  
  df2 <- bind_rows(df, df2)

}

for(i in 2015){
  df <- read_xls(path = paste0("AP/data/national-summary-", i, ".xls"), range = "B5:D75")
  names(df) <- c("Type", "Score", "Count")
  df$Score[seq(7,70,7)] <- "Mean Score"
  df$Type[df$Score == "Mean Score"] <- NA
  df$Type <- rep(head(unique(df$Type[!is.na(df$Type)]), 10), each = 7)
  df$Year <-  i
  
  df2 <- bind_rows(df, df2)
  
}

removetypes = c("AMERICAN INDIAN", "AMERICAN INDIAN/ALASKA NATIVE", "NATIVE HAWAIIAN/OTH PACF ISL",
                "NO RESPONSE", "NOT STATED", "OTHER", "TWO OR MORE RACES", "AMER IND./ALASKAN",
                "AMER IND./ALASKAN")

hispanictypes = c("MEXICAN AMERICAN", "OTHER HISPANIC", "PUERTO RICAN", "LATINO:  CHICANO/MEX. AMER",
                  "LATINO:  PUERTO RICAN", "LATINO: OTHER")

df3 <- df2 %>% 
  filter(!is.na(Type),
         !(Type %in% removetypes),
         Score != "Mean Score",
         Score != "T") %>% 
  mutate(Type = ifelse(Type == "National TOTAL", "NATIONAL TOTAL", Type),
         Type = ifelse(Type %in% hispanictypes, "HISPANIC/LATINO", Type),
         Type = ifelse(Type == "ASIAN/ASIAN AMER", "ASIAN", Type),
         Type = ifelse(Type == "BLACK/AFRO-AMER", "BLACK", Type)) %>% 
  group_by(Type, Year, Score) %>% 
  summarise(Count = sum(Count)) 


df4 <- df3 %>% 
  group_by(Type, Year) %>% 
  mutate(Total = sum(Count)) %>% 
  ungroup() %>% 
  mutate(Pass = ifelse(Score > 2, TRUE, FALSE)) %>% 
  group_by(Type, Year, Pass, Total) %>% 
  summarise(Count = sum(Count)) %>% 
  ungroup() %>% 
  filter(Pass == TRUE) %>% 
  mutate(Passrate = Count/ Total,
         Type = factor(Type, 
                       levels = c("ASIAN", "WHITE", "NATIONAL TOTAL", "HISPANIC/LATINO", "BLACK")))

# Graph --------------------------------------------------------------------
font_add_google("Source Sans Pro")

racetext <- data.frame(x = rep(2009, 5),
                       y = c(0.685, 0.61, 0.555, 0.45, 0.3),
                       text = c("Asian", "White", "Total", "Hispanic", "Black"),
                       color = c("#EF0F52", "#F6C241", "#333333", "#C2C832", "#2BA8E0"))

fig1 <- ggplot() +
  geom_line(data = df4,
            aes(x = Year,
                y = Passrate, 
                color = Type),
            size = 2) +
  scale_color_manual(values = c("#EF0F52", "#F6C241", "#333333", "#C2C832", "#2BA8E0")) +
  geom_label(data = racetext,
            aes(label = text,
                y = y, 
                x = x),
            color =  c("#EF0F52", "#F6C241", "#333333", "#C2C832", "#2BA8E0"),
            family = "Source Sans Pro",
            size = 8) +
  theme_minimal()+
  theme(text = element_text(family = "Source Sans Pro", size = 20),
        axis.text = element_text(size = 18, color = "black"),
        plot.subtitle = element_text(margin = margin(0, 0, 15, 0))) +
  scale_x_continuous(breaks = seq(2001, 2017, 2)) +
  scale_y_continuous(breaks = seq(0.25, 0.7, .05), labels = paste0(seq(25, 70, 5) , "%")) +
  labs(y = "", 
       x = "", 
       title = "Share of Advanced Placement tests passed",
       color = "") +
  guides(color = FALSE)


fig1
showtext_auto()

ggsave("AP/passrates.jpeg", height = 7.5, width = 10)


# More Analysis -----------------------------------------------------------


df5 <- df2 %>% 
  filter(!is.na(Type),
         !(Type %in% removetypes),
         Score != "Mean Score",
         Score == "T") %>% 
  mutate(Type = ifelse(Type == "National TOTAL", "NATIONAL TOTAL", Type),
         Type = ifelse(Type %in% hispanictypes, "HISPANIC/LATINO", Type),
         Type = ifelse(Type == "ASIAN/ASIAN AMER", "ASIAN", Type),
         Type = ifelse(Type == "BLACK/AFRO-AMER", "BLACK", Type)) %>% 
  group_by(Type, Year, Score) %>% 
  summarise(Count = sum(Count)) %>% 
  group_by(Year) %>% 
  filter(Type != "NATIONAL TOTAL") %>% 
  mutate(Total = sum(Count)) %>% 
  ungroup() %>% 
  mutate(Percent = Count/Total) 

ggplot() +
  geom_line(data = df5, 
            aes(x  = Year, 
                y = Percent,
                color = Type))

  

