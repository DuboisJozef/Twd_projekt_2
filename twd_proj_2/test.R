library(rjson)
library(dplyr)
library(lubridate)
library(ggplot2)


lok_joz <- fromJSON(file = "/home/joziop/Documents/twd_proj_2/Os_czasu.json")
Sys.setlocale("LC_TIME", "C")
semanticSegments <- lok_joz$semanticSegments

transport <- function(x){
  col_names <- c("activity", "distance", "startTime", "endTime")
  
  
  result <- data.frame(matrix(ncol = length(col_names), nrow = 0))
  colnames(result) <- col_names
  
  counter = 1
  for(i in 1:length(x)){
    if(!is.null(x[[i]]$activity)){
      
      result <- rbind(result, data.frame(activity = x[[i]]$activity$topCandidate$type, 
                                         distance = x[[i]]$activity$distanceMeters,
                                         startTime = x[[i]]$startTime, endTime = x[[i]]$endTime))
      counter <- counter + 1
    }
  }
  result
}

miejsca <- function(x){
  
  col_names <- c("place", "startTime", "endTime")
  
  
  result <- data.frame(matrix(ncol = length(col_names), nrow = 0))
  colnames(result) <- col_names
  
  counter = 1
  for(i in 1:length(x)){
    if(!is.null(x[[i]]$visit)){
      
      place = case_when(x[[i]]$visit$topCandidate$placeId == "ChIJOWqUcOzMHkcROp3KVw-z22k" ~ "Politechnika", # mini
                        #x[[i]]$visit$topCandidate$placeId == "ChIJF8u2SOnMHkcR7TrJJ2_WP80" ~ "Politechnika", # gg
                        x[[i]]$visit$topCandidate$placeId == "ChIJU-Q9-DzMHkcRARYuoIMMCx8" ~ "Dom",
                        x[[i]]$visit$topCandidate$placeId == "ChIJGVrIUenMHkcRkSvEAxUOK3E" ~ "Politechnika", # angielski
                        x[[i]]$visit$topCandidate$placeId == "ChIJWTUjZY3MHkcR2U7HZ_LJC-s" ~ "Warszawa Centralna",
                        x[[i]]$visit$topCandidate$placeId == "ChIJB4mvyljUG0cRYV0oM-DLm2g" ~ "Koluszki",
                        x[[i]]$visit$topCandidate$placeId == "ChIJsQtqT0rTG0cRVbB818vuHhw" ~ "Działka",
                        x[[i]]$visit$topCandidate$placeId == "ChIJSS5pkozMHkcRwi0fMeV66cI" ~ "Basen",
                        #x[[i]]$visit$topCandidate$placeId == "ChIJj1FLqc_MHkcR4HsjgGwILO4" ~ "Kampus Południowy",
                        #x[[i]]$visit$topCandidate$placeId == "CwILO4" ~ "Hen daleko(kampus poludniowy)",
                        TRUE ~ "Inne")
                        
      day_diff <- ceiling(as.numeric(difftime(as.POSIXct(substr(x[[i]]$endTime, 1, 10), format = "%Y-%m-%d"),
                           as.POSIXct(substr(x[[i]]$startTime, 1, 10), format = "%Y-%m-%d"), units = "days"))) + 1
      
      
      for(j in 1:day_diff){
        result <- rbind(result, data.frame(place = place,
                        startTime = case_when(j == 1 ~ substr(x[[i]]$startTime, 1, 19),
                                              TRUE ~ paste0(as.character(tail(seq(as.POSIXct(substr(x[[i]]$startTime, 1, 19), format = "%Y-%m-%d"), by= "day", length = j), n = 1)), "T00:00:00")), 
                        endTime = case_when(j == day_diff ~ substr(x[[i]]$endTime, 1, 19),
                                            j == 1 ~ paste0(as.character(as.POSIXct(substr(x[[i]]$startTime, 1, 19), format = "%Y-%m-%d")), "T23:59:59"),
                                              TRUE ~ paste0(as.character(tail(seq(as.POSIXct(substr(x[[i]]$startTime, 1, 19), format = "%Y-%m-%d"), by= "day", length = j), n = 1)), "T23:59:59"))))
      }
      counter <- counter + 1
    }
  }
  
  
  
  
  result[result$place != "Inne",]
}


podroze <- transport(semanticSegments) %>% 
  mutate(timeDurSec = as.integer(difftime(as.POSIXct(substr(endTime, 1, 19), format = "%Y-%m-%dT%H:%M:%S"),
                            as.POSIXct(substr(startTime, 1, 19), format = "%Y-%m-%dT%H:%M:%S"), #
                            units = "secs"))) %>% 
  mutate(day = as.POSIXct(substr(endTime, 1, 10), format = "%Y-%m-%d"))

wizyty <- miejsca(semanticSegments) %>% # nieaktualne
  
  mutate(timeDurSec = as.integer(difftime(as.POSIXct(substr(endTime, 1, 19), format = "%Y-%m-%dT%H:%M:%S"),
                                          as.POSIXct(substr(startTime, 1, 19), format = "%Y-%m-%dT%H:%M:%S"), #
                                          units = "secs")))%>% 
  
  group_by(place) %>% 
  mutate(sumTime = sum(timeDurSec))
  

czas_na_uczelni <- wizyty %>% 
  filter(place == "Politechnika") %>% 
  mutate(day = as.POSIXct(substr(endTime, 1, 10), format = "%Y-%m-%d")) %>% 
  left_join(podroze[podroze$activity == "WALKING",], by = "day") %>% 
  mutate(dayOfWeek = weekdays(as.POSIXct(day, format = "%Y-%m-%d"))) %>% 
  mutate(dayOfWeek = factor(dayOfWeek, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))) %>% 
  mutate(befOrAft = ifelse(as.integer(difftime(as.POSIXct(substr(endTime.x, 1, 19), format = "%Y-%m-%dT%H:%M:%S"),
                                               as.POSIXct(substr(endTime.y, 1, 19), format = "%Y-%m-%dT%H:%M:%S"), #
                                               units = "secs"))< 0, "after", "before")) %>% 
  mutate(befOrAft = factor(befOrAft, levels = c("before", "after"))) %>%
  group_by(day, befOrAft) %>% 
  mutate(speed = sum(distance)/sum(timeDurSec.y)) %>% 
  ungroup() %>% 
  filter(startTime.x != "2024-12-05T19:01:18.000+01:00")


wyk3 <- ggplot(czas_na_uczelni, aes(x = dayOfWeek, group = befOrAft, y = speed, fill = befOrAft)) +
  geom_col(position = "dodge") +
  labs(fill = "Before of after\n going to the faculty") +  
  xlab("Day") +         
  ylab("Speed m/s") +
  theme_minimal()

wyk3

ggsave("speedAfterFaculty.png", plot = wyk3, bg = "transparent", width = 10, height = 6, dpi = 300)


czas_w_transporcie <- podroze %>%
  mutate(dayOfWeek = weekdays(as.Date(endTime, format = "%Y-%m-%d")),
         dayOfWeek = factor(dayOfWeek, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))) %>% 
  group_by(dayOfWeek) %>% 
  mutate(meanTimeDur = sum(timeDurSec)/2) %>% # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!1 zmień później!!!!!!!!!!!!!!!!!!!!!!
  ungroup() %>% 
  group_by() %>% 
  select(dayOfWeek, meanTimeDur) %>% 
  group_by(dayOfWeek, meanTimeDur) %>% 
  slice(1) %>% 
  mutate(place = "transport")

czas_miejsca <- wizyty %>% 
  mutate(day = as.POSIXct(substr(endTime, 1, 10), format = "%Y-%m-%d")) %>%
  mutate(dayOfWeek = weekdays(as.Date(endTime, format = "%Y-%m-%d")),
         dayOfWeek = factor(dayOfWeek, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))) %>%
  select(dayOfWeek, place, timeDurSec) %>% 
  group_by(dayOfWeek, place) %>% 
  mutate(meanTimeDur = sum(timeDurSec)/2) %>% # !!!!!!!! tu też zmień
  ungroup() %>% 
  select(dayOfWeek, place, meanTimeDur) %>% 
  group_by(dayOfWeek, place, meanTimeDur) %>% 
  slice(1)
  
czas <- rbind(czas_miejsca, czas_w_transporcie)
  
  

wyk2 <- ggplot(czas, aes(y = meanTimeDur, x = dayOfWeek, fill = place))+
  geom_bar(stat = "identity", position = "fill") +
  labs(fill = "Place") +                         
  xlab("Day of week") +                                     
  ylab("% of time of day") +                      
  ggtitle("Average daily time spent in each place") +     
  theme_minimal()     

wyk2
ggsave("time.png", plot = wyk2, bg = "transparent", width = 10, height = 6, dpi = 300)



podroze2 <- podroze %>%
  mutate(Day = weekdays(as.Date(startTime, format = "%Y-%m-%d")),
         Day = factor(Day, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))) 


agg_data <- podroze2 %>%
  group_by(Day, activity) %>%
  summarise(mean_speed = mean(distance / timeDurSec, na.rm = TRUE), .groups = 'drop') %>% 
  mutate(activity2 = case_when(activity == "CYCLING" ~ "cycling",
                               activity == "IN_PASSENGER_VEHICLE" ~ "in car",
                               activity == "IN_SUBWAY" ~ "in subway",
                               activity == "IN_TRAM" ~ "in tram",
                               activity == "WALKING" ~ "walking",
                               activity == "IN_TRAIN" ~ "in train"))


wyk1 <- ggplot(agg_data, aes(x = Day, y = mean_speed, fill = activity2)) +
  geom_col(position = position_dodge(width = 0.9)) + 
  labs(fill = "") +                        
  xlab("Day") +                                     
  ylab("Average Speed (m/s)") +                      
  ggtitle("Average speed by different means of transport") +     
  theme_minimal()                                    


print(wyk1)
ggsave("speedByMode.png", plot = wyk1, bg = "transparent", width = 10, height = 6, dpi = 300)
