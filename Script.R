# Recommend a data-driven strategy for the Maven Motors Super Bowl spot, and present it in the form of a single page report or dashboard.
# https://www.mavenanalytics.io/blog/maven-super-bowl-challenge

# For this challenge, you'll be assuming the role of Marketing Analyst at Maven Motors, an up-and-coming US car manufacturer looking
# to make a splash in the market. They have approved the budget to run a TV spot during the 2022 Super Bowl, but need you to analyze 
# historical data to help guide the creative direction.

# Your task is to recommend a data-driven strategy for the Maven Motors Super Bowl spot, and present it in the form of a single page
# report or dashboard.

# Finalists will be chosen by the Maven team based on insights, creativity, design, visualizations, and overall storytelling ability.
# The winner will be selected from the finalist pool by the Maven team, via live voting.

#1. LOAD NECESSARY LIBRARIES
library (tidyverse)

#2. LOAD DATA
data <- read.csv(file = "data/superbowl_commercials.csv")

#3. ANALYSIS

# TV.Viewers
data %>%
  ggplot (aes (x= as.factor(Year), y = TV.Viewers)) +
  geom_jitter()
# It seems that TV.Viewers is more function of something else and not an inherent effect of the qualities of each year TV spot
# One strange outlier in 2014 - https://superbowl-ads.com/2014-budweiser-puppy-love/

# Youtube.Views
data %>%
  ggplot (aes (x= as.factor(Year), y = Youtube.Views)) +
  geom_jitter()
# You Tube views seems more interesting, there is an outlier in 2012 - https://superbowl-ads.com/2012-doritos-sling-baby/

# Youtube.Likes
data %>%
  ggplot (aes (x= as.factor(Year), y = Youtube.Likes)) +
  geom_jitter()
# You Tube Likes seems more interesting, there is an outlier in 2012 - https://superbowl-ads.com/2012-doritos-sling-baby/

data %>%
  ggplot (aes (x= Youtube.Views, y = Youtube.Likes)) +
  geom_point() 

# let's try to mesure a views to likes ratio and see how it goes
data <- data %>%
  mutate (likes_to_views = Youtube.Likes/Youtube.Views)



data %>%
  filter (Youtube.Views < 10^7) %>%
  ggplot (aes (x= Youtube.Views, y = likes_to_views)) +
  geom_point() 


hist (data$likes_to_views)
summary(data$likes_to_views)

hist (data$Estimated.Cost)
summary(data$Estimated.Cost)


head (data)

nrow(data)

summary(data$TV.Viewers)
summary(data$Youtube.Views)
summary(data$Youtube.Likes)

hist(data$Youtube.Views)

data <- data %>%
  filter (!is.na(likes_to_views)) %>%  
  mutate (total_points = (Youtube.Views + Youtube.Likes / mean (likes_to_views)))


data %>%
  #filter (total_points > 10^5) %>%
  ggplot (aes (x = as.factor (Year), y = Youtube.Views, fill = Brand)) +
  geom_bar(stat = "identity") 


# developing some grouped statistical data
data2 <- data %>%
  group_by (Brand) %>%
  summarize (n_spots = n(), 
             avg_cost = sum(Estimated.Cost)/n_spots,                             #average cost of TV spot
             avg_views = sum(Youtube.Views)/n_spots,                             #average view in YouTube  
             avg_likes_per_view = sum(Youtube.Likes)/sum(Youtube.Views),         #average likes per view ratio
             avg_lenght = sum(Length)/n_spots,                                   #average lenght of video
             avg_view_per_1M_dollar = avg_views/avg_cost)                        #average views per 1M$ spot

# average YT views per spot VS average cost of the TV spot
data2 %>%
  ggplot (aes (x = avg_cost, y = avg_views, size = avg_likes_per_view, color = as.factor(Brand))) +
  geom_point() 

# it does not look that Auto OEM work well with Number of Views on YT per $ spent
data2 %>%
  ggplot (aes (x = reorder(Brand, -avg_view_per_1M_dollar), y = avg_view_per_1M_dollar )) +
  geom_bar(stat = "identity") 

# engagement of users (likes per view) - KIA does quite well (2nd place) and Hyundayi is not too far away % speaking
data2 %>%
  ggplot (aes (x = reorder(Brand, -avg_likes_per_view), y = avg_likes_per_view )) +
  geom_bar(stat = "identity") 

# finally, total views 
data2 %>%
  ggplot (aes (x = reorder(Brand, -avg_views), y = avg_views )) +
  geom_bar(stat = "identity") 


head (data2)
summary(data$total_points)
hist (data$total_points)

hist (data$Youtube.Likes)

plot (data$Estimated.Cost, data$TV.Viewers)

plot (data$TV.Viewers, data$Youtube.Views)

plot (data$Estimated.Cost, data$Youtube.Views)

data %>%
  ggplot (aes (x= as.factor(Year), y = TV.Viewers))+
  geom_boxplot()+
  geom_point()
