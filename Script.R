
# INTRODUCTION ------------------------------------------------------------
# Recommend a data-driven strategy for the Maven Motors Super Bowl spot, and present it in the form of a single page report or dashboard.
# https://www.mavenanalytics.io/blog/maven-super-bowl-challenge

# For this challenge, you'll be assuming the role of Marketing Analyst at Maven Motors, an up-and-coming US car manufacturer looking
# to make a splash in the market. They have approved the budget to run a TV spot during the 2022 Super Bowl, but need you to analyze 
# historical data to help guide the creative direction.

# Your task is to recommend a data-driven strategy for the Maven Motors Super Bowl spot, and present it in the form of a single page
# report or dashboard.

# Finalists will be chosen by the Maven team based on insights, creativity, design, visualizations, and overall storytelling ability.
# The winner will be selected from the finalist pool by the Maven team, via live voting.


# LIBRARIES ---------------------------------------------------------------
#1. LOAD NECESSARY LIBRARIES





# LOAD DATA ---------------------------------------------------------------
#2. LOAD DATA


# FIXING ERRORS IN DATA ---------------------------------------------------
#2.1 TV VIEWERS IN https://superbowl-ads.com/2014-budweiser-puppy-love/ IS NOT CORRECT AS IT REPORTS A DIFFERENT NUMBER RESPECT ALL OTHER 2014 VIDEOS



#2.2 CREATING SOME NEW VARIABLES()
head(data)





# ANALYSIS ----------------------------------------------------------------
#3. ANALYSIS
# TV.Viewers


# It seems that TV.Viewers is more function of something else and not an inherent effect of the qualities of each year TV spot
# One strange outlier in 2014 - https://superbowl-ads.com/2014-budweiser-puppy-love/



# Youtube.Views
data %>%
  ggplot (aes (x= Estimated.Cost, y = Youtube.Views, color = Brand, size = YT_likes_to_views)) +
  geom_point () +
  scale_y_log10()
# You Tube views seems more interesting, there is an outlier in 2012 - https://superbowl-ads.com/2012-doritos-sling-baby/

# Youtube.Likes
data %>%
  ggplot (aes (x= as.factor(Year), y = Youtube.Likes)) +
  geom_jitter()
# You Tube Likes seems more interesting, there is an outlier in 2012 - https://superbowl-ads.com/2012-doritos-sling-baby/

data %>%
  #filter (Youtube.Views <10^7) %>%         #filtering out the Outlier
  ggplot (aes (x= Estimated.Cost, y = Youtube.Views, color = Brand, size = YT_likes_to_views)) +
  geom_point(alpha = .4) +
  scale_color_brewer(palette = "Paired") +
  scale_y_log10() 
  #coord_flip()
# Cost does not explain YT views neither likes to views

head(data2)
# developing some grouped statistical data
data2 <- data %>%
  group_by (Brand) %>%
  filter (!is.na(Youtube.Views) & !is.na(Youtube.Likes)) %>%
  summarize (n_spots = n(), 
             avg_cost = mean(Estimated.Cost),                                    #average cost of TV spot
             avg_views = mean(Youtube.Views),                                    #average view in YouTube  
             avg_likes_per_view = sum(Youtube.Likes)/sum(Youtube.Views),         #average likes per view ratio
             avg_lenght = mean(Length),                                          #average lenght of video
             avg_view_per_1M_dollar = avg_views/avg_cost)                        #average views per 1M$ spot

head(data2)

# average YT views per spot VS average cost of the TV spot
data2 %>%
  ggplot (aes (x = avg_cost, y = avg_views, size = avg_likes_per_view, color = as.factor(Brand))) +
  geom_point() +
  scale_color_brewer(palette = "Set3")

data2 %>%
  ggplot (aes (x = avg_view_per_1M_dollar, y = avg_likes_per_view, size = avg_lenght, color = as.factor(Brand))) +
  geom_point() 


# it does not look that Auto OEM work well with Number of Views on YT per $ spent
data2 %>%
  ggplot (aes (x = reorder(Brand, -avg_view_per_1M_dollar), y = avg_view_per_1M_dollar )) +
  geom_bar(stat = "identity") 

# engagement of users (likes per view) - KIA does quite well (2nd place) and Hyundayi is not too far away % speaking
data2 %>%
  ggplot (aes (x = reorder(Brand, -avg_likes_per_view), y = avg_likes_per_view)) +
  geom_bar(stat = "identity") 

# finally, total views 
data2 %>%
  ggplot (aes (x = reorder(Brand, -avg_views), y = avg_views )) +
  geom_bar(stat = "identity") 



# FOCUS ON AUTO OEMs ------------------------------------------------------
# FOCUS ON TOYOTA, HYUNDAI AND KIA
# developing some grouped statistical data
data3 <- data %>%
  filter (Brand %in% c("Toyota", "Hyundai", "Kia")) %>%
  mutate (likes_per_view = Youtube.Likes / Youtube.Views,         #likes per view ratio
          view_per_1M_dollar = Youtube.Views / Estimated.Cost)    #views per 1M$ spot


# average YT views per spot VS average cost of the TV spot
data3 %>%
  ggplot (aes (x = Estimated.Cost, y = Youtube.Views, size = likes_per_view, color = as.factor(Brand))) +
  geom_point() 










##FOCUS ON ECONOMIC SECTOR
data4 <- data %>%
  group_by (econ_sector) %>%
  filter (!is.na(Youtube.Views) & !is.na(Youtube.Likes)) %>%
  summarize (n_spots = n(), 
             avg_cost = mean(Estimated.Cost),                                    #average cost of TV spot
             avg_views = mean(Youtube.Views),                                    #average view in YouTube  
             avg_likes_per_view = sum(Youtube.Likes)/sum(Youtube.Views),         #average likes per view ratio
             avg_lenght = mean(Length),                                          #average lenght of video
             avg_view_per_1M_dollar = avg_views/avg_cost)                        #average views per 1M$ spot


data4 %>%
  ggplot (aes (x = reorder(econ_sector, -avg_likes_per_view), y = avg_likes_per_view, fill = econ_sector)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("Soft Drinks" = "grey",
                               "Cars" = "red",
                               "Sports" = "grey",
                               "Beers" = "grey",
                               "Chips" = "grey",
                               "Online Investments" = "grey"))

data4 %>%
  ggplot (aes (x = reorder(econ_sector, -avg_view_per_1M_dollar), y = avg_view_per_1M_dollar, fill = econ_sector)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("Soft Drinks" = "grey",
                               "Cars" = "red",
                               "Sports" = "grey",
                               "Beers" = "grey",
                               "Chips" = "grey",
                               "Online Investments" = "grey"))

data4 %>%
  ggplot (aes (x = reorder(econ_sector, -avg_cost), y = avg_cost, fill = econ_sector)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("Soft Drinks" = "grey",
                               "Cars" = "red",
                               "Sports" = "grey",
                               "Beers" = "grey",
                               "Chips" = "grey",
                               "Online Investments" = "grey"))


data4 %>%
  ggplot (aes (x = reorder(econ_sector, -avg_views), y = avg_views, fill = econ_sector)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("Soft Drinks" = "grey",
                               "Cars" = "red",
                               "Sports" = "grey",
                               "Beers" = "grey",
                               "Chips" = "grey",
                               "Online Investments" = "grey"))









# FOCUS ON BOOLEAN PARAMETERS ---------------------------------------------

# 
data %>%
  filter (!is.na(Youtube.Views)) %>%
  group_by(Length) %>%
  summarize (views = mean (Youtube.Views)) %>%
  ggplot (aes (x= as.factor(Length), y = views)) +
  geom_point(size = 2) +
  scale_y_log10() +
  #facet_grid(~ econ_sector) +
  #scale_color_manual (values = c("red", "black"))+
  theme (axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position = "")

head(data)
