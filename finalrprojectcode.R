library(tidyverse)
library(ggplot2)
library(dplyr)
library(tidyr)
getwd()
setwd("C:/Users/tanvi/Downloads/spotify")
spotify_data <- read.csv("finaldataset.csv")
View(spotify_data)
#TOP 10 ARTIST BASED ON POPULARITY
popularity_by_artist <- spotify_data %>% 
  group_by(artists) %>% 
  summarise(Total_Popularity = sum(popularity)) %>% 
  arrange(desc(Total_Popularity)) %>% 
  head(10)
 #BAR CHART
ggplot(popularity_by_artist, aes(x = reorder(artists, Total_Popularity), y = Total_Popularity)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +
  labs(title = "Top 10 Artists by Popularity",
       x = "Artist Name",
       y = "Total Popularity")
ggsave("top10_artists.png", plot = last_plot(), dpi = 300)
#MOST LIKED ALBUM
likes_by_album <- spotify_data %>% 
  group_by(album_name) %>% 
  summarise(Total_Likes = sum(Likes)) %>% 
  arrange(desc(Total_Likes)) %>% 
  head(10)
head(10)
# Replace invalid characters with valid ones
likes_by_album$album_name <- iconv(likes_by_album$album_name, from = "UTF-8", to = "ASCII//TRANSLIT")
ggplot(likes_by_album, aes(x = 1, y = reorder(album_name, Total_Likes), fill = Total_Likes)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "#0d70a5", na.value = "grey90") +
  labs(title = "Most Liked Album",
       x = "",
       y = "Album Name",
       fill = "Total Likes") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
        axis.text.y = element_text(size = 12),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14),
        legend.position = "bottom")

# Calculate the count of tracks by genre
genre_counts <- spotify_data %>%
  group_by(track_genre) %>%
  summarize(count = n())
# Create pie chart
ggplot(genre_counts, aes(x = "", y = count, fill = track_genre)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Distribution of Track Genres", fill = "Genre") +
  theme(legend.position = "right")
library(ggplot2)
# Count the number of albums released in each year
album_counts <- table(spotify_data$year)
# Display the results
print(album_counts)
# Convert the album counts to a data frame
album_counts_df <- data.frame(year = as.numeric(names(album_counts)), count = as.numeric(album_counts))
library(plotly)
# Create an interactive line graph of album releases over time
plot_ly(album_counts_df, x = ~year, y = ~count, type = "scatter", mode = "lines+markers") %>%
  layout(title = "Album Releases Over Time", 
         xaxis = list(title = "Year"), 
         yaxis = list(title = "Number of Albums"), 
         plot_bgcolor = "lightgrey",
         paper_bgcolor = "lightgrey")
#library(dplyr)

# Select relevant columns from the Spotify dataset
album_data <- select(spotify_data, album_name, danceability, energy)

# Group data by album name and calculate average danceability and energy values
album_summary <- album_data %>%
  group_by(album_name) %>%
  summarize(avg_danceability = mean(danceability),
            avg_energy = mean(energy))

# Sort albums by their average danceability and energy values
top_albums <- album_summary %>%
  arrange(desc(avg_danceability), desc(avg_energy)) %>%
  head(30)
print(top_albums)
install.packages("treemap")
library(treemap)
install.packages("treemapify")
library(treemapify)

library(treemap)
library(dplyr)

# Filter top 30 albums
top_albums <- spotify_data %>%
  group_by(album_name) %>%
  summarize(mean_danceability = mean(danceability), mean_energy = mean(energy)) %>%
  top_n(30, mean_danceability * mean_energy)

# Create data frame for treemap
treemap_data <- data.frame(
  label = top_albums$album_name,
  value = top_albums$mean_danceability * top_albums$mean_energy,
  group = rep(c("Danceability", "Energy"), each = nrow(top_albums))
)

# Create treemap
treemapify(album_data, 
           area = "value",
           title = "Albums by Danceability and Energy",
           fontsize.labels = 14,
           palette = "Blues")
This will create a tree map with the top albums, where the size of the rectangles is proportional to the album's danceability * energy. The treemap will be titled "Albums by Danceability and Energy", with a font size of 14 for the labels. The palette argument specifies the color palette to use for the rectangles (in this case, "Blues").




tanvibhardwaj306@gmail.com
code to display  treemap of top 30 albums based on danceability and energy using spotify_data
Here's a code to display a treemap of the top 30 albums based on danceability and energy using the spotify_data dataset:
  
  scss
Copy code
library(treemap)
library(dplyr)

# Filter top 30 albums
top_albums <- spotify_data %>%
  group_by(album_name) %>%
  summarize(mean_danceability = mean(danceability), mean_energy = mean(energy)) %>%
  top_n(30, mean_danceability * mean_energy)

# Create data frame for treemap
treemap_data <- data.frame(
  label = top_albums$album_name,
  value = top_albums$mean_danceability * top_albums$mean_energy,
  group = rep(c("Danceability", "Energy"), each = nrow(top_albums))
)

# Create treemap
treemap(treemap_data,
        index = "group",
        vSize = "value",
        type = "index",
        title = "Top 30 Albums by Danceability and Energy")
This code first filters the spotify_data dataset to obtain the top 30 albums based on their danceability and energy scores. It then creates a data frame in the format required for the treemap function, with album names as labels, the product of danceability and energy scores as values, and "Danceability" and "Energy" as group labels. Finally, the treemap function is called with the appropriate arguments to create the treemap.

treemapify(album_data, 
           area = "value",
           title = "Albums by Danceability and Energy",
           fontsize.labels = 14,
           palette = "Blues")





library(treemap)
library(dplyr)

# Filter top 30 albums
top_albums <- spotify_data %>%
  group_by(album_name) %>%
  summarize(mean_danceability = mean(danceability), mean_energy = mean(energy)) %>%
  top_n(30, mean_danceability * mean_energy)

# Create data frame for treemap
treemap_data <- data.frame(
  label = top_albums$album_name,
  value = top_albums$mean_danceability * top_albums$mean_energy,
  group = rep(c("Danceability", "Energy"), each = nrow(top_albums))
)

# Create treemap
treemap(treemap_data,
        index = "group",
        vSize = "value",
        type = "index",
        title = "Top 30 Albums by Danceability and Energy")
This code first filters the spotify_data dataset to obtain the top 30 albums based on their danceability and energy scores. It then creates a data frame in the format required for the treemap function, with album names as labels, the product of danceability and energy scores as values, and "Danceability" and "Energy" as group labels. Finally, the treemap function is called with the appropriate arguments to create the treemap.

treemapify(album_data, 
           area = "value",
           title = "Albums by Danceability and Energy",
           fontsize.labels = 14,
           palette = "Blues")
This will create a tree map with the top albums, where the size of the rectangles is proportional to the album's danceability * energy. The treemap will be titled "Albums by Danceability and Energy", with a font size of 14 for the labels. The palette argument specifies the color palette to use for the rectangles (in this case, "Blues").




tanvibhardwaj306@gmail.com
code to display  treemap of top 30 albums based on danceability and energy using spotify_data
Here's a code to display a treemap of the top 30 albums based on danceability and energy using the spotify_data dataset:
  
  scss
Copy code
library(treemap)
library(dplyr)

# Filter top 30 albums
top_albums <- spotify_data %>%
  group_by(album_name) %>%
  summarize(mean_danceability = mean(danceability), mean_energy = mean(energy)) %>%
  top_n(30, mean_danceability * mean_energy)

# Create data frame for treemap
treemap_data <- data.frame(
  label = top_albums$album_name,
  value = top_albums$mean_danceability * top_albums$mean_energy,
  group = rep(c("Danceability", "Energy"), each = nrow(top_albums))
)

# Create treemap
treemap(treemap_data,
        index = "group",
        vSize = "value",
        type = "index",
        title = "Top 30 Albums by Danceability and Energy")
This code first filters the spotify_data dataset to obtain the top 30 albums based on their danceability and energy scores. It then creates a data frame in the format required for the treemap function, with album names as labels, the product of danceability and energy scores as values, and "Danceability" and "Energy" as group labels. Finally, the treemap function is called with the appropriate arguments to create the treemap.

treemapify(album_data, 
           area = "value",
           title = "Albums by Danceability and Energy",
           fontsize.labels = 14,
           palette = "Blues")



library(treemap)
library(dplyr)

# Filter top 30 albums
top_albums <- spotify_data %>%
  group_by(album_name) %>%
  summarize(mean_danceability = mean(danceability), mean_energy = mean(energy)) %>%
  top_n(30, mean_danceability * mean_energy)

# Create data frame for treemap
treemap_data <- data.frame(
  label = top_albums$album_name,
  value = top_albums$mean_danceability * top_albums$mean_energy,
  group = rep(c("Danceability", "Energy"), each = nrow(top_albums))
)

# Create treemap
treemap(treemap_data,
        index = "group",
        vSize = "value",
        type = "index",
        title = "Top 30 Albums by Danceability and Energy")
library(treemapify)

# Subset the data to the top 30 most enjoyable albums based on danceability and energy
top_albums <- spotify_data %>%
  
  group_by(album_name) %>%
  summarize(mean_danceability = mean(danceability), 
            mean_energy = mean(energy),
            mean_popularity = mean(popularity)) %>%
  top_n(30, mean_danceability + mean_energy)


# Highlight the most liked album based on popularity

library(treemapify)                        
# Create the treemap plot
# Create the treemap plot
treemapify(top_albums,
           area = "mean_danceability",
           title = "Top 30 Most Enjoyable Albums",
           palette = "Blues",
           asp = 0.7,
           border.col = "white",
           fontsize.title = 20,
           fontsize.labels = 12,
           type = "index",
           bg.labels = "white",
           highlight.border.col = "black",
           highlight.col = top_albums$album_name[top_albums$most_liked == TRUE])
           















