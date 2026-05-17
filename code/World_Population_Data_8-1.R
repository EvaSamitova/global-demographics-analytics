library("tidyverse")
library("ggforce")
library("maps")

# view the documentation for the data set
# note that 0 is a non-capital, 1 is a capital city, and 2 and 3 are special types of cities in China
? world.cities

# get the data for cities of the world
# this data is available via the maps package
cities <- as_tibble(world.cities)

# convert the capital column to the factor type
cities <- cities %>% mutate(capital = as.factor(capital))
cities 

# generate stats for the average population and standard deviation based on capital status
city_stats <- cities %>% group_by(capital) %>%
  summarize(MeanPop = mean(pop), SDPop = sd(pop))
city_stats

# generate a bar plot that displays the average population statistics
# additionally, add error bars and set the fill palette
# if capital was not the factor type, scale_fill_brewer() would not work
ggplot(city_stats, aes(x = capital, y = MeanPop, fill = capital)) +
  geom_col() +
  geom_errorbar(aes(x = capital, 
                    ymin = MeanPop - SDPop/2, 
                    ymax = MeanPop + SDPop/2), 
                width = 0.25) +
  scale_fill_brewer(palette = "Dark2")

# create a scatter plot comparing population and longitude
ggplot(cities, 
       aes(x = pop, y = lat, color = capital, shape = capital)) +
  geom_point(size = 2, alpha = .5)

# create a facet matrix comparing population, longitude, and latitude
ggplot(cities, aes(x = .panel_x, y = .panel_y, color = capital, shape = capital)) +
  geom_point(size = .5, alpha = .5) +
  facet_matrix(vars(pop, lat, long))

# create a facet matrix comparing population, longitude, and latitude
# three types of graphs
ggplot(cities, aes(x = .panel_x, y = .panel_y)) +
  geom_boxplot(aes(fill = capital)) +
  geom_autodensity(aes(fill = capital)) +
  geom_point(aes(color = capital, shape = capital), size = .5, alpha = .5) +
  facet_matrix(vars(pop, lat, long),
               layer.lower = 1, 
               layer.diag = 2, 
               layer.upper = 3)



# view the data for the world map
map_data("world")

# plot the world as a scatter plot
# note that it takes a little while to render
ggplot(map_data("world"), aes(x = long, y = lat)) + 
  geom_point() + 
  coord_fixed()

# plot the world as polygons. What happened?
ggplot(map_data("world"), aes(x = long, y = lat)) + 
  geom_polygon() + 
  coord_fixed()

# plot the world, but this time set group to the group column
# this keeps the country polygons individual instead of turning into one big polygon
ggplot(map_data("world"), aes(x = long, y = lat, group = group)) + 
  geom_polygon() + 
  coord_fixed()


# plot cities with large populations on the map of the world
ggplot() + 
  geom_polygon(data = map_data("world"), 
               aes(x = long, y = lat, group = group), 
               fill = "white", color = "black") + 
  geom_point(data = filter(cities, pop > 2000000), 
             aes(x = long, y = lat, shape = capital, color = capital), 
             size = 2) +
  coord_fixed()

# plot all cities on the map of the world
ggplot() + 
  geom_polygon(data = map_data("world"), 
               aes(x = long, y = lat, group = group), 
               fill = "white", color = "black") + 
  geom_point(data = cities, 
             aes(x = long, y = lat, shape = capital, color = capital, 
                 alpha = pop, size = pop)) +
  coord_fixed()

# zoom in on the Pacific Ocean to view the cities there
ggplot() + 
  geom_polygon(data = map_data("world"), 
               aes(x = long, y = lat, group = group), 
               fill = "white", color = "black") + 
  geom_point(data = cities, 
             aes(x = long, y = lat, shape = capital, color = capital, 
                 alpha = pop, size = pop)) +
  facet_zoom(xlim = c(-175,-135), ylim = c(-25,-5), zoom.size = 2)

# use coord_cartesian() instead of facet_zoom()
# customize labels and legend
ggplot() + 
  geom_polygon(data = map_data("world"), 
               aes(x = long, y = lat, group = group), 
               fill = "white", color = "black") + 
  geom_point(data = cities, 
             aes(x = long, y = lat, shape = capital, color = capital, 
                 alpha = pop, size = pop)) +
  coord_cartesian(xlim = c(-175,-135), ylim = c(-25,-5)) +
  labs(title = "Pacific Islands", x = "Longitude", y = "Latitude") +
  guides(size = guide_legend(title = "Population"), 
         alpha = guide_legend(title = "Population"),
         color = guide_legend(title = "City Type"), 
         shape = guide_legend(title = "City Type")) +
  theme(panel.background = element_rect(fill = "lightblue"))
  

# View the documentation for the theme() function
? theme

# View the documentation for the element_rect() function
? element_rect

