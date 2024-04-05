install.packages("tidyverse")
install.packages("quarto")
install.packages("plotly")

library(plotly)
library(tidyverse)
unicef_indicator_1 <- read_csv("unicef_indicator_1.csv")
unicef_metadata <- read_csv("unicef_metadata.csv")
unicef_indicator_2 <- read_csv("unicef_indicator_2.csv")
continents <- read_csv("continents.csv")

data_join1 <- full_join(unicef_indicator_1, unicef_metadata, by = join_by(country, alpha_2_code, alpha_3_code,))

data_join2 <- full_join(unicef_indicator_2, unicef_metadata, by = join_by(country, alpha_2_code, alpha_3_code,))

data_join3 <- full_join(unicef_indicator_1, unicef_indicator_2, by = join_by(country, alpha_2_code, alpha_3_code,))

data_join4 <- unicef_metadata %>%
  full_join(unicef_indicator_1, by = c("country")) %>%
  full_join(unicef_indicator_2, by = c("country"))

install.packages("scales")
library(scales)

map_world <- map_data("world")

# Create a custom color scale
color_scale <- scale_fill_gradientn(
  colors = c("darkred", "red", "coral"),  # Define your custom colors
  values = rescale(c(0, 40, 60, 85)),  # Define the breaks for the color scale
  limits = c(0, 85),  # Set the overall limits for the indicator values
  name = "Indicator"
)

# Anaemia Map #
map_data_join <- full_join(unicef_indicator_1, map_world, by = c("country" = "region"))

# Create the map with the custom color scale
map_Object <- ggplot(map_data_join, aes(long, lat, group = group, fill = obs_value)) +
  geom_polygon(color = "black") +
  color_scale + 
  labs(x = "", y = "", title = "Anaemia Infection Rates") +
  theme(text = element_text(family = "Times New Roman"))

ggplotly(map_Object)

# TimeSeries #

data_join_Continents <- full_join(continents, unicef_metadata, by = join_by(country))

timeseries_plot_1 <- data_join_Continents %>%
  na.omit() %>%
  ggplot() +
  aes(time_period, lifeExp, group = country, color = continent) +
  geom_line() +
  scale_color_manual(values = c("Asia" = "blue", 
                                "Europe" = "yellow",
                                "Africa" = "red",
                                "Americas" ="orange",
                                "Oceania" = "green")) + 
  labs(x = "Life Expectancy", y = "Year", title = "Evolution of Life Expectancy") +
  theme_classic() + 
  theme(text = element_text(family = "Times New Roman"))


ggplotly(timeseries_plot_1)
  
# Scatter Plot #

data_scatterplot_1 <- data_join_Continents %>%
  na.omit()%>%
  ggplot() +
  aes(gdp, lifeExp, color = continent, size = population) +
  geom_point(alpha = 0.5) +
  facet_wrap(~ continent) +
  scale_color_manual(values = c("Asia" = "blue", 
                                "Europe" = "yellow",
                                "Africa" = "red",
                                "Americas" ="orange",
                                "Oceania" = "green")) +
  scale_x_continuous(limits = c(0, 75000), breaks = c(25000, 50000, 75000), labels = scales::unit_format(unit = "K", scale = 0.001)) + 
  labs(x = "GDP per Capita in USD", y = "Life Expectancy at Birth", title = "Continental Correlation Between Life Expectancy and GDP") +
  theme_light() + 
  theme(text = element_text(family = "Times New Roman"))

ggplotly(data_scatterplot_1)


# Bar Chart 2021 #

data_2021 <- subset(data_join_Continents, time_period == 2021)

total_population <- aggregate(population ~ continent, data = data_2021, FUN = sum)

BarChart_2021 <- ggplot(total_population, aes(x = reorder(continent,population), y = population, fill = continent)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Population by Continent in 2021",
       x = "Continent", y = "Total Population")+
  scale_y_continuous(labels = scales::comma)+
  theme_classic() + 
  theme(text = element_text(family = "Times New Roman"))

ggplotly(BarChart_2021)

# Bar Chart 1961 #

data_1961 <- subset(data_join_Continents, time_period == 1961)

total_population_1961 <- aggregate(population ~ continent, data = data_1961, FUN = sum)

BarChart_1961 <- ggplot(total_population_1961, aes(x = reorder(continent,population), y = population, fill = continent)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Population by Continent in 1961",
       x = "Continent", y = "Total Population")+
  scale_y_continuous(labels = scales::comma)+
  theme_classic() + 
  theme(text = element_text(family = "Times New Roman"))

ggplotly(BarChart_1961)
