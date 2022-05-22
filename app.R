library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(ggrepel)
library(readr)
library(stringr)
library(skimr)
library(scales)
library(shiny)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(maps)
library(rgdal)
options(scipen=10000)

#read in beer data, as well as world/north america data to draw maps 
beer_states <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/beer_states.csv')
world <- ne_countries(scale = "medium", returnclass = "sf", continent = "north america")
na <- ne_countries(continent = 'north america')

#lakes and states from https://stackoverflow.com/questions/68219487/how-to-make-the-great-lakes-the-same-color-as-the-ocean-in-r
lakes <- rnaturalearth::ne_download(scale = 110, 
                                    type = 'lakes', 
                                    category = 'physical') %>% 
  sf::st_as_sf(lakes110, crs = 4269)

usa_states <- st_as_sf(maps::map("state", 
                                 fill=TRUE, 
                                 plot =FALSE),
                       crs = 4269)

#removes Washington DC from drawn states
usa_states = usa_states %>% 
  filter(ID != "district of columbia")

#Here only bottles and cans are looked at and not other alcohol production 
beer_states = beer_states %>% 
  mutate(Year = year) %>% 
  group_by(state, Year) %>% 
  filter(type == "Bottles and Cans") %>% 
  select(-year)

#total, alasks, hawaii, and DC are removed from the beer data 
beer_states = beer_states %>% 
  filter(state != "total") %>% 
  filter(state != "AK") %>% 
  filter(state != "HI") %>% 
  filter(state != "DC") 

#regions were manually created as a way to encode the region each state belongs to (e.g., WI = Midwest)
regions = c("Southern", "Southern", "Southwest", "Pacific Coastal", "Rocky Mountains",
            "New England", "Southern", "Southern", "Southern", "Midwest",
            "Rocky Mountains",
            "Midwest", "Midwest", "Southern", "Southern",
            "Southern", "New England", "Southern", "New England",
            "Midwest", "Midwest",
            "Midwest", "Southern", "Rocky Mountains", "Southern",
            "Midwest", "Midwest", "New England", "Mid-Atlantic", "Southwest",
            "Rocky Mountains", "Mid-Atlantic", "Midwest", "Southwest", "Pacific Coastal",
            "Mid-Atlantic", "New England", "Southern", "Midwest", "Southern",
            "Southwest", "Rocky Mountains", "Southern", "New England",
            "Pacific Coastal", "Midwest", "Southern", "Rocky Mountains")

#since the data repeats differently than Alabama - Wyoming, aa function needed to be called to repeat each region object 12x to match up with the full dataset 
regions_repeated = c()
for (i in 1:48) {
  if (i == 1) {
    regions_repeated[i:(i*12)] = regions[i]
  } else { 
    regions_repeated[(((i-1)*12)+1):(i*12)] = regions[i]
  }}

#this is the state order that comes from the US states map data
state_order = c("AL", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "ID", "IL", 
                "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", 
                "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", 
                "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY")

#then these repeated regions were appended to the beer data 
beer_states$region = regions_repeated

#this function is a way for users to select the year they want, then match up the beer data state order to the usa state map order 
filter_states = function(year) {
  beer_states %>% 
    filter(Year == year) %>% 
    mutate(state = factor(state, levels = state_order)) %>% 
    arrange(state)
}

#this function then appends the total beer produced in a year, for a state, to the us map dataa 
usa_states_filtered = function(year) {
  barrels = filter_states(year)$barrels
  usa_states %>% 
    mutate(total = barrels)
}

#this function draws the US map and fills in each state by the total beer produced in that year 
beerStatesPlotter = function(year) {
  ggplot(data = world) +
    geom_sf() +
    geom_sf(data = lakes,
            mapping = aes(geometry = geometry),
            color = "grey",
            fill = "grey") +
    geom_sf(data = usa_states,
            mapping = aes(geometry = geom, fill = usa_states_filtered(year)$total),
            color = "black") +
    labs(x = "Longitude", y = "Latitude", fill ="Total Beer Sales") +
    scale_fill_gradient(low = "white", high = "yellow", name = "Beer Cans/Bottles Produced", labels = comma) + 
    labs(
      title = paste0("United States Beer Production in ", year)) +
    coord_sf(xlim = c(-125, -65), ylim = c(25, 50)) +
    theme_bw() 
}

#this function makes the top-15 states, in terms of beer production, for a given year 
bar_fun = function(year) {
  beer_states %>% 
    filter(Year == year) %>% 
    arrange(desc(barrels)) %>% 
    head(n = 15) %>% 
    ggplot() +
    geom_bar(aes(barrels, reorder(state, barrels), fill = region), stat = "identity", position = "dodge") +
    theme_bw() + 
    theme(panel.grid.major.y = element_blank()) + 
    scale_x_continuous(labels = label_number(suffix = "M", scale = 1e-6), expand = c(0,0)) + 
    labs(
      x = "Number of Barrels Produced",
      y = "State",
      fill = "US Region",
      title = paste0("Top 15 States in Beer Can/Bottle Production During ", year)) 
}

#this is the created UI, with a description and instructions 
ui <- fluidPage(title = "Beer Production in America", 
                tags$div(h4("Below is a bar plot representing the top 15 states in beer can or bottle production in a given year. The other plot is an area map of the United States, where dark yellow states indicate higher beer can or bottle production in a given year. Grey states indicate no production."),
                         h5("Instructions: In the dropdown below, feel free to select any year of interest to see how the the plots change"), br()),
  selectInput("Year", "Select Year", 2009:2019),
  fluidRow(
    column(6, plotOutput("bar")),
    column(6, plotOutput("us"))
  )
)

#this is the server funciton
server <- function(input, output) {
  output$bar <- renderPlot(bar_fun(input$Year))
  output$us = renderPlot(beerStatesPlotter(input$Year))
}

shinyApp(ui, server)
