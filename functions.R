
# Define a function for creating a crazy looking map
crazy_map <- function(){
  # Add some colors
  lots_of_colors <- c(rainbow(100), grey(seq(0.001, 0.999, length = 100)))
  ont_crazy_colors <- sample(lots_of_colors, nrow(ont_crazy))
  plot(ont_crazy, col = ont_crazy_colors)
}


# Function for a time chart
time_chart <- function(x,y,
                       ylab = '',
                       fill = TRUE){
  require(ggplot2)
  df <- data.frame(x,y)
  g <-
    ggplot(data = df,
           aes(x = x,
               y = y)) +
    geom_line(alpha = 0.6,
              color = '#0d63c4') +
    geom_point(alpha = 0.6,
               color = '#0d63c4') +
    theme_databrew() +
    labs(x = 'Date',
         y = ylab)
  if(fill){
    g <-
      g +
      geom_area(fill = '#0d63c4',
                alpha = 0.3)
  }
  return(g)
}

# Function for map
ontario_map <- function(x){
  require(dplyr)
  require(ggplot2)
  # This function expects "x" to be a dataframe with a column named "geography"
  # and another named "value"
  # Keep only the numbered values
  right <- x %>%
    filter(!is.na(as.numeric(geography))) %>%
    mutate(geography = substr(geography, 3,4))
  # join to ont fortified
  shp <- ont_fortified
  shp <- shp %>%
    left_join(right,
              by = 'geography')
  # Make a plot
  g <-
    ggplot(data = shp,
           aes(x = long,
               y = lat,
               group = group,
               fill = value)) +
    geom_polygon() +
    coord_map() +
    theme_databrew() +
    scale_fill_continuous(low = 'lightblue', high = 'darkorange', name = '', na.value = 'white') +
    theme(legend.text = element_text(size = 7),
          legend.position = 'right') +
    labs(x = '',
         y = '')
  return(g)
}

# # Example
# df <- age_sex %>%
#   filter(year == 2011,
#          age_sex == 'total_15_up') %>%
#   group_by(geography) %>%
#   summarise(value = sum(value))
# ontario_map(x = df)

# Define function for generating a leaflet plot
leaf <- function(x, tile = 'Stamen.Toner', palette = 'YlOrRd',
                 show_legend = TRUE){
  require(dplyr)
  require(leaflet)
  require(RColorBrewer)
  # This function expects "x" to be a dataframe with a column named "geography"
  # and another named "value"
  # Keep only the numbered values
  right <- x %>%
    filter(!is.na(as.numeric(geography))) %>%
    mutate(geography = substr(geography, 3,4))
  # join to ont shapefile
  shp <- ont2
  shp@data <- shp@data %>%
    mutate(geography = CCA_2) %>%
    left_join(right,
              by = 'geography')

  # Create a color palette
  # pal <- colorQuantile("Blues", NULL, n = 9)
  # bins <- round(c(quantile(shp@data$value, na.rm = TRUE), Inf))
  bins <- round(c(quantile(shp@data$value, na.rm = TRUE)))
  pal <- colorBin(palette, domain = shp@data$value, bins = bins)

  # Create a popup
  popper <- paste0(shp@data$NAME_2, ': ',
                   shp@data$value)

  # Create map
  l <- leaflet(data = shp) %>%
    addProviderTiles(tile)
  if(show_legend){
    l <- l %>%
      addLegend(pal = pal, values = ~value, opacity = 0.7, title = NULL,
                position = "bottomleft")
  }
  l <- l %>%
    addPolygons(fillColor = ~pal(value),
                fillOpacity = 0.8,
                color = "#BDBDC3",
                weight = 1,
                # popup = popper,
                highlight = highlightOptions(
                  weight = 5,
                  color = "#666",
                  dashArray = "",
                  fillOpacity = 0.7,
                  bringToFront = TRUE),
                label = popper,
                labelOptions = labelOptions(
                  style = list("font-weight" = "normal", padding = "3px 8px"),
                  textsize = "15px",
                  direction = "auto"))
  return(l)
}


# Clean up special indicators
si <- special_indicators_by_vismin_2006
names(si)[2:ncol(si)] <- unlist(lapply(strsplit(names(si)[2:ncol(si)], ' '), function(x){paste0(x[1:(length(x) - 1)], collapse = ' ')}))
