# Plotly examples


library(plotly)
plot_ly(z = ~volcano)


p <- ggplot(txhousing, aes(date, median)) +
  geom_line(aes(group = city), alpha = 0.2)
p


subplot(
  p, ggplotly(p, tooltip = "city"), 
  ggplot(txhousing, aes(date, median)) + geom_bin2d(),
  ggplot(txhousing, aes(date, median)) + geom_hex(),
  nrows = 2, shareX = TRUE, shareY = TRUE,
  titleY = FALSE, titleX = FALSE
)


tx <- group_by(txhousing, city)
# initiate a plotly object with date on x and median on y
p <- plot_ly(tx, x = ~date, y = ~median)
# plotly_data() returns data associated with a plotly object
plotly_data(p)
p

add_lines(
  # plots one line per city since p knows city is a grouping variable
  add_lines(p, alpha = 0.2, name = "Texan Cities", hoverinfo = "none"),
  name = "Houston", data = filter(txhousing, city == "Houston")
)

allCities <- txhousing %>%
  group_by(city) %>%
  plot_ly(x = ~date, y = ~median) %>%
  add_lines(alpha = 0.2, name = "Texan Cities", hoverinfo = "none")

allCities %>%
  filter(city == "Houston") %>%
  add_lines(name = "Houston")

allCities %>%
  add_fun(function(plot) {
    plot %>% filter(city == "Houston") %>% add_lines(name = "Houston")
  }) %>%
  add_fun(function(plot) {
    plot %>% filter(city == "San Antonio") %>% 
      add_lines(name = "San Antonio")
  })


# reusable function for highlighting a particular city
layer_city <- function(plot, name) {
  plot %>% filter(city == name) %>% add_lines(name = name)
}

# reusable function for plotting overall median & IQR
layer_iqr <- function(plot) {
  plot %>%
    group_by(date) %>% 
    summarise(
      q1 = quantile(median, 0.25, na.rm = TRUE),
      m = median(median, na.rm = TRUE),
      q3 = quantile(median, 0.75, na.rm = TRUE)
    ) %>%
    add_lines(y = ~m, name = "median", color = I("black")) %>%
    add_ribbons(ymin = ~q1, ymax = ~q3, name = "IQR", color = I("black"))
}

allCities %>%
  add_fun(layer_iqr) %>%
  add_fun(layer_city, "Houston") %>%
  add_fun(layer_city, "San Antonio")





library(forecast)
layer_forecast <- function(plot) {
  d <- plotly_data(plot)
  series <- with(d, 
                 ts(median, frequency = 12, start = c(2000, 1), end = c(2015, 7))
  )
  fore <- forecast(ets(series), h = 48, level = c(80, 95))
  plot %>%
    add_ribbons(x = time(fore$mean), ymin = fore$lower[, 2],
                ymax = fore$upper[, 2], color = I("gray95"), 
                name = "95% confidence", inherit = FALSE) %>%
    add_ribbons(x = time(fore$mean), ymin = fore$lower[, 1],
                ymax = fore$upper[, 1], color = I("gray80"), 
                name = "80% confidence", inherit = FALSE) %>%
    add_lines(x = time(fore$mean), y = fore$mean, color = I("blue"), 
              name = "prediction")
}

txhousing %>%
  group_by(city) %>%
  plot_ly(x = ~date, y = ~median) %>%
  add_lines(alpha = 0.2, name = "Texan Cities", hoverinfo = "none") %>%
  add_fun(layer_iqr) %>%
  add_fun(layer_forecast)


p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point() + geom_smooth()
p %>%
  ggplotly(layerData = 2, originalData = FALSE) %>%
  plotly_data()

p %>%
  ggplotly(layerData = 2, originalData = F) %>%
  add_fun(function(p) {
    p %>% slice(which.max(se)) %>%
      add_segments(x = ~x, xend = ~x, y = ~ymin, yend = ~ymax) %>%
      add_annotations("Maximum uncertainty", ax = 60)
  }) %>%
  add_fun(function(p) {
    p %>% slice(which.min(se)) %>%
      add_segments(x = ~x, xend = ~x, y = ~ymin, yend = ~ymax) %>%
      add_annotations("Minimum uncertainty")
  })



p <- ggplot(diamonds, aes(cut, fill = clarity)) +
  geom_bar(position = "fill")

ggplotly(p, originalData = FALSE) %>%
  mutate(ydiff = ymax - ymin) %>% 
  add_text(
    x = ~x, y = ~1 - (ymin + ymax) / 2,
    text = ~ifelse(ydiff > 0.02, round(ydiff, 2), ""),
    showlegend = FALSE, hoverinfo = "none",
    color = I("black"), size = I(9)
  )



p <- ggplot(MASS::geyser, aes(x = waiting, y = duration)) +
  geom_density2d()

ggplotly(p, originalData = FALSE) %>% 
  group_by(piece) %>%
  slice(which.min(y)) %>% 
  add_text(
    text = ~level, size = I(9), color = I("black"), hoverinfo = "none"
  )

subplot(
  plot_ly(mpg, x = ~cty, y = ~hwy, name = "default"),
  plot_ly(mpg, x = ~cty, y = ~hwy) %>% 
    add_markers(alpha = 0.2, name = "alpha"),
  plot_ly(mpg, x = ~cty, y = ~hwy) %>% 
    add_markers(symbol = I(1), name = "hollow")
)

p <- plot_ly(mpg, x = ~cty, y = ~hwy, alpha = 0.3) 
subplot(
  add_markers(p, symbol = ~cyl, name = "A single trace"),
  add_markers(p, symbol = ~factor(cyl), color = I("black"))
)


p <- plot_ly(mpg, x = ~cty, y = ~hwy, alpha = 0.5)
subplot(
  add_markers(p, color = ~cyl, showlegend = FALSE) %>% 
    colorbar(title = "Viridis"),
  add_markers(p, color = ~factor(cyl))
)


library(GGally)
pm <- GGally::ggpairs(iris)
ggplotly(pm)


library(maps)
dat <- map_data("world", "canada") %>% group_by(group)

map1 <- plot_mapbox(dat, x = ~long, y = ~lat) %>% 
  add_paths(size = I(2)) %>%
  add_segments(x = -100, xend = -50, y = 50, 75) %>%
  layout(mapbox = list(
    zoom = 0,
    center = list(lat = ~median(lat), lon = ~median(long))
  ))


p <- plot_ly(data, x = ~x.data, y = ~y.data, text = ~text.data, type = 'scatter', 
             mode = 'markers', marker = list(size = ~size.data, opacity= 0.5)) %>%
  layout(shapes=list(type='line', x0= 0.2, x1= 0.2, y0=min(allyvalues), y1=max(allyvalues), line=list(dash='dot', width=1)),
         title = 'This is the Title',
         xaxis = list(title = "X-Axis", showgrid = TRUE),
         yaxis = list(title = "Y-Axis", showgrid = TRUE))
p



