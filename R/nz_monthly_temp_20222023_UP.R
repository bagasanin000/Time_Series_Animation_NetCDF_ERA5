# SET WORKING DIRECTORY

setwd("your_working_dir")
main_dir <- getwd()
dir.create("weather")
weather_dir_path <- main_dir |>
  paste0("/", "weather")
setwd(weather_dir_path)

# INSTALL AND LOAD LIBRARIES

install.packages("devtools")
devtools::install_github("https://github.com/ErikKusch/KrigR")

# to download the data
library(KrigR)
# to manipulate the data
library(sf)
library(giscoR)
library(tidyverse)
library(tidyr)
# to visualize th data
library(classInt)
library(RColorBrewer)
library(gganimate)
library(gifski)

# SET ROI

nz_sf <- giscoR::gisco_get_countries(
  country = "NZ",
  resolution = "10"
)

# DOWNLOAD THE CLIMATE DATA

start_date <- "2022-09-30"
end_date <- "2023-09-30"

my_api <- *****
my_key <- ("***********************")

nz_temp <- KrigR::download_ERA(
  Variable = "2m_temperature",
  DataSet = "era5-land",
  DateStart = start_date,
  DateStop = end_date,
  TResolution = "month",
  TStep = 1,
  Dir = weather_dir_path,
  FileName = "NZ_2m_temperature_20222023",
  Extent = as(nz_sf, "Spatial"),
  API_User = my_api,
  API_Key = my_key
)

head(nz_temp)

#test plot
nz_temp[["X2022.11.30"]] |>
  as.data.frame(xy = T, na.rm = T) |>
  ggplot() +
  geom_tile(aes(x = x, y = y, fill = X2022.11.30)) +
  coord_sf() +
  scale_fill_viridis_c(option = "plasma") +
  theme_void()

# NC TO DF

nz_temp_df <- as.data.frame(
  nz_temp,
  xy = T, na.rm = T
)

head(nz_temp_df)

nz_temp_long <- nz_temp_df |>
  tidyr::pivot_longer(
    !c(x, y),
    names_to = "layer",
    values_to = "value"
  )

head(nz_temp_long)

# GET DATES

nz_temp_long$datum <- sub(
  ".", "", as.character(nz_temp_long$layer)
)

head(nz_temp_long)

# run it twice to remove all of the dot (.)
nz_temp_long$datum <- str_replace(nz_temp_long$datum, "[^[:alnum:]]" ,"")
nz_temp_long$datum <- str_replace(nz_temp_long$datum, "[^[:alnum:]]" ,"")

# change value to Date format
nz_temp_long$datum <- as.Date(nz_temp_long$datum, "%Y%m%d")

# drop some column and create celcius column
nz_temp_dates <- nz_temp_long |>
  dplyr::mutate(
    celsius = value - 273.15
  ) |>
  dplyr::select(
    -layer, -value
  )

head(nz_temp_dates)

# TEST CHART

ggplot(nz_temp_dates, aes(datum, celsius)) +
  geom_smooth() +
  theme_bw() +
  xlab("") +
  ylab("Temp (Celsius)")

# BREAKS

vmin <- min(nz_temp_dates$celsius)
vmax <- max(nz_temp_dates$celsius)

breaks <- classInt::classIntervals(
  nz_temp_dates$celsius,
  n = 14,
  style = "pretty"
)$brks

# COLOR

cols <- colorRampPalette(rev(RColorBrewer::brewer.pal(
  11, "Spectral"
)))

# MAP

nz_map <- ggplot(nz_temp_dates) +
  geom_tile(aes(x = x, y = y, fill = celsius)) +
  facet_wrap(~datum) +
  scale_fill_gradientn(
    name = "Celsius Degree",
    colours = cols(15),
    limits = c(vmin, vmax),
    breaks = breaks
  ) +
  guides(
    fill = guide_legend(
      direction = "horizontal",
      keyheight = unit(1.1, units = "mm"),
      keywidth = unit(10, units = "mm"),
      title.position = "top",
      label.position = "bottom",
      title.hjust = .5,
      label.hjust = .5,
      nrow = 1,
      byrow = T
    )
  ) +
  theme_minimal() +
  theme(
    axis.line = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "top",
    legend.title = element_text(
      size = 11, color = "grey10"
    ),
    legend.text = element_text(
      size = 10, color = "grey10"
    ),
    plot.title = element_text(
      size = 16, color = "grey10",
      hjust = .5, vjust = -2
    ),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = unit(
      c(t = 1, r = 0, l = 0, b = 0), "lines"
    )
  ) +
  labs(
    x = "",
    y = "",
    title = "Monthly Temperature in New Zealand"
  )

print(nz_map)

ggsave(
  "newzealand_monthly_temp_2223.png",
  plot = nz_map,
  device = "png",
  path = "output_folder",
  scale = 1,
  width = 800,
  height = 1280,
  units = c("px"),
  dpi = 144,
  limitsize = TRUE,
  bg = "white",
)

# ANIMATE GIF

nz_map <- ggplot(nz_temp_dates) +
  geom_tile(aes(x = x, y = y, fill = celsius)) +
  scale_fill_gradientn(
    name = "Celsius Degree",
    colours = cols(15),
    limits = c(vmin, vmax),
    breaks = breaks
  ) +
  guides(
    fill = guide_legend(
      direction = "horizontal",
      keyheight = unit(1.5, units = "mm"),
      keywidth = unit(10, units = "mm"),
      title.position = "top",
      label.position = "bottom",
      title.hjust = .5,
      label.hjust = .5,
      nrow = 1,
      byrow = T
    )
  ) +
  theme_minimal() +
  theme(
    axis.line = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(
      size = 11, color = "grey10"
    ),
    legend.text = element_text(
      size = 10, color = "grey10"
    ),
    plot.title = element_text(
      size = 20, color = "grey10",
      hjust = .5, vjust = -2
    ),
    plot.subtitle = element_text(
      size = 30, color = "#c43c4e",
      hjust = .5, vjust = -2
    ),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = unit(
      c(t = 1, r = 0, l = 0, b = 0), "lines"
    )
  ) +
  labs(
    x = "",
    y = "",
    title = "Monthly Temperature in New Zealand",
    subtitle = "{as.Date(frame_time)}"
  )

timelapse_nz_map <- nz_map +
  transition_time(time = as.Date(nz_temp_long$datum)) +
  enter_fade() +
  exit_fade() +
  ease_aes("linear", interval = .1)

animated_temp_map <- gganimate::animate(
  timelapse_nz_map,
  nframes = 65,
  duration = 20,
  start_pause = 3,
  end_pause = 10,
  height = 1280,
  width = 800,
  res = 144,
  fps = 10,
  renderer = gifski_renderer(loop = T)
)

gganimate::anim_save(
  "newzealand_temperature_20222023.gif", animated_temp_map,
  path = "output_folder"
)
