
library(pacman)
p_load(tidyverse,sf,tmap)


fahr_to_kelvin <- function(temp) {
  kelvin <- ((temp - 32) * (5 / 9)) + 273.15
  return(kelvin)
}

kelvin_to_celsius <- function(temp) {
  celsius <- temp - 273.15
  return(celsius)
}

32 %>%
  fahr_to_kelvin() %>%
  kelvin_to_celsius()



conv_temp <- function(temp, option) {
  if (option == "f2k") {
    kelvin <- ((temp - 32) * (5 / 9)) + 273.15
    return(kelvin)
  } else if (option == "k2c") {
    celsius <- temp - 273.15
    return(celsius)
  } else {
    print("Wrong option input")
  }
}


conv_temp(32, "f2k")



conv_temp(100, "unknown")


fahr_to_kelvin <- function(temp) {
  if (!is.numeric(temp)) {
    stop("temp must be a numeric vector.")
  }
  kelvin <- ((temp - 32) * (5 / 9)) + 273.15
  return(kelvin)
}



fahr_to_kelvin <- function(temp) {
  stopifnot(is.numeric(temp))
  kelvin <- ((temp - 32) * (5 / 9)) + 273.15
  return(kelvin)
}


# metric is a factor instead of numeric
fahr_to_kelvin(temp = as.factor(32))




#simulation models
#basic predator/prey model
f_func <- function(P, a, b) {
  a - b * P
}

g_func <- function(H, c, d) {
  c * H - d
}

n_iter <- 500
pop_tbl <- tibble(
  time = 0:n_iter,
  lynx = c(1.1, rep(NA, n_iter)),
  hare = c(1.1, rep(NA, n_iter))
)
for (i in 1:n_iter) {
  pop_tbl$lynx[i + 1] <- pop_tbl$lynx[i] + pop_tbl$lynx[i] * f_func(pop_tbl$hare[i], a = 0.01, b = 0.01)
  pop_tbl$hare[i + 1] <- pop_tbl$hare[i] + pop_tbl$hare[i] * g_func(pop_tbl$lynx[i], c = 0.01, d = 0.01)
}
#Create a plot to show the population sizes of hare (x) and lynx (y) over time.

ggplot(pop_tbl, aes(x = time)) +
  geom_line(aes(y = lynx), col = "blue") +
  geom_line(aes(y = hare), col = "red")


ggplot(pop_tbl, aes(lynx, hare)) +
  geom_path()



###PRACTICE
#time=1000
n_iter <- 1000
pop_tbl <- tibble(
  time = 0:n_iter,
  lynx = c(1.1, rep(NA, n_iter)),
  hare = c(1.1, rep(NA, n_iter))
)
for (i in 1:n_iter) {
  pop_tbl$lynx[i + 1] <- pop_tbl$lynx[i] + pop_tbl$lynx[i] * f_func(pop_tbl$hare[i], a = 0.01, b = 0.01)
  pop_tbl$hare[i + 1] <- pop_tbl$hare[i] + pop_tbl$hare[i] * g_func(pop_tbl$lynx[i], c = 0.01, d = 0.01)
}


ggplot(pop_tbl, aes(x = time)) +
  geom_line(aes(y = lynx), col = "blue") +
  geom_line(aes(y = hare), col = "red")


ggplot(pop_tbl, aes(lynx, hare)) +
  geom_path()

#time=9000
n_iter <- 9000
pop_tbl <- tibble(
  time = 0:n_iter,
  lynx = c(1.1, rep(NA, n_iter)),
  hare = c(1.1, rep(NA, n_iter))
)
for (i in 1:n_iter) {
  pop_tbl$lynx[i + 1] <- pop_tbl$lynx[i] + pop_tbl$lynx[i] * f_func(pop_tbl$hare[i], a = 0.01, b = 0.01)
  pop_tbl$hare[i + 1] <- pop_tbl$hare[i] + pop_tbl$hare[i] * g_func(pop_tbl$lynx[i], c = 0.01, d = 0.01)
}


ggplot(pop_tbl, aes(x = time)) +
  geom_line(aes(y = lynx), col = "blue") +
  geom_line(aes(y = hare), col = "red")


ggplot(pop_tbl, aes(lynx, hare)) +
  geom_path()

#play w/ starting conditions
n_iter <- 9000
pop_tbl <- tibble(
  time = 0:n_iter,
  lynx = c(1.02, rep(NA, n_iter)),
  hare = c(1.01, rep(NA, n_iter))
)
for (i in 1:n_iter) {
  pop_tbl$lynx[i + 1] <- pop_tbl$lynx[i] + pop_tbl$lynx[i] * f_func(pop_tbl$hare[i], a = 0.02, b = 0.01)
  pop_tbl$hare[i + 1] <- pop_tbl$hare[i] + pop_tbl$hare[i] * g_func(pop_tbl$lynx[i], c = 0.01, d = 0.01)
}

ggplot(pop_tbl, aes(x = time)) +
  geom_line(aes(y = lynx), col = "blue") +
  geom_line(aes(y = hare), col = "red")




#improve path graph
ggplot(pop_tbl, aes(lynx, hare)) +
  geom_path(aes(color=time))



#a model w/ carrying capacity
f_k_func <- function(H, P, a, b, K = 10) {
  a * (1 - H / K) - b * P
}

pop_k_tbl <- tibble(
  time = 0:n_iter,
  lynx = c(1.1, rep(NA, n_iter)),
  hare = c(10, rep(NA, n_iter))
)
for (i in 1:n_iter) {
  pop_k_tbl$lynx[i + 1] <- pop_k_tbl$lynx[i] + pop_k_tbl$lynx[i] * f_k_func(pop_k_tbl$lynx[i], pop_k_tbl$hare[i], a = 0.01, b = 0.01)
  pop_k_tbl$hare[i + 1] <- pop_k_tbl$hare[i] + pop_k_tbl$hare[i] * g_func(pop_k_tbl$lynx[i], c = 0.01, d = 0.01)
}


ggplot(pop_k_tbl, aes(x = time)) +
  geom_line(aes(y = lynx), col = "blue") +
  geom_line(aes(y = hare), col = "red")

#improve path graph
ggplot(pop_k_tbl, aes(lynx, hare)) +
  geom_path(aes(color=time))












#add noise + error
set.seed(42) # ensure reproducibility of random results
x <- rnorm(n = 100, mean = 0, sd = 1)

# histogram to show distribution and mean
ggplot(tibble(x), aes(x = x)) +
  geom_histogram(color = "black", fill = "white") +
  annotate("text", x = -0.30, y = 9, label = round(mean(x), 4)) +
  annotate("text", x = -0.30, y = 9.5, label = "Mean") +
  ylab("Count") +
  geom_vline(
    xintercept = mean(x), linetype = "dashed",
    size = 1, colour = "red"
  ) +
  theme_classic()


#again
set.seed(43)
x <- rnorm(n = 10000, mean = 0, sd = 1)

# histogram
ggplot(tibble(x), aes(x = x)) +
  geom_histogram(color = "black", fill = "white") +
  annotate("text", x = -0.055, y = 1050, label = round(mean(x), 4)) +
  annotate("text", x = -0.055, y = 1100, label = "Mean") +
  ylab("Count") +
  geom_vline(
    xintercept = mean(x), linetype = "dashed",
    size = 1, colour = "red"
  ) +
  theme_classic()


set.seed(50)
n_iter <- 1000
pop_eps_tbl <- tibble(
  time = 0:n_iter,
  lynx = c(1.1, rep(NA, n_iter)),
  hare = c(1.1, rep(NA, n_iter))
)
for (i in 1:n_iter) {
  pop_eps_tbl$lynx[i + 1] <- pop_eps_tbl$lynx[i] + pop_eps_tbl$lynx[i] * f_func(pop_eps_tbl$hare[i], a = 0.01, b = 0.01) + rnorm(1, 0, .009)
  pop_eps_tbl$hare[i + 1] <- pop_eps_tbl$hare[i] + pop_eps_tbl$hare[i] * g_func(pop_eps_tbl$lynx[i], c = 0.01, d = 0.01)
}


ggplot(pop_eps_tbl, aes(x = time)) +
  geom_line(aes(y = lynx), col = "blue") +
  geom_line(aes(y = hare), col = "red")

ggplot(pop_eps_tbl, aes(lynx, hare, color=time)) +
  geom_path()




#####mappin
#typically don't load the raster package b/c it has a select() function in conflict w/ tidyverse
#use raster::select() instead

#recommended to safe images as png (raster)--it's uncompressed. vs. jpeg which is compressed. easier to go from uncompressed to compressed.
#pdf for vector data.
mpas_westcoast <- st_read("data/shapefiles/mpas_westcoast.shp")

ggplot(mpas_westcoast) +
  geom_sf(aes(fill = State)) +
  ggtitle("Marine Protected Areas on the West Coast") +
  labs(x = "longitude", y = "latitude")

mpas_area <- mpas_westcoast %>%
  select(State, Site_ID, Shape_Area) %>%
  group_by(State) %>%
  summarize(sum = sum(Shape_Area)) %>%
  arrange(desc(sum)) # rank

mpas_area[[1]][1]


#raster
list_files <- list.files("data/raster/", pattern = "average_", full.names = TRUE)
list_rasters <- map(list_files, raster::raster) # map functions to a list

raster::plot(list_rasters[[1]],
             main = "Average Sea Surface Temperature 2008 (K)",
             xlab = "Longitude (deg)",
             ylab = "Latitude (deg)"
)

raster::plot(list_rasters[[2]],
             main = "Average Sea Surface Temperature 2009 (K)",
             xlab = "Longitude (deg)",
             ylab = "Latitude (deg)"
)

raster::plot(list_rasters[[3]],
             main = "Average Sea Surface Temperature 2010 (K)",
             xlab = "Longitude (deg)",
             ylab = "Latitude (deg)"
)

raster::plot(list_rasters[[4]],
             main = "Average Sea Surface Temperature 2011 (K)",
             xlab = "Longitude (deg)",
             ylab = "Latitude (deg)"
)

raster::plot(list_rasters[[5]],
             main = "Average Sea Surface Temperature 2012 (K)",
             xlab = "Longitude (deg)",
             ylab = "Latitude (deg)"
)

get_values <- function(x) {
  min_val <- raster::minValue(x)
  max_val <- raster::maxValue(x)
  tibble(min_val, max_val)
}

summaries_minmax <- map_df(list_rasters, get_values) %>% # again, this is to map a function to a list
  mutate(year = c(2008, 2009, 2010, 2011, 2012))

summaries_minmax %>%
  pivot_longer(min_val:max_val, names_to = "minmax", values_to = "sst") %>%
  mutate(minmax = case_when(
    minmax == "max_val" ~ "Maximum SST",
    minmax == "min_val" ~ "Minimum SST"
  )) %>%
  ggplot(aes(x = sst)) +
  geom_histogram() +
  facet_grid(rows = vars(minmax)) +
  labs(x = "Annual Sea Surface Temperature (K)")

avg_ssts <- raster::stack(list_rasters)
raster::plot(avg_ssts, xlab = "Longitude (deg)", ylab = "Latitude (deg)")



#raster calculations
kelvin_celsius <- function(kelvin) {
  celsius <- kelvin - 273.15
  celsius
}

avg_cel <- raster::calc(avg_ssts, kelvin_celsius)
avg_sst <- raster::mean(avg_cel)

avg_sst %>%
  na.omit() %>%
  raster::plot(main = "Average Annual Sea Surface Temperature 2008-2012 (C)", xlab = "Longitude (deg)", ylab = "Latitude (deg)")

npp <- raster::raster("data/raster/annual_npp.tif")
raster::plot(npp, main = "Net Primary Productivity (mgC/m2/day)", xlab = "Easting (m)", ylab = "Northing (m)")

raster::crs(avg_sst)

raster::crs(npp)

st_crs(mpas_westcoast)

npp_crs <- raster::crs(npp, asText = TRUE)
projected_avg <- raster::projectRaster(avg_sst, crs = npp_crs)
sp::identicalCRS(npp, projected_avg)

raster::crs(avg_sst)

raster::crs(npp)

st_crs(mpas_westcoast)

npp_crs <- raster::crs(npp, asText = TRUE)
projected_avg <- raster::projectRaster(avg_sst, crs = npp_crs)
sp::identicalCRS(npp, projected_avg)

npp_avg_stack <- raster::stack(projected_avg, npp)
raster::plot(npp_avg_stack, main = c("Average SST (C)", "NPP (mgC/m2/day)"), xlab = "Easting (m)", ylab = "Northing (m)")

westcoast_sample <- st_sample(mpas_westcoast, size = 1000) %>%
  st_sf() %>%
  st_join(mpas_westcoast)

sample_data <- raster::extract(npp_avg_stack, westcoast_sample) %>%
  as_tibble()

westcoast_data <- westcoast_sample %>%
  mutate(
    average_annual_sst = sample_data$layer,
    annual_npp = sample_data$annual_npp
  )

filtered_data <- westcoast_data %>%
  filter(average_annual_sst < 18, average_annual_sst > 12) %>%
  filter(annual_npp >= 2.6, annual_npp <= 3)

nrow(filtered_data) / nrow(westcoast_data)

filtered_data$geometry

st_bbox(filtered_data)

raster::plot(npp_avg_stack[[2]], main = "Suitable Lumpsucker Habitat over NPP (mgC/m2/day)", xlab = "(m)", ylab = "(m)")
raster::plot(filtered_data, col = 10, add = T)

raster::plot(npp_avg_stack[[1]], main = "Suitable Lumpsucker Habitat over Average SST (C)", xlab = "(m)", ylab = "(m)")
raster::plot(filtered_data, col = 10, add = T)
