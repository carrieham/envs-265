
library(pacman)
p_load(tidyverse,tmap,sf)


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
