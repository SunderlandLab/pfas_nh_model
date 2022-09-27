# Purpose of this script is to try spatial regression with random forest

# Library
library(tidyverse)
library(spatialRF)
library(kableExtra)
library(rnaturalearth)
library(rnaturalearthdata)
library(randomForestExplainer)
library(pdp)
library(sf)

# Load --------------------------------------------------------------------
compounds_data <- readRDS(file.path(here::here(), '../modeling_data/compounds_data.rds'))

well_location <- read_csv(file.path(here::here(), "../raw_data/actual_unique.csv"))[,-1]
# join lat/long to compounds_data, drop ID and continuous outcome
compounds_data <- compounds_data %>%
    map(function(x){x %>% 
            left_join(well_location, by = "StationID") %>%
            select(-c(StationID, reg)) %>%
            mutate_if(is.factor, as.character) %>% 
            mutate_if(is.character, as.numeric) %>% 
            rename(x = Longitude,
                   y = Latitude)})

pfoa_data <- compounds_data[[1]]

# data requirements
nrow(pfoa_data)
sum(apply(pfoa_data, 2, is.na))
pfoa_data <- na.omit(pfoa_data)
apply(pfoa_data, 2, var) == 0

# set up
dependent.variable.name <- "final"
predictor.variable.names <- colnames(pfoa_data)[2:23]
xy <- pfoa_data[, c("x", "y")] 

#distance matrix
# first turn pfoa_data to sf object
sf_pfoa <- sf::st_as_sf(pfoa_data, coords = c("x", "y")) %>%
    st_set_crs("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0") %>%
    st_transform("+proj=utm +zone=18 +datum=WGS84")

distance.matrix <- as.matrix(dist(st_coordinates(sf_pfoa)))

#distance thresholds (same units as distance_matrix), using meter for now
distance.thresholds <- seq(0, max(distance.matrix)/10, length.out = 5)

#random seed for reproducibility
random.seed <- 123

# fitting a non-spatial random forest
model.non.spatial <- spatialRF::rf(
    data = pfoa_data,
    dependent.variable.name = dependent.variable.name,
    predictor.variable.names = predictor.variable.names,
    distance.matrix = distance.matrix,
    distance.thresholds = distance.thresholds,
    xy = xy, #not needed by rf, but other functions read it from the model
    seed = random.seed,
    verbose = FALSE
)

# spatial autocorrelation 
spatialRF::plot_moran(
    model.non.spatial, 
    verbose = FALSE
)

# variable importance
spatialRF::plot_importance(
    model.non.spatial,
    verbose = FALSE
)

# spatial cross-validation
model.non.spatial <- spatialRF::rf_evaluate(
    model = model.non.spatial,
    xy = xy,                  #data coordinates
    repetitions = 30,         #number of spatial folds
    training.fraction = 0.75, #training data fraction on each fold
    metrics = "auc",
    seed = random.seed,
    verbose = FALSE
)

spatialRF::print_evaluation(model.non.spatial)

# visualize the cross-validation sets
pr <- pfoa_data[, c("x", "y")]
pr$group.2 <- pr$group.1 <- "Training"
pr[model.non.spatial$evaluation$spatial.folds[[1]]$testing, "group.1"] <- "Testing"
pr[model.non.spatial$evaluation$spatial.folds[[30]]$testing, "group.2"] <- "Testing"
nh_map <- tigris::counties("New Hampshire", cb = TRUE) %>%
    st_as_sf() 
p1 <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = nh_map, fill = "white") +
    ggplot2::geom_point(data = pr,
                        ggplot2::aes(
                            x = x,
                            y = y,
                            color = group.1
                        ),
                        size = 2
    ) +
    ggplot2::scale_color_viridis_d(
        direction = -1, 
        end = 0.5, 
        alpha = 0.8, 
        option = "F"
    ) +
    ggplot2::theme_bw() +
    ggplot2::labs(color = "Group") +
    # ggplot2::scale_x_continuous(limits = c(-170, -30)) +
    # ggplot2::scale_y_continuous(limits = c(-58, 80))  +
    ggplot2::ggtitle("Spatial fold 1") + 
    ggplot2::theme(
        legend.position = "none", 
        plot.title = ggplot2::element_text(hjust = 0.5)
    ) + 
    ggplot2::xlab("Longitude") + 
    ggplot2::ylab("Latitude")

p2 <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = nh_map, fill = "white") +
    ggplot2::geom_point(data = pr,
                        ggplot2::aes(
                            x = x,
                            y = y,
                            color = group.2
                        ),
                        size = 2
    ) +
    ggplot2::scale_color_viridis_d(
        direction = -1, 
        end = 0.5, 
        alpha = 0.8, 
        option = "F"
    ) +
    ggplot2::theme_bw() +
    ggplot2::labs(color = "Group") +
    # ggplot2::scale_x_continuous(limits = c(-170, -30)) +
    # ggplot2::scale_y_continuous(limits = c(-58, 80)) +
    ggplot2::theme(
        plot.title = ggplot2::element_text(hjust = 0.5)
    ) + 
    ggplot2::ggtitle("Spatial fold 30") + 
    ggplot2::xlab("Longitude") + 
    ggplot2::ylab("")

p1 | p2


## fitting a spatial model
model.spatial <- spatialRF::rf_spatial(
    model = model.non.spatial,
    method = "mem.moran.sequential", #default method
    verbose = FALSE,
    seed = random.seed
)

spatialRF::plot_moran(
    model.spatial, 
    verbose = FALSE
)

# variable importance
p1 <- spatialRF::plot_importance(
    model.non.spatial, 
    verbose = FALSE) + 
    ggplot2::ggtitle("Non-spatial model") 

p2 <- spatialRF::plot_importance(
    model.spatial,
    verbose = FALSE) + 
    ggplot2::ggtitle("Spatial model")

p1 | p2 

# tuning
model.spatial <- spatialRF::rf_spatial(
    model = model.non.spatial,
    method = "mem.moran.sequential", #default method
    ranger.arguments = list(
        mtry = 5,
        min.node.size = 20,
        num.trees = 1000
    ),
    verbose = FALSE,
    seed = random.seed
)


model.spatial <- rf_tuning(
    model = model.spatial,
    xy = xy,
    repetitions = 30,
    num.trees = c(500, 1000),
    mtry = seq(
        2,
        length(model.spatial$ranger.arguments$predictor.variable.names), #number of predictors
        by = 9),
    min.node.size = c(5, 15),
    seed = random.seed,
    verbose = FALSE
)

spatialRF::print_evaluation(model.spatial)

# spatial autocorrelation 
spatialRF::plot_moran(
    model.spatial, 
    verbose = FALSE
)

# variable importance
spatialRF::plot_importance(
    model.spatial,
    verbose = FALSE
)
