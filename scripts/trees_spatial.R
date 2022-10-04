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
source("scripts/helpers.R")

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

#pfoa_data <- compounds_data[[1]]

# check data requirements
for (pfas_data in compounds_data){
    print(nrow(pfas_data))
    sum(apply(pfas_data, 2, is.na)) %>% print()
    pfas_data <- na.omit(pfas_data)
    print(nrow(pfas_data))
    print(apply(pfas_data, 2, var) == 0) 
}

# drop observations with missing features, a small set 16 - 22
compounds_data <- compounds_data %>% map(na.omit)

# set up
dependent.variable.name <- "final"
predictor.variable.names <- colnames(compounds_data[[1]])[2:23]

# for each chemical
for (i in 1){#:length(compounds_data)){
    print(paste("Starting setting up for", names(compounds_data)[i]))
    pfas_data <- compounds_data[[i]]
    xy <- pfas_data[, c("x", "y")] 
    
    # distance matrix
    # first turn pfoa_data to sf object
    sf_pfas <- sf::st_as_sf(pfas_data, coords = c("x", "y")) %>%
        st_set_crs("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0") %>%
        st_transform("+proj=utm +zone=18 +datum=WGS84")
    
    distance.matrix <- as.matrix(dist(st_coordinates(sf_pfas)))
    
    #distance thresholds (same units as distance_matrix), using meter for now
    distance.thresholds <- seq(0, max(distance.matrix)/10, length.out = 5)
    
    #random seed for reproducibility
    random.seed <- 123
    
    print(paste("Fitting a non-spatial model for", names(compounds_data)[i]))
    
    # fitting a non-spatial random forest
    model.non.spatial <- spatialRF::rf(
        data = pfas_data,
        dependent.variable.name = dependent.variable.name,
        predictor.variable.names = predictor.variable.names,
        distance.matrix = distance.matrix,
        distance.thresholds = distance.thresholds,
        xy = xy, #not needed by rf, but other functions read it from the model
        seed = random.seed,
        verbose = FALSE
    )
    
    # # tuning
    # model.non.spatial <- rf_tuning(
    #     model = model.non.spatial,
    #     xy = xy,
    #     repetitions = 30,
    #     num.trees = c(500, 1000),
    #     mtry = seq(
    #         2,
    #         length(model.non.spatial$ranger.arguments$predictor.variable.names), #number of predictors
    #         by = 2),
    #     min.node.size = c(2, 5, 9),
    #     seed = random.seed,
    #     verbose = FALSE
    # )
    
    # spatial cross validation
    model.non.spatial <- spatialRF::rf_evaluate(
        model = model.non.spatial,
        xy = xy,                  #data coordinates
        repetitions = 10,         #number of spatial folds
        training.fraction = 0.75, #training data fraction on each fold
        metrics = "auc",
        seed = random.seed,
        verbose = FALSE
    )
    
    spatialRF::print_evaluation(model.non.spatial)
    
    # check autocorrelation
    spatialRF::plot_moran(
        model.non.spatial, 
        verbose = FALSE
    ) 
    
    # turn non-spatial model into spatial model
    model.spatial <- modified_rf_spatial(
        model = model.non.spatial,
        method = "mem.moran.sequential", #default method
        verbose = T,
        seed = random.seed
    )
    
    spatialRF::print_performance(model.spatial)
    spatialRF::plot_optimization(model.spatial)
    ggsave("../output/SDSC2022_spatial_predictors_optimiz.png", width = 4, height = 3, units = "in")
    
    # spatial cross validation
    model.spatial <- spatialRF::rf_evaluate(
        model = model.spatial,
        xy = xy,                  #data coordinates
        repetitions = 10,         #number of spatial folds
        training.fraction = 0.75, #training data fraction on each fold
        metrics = "auc",
        seed = random.seed,
        verbose = FALSE
    )
    spatialRF::print_evaluation(model.spatial)
    
    # check autocorrelation
    spatialRF::plot_moran(
        model.spatial, 
        verbose = FALSE
    )
    
    # variable importance
    spatialRF::plot_importance(
        model.spatial,
        verbose = FALSE
    )
}



# model evaluation
spatialRF::print_evaluation(model.non.spatial)
spatialRF::print_evaluation(model.spatial)

# spatial cross-validation
# visualize the cross-validation sets
pr <- pfas_data[, c("x", "y")]
pr$group.2 <- pr$group.1 <- "Training"
# tag the training wells from the spatial fold 1 as testing
pr[model.non.spatial$evaluation$spatial.folds[[1]]$testing, "group.1"] <- "Testing"
# random select 25% as the testing set to illustrate random CV
pr[sample(nrow(pr), floor(nrow(pr)*0.25)), "group.2"] <- "Testing"
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
                        size = 1
    ) +
    ggplot2::scale_color_viridis_d(
        direction = -1, 
        end = 0.5, 
        alpha = 0.8, 
        option = "F"
    ) +
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::labs(color = "Group") +
    ggplot2::ggtitle("Spatial CV") + 
    ggplot2::theme(
        legend.position = "none", 
        plot.title = ggplot2::element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90)
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
                        size = 1
    ) +
    ggplot2::scale_color_viridis_d(
        direction = -1, 
        end = 0.5, 
        alpha = 0.8, 
        option = "F"
    ) +
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::labs(color = "Group") +
    ggplot2::theme(
        plot.title = ggplot2::element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90)
    ) + 
    ggplot2::ggtitle("Random CV") + 
    ggplot2::xlab("Longitude") + 
    ggplot2::ylab("")

p1 | p2
ggsave("../output/SDSC2022_spatial_cross_validation.png", width = 6, height = 5, units = "in")


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
ggsave("../output/SDSC2022_var_imp_sp_vs_non.png", width = 6, height = 5, units = "in")



# spatial autocorrelation 
spatialRF::plot_moran(
    model.non.spatial, 
    verbose = FALSE
)
ggsave("../output/SDSC2022_moran_non.png", width = 4, height = 3, units = "in")

