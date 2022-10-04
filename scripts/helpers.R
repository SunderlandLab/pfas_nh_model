modified_rf_spatial <- function (model = NULL, data = NULL, dependent.variable.name = NULL, 
          predictor.variable.names = NULL, distance.matrix = NULL, 
          distance.thresholds = NULL, xy = NULL, ranger.arguments = NULL, 
          scaled.importance = TRUE, method = c("mem.moran.sequential", 
                                               "mem.effect.sequential", "mem.effect.recursive", 
                                               "hengl", "hengl.moran.sequential", "hengl.effect.sequential", 
                                               "hengl.effect.recursive", "pca.moran.sequential", 
                                               "pca.effect.sequential", "pca.effect.recursive"), 
          max.spatial.predictors = NULL, weight.r.squared = NULL, weight.penalization.n.predictors = NULL, 
          seed = 1, verbose = TRUE, n.cores = parallel::detectCores() - 
              1, cluster = NULL) 
{
    moran.i <- NULL
    variable <- NULL
    interpretation <- NULL
    method <- match.arg(arg = method, choices = c("mem.moran.sequential", 
                                                  "mem.effect.sequential", "mem.effect.recursive", 
                                                  "hengl", "hengl.moran.sequential", "hengl.effect.sequential", 
                                                  "hengl.effect.recursive", "pca.moran.sequential", 
                                                  "pca.effect.sequential", "pca.effect.recursive"), 
                        several.ok = FALSE)
    if (is.null(model)) {
        if (is.null(data)) {
            stop("The argument 'data' is missing.")
        }
        else {
            if (inherits(data, "tbl_df") | inherits(data, 
                                                    "tbl")) {
                data <- as.data.frame(data)
            }
        }
        if (inherits(xy, "tbl_df") | inherits(xy, "tbl")) {
            xy <- as.data.frame(xy)
        }
        if (is.null(distance.matrix)) {
            stop("The argument 'distance.matrix' is missing.")
        }
        if (is.null(dependent.variable.name)) {
            stop("The argument 'dependent.variable.name' is missing.")
        }
        if (is.null(predictor.variable.names)) {
            stop("The argument 'predictor.variable.names' is missing.")
        }
        else {
            if (inherits(predictor.variable.names, "variable_selection")) {
                predictor.variable.names <- predictor.variable.names$selected.variables
            }
        }
        ranger.arguments$num.threads <- n.cores
        model <- rf(data = data, dependent.variable.name = dependent.variable.name, 
                    predictor.variable.names = predictor.variable.names, 
                    distance.matrix = distance.matrix, distance.thresholds = distance.thresholds, 
                    xy = xy, ranger.arguments = ranger.arguments, scaled.importance = FALSE, 
                    seed = seed, verbose = FALSE)
    }
    else {
        ranger.arguments <- model$ranger.arguments
        data <- ranger.arguments$data
        dependent.variable.name <- ranger.arguments$dependent.variable.name
        predictor.variable.names <- ranger.arguments$predictor.variable.names
        distance.matrix <- ranger.arguments$distance.matrix
        if (is.null(distance.matrix)) {
            stop("The argument 'distance.matrix' is missing.")
        }
        distance.thresholds <- ranger.arguments$distance.thresholds
        if (is.null(distance.thresholds)) {
            distance.thresholds <- default_distance_thresholds(distance.matrix = distance.matrix)
        }
        scaled.importance <- ranger.arguments$scaled.importance
        seed <- model$ranger.arguments$seed
        if (is.null(cluster) & "cluster" %in% names(model)) {
            cluster <- model$cluster
        }
    }
    if (!is.null(cluster)) {
        n.cores <- NULL
        stop.cluster <- FALSE
    }
    else {
        cluster <- parallel::makeCluster(n.cores, type = "PSOCK")
        stop.cluster <- TRUE
    }
    doParallel::registerDoParallel(cl = cluster)
    if (!is.null(model$residuals$autocorrelation$max.moran)) {
        reference.moran.i <- model$residuals$autocorrelation$max.moran
    }
    else {
        reference.moran.i <- 1
    }
    model.moran.i <- model$residuals$autocorrelation$per.distance %>% 
        dplyr::arrange(dplyr::desc(moran.i)) %>% dplyr::filter(interpretation != 
                                                                   "No spatial correlation")
    if (nrow(model.moran.i) == 0) {
        if (verbose == TRUE) {
            message("The model residuals are not spatially correlated, there is no need to fit a spatial model")
        }
        return(model)
    }
    else {
        if (verbose == TRUE) {
            message("The model residuals are spatially correlated, fitting a spatial model.")
        }
    }
    distance.thresholds.with.ac <- model.moran.i$distance.threshold
    if (method %in% c("hengl", "hengl.moran.sequential", 
                      "hengl.effect.sequential", "hengl.effect.recursive")) {
        spatial.predictors.df <- distance.matrix
        colnames(spatial.predictors.df) <- rownames(spatial.predictors.df) <- paste0("spatial_predictor_", 
                                                                                     seq(1, ncol(distance.matrix)))
        spatial.predictors.selected <- colnames(spatial.predictors.df)
        if (verbose == TRUE) {
            message("Using the distance matrix columns as spatial predictors.")
        }
    }
    if (method %in% c("pca.moran.sequential", "pca.effect.sequential", 
                      "pca.effect.recursive")) {
        spatial.predictors.df <- pca_multithreshold(distance.matrix = distance.matrix, 
                                                    distance.thresholds = distance.thresholds.with.ac, 
                                                    max.spatial.predictors = max.spatial.predictors)
        if (verbose == TRUE) {
            message("Using PCA factors of the distance matrix as spatial predictors.")
        }
    }
    if (method %in% c("mem.moran.sequential", "mem.effect.sequential", 
                      "mem.effect.recursive")) {
        spatial.predictors.df <- mem_multithreshold(distance.matrix = distance.matrix, 
                                                    distance.thresholds = distance.thresholds.with.ac, 
                                                    max.spatial.predictors = max.spatial.predictors)
        if (verbose == TRUE) {
            message("Using Moran's Eigenvector Maps as spatial predictors.")
        }
    }
    if (method %in% "hengl") {
        ranking.method <- NULL
    }
    if (method %in% c("hengl.moran.sequential", "pca.moran.sequential", 
                      "mem.moran.sequential")) {
        ranking.method <- "moran"
        if (verbose == TRUE) {
            message("Ranking spatial predictors by their Moran's I.")
        }
    }
    if (method %in% c("hengl.effect.sequential", "hengl.effect.recursive", 
                      "pca.effect.sequential", "pca.effect.recursive", 
                      "mem.effect.sequential", "mem.effect.recursive")) {
        ranking.method <- "effect"
        if (verbose == TRUE) {
            message("Ranking spatial predictors by how much they reduce Moran's I of the model residuals.")
        }
    }
    if (!is.null(ranking.method)) {
        spatial.predictors.df <- filter_spatial_predictors(data = data, 
                                                           predictor.variable.names = predictor.variable.names, 
                                                           spatial.predictors.df = spatial.predictors.df, cor.threshold = 0.5)
        spatial.predictors.ranking <- rank_spatial_predictors(data = data, 
                                                              dependent.variable.name = dependent.variable.name, 
                                                              predictor.variable.names = predictor.variable.names, 
                                                              distance.matrix = distance.matrix, distance.thresholds = distance.thresholds, 
                                                              ranger.arguments = ranger.arguments, spatial.predictors.df = spatial.predictors.df, 
                                                              ranking.method = ranking.method, reference.moran.i = reference.moran.i, 
                                                              verbose = FALSE, n.cores = n.cores, cluster = cluster)
    }
    if (method %in% c("hengl.moran.sequential", "mem.moran.sequential", 
                      "pca.moran.sequential", "hengl.effect.sequential", 
                      "mem.effect.sequential", "pca.effect.sequential")) {
        if (verbose == TRUE) {
            message("Sequential selection of spatial predictors.")
        }
        spatial.predictors.selection <- select_spatial_predictors_sequential(data = data, 
                                                                             dependent.variable.name = dependent.variable.name, 
                                                                             predictor.variable.names = predictor.variable.names, 
                                                                             distance.matrix = distance.matrix, distance.thresholds = distance.thresholds, 
                                                                             ranger.arguments = ranger.arguments, spatial.predictors.df = spatial.predictors.df, 
                                                                             spatial.predictors.ranking = spatial.predictors.ranking, 
                                                                             weight.r.squared = weight.r.squared, weight.penalization.n.predictors = weight.penalization.n.predictors, 
                                                                             verbose = FALSE, n.cores = n.cores, cluster = cluster)
        spatial.predictors.selected <- as.character(spatial.predictors.selection$best.spatial.predictors)
    }
    if (method %in% c("hengl.effect.recursive", "pca.effect.recursive", 
                      "mem.effect.recursive")) {
        if (verbose == TRUE) {
            message("Recursive selection of spatial predictors.")
        }
        spatial.predictors.selection <- select_spatial_predictors_recursive(data = data, 
                                                                            dependent.variable.name = dependent.variable.name, 
                                                                            predictor.variable.names = predictor.variable.names, 
                                                                            distance.matrix = distance.matrix, distance.thresholds = distance.thresholds, 
                                                                            ranger.arguments = ranger.arguments, spatial.predictors.df = spatial.predictors.df, 
                                                                            spatial.predictors.ranking = spatial.predictors.ranking, 
                                                                            weight.r.squared = weight.r.squared, weight.penalization.n.predictors = weight.penalization.n.predictors, 
                                                                            n.cores = n.cores, cluster = cluster)
        spatial.predictors.selected <- spatial.predictors.selection$best.spatial.predictors
    }
    if (method != "hengl") {
        spatial.predictors.df <- spatial.predictors.df[, spatial.predictors.selected]
    }
    data.spatial <- data.frame(data, spatial.predictors.df)
    colnames(data.spatial) <- c(colnames(data), spatial.predictors.selected)
    predictor.variable.names.spatial <- c(predictor.variable.names, 
                                          spatial.predictors.selected)
    model.spatial <- rf(data = data.spatial, dependent.variable.name = dependent.variable.name, 
                        predictor.variable.names = predictor.variable.names.spatial, 
                        distance.matrix = distance.matrix, distance.thresholds = distance.thresholds, 
                        xy = xy, ranger.arguments = ranger.arguments, seed = seed, 
                        verbose = FALSE)
    class(model.spatial) <- c("rf", "rf_spatial", 
                              "ranger")
    model.spatial$residuals$autocorrelation$plot <- plot_moran(model.spatial, 
                                                               verbose = FALSE)
    model.spatial$importance <- prepare_importance_spatial(model = model.spatial)
    model.spatial$importance$local <- model.spatial$variable.importance.local
    model.spatial$spatial <- list()
    model.spatial$spatial$method <- method
    model.spatial$spatial$names <- spatial.predictors.selected
    model.spatial$spatial$spatial.predictors <- spatial.predictors.df
    if (exists("spatial.predictors.selection")) {
        model.spatial$spatial$optimization <- spatial.predictors.selection$optimization
        model.spatial$spatial$plot <- plot_optimization(spatial.predictors.selection$optimization, 
                                                        verbose = FALSE)
    }
    if (verbose == TRUE) {
        message("Details about the spatial predictors stored in model$spatial.")
    }
    if (verbose == TRUE) {
        print(model.spatial)
        plot_importance(model.spatial)
        plot_residuals_diagnostics(model.spatial)
    }
    if (stop.cluster == TRUE) {
        parallel::stopCluster(cl = cluster)
    }
    else {
        model.spatial$cluster <- cluster
    }
    model.spatial
}
