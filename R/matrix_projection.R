#' Matrix Projection of State Transitions
#'
#' Projects vegetation state transitions annually using transition matrices and spatial data.
#'
#' @param spatial_preprocessing_output List output from spatial_preprocessing().
#' @param spells_analysis_output List output from spells_analysis().
#' @param transition_rules_list Processed transition rules list from format_rules().
#'
#' @return A list containing:
#' - `annual_raster_list`:
#' - summarized state transitions.
#' @export
matrix_projection <- function(spatial_preprocessing_output = spatial_preprocessing_output,
                              spells_analysis_output = spells_analysis_output,
                              transition_rules_list = transition_rules_list) {

  vect_states <- spatial_preprocessing_output$non_NA_vect_states
  matrices <- spells_analysis_output$matrices
  ID_mat <- spells_analysis_output$ID_mat
  water_years <- spells_analysis_output$water_years
  complete_raster_vals <- vect_states %>% purrr::keep(~nrow(.x) > 1)
  complete_raster_names <- names(complete_raster_vals)

  plotting_data <- list()
  start_time <- Sys.time()

  for (i in seq_along(complete_raster_vals)) {
    tile_name <- complete_raster_names[i]
    tile_vect_states <- vect_states[[tile_name]]

    all_unit_pixelname <- tile_vect_states$pixel
    inun <- tile_vect_states$inundation.threshold
    state <- tile_vect_states$state

    state_vec <- purrr::map(state, ~as.matrix(ID_mat[spatial_preprocessing_output$vegetation_lookup$vegetation_type[.x],]))

    initialised_output <- purrr::map(state_vec, ~{
      m <- matrix(nrow = length(water_years) + 1, ncol = ncol(ID_mat))
      m[1,] <- .x
      m[-1,] <- 0
      colnames(m) <- colnames(ID_mat)
      return(m)
    })

    matrix_multiplier <- function(list.ts.mat, initialised_output) {
      for (k in 2:nrow(initialised_output)) {
        initialised_output[k, ] <- initialised_output[k - 1, ] %*% t(list.ts.mat[[k - 1]])
      }
      return(initialised_output)
    }

    final_states <- purrr::map2(state_vec, seq_along(state_vec), ~{
      list_ts_of_matrices <- purrr::map(matrices, ~.x[[as.character(inun[.y])]])
      matrix_multiplier(list_ts_of_matrices, initialised_output[[.y]])
    })

    final_states_chr <- purrr::map(final_states, ~{
      purrr::map(.x = array_branch(.x, 1), ~names(.x[.x == 1]))
    })

    plotting_data[[i]] <- dplyr::bind_rows(purrr::map2(final_states_chr, all_unit_pixelname, ~{
      df <- data.frame(pixel = .y, t(as.data.frame(.x)))
      colnames(df)[-1] <- c("initial_state", water_years)
      df
    }))
  }

  plotting_data <- plotting_data %>% purrr::set_names(complete_raster_names)

  rast_vals <- spatial_preprocessing_output$smlr_rasters %>%
    purrr::keep(names(.) %in% complete_raster_names) %>%
    purrr::map(~tibble::tibble(inundation_threshold = raster::values(.x), pixel = seq_along(.x[])))

  veg_pixels_by_tile <- purrr::map(plotting_data, ~dplyr::select(.x, pixel))

  vegetation_plotting_data <- purrr::map(plotting_data, ~.x %>% dplyr::select(-pixel) %>%
                                           array_branch(2) %>%
                                           purrr::map(~{
                                             veg_state_chr <- factor(.x, levels = levels(transition_rules_list$states_ordered))
                                             tibble::tibble(veg_state_chr = veg_state_chr,
                                                            veg_state_numeric = as.numeric(veg_state_chr))
                                           }))

  joiner <- function(time_series, pixels) purrr::map(time_series, ~dplyr::bind_cols(pixels, .x))
  veg_inundation_pixels <- purrr::map2(vegetation_plotting_data, veg_pixels_by_tile, joiner)

  complete_rast_vals <- rast_vals %>% purrr::keep(names(.) %in% complete_raster_names)
  joiner_2 <- function(vts, rvs) purrr::map(vts, ~dplyr::left_join(rvs, .x, by = "pixel"))
  complete_states_to_plot <- purrr::map2(veg_inundation_pixels, complete_rast_vals, joiner_2)

  veg_rasters <- purrr::map2(complete_states_to_plot, spatial_preprocessing_output$smlr_rasters[complete_raster_names],
                             ~purrr::map(.x, ~raster::setValues(.y, .x$veg_state_numeric)))
  veg_rasters <- veg_rasters %>% purrr::set_names(complete_raster_names)

  annual_veg_rasters <- purrr::map(water_years, ~purrr::flatten(purrr::map(veg_rasters, ~.x[[.y]])))
  merged_output <- purrr::map(annual_veg_rasters, ~do.call(raster::merge, .x)) %>%
    purrr::set_names(water_years)

  total_annual_states <- purrr::map_dfr(merged_output, ~{
    tab <- table(raster::values(.x))
    tibble::tibble(state = names(tab), pixels = as.integer(tab)) %>%
      dplyr::mutate(veg_state = levels(transition_rules_list$states_ordered)[as.integer(state)])
  }, .id = "water_year")

  list(
    annual_raster_list = merged_output,
    summarised_annual_states = total_annual_states
  )
}
