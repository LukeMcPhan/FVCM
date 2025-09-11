#' Spatial Preprocessing for Inundation and Vegetation Layers
#'
#' Tiles the inundation raster and extracts vegetation state by pixel.
#'
#' @param inundation_raster Raster layer of inundation thresholds.
#' @param vegetation_data sf object of vegetation polygons.
#' @param tile_dimension Tile size (number of raster cells per tile side).
#'
#' @return A list of small rasters, pixel-to-state lookup tables, and vegetation state keys.
#' @export
spatial_preprocessing <- function(inundation_raster, vegetation_data, tile_dimension = 150) {
  crs_inundation <- sf::st_crs(inundation_raster)

  if (!identical(crs_inundation, sf::st_crs(vegetation_data))) {
    message("Warning: CRS mismatch. Reprojecting vegetation data to match raster.")
    vegetation_data <- sf::st_transform(vegetation_data, crs = crs_inundation)
  }

  all_extent <- raster::extent(inundation_raster)
  all_resolution <- raster::res(inundation_raster)

  x_split_vectors <- seq(all_extent[1], all_extent[2], by = all_resolution[1]) %>%
    split(ceiling(seq_along(.) / tile_dimension)) %>%
    purrr::map(~ c(min(.), max(.) + all_resolution[1])) %>%
    rlang::set_names(nm = paste0("sub_x", stringr::str_pad(1:length(.), 2, pad = 0)))

  y_split_vectors <- seq(all_extent[3], all_extent[4], by = all_resolution[2]) %>%
    split(ceiling(seq_along(.) / tile_dimension)) %>%
    purrr::map(~ c(min(.), max(.) + all_resolution[2])) %>%
    rlang::set_names(nm = paste0("sub_y", stringr::str_pad(1:length(.), 2, pad = 0)))

  extent_names <- tidyr::expand_grid(x = names(x_split_vectors), y = names(y_split_vectors)) %>%
    dplyr::mutate(names_extent = paste0(x, "_", y)) %>%
    dplyr::pull(names_extent)

  list_of_extents <- purrr::cross2(y_split_vectors, x_split_vectors) %>%
    purrr::map(~ c(.x[[2]], .x[[1]])) %>%
    rlang::set_names(extent_names)

  small_rasters <- purrr::map(list_of_extents, ~ raster::crop(inundation_raster, .x))
  useful_rast_names <- purrr::keep(small_rasters, ~ any(!is.na(raster::values(.x)))) %>% names()
  smlr_rasters <- small_rasters[useful_rast_names]

  all_vectorised_inundation <- purrr::map(smlr_rasters, ~ tibble::tibble(inundation.threshold = raster::values(.x), pixel = seq_along(.x[])))

  vegetation_states <- unique(vegetation_data$state_ID)
  levels_vegetation <- forcats::as_factor(vegetation_states)
  veg_type <- forcats::as_factor(seq_along(levels(levels_vegetation)))

  vegetation_lookup <- tibble::tibble(
    vegetation_levels = levels_vegetation,
    vegetation_type = as.numeric(veg_type),
    state_ID = vegetation_states
  )

  veg_type_rasters <- purrr::map(smlr_rasters, ~ fasterize::fasterize(
    sf = dplyr::left_join(vegetation_data, vegetation_lookup, by = "state_ID"),
    raster = .x,
    field = "vegetation_type"
  ))

  all_vectorised_states <- purrr::map2(
    .x = purrr::map(veg_type_rasters, ~ tibble::tibble(state = raster::values(.x), pixel = seq_along(.x[]))),
    .y = all_vectorised_inundation,
    ~ dplyr::left_join(.x, .y, by = "pixel")
  )

  pixel_count_initialisation <- purrr::map_dfr(all_vectorised_states, ~ .x, .id = "tile") %>%
    dplyr::filter(!is.na(inundation.threshold)) %>%
    dplyr::group_by(state) %>%
    dplyr::summarise(pix_count = dplyr::n(), .groups = "drop")

  purrr::walk2(
    .x = pixel_count_initialisation$state,
    .y = pixel_count_initialisation$pix_count,
    ~ message("Initial pixels for state ", vegetation_lookup$state_ID[vegetation_lookup$vegetation_type == .x], ": ", scales::comma(.y))
  )

  non_NA_vect_states <- purrr::map(all_vectorised_states, ~ dplyr::filter(.x, !is.na(state), !is.na(inundation.threshold)))

  list(
    useful_rast_names = useful_rast_names,
    smlr_rasters = smlr_rasters,
    non_NA_vect_states = non_NA_vect_states,
    vegetation_lookup = vegetation_lookup
  )
}
