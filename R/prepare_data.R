#' Prepare Input Data for State Transition Model
#'
#' Loads and processes transition rules, flow data, raster inundation layers, and vegetation layers.
#'
#' @param transition_rules_file chr: File path to the CSV containing transition rules.
#' @param flow_files list: List of file paths to CSV flow data.
#' @param inundation_raster_path chr: File path to the inundation raster.
#' @param vegetation_gdb_path chr: Path to the geodatabase with vegetation layer.
#' @param vegetation_layer_name chr: Name of the vegetation layer in the GDB.
#' @param ordered_states fct: An ordered factor containing ordered states from Highest/First through to Lowest/Worst levels.
#'
#' @return A list containing transition rules, ordered state factors, flow data, inundation raster, and vegetation data.
#' @export
prepare_data <- function(
    transition_rules_file,
    flow_files,
    inundation_raster_path,
    vegetation_gdb_path,
    vegetation_layer_name,
    ordered_states
) {
  requireNamespace("readr")
  requireNamespace("raster")
  requireNamespace("sf")
  requireNamespace("dplyr")
  requireNamespace("purrr")

  transition_rules_raw <- readr::read_csv(transition_rules_file)

  flow_data <- flow_files %>%
    map(~readr::read_csv(.x))

  inundation_raster <- raster::raster(inundation_raster_path)

  vegetation_data <- sf::st_read(vegetation_gdb_path, layer = vegetation_layer_name)

  list(
    transition_rules = transition_rules_raw,
    ordered_states = ordered_states,
    flow_data = flow_data,
    inundation_raster = inundation_raster,
    vegetation_data = vegetation_data
  )
}
