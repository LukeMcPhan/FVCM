#' Prepare Input Data for State Transition Model
#'
#' Loads and processes transition rules, flow data, raster inundation layers, and vegetation layers.
#'
#' @param transition_rules_file File path to the CSV containing transition rules.
#' @param flow_files List of file paths to CSV flow data.
#' @param inundation_raster_path File path to the inundation raster.
#' @param vegetation_gdb_path Path to the geodatabase with vegetation layer.
#' @param vegetation_layer_name Name of the vegetation layer in the GDB.
#'
#' @return A list containing transition rules, ordered state factors, flow data, inundation raster, and vegetation data.
#' @export
prepare_data <- function(
    transition_rules_file,
    flow_files,
    inundation_raster_path,
    vegetation_gdb_path,
    vegetation_layer_name
) {
  requireNamespace("readr")
  requireNamespace("raster")
  requireNamespace("sf")
  requireNamespace("dplyr")

  transition_rules_raw <- readr::read_csv(transition_rules_file)

  ordered_states <- factor(
    c("RRGF_G","RRGF_M","RRGF_I_recruit","RRGF_I_P",
      "RRGF_I_C","RRGF_P_GM","RRGF_P_I","RRGF_P",
      "RRGF_C_GMP","RRGF_C_IP","RRGF_C_P","RRGF_C",
      "RRG_Recruit","RRG_Inun","RRG_Dry","RRG_Dry_C",
      "RRG_Dry_PC","RRG_Dry_IPC","RRG_Dry_GMPC","RRG_Dead"),
    levels = c("RRGF_G","RRGF_M","RRGF_I_recruit","RRGF_I_P",
               "RRGF_I_C","RRGF_P_GM","RRGF_P_I","RRGF_P",
               "RRGF_C_GMP","RRGF_C_IP","RRGF_C_P","RRGF_C",
               "RRG_Recruit","RRG_Inun","RRG_Dry","RRG_Dry_C",
               "RRG_Dry_PC","RRG_Dry_IPC","RRG_Dry_GMPC","RRG_Dead")
  )

  flow_data <- lapply(flow_files, readr::read_csv)
  inundation_raster <- raster::raster(inundation_raster_path)

  vegetation_data <- sf::st_read(vegetation_gdb_path, layer = vegetation_layer_name) |>
    dplyr::filter(Broad_cat == "River red gum forest") |>
    dplyr::mutate(state_ID = "RRGF_G")

  list(
    transition_rules = transition_rules_raw,
    ordered_states = ordered_states,
    flow_data = flow_data,
    inundation_raster = inundation_raster,
    vegetation_data = vegetation_data
  )
}
