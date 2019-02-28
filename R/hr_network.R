#' Homerange Networks
#'
#' Build home range networks using `adehabitatHR::kerneloverlap` and returns either graph statitics or home range overlap. 
#' 
#' `DT` provided with columns EASTING, NORTHING for UTM coordinates. `by` argument used to specify grouping. Defaults only (as used in the paper). 
#' 
#' @param DT `data.table`` of relocations.
#' @param id individual identifier column name. 
#' @param utm proj4string indicating coordinate system of coordinates
#' @param by columns in input DT to split home range network generation and comparison by. For example: c('season', 'year') or 'herd'. Expects character vector. 
#' @param returns either 'network-stats' or 'overlap'. See Details. 
#' 
#' @return graph strength for each individual
#' @export
hr_network <- function(DT = NULL, id = NULL, utm = NULL, by = NULL, returns = NULL) {
  # NSE
  value <- NULL
  
  if (is.null(DT) | is.null(id) | is.null(utm)) {
    stop('DT, id and utm must be provided')
  }
  
  if (is.null(returns) | !(returns %in% c('network-stats', 'overlap'))) {
    stop('must specify return type either "network-stats" or "overlap"')
  } 
  
  if (returns == 'network-stats') {
    
    DT[, {
      KOver <- build_hr_net(.SD, id = id, utm = utm)
      hr.grph_df <-
        igraph::graph.adjacency(KOver,
                                mode = "undirected",
                                diag = FALSE,
                                weighted = TRUE)
      list(strength = igraph::graph.strength(hr.grph_df),
           ID = names(igraph::degree(hr.grph_df)))
    }, by = by, .SDcols = c('EASTING', 'NORTHING', 'ANIMAL_ID', by, id)]
    
  } else if (returns == 'overlap') {
    
    DT[, {
      KOver <- build_hr_net(.SD, id = id, utm = utm)
      out.dt <- data.table::data.table(
        data.table::melt(KOver))[!is.na(value)]
      
      if (nrow(out.dt) == 0) {
        list(
          leftYear = as.integer(999),
          rightYear = as.integer(999),
          value = 999
        )
      } else {
        list(
          leftYear = unlist(
            data.table::tstrsplit(
              out.dt$Var1, '_', keep = 3, type.convert = TRUE),
            use.names = FALSE),
          rightYear = unlist(
            data.table::tstrsplit(
              out.dt$Var2, '_', keep = 3, type.convert = TRUE),
            use.names = FALSE),
          value = out.dt$value)
      }
    }, by = by, .SDcols = c('EASTING', 'NORTHING', 'ANIMAL_ID', by, id)]
  }
}

#' @import data.table
build_hr_net <- function(DT, id, utm) {
  xy <- sp::SpatialPointsDataFrame(
    coords = DT[, .SD, .SDcols = c('EASTING', 'NORTHING')],
    proj4string = sp::CRS(utm),
    data = DT[, .SD, .SDcols = id])

  KOver = adehabitatHR::kerneloverlap(xy,
                                      method = "UDOI",
                                      percent = 95,
                                      grid = 700)

  KOver <- as.matrix(KOver)
  diag(KOver) <- NA
  KOver[lower.tri(KOver)] <- NA
  return(KOver)
}

