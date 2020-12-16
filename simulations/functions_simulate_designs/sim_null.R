#' Title scripts to create combination of distributions for different designs
#'
#' @param nx  levels of x
#' @param nfacet  levels of facets
#' @param mean mean of starting level (could be vector or scalar)
#' @param sd sd of starting level (could be vector or scalar)
#' @param w  scalar denoting increment
#'
# functions to generate distribution vector across combinations so that they are same across x and across facet

sim_null_normal= function(nx, nfacet, mean, sd, w = 0){
  rep(distributional::dist_normal(mean, sd), nx*nfacet)
}
