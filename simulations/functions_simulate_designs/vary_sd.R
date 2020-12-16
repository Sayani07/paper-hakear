#' Title scripts to create combination of distributions for different designs
#'
#' @param nx  levels of x
#' @param nfacet  levels of facets
#' @param mean mean of starting level (could be vector or scalar)
#' @param sd sd of starting level (could be vector or scalar)
#' @param w  scalar denoting increment
#'

# functions to generate distribution vector across combinations so that they are same across x and different across facet

sim_varf_normal_varysd = function(nx, nfacet, mean, sd, w)
{
  rep(dist_normal(mean, (sd + seq(0, nfacet-1, by  = 1)*w)), each = nx)
}

# functions to generate distribution vector across combinations so that they are same across facet and different across x

sim_varx_normal_varysd = function(nx, nfacet, mean, sd, w)
{
  rep(dist_normal(mean, (sd + seq(0, nx-1, by  = 1)*w)), nfacet)
}

# functions to generate distribution vector across combinations so that they are different across both facet and x

sim_varall_normal_varysd = function(nx, nfacet, mean, sd, w)
{
  dist_normal(mean, (sd + seq(0, (nx*nfacet - 1), by  = 1)*w))
}



