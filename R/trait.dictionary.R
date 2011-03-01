#' Dictionary of terms used to identify traits in ed, filenames, and figures 
#'
#' @return a dataframe with id, the name used by ED and BETY for a parameter; fileid, an abbreviated  
#'     name used for files; figid, the parameter name written out as best known in english for figures 
#'     and tables.
#'
#' @param traits a vector of trait names, if traits = 'all', all of the traits will be returned.
trait.dictionary <- function(traits) {
  defs<-data.frame(id = c( "c2n_leaf", "dark_respiration_factor", "f_labile", "growth_resp_factor", "leaf_turnover_rate", "leaf_width", "mort2", "nonlocal_dispersal", "q", "quantum_efficiency", "root_respiration_factor", "root_turnover_rate", "SLA", "stomatal_slope", "Vm0", "Vm_low_temp", "water_conductance","cuticular_cond","seedling_mortality","r_fract","storage_turnover_rate", "T"),
                   fileid = c("c2nleaf", "darkresp", "flabile", "growthresp", "leaftnvrrate", "leafwidth", "mort2", "nldisprs", "q", "quantef", "rtresp", "rttnvrrate", "SLA", "stmslope", "Vm0", "Vmlowtemp", "h2ocndctnc", "cuticularcond","seedlingmortality","rfract","storagetnvrrate","T"),
                   figid = c("Leaf C:N" ,"Dark Respiration Rate", "Litter% Labile C", "Growth Respiration", "Leaf Turnover Rate", "Leaf Width", "Mortality Rate", "Seed Dispersal", "Fine Root Allocation","Quantum Efficiency", "Root Respiration Rate", "Root Turnover Rate", "Specific Leaf Area", "Stomatal Slope", "Vcmax", "Photosynthesis min temp", "Water Conductance","Cuticular Conductance", "Seedling Mortality", "Reproductive Allocation","Storage Turnover Rate","Transpiration")
                   )
  
  if(!("all" %in% traits)) {
    trait.defs <- merge(data.frame(id=traits), defs, by.x = 'id', by.y = 'id', sort = FALSE)
  } else {
    trait.defs <- defs
    return(trait.defs)
  }
}


#' @examples
#' #translate a parameter name
#' trait.dictionary(c('growth_resp_factor'))
#' trait.dictionary(c('growth_resp_factor'))$figid
#' 
#' #append the names to a dataframe of priors
#' priors <- query.bety("select priors.id, name, phylogeny, distn, parama, paramb, from priors join variables
#'                       on priors.variable_id = variables.id where priors.id in
#'                       (select prior_id from pfts_priors where pft_id = 10);")
#' data.frame(name = trait.dictionary(priors$name)$figid, priors)
#'
#' 

