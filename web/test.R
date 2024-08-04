library("PEcAn.all")

args <- PEcAn.settings::get_args()
settings <- PEcAn.settings::read.settings(args$settings)
settings <- PEcAn.settings::prepare.settings(settings, force = FALSE)
settings <- PEcAn.workflow::do_conversions(settings)
