
# Install dependencies declared by a Shiny app
# Relies on the undocumented behavior that devtools::install_deps will work
# on any directory containing a DESCRIPTION file, even if not an R package.

path <- commandArgs(trailingOnly = TRUE)[[1]]

devtools::install_deps(path)
dep <- desc::desc_get_deps(file.path(path, "DESCRIPTION"))
purrr::walk(
	dep$package,
	~if (system.file(package = .) == "") {
		stop("Don't know how to install dependency ", .,
			", which is required by ", path)
	}
)
