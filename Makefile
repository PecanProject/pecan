BASE_PACKAGES = utils db settings visualization
MODELS = models/jules models/clm45 models/maat/ \
	 models/maespa models/biocro models/dalec \
	 models/ed models/gday models/linkages


biocro
clm45
dalec
ed
gday
jules
linkages
lpjguess
maat
maespa
preles
sipnet
template


models.%: .install.models.%

.install.models.%: $(wildcard models/%/**/*)
	R CMD INSTALL models/$<
	touch .install.models.$<

models.biocro: .install.models.biocro
.install.models.biocro: $(wildcard models/biocro/**/*)
	R CMD INSTALL models/biocro

models.clm45: .install.models.clm45
.install.models.clm45: $(wildcard models/clm45/**/*)
	R CMD INSTALL models/clm45

