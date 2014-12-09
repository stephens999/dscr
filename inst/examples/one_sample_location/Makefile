clean:
	rm -r param
	rm -r data

#this just calls the R scripts one at a time to give simple example
eg:
	Rscript ../../Rscripts/make_params.R parammaker.R 1 normal 
	Rscript ../../Rscripts/make_data.R datamaker.R 1 normal


#The following uses eval and foreach to produce a target for each SEED, SCENARIO pair

define MAKERULE 
param/$2/param.$1.RData:
	Rscript ../../Rscripts/make_params.R parammaker.R $1 $2
data/$2/data.$1.RData:
	Rscript ../../Rscripts/make_data.R datamaker.R $1 $2
endef

SEEDS = 1 2 3 4 5 6 7 8 9 10
SCENARIOS = normal uniform Cauchy 

$(foreach T,$(SEEDS),$(foreach S,$(SCENARIOS),$(eval $(call MAKERULE,$T,$S))))
$(foreach T,$(SEEDS),$(foreach S,$(SCENARIOS),$(info $(call MAKERULE,$T,$S))))

