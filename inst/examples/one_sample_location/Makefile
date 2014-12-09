#use with caution in real applications - but convenient for testing
clean:
	rm -r param
	rm -r data
	rm -r output
	rm -r results

#very simple test to see if the results from running scripts match those from vignette
test:
	diff ../../../vignettes/results/normal/mean/results.1.RData results/normal/mean/results.1.RData 

#this just calls the R scripts one at a time to give simple example
eg:
	Rscript ../../Rscripts/make_params.R parammaker.R 1 normal 
	Rscript ../../Rscripts/make_data.R datamaker.R 1 normal
	Rscript ../../Rscripts/run_method.R method.mean.R 1 normal
	Rscript ../../Rscripts/score_method.R score.R method.mean.R 1 normal

# this target uses the MAKERULE below to provide rules for these target files 
# it will only update those that need updating
eg2: param/normal/param.1.RData data/normal/data.1.RData output/normal/mean/output.1.RData results/normal/mean/results.1.RData

#The following uses eval and foreach to produce a target for each SEED, SCENARIO pair
#I don't yet have a mechanism for creating a target with all these as dependencies....

define MAKERULE 
param/$2/param.$1.RData:
	Rscript ../../Rscripts/make_params.R parammaker.R $1 $2
data/$2/data.$1.RData:
	Rscript ../../Rscripts/make_data.R datamaker.R $1 $2
endef

define MAKERULE2
output/$2/$3/output.$1.RData:
	Rscript ../../Rscripts/run_method.R method.$3.R $1 $2
results/$2/$3/results.$1.RData:
	Rscript ../../Rscripts/score_method.R score.R method.$3.R $1 $2
endef

SEEDS = 1 2 3 4 5 6 7 8 9 10
SCENARIOS = normal uniform Cauchy 
METHODS = mean median Winsor
 
$(foreach T,$(SEEDS),$(foreach S,$(SCENARIOS),$(eval $(call MAKERULE,$T,$S))))

#uncomment this line if you want to see which targets are being created by this rule
#$(foreach T,$(SEEDS),$(foreach S,$(SCENARIOS),$(info $(call MAKERULE,$T,$S))))

$(foreach T,$(SEEDS),$(foreach S,$(SCENARIOS),$(foreach M,$(METHODS),$(eval $(call MAKERULE2,$T,$S,$M)))))

#uncemment this line if you want to see which targets are being created by this rule
#$(foreach T,$(SEEDS),$(foreach S,$(SCENARIOS),$(foreach M,$(METHODS),$(info $(call MAKERULE2,$T,$S,$M)))))


