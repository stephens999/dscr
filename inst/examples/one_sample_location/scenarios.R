sourceDir("datamakers")
addScenario(dsc_eg,"normal",datamaker,list(disttype="normal",nsamp=1000),1:100)
addScenario(dsc_eg,"uniform",datamaker,list(disttype="uniform",nsamp=1000),1:100)
addScenario(dsc_eg,"Cauchy",datamaker,list(disttype="Cauchy",nsamp=1000),seed=1:100)


