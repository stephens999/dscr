library(dscr)
sourceDir("methods")
sourceDir("datamakers")

###### Initialize #######

dsc_shrink=new.dsc("shrinkage","dsc-shrink-files")

###### Add Scenarios #####

addScenario(dsc_shrink,name="A",
            fn=rnormmix_datamaker,
            args=list(
              g=normalmix(c(2/3,1/3),c(0,0),c(1,2)),
              min_pi0=0,
              max_pi0=0,
              nsamp=1000,
              betahatsd=1
            ),
            seed=1:2)

# addScenario(dsc_shrink,name="B",
#             fn=rnormmix_datamaker,
#             args=list(
#               g=normalmix(rep(1/7,7),c(-1.5,-1,-0.5,0,0.5,1,1.5),rep(0.5,7)),
#               min_pi0=0,
#               max_pi0=0,
#               nsamp=1000,
#               betahatsd=1
#             ),
#             seed=1:100)
# 
# 
# addScenario(dsc_shrink,name="C",
#             fn=rnormmix_datamaker,
#             args=list(
#               g=normalmix(c(1/4,1/4,1/3,1/6),c(-2,-1,0,1),c(2,1.5,1,1)),
#               min_pi0=0,
#               max_pi0=0,
#               nsamp=1000,
#               betahatsd=1
#             ),
#             seed=1:100)
# 
# #scenarios An, Bn, Cn are the same as A,B,C but with nulls included (pi0 uniform on [0,1])
# addScenario(dsc_shrink,name="An",
#             fn=rnormmix_datamaker,
#             args=list(
#               g=normalmix(c(2/3,1/3),c(0,0),c(1,2)),
#               min_pi0=0,
#               max_pi0=1,
#               nsamp=1000,
#               betahatsd=1
#             ),
#             seed=1:100)
# 
# addScenario(dsc_shrink,name="Bn",
#             fn=rnormmix_datamaker,
#             args=list(
#               g=normalmix(rep(1/7,7),c(-1.5,-1,-0.5,0,0.5,1,1.5),rep(0.5,7)),
#               min_pi0=0,
#               max_pi0=1,
#               nsamp=1000,
#               betahatsd=1
#             ),
#             seed=1:100)
# 
# 
# addScenario(dsc_shrink,name="Cn",
#             fn=rnormmix_datamaker,
#             args=list(
#               g=normalmix(c(1/4,1/4,1/3,1/6),c(-2,-1,0,1),c(2,1.5,1,1)),
#               min_pi0=0,
#               max_pi0=1,
#               nsamp=1000,
#               betahatsd=1
#             ),
#             seed=1:100)
# 
# addScenario(dsc_shrink,name="hard",
#             fn=rnormmix_datamaker,
#             args=list(
#               g=normalmix(c(.4,.2,.2,.2),c(0,0,0,0),c(.25,.5,1,2)),
#               min_pi0=0,
#               max_pi0=1,
#               nsamp=1000,
#               betahatsd=1
#             ),
#             seed=1:100)
# 
# addScenario(dsc_shrink,name="easy",
#             fn=rnormmix_datamaker,
#             args=list(
#               g=normalmix(c(1),c(0),c(4)),
#               min_pi0=0,
#               max_pi0=1,
#               nsamp=1000,
#               betahatsd=1
#             ),
#             seed=1:100)

###### Add Methods #####

addMethod(dsc_shrink,"ashr",ash.wrapper,outputtype="ash_output")



####### Define Score and Add it #######

score = function(data, output){
  return(list(RMSE=sqrt(mean((output$beta_est-data$meta$beta)^2))))
}

addScore(dsc_shrink,"basicscore",score,outputtype="est_output")

######## Run the DSC #################

reset_dsc(dsc_shrink,force=TRUE)
res1=run_dsc(dsc_shrink)
save(dsc_shrink,file="dsc_shrink.RData")

### That doesn't work because the output format was not right - we need to add a parser

#this parser converts the ash output to a list with element beta_est
ash2beta_est =function(output){
  return (list(beta_est=output$PosteriorMean))
} 

addParser(dsc_shrink,"ash2beta",ash2beta_est,"ash_output","est_output")

#this parser extracts the estimate of pi0
ash2pi0 =function(output){
  return (list(pi0_est=get_pi0(output)))
} 

addParser(dsc_shrink,"ash2pi0",ash2pi0,"ash_output","pi0_output")
res2=run_dsc(dsc_shrink)


score2 = function(data, output){
  return(list(pi0_est=output$pi0_est,pi0=data$meta$pi0))
}

addScore(dsc_shrink,"pi0score",score2,outputtype="pi0_output")
res3=run_dsc(dsc_shrink)

runScore(dsc_shrink,1,"A","ashr","pi0score")
