
# Introduction to Dynamic Statistical Comparisons

Dynamic statistical comparisons (DSCs) are an attempt to change the way that researchers perform statistical comparisons
of methods. When a new statistical method is developed, it is almost inevitable that it will be useful to compare
it to other methods for tackling the same problem. However, the way these comparisons are currently (usually) done is
suboptimal in so many ways. First, comparisons are usually performed by the research group that developed one of the methods, which almost inevitably favors that method. Furthermore, performing these kinds of comparisons is incredibly time-consuming, requiring careful familiarization with software implementing the methods, and the creation of pipelines and scripts for running and comparing them. And in fast-moving fields new methods or software updates appear so frequently that comparisons are out of date before they even appear. In summary, the current system results in a large amount of wasted effort, with multiple groups performing redundant and sub-optimal comparisons.

A DSC is a public Internet repository that allows methods to be compared with one another in a reproducible and easily-extensible way. (The emphasis being on easily-extensible first, reproducible second.) The ultimate goal is that the
repositories will provide “push-button” reproducibility of all comparisons: running a single script will run all the methods in the repository on all the data sets, and produce results comparing performances. 
It should be simple to add code for a new method, or a new data set, and re-run the comparison script. 
These repositories will help establish which methods perform well on which data, and allow users to easily investigate how software settings or types of input data affect performance. Having code for each method available will substantially reduce the bar for others to build on and improve methods. The repositories should be under version control (eg git), so that the full history will be available, progress can be tracked, and citations can reference a specific version. 

We envisage DSCs as complementing, rather than replacing, the standard publication model: papers introducing a new method will “deposit” the method in the DSC repo, in the same way that scientific data are deposited in data repositories.

The DSC project is funded by a Data Drive Discovery Investigator award from the Moore Foundation to Matthew Stephens.
