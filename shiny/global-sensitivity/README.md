# Global-sensitivity-analysis
https://github.com/PecanProject/pecan/issues/491

Mike's email:
I’m going to propose that you generate some form of global sensitivity analysis. 

You’ll probably want to look at Istem’s recent commits to the ensemble analysis sampler, as it added new options for sampling and a structure that’s generalizable — that should give you a method for generating samples under an alternative design than the default OAT sensitivity analysis. 

Since this is a prototype you can do the post-hoc analysis of the runs outside of the main PEcAn workflow, based on that ensemble output. But it wouldn’t hurt to look at the SA and EA post-processing code, since ultimately it would be good to implement the prototype for real (at a minimum we’ll drop it in the modules/uncertainty/inst folder and document that it can be run on the output of whatever settings you say is needed).
