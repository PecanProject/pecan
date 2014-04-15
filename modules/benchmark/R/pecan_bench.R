pecan_bench <- function(comp_run,bench_id,imp_limit,high_limit){

#	    comp_run = The results which we want to do the comparison results with. I would recommend having the input already be in similar table format than the style
#	    the bench mark runs have been saved as.
#	    bench_id = The ID number of the benchmark run the results are to be compared against. I would recommend setting this to be the latest saved benchmark run by default,
#	    so that the value needs to be only specifically given when wanting to examine a specific benchmark comparison.
## MCD2: this naming and the descriptions are ambiguous. I'd go with something like new_id and ref_id. I think it'd be fine for both to have defaults (e.g. newest and best runs respectively)
  
#	    imp_limit = The ratio of comparison values which need to be improved in order for the run to be accepted as a new benchmark. Should be set to some default value if user
#	    	      doesn't want to give a specific value.
## MCD2: should default to a value that never causes an insert (e.g. NULL or -1). 
## That way if the user plays with this function on the command line they don't have to worry about it, but we can set the value in the workflow.
#	    high_limit = The ratio where the increase in value is decided to be large enough for alert. Again, should be set to some default value if user doesn't want to give a specific value.

  
## MCD: Ok, the #1 thing that has to happen next is that function definitions and interfaces need to be flushed out more
## In particular we need to know if the database design has anything missing or any thing superfluous
## as well as what needs to be passed to each function vs looked up internally

#     	    I am now starting with the program by checking if the submitted run results are valid with a subroutine. I also think that instead of having those logical checks as a part
#	    of a table, they should be just written out in the subroutine and if the results do not fill the given standards, the subroutine should print out the specific instances.
#	    If the submitted run fails to fill all the given validation standards, the outcome of the subroutine should be FALSE at which point the program is stopped.
  
##MCD2: What do you mean by "valid"?  I agree that we shouldn't run the benchmark if the run failed to complete, but I'm not sure what other checks you envision
  
     	    logic_check <- validation_check(comp_run)

	    if(isFALSE(logic_check){
	     print('The results were found to be invalid')
	     stop()
	    }

#	    The observed values against which the runs will be compared to. Should be the same for all benchmark runs. N here is the number of observed values. I left the read_values vague,
#	    as I am not certain which format the data will be in.
## MCD: some path or index for this data file needs to be passed as an argument or looked up in the database
#  	    The comparison values should be the same for all benchmark runs and if they are changed, then all the runs should be redone. Because of this, I would rather have these
#	    read from a set file instead of a passed argument. As for the structure of the file, I would suggest each line holds at least the observation value, associated uncertainty,
#	    observation type and location.
## MCD2: I agree that for a given benchmark the data should never be changed, but we'll likely have more than one benchmark 
## (e.g. runs at more than one site, more than one variable we want to track) so we need flexibility.
## More generally, files should never be hardcoded since these tests will run on many different machines

	    obs_file <- read.table('Location of the data table')
	    obs_value <- obs_file$value
	    N <- size(obs_value)

#	    Read the previously accepted benchmark standard run values
## MCD: again, this needs to be an argument or database item. I'd favor passing it so that the function could be used to compare different reference points as needed
#  	    I do not necessarily agree on passing the item, as I was thinking that all the benchmark runs being a part of the same growing table. Instead I would use the bench_id to determine
#	    which bench run the current results are compared against, although I am not currently completely certain how to specify the part in the data table with bench_id.
#	    I would suggest each benchmark run contains the results from the run, their associated uncertainties, associated id number, location and type for each result, 
#	    model identification and a brief describtion of what was changed for this run.
## MCD2: on the last part, I think it would be great to have the description to default to a version control reference. 
## More generally, I agree that it should be easier to annotate the runs we do, both in the runs table and here, 
## since right now the History table is not very informative to WHY a run was done unless the purpose of that run was recorded elsewhere.
## seems like it would be much better to have all that in one place

	    bench_file <- read.table('Location of the data table',bench_id)
	    bench_value <- bench_file$value

#	    First calculate the differences between the current benchmark run and the observed values

	    bench_dif <- obs_value - bench_value

#	    Calculate the differences between the current run and the observed values

	    comp_dif <- obs_value - comp_value

#	    Determine the relative changes in differences between runs and observed values
	    
	    bench_ratio <- comp_dif/bench_dif
## MCD2: this could easily result in division by zero. You might want to compare the ratio of RMSEs not the individual point-wise differences

#	    If uncertainties available, calculate the uncertainty for the ratio. Here I used the traditional error propagation, but other approaches should be possible.

	    bench_ratio_uncertainty <- bench_ratio*sqrt((comp_dif_uncertainty/comp_dif)**2 + (ratio_dif_uncertainty/ratio_dif)**2)

## MCD: We seem to be calculating this but not using it anywhere below
##   Kevin Schaefer advocates the Chi-Sq metric since it allows for uncertainty See Schwalm et al 2010 eqn 2
##   However that doesn't allow for error in both the model and data.  A bit of Googling suggests  Welch's t may be a metric that includes both uncertainties
##   But there may be other/better options
#  I originally didn't use the t test as I wanted to have an output value that is really clear for the user to interpret and the t test will give a statistical value, but
#  will not indicate how large the change is. However, on thinking more, I agree that this should also be included in the output and, instead of having a set change limit to determine
#  whetever the new run is save as the current benchmark comparison run, we print out the new results and t-test result for it and ask the user to decide whetever this satisfies them
#  and they want to save it as the comparison run.
## MCD2: I wasn't suggesting that any decision be made based on any sort of p-value, I was just suggesting an alternate metric for scoring model performance that accounts for uncertainties.
## I'm not sure there's a point in accounting for uncertainty in the model-model comparision, since the models are all deterministinc
## but we definitely want to account for model and data uncertainty in the model-data comparisons.
## Overall, the focus here should be on the model-data comparison, but obviously we need to check against the previous model version in order to flag benchmark runs
## where performance decreases.

#  The slight hesitation I have for this is due to not knowing if it will be only one person making the runs and decisions, or if there is a group independently submitting sets of runs.
#  Thus if you had a larger number of people making their own individual decisions which are saved to the table used by the whole group, there might be large variance on how strict 
#  the criteria was for different accepted runs.
## MCD2: Good point. A group will be submitting runs independently.

#  As for the method, I am open for both methods or any other being suggested, but it is important to note that the test would be a comparison between comp_dif and bench_dif, and thus
#  between two model uncertainties. Thus before actually deciding the exact test chosen for this purpose, we need to be clear on how that uncertainty would be determined. Is it just
#  the ensemble of runs? Can it be considered to be normally distributed? etc. I am perhaps myself leaning on the Welch's t-test at the moment.

#	    Here we calculate the average bench_ratio and the proportional amount of components which show improvement. Here a reduction of 0.025 as ratio was used as the indicator of
#	    the current run doing better, but it is a whimsical choice for now. The actual value needs to be decided. An option of how to do that is to set the limit to be larger than
#	    the associated uncertainty of calculated benchmark ratio uncertainty.
#	    Here I used a simple average of absolute values as to calculate the bench ratio mean. Another option would be to calculate an averaged weighed by individual uncertainties.
#	    I would however prefer to keep the benchmark comparison results easily understandable.
## MCD: I think one critical component in this has to be assessing stability of results, not improvement
## a huge chunk of the time we're just wanting to make sure first-and-foremost that some change in the code didn't break something
## absolute equality may not be possible, but results shouldn't get worse by more than some tolerance without throwing some flag
## Also, we need metrics that catch cases where the average error may improve but specific predictions get worse
## I think the bench_ratio_count may be on the right track here, but with a threshold of 1.0+delta
#    	    I changed the cut_off limit for improvement to be an input value. I also added an additional value bench_ratio_warn, which shows the sum of values where the 
#	    bench_ratio increases too much as a potential warning. This is not a fraction in order to avoid it being buried by a large N value.

	    bench_ratio_mean <- mean(abs(bench_ratio))
	    bench_ratio_count <- sum(bench_ratio < imp_limit)/N
	    bench_ratio_high <- sum(bench_ratio > high_limit)

#	    Print out both the average value and amount of comparisons where we saw improvement. Also, if we saw too high values, print out a warning for that.

	    bench_print <- c(bench_ratio_mean,bench_ratio_count)
	    print(bench_print)

	    if(bench_ratio_high > 0){
	     print(paste('Warning:', N, 'variables showed a larger than suggested increase in value')
	    }

## MCD: the specific stats involved could vary, but they definitely need to written to database not screen
## For the benchmark report itself I'd be in favor of doing all this in a RMarkdown document or in Shiny so that we can combine figures, tables, text, etc and spit out a html report.
## In terms of graphics, David and I are both fans of Taylor Plots (and specificially some form of cumulative Taylor Plot showing the trajectory of banchmark stats over time)
# I am open to those suggestions, although I am not certain on how to do the cumulative Taylor Plot effectively. 
## MCD2: in R you use the function taylor.diagram in the "plotrix" library to make the plot. you then call the function again with add=TRUE to add additional points to the plot. 
## by "cumulative" I just mean that we should include more than just the current run and the reference run, but that it would be nice to show the history
## maybe not every individual minor change, but maybe any time the model changed non-trivially. 
## It would also be good to be able to show multi-model taylor plots -- e.g. the current, the reference, and the "best" for all models that have run that same benchmark

#	    Here we determine if the current run performs better than the previous benchmark run. The ratio of 0.975 is set as the limit for ratio reduction for the bench mark to be successful.
#	    Additionally we require that a certain number of control variables need to show improvement. Here that number was chosen to b 80 % of the sites.
#	    Improvement in uncertainty can be included in several ways, not touched on here yet.

#	    If the current runs performed better, it is stored as the new bench mark. Additionally, we calculate the 

	    if(bench_ratio_mean < imp_limit .and. bench_ratio_count > 0.8){
	     Set_Bench(...)	#Subroutine which sets the current run to be the new benchmark comparison run as well as saves the previous benchmark comparison run a database
	     Compare_Bench(...) #Subroutine which calculates a new vector of values which are essentially the benchmark ratios between old runs and the current comparison run.
}
## MCD: I wonder if this should be automatic (i.e. every time the model does better it becomes the new benchmark) 
## or whether it should be something where the user is prompted to confirm that we want to raise the bar
# I think it would work to have the user decide whetever or not to accept this new value, but in order to have that, we need to be able to have the user to access the more specific
# results before they need to decide. Also, I did not find out how to make the prompt a yes/no question.
}