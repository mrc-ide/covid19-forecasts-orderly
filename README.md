# orderly

This is an [`orderly`](https://github.com/vimc/orderly) project.  The directories are:

* `src`: create new reports here
* `archive`: versioed results of running your report
* `data`: copies of data used in the reports

Each step of the analysis is an orderly task and corresponds to a
directory on the src directory.

# Description of tasks

1. prepare_ecdc_data This task prepares the ECDC data in a format
   required by the modeling teams. Download the latest data from ECDC
   on Sunday evening and save it as a csv in the folder
   prepare_ecdc_data. Update the resources section in orderly.yml to
   reflect the name of the latest file. Then 
   
   ```
   a <- orderly::orderly_run("prepare_ecdc_data")
   orderly::orderly_commit(a)
   ```
   
   This will generate two aretfacts (outputs of a task are called
   artefacts in orderly). They are exactly the same files but one has
   the date in the file name - this file will be shared with the
   mdoeling groups, and the second file is called
   latest_model_input.rds. This is so that the downstream tasks can
   read the latest file without the need to manually update
   orderly.yml.

2. process_individual_models By Monday afternoon, all modelling teams
   will produce outputs for the report in a prescribed format. These
   could be shared via DropBox for example i.e. the generation of
   model outputs is not part of the orderly workflow. Once these
   outputs are recieved, save them in a directory on your machine, and
   update the location in orderly_envir.yml. For example, this is the
   file on my machine:
   
   ```
   COVID19_INPUT_PATH: "/Users/sbhatia/OneDrive - Imperial College London/covid19-short-term-forecasts/model_outputs/"
   ```
   Note that the trailing slash in the directory name must be
   included. You can run the task as: 
   
   ```
   a <- orderly::orderly_run("process_individual_models", parameter =
   list(week_ending = "2020-04-26"))
   orderly::orderly_commit(a)
   ```

3. produce_ensemble_outputs This task creates an ensemble model. 

   
   ```
   a <- orderly::orderly_run("produce_ensemble_outputs", parameters = list(week_ending = "2020-04-26"))
   orderly::orderly_commit(a)
   ```
   
4. format_model_outputs Pretty formatting of model outputs for
   plugging into the final report.
   
   
   ```
   a <- orderly::orderly_run("format_model_outputs")
   orderly::orderly_commit(a)
   ```
5. produce_visualisations As the name suggests, makes all the graphs
   needed for the report.


   ```
   a <- orderly::orderly_run("produce_visualisations")
   orderly::orderly_commit(a)
   ```
   
6. produce_exec_summary Generates the summary that goes into the
   report. Run as all other tasks. It is the execuive summary which is
   the summary of the new report. produce_summary task is redundant
   and can be ignored for now.

7. produce_full_report Generate the full report.
   
