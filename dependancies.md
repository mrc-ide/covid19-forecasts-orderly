prepare_ecdc_data
process_individual_models <-- prepare_ecdc_data
DeCA_model
collate_model_outputs <-- produce_weighted_ensemble;
produce_ensemble_outputs;prepare_ecdc_data

** collate_model_outputs orderly.yml file needs to be updated to 
include the latest runs of produce_weighted_ensemble and 
produce_ensemble_outputs


compare_ensemble_outputs <-- collate_model_outputs; produce_performace_metrics


compute_model_weights <-- produce_performace_metrics

format_model_outputs <-- process_individual_models;
produce_ensemble_outputs

produce_ensemble_outputs <-- 


produce_performace_metrics <-- produce_weighted_ensemble;
produce_ensemble_outputs

** orderly.yml file needs to be update manually to include latest
reports of produce_weighted_ensemble and produce_ensemble_outputs


produce_weighted_ensemble <-- compute_model_weights

