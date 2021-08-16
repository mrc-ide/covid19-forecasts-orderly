cd draft/prepare_hm_outputs/20210809-143416-f2136e9e
aws s3 cp cases_all.csv s3://s3-ftp-dev.mriids.org/uploads/
aws s3 cp deaths_all.csv s3://s3-ftp-dev.mriids.org/uploads/
aws s3 cp cases_per_1e6.csv s3://s3-ftp-dev.mriids.org/uploads/
aws s3 cp deaths_per_1e6.csv s3://s3-ftp-dev.mriids.org/uploads/
aws s3 cp latest_model_outputs.csv s3://s3-ftp-dev.mriids.org/uploads/
aws s3 cp deploy.txt s3://s3-ftp-dev.mriids.org/uploads/


aws s3 cp cases_all.csv s3://s3-ftp.mriids.org/uploads/ --profile prod
aws s3 cp deaths_all.csv s3://s3-ftp.mriids.org/uploads/ --profile prod
aws s3 cp cases_per_1e6.csv s3://s3-ftp.mriids.org/uploads/ --profile prod
aws s3 cp deaths_per_1e6.csv s3://s3-ftp.mriids.org/uploads/ --profile prod
aws s3 cp latest_model_outputs.csv s3://s3-ftp.mriids.org/uploads/ --profile prod
aws s3 cp deploy.txt s3://s3-ftp.mriids.org/uploads/ --profile prod
