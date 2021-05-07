## Logging into the server

```
ssh vagrant@covid19-forecasts.dide.ic.ac.uk
cd covid19-forecasts-orderly-image/
./shell
```

## Pulling from github

```
git fetch
git checkout <branch_name>
git merge
```
## Updating packages in the container

Nicked from instructions on ebola repo

```
./shell
R
remotes::install_github("reconhub/projections")
install.packages("whatever") 	
```

You will also need to edit the dockerfile to include this package.
Do this via a PR in the repository
https://github.com/mrc-ide/covid19-forecasts-orderly-image/ and add
Rich as a reviewer.



## Running tasks

```
orderly run produce_epi_params
orderly run prepare_ecdc_data week_ending=2020-03-08
```

## Orderly web interface

Outputs can be accessed through the web-interface here
https://covid19-forecasts.dide.ic.ac.uk/


## Pulling dependancies

To pull task outputs from the server to your machine, do 

```
orderly::orderly_pull_archive(<task-name>)
```
