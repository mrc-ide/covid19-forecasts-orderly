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
## Running tasks

```
orderly run produce_epi_params
orderly run prepare_ecdc_data week_ending=2020-03-08
```
