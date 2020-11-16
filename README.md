# lake-temperature-out
outputs and summaries from lake modeling pipelines

# Running models on USGS clusters

## Yeti quickstart

Once everything is set up (see below), log into Yeti and get started like this:
```sh
ssh yeti.cr.usgs.gov
cd /cxfs/projects/usgs/water/iidd/data-sci/lake-temp/lake-temperature-out
```

## Using Interactive:
make sure you belong to the watertemp group -or- use iidd (or cida) in place of it below:

```sh
salloc -A iidd -n 4 -p normal -t 7:00:00
```
this ☝️ gives you 4 cores on normal for 7 hours. You probably want way more than 4, but this is a start.
Then ssh into the node you are given, and from there, go to the working directory
```sh
ssh n3-98
cd /cxfs/projects/usgs/water/iidd/data-sci/lake-temp/lake-temperature-out
```
Load modules as before:
```sh
module load legacy R/3.6.3 tools/nco-4.7.8-gnu tools/netcdf-c-4.3.2-intel
```
then start R
```sh
R
```
Now you are in R but on a big cluster, so the number of cores you have available is much greater than on your own machine (unless you only asked for 4 cores...)
```sh
library(scipiper)
sbtools::authenticate_sb('cidamanager')
scmake('3_summarize/out/annual_metrics_pgdl.csv')
```
but code will need to be modified so that you use loop tasks and also so you can specify the number of cores loop tasks is using...

If the job fails or you are kicked off Yeti, no worries, as remake/scipiper will pick back up where you left off in the task table. 🎉


## Editing files on the cluster

You can use `vim` to edit files locally.

You can also use the Jupyter interface to edit files via a browser-based IDE. See https://hpcportal.cr.usgs.gov/hpc-user-docs/Yeti/Guides_and_Tutorials/how-to/Launch_Jupyter_Notebook.html for more.

Once you have set up a script to launch Jupyter Lab for the project, and created the jlab environment for the user (see instructions below),

Steps:

1. In a new terminal window (call this one Terminal #2, assuming you'll keep one open for terminal access to Yeti):
```sh
ssh yeti.cr.usgs.gov
cd /cxfs/projects/usgs/water/iidd/data-sci/lake-temp/lake-temperature-out
module load legacy
module load python/anaconda3
salloc -J jlab -t 2:00:00 -p normal -A iidd -n 1 -c 1
sh launch-jlab.sh
```
and copy the first line printed out by that script (begins with `ssh`). Note that this terminal is now tied up.

2. In another new terminal window (call this one Terminal #3), paste the ssh command, which will look something like this:
```sh
 ssh -N -L 8599:igskahcmgslih03.cr.usgs.gov:8599 hcorson-dosch@yeti.cr.usgs.gov
```
Enter the command. Note that this terminal is now tied up.


#### Creating a script to launch Jupyter Lab (once per project)
Save the following script to `launch-jlab.sh`.

```sh
#!/bin/bash

JPORT=`shuf -i 8400-9400 -n 1` 

source activate jlab

echo "ssh -N -L $JPORT:`hostname`:$JPORT $USER@yeti.cr.usgs.gov"

jupyter lab --ip '*' --no-browser --port $JPORT --notebook-dir=. &

wait
```

Next we need to add the base R library from the Yeti R 3.6.3 module to our .Renviron file, so that it can be accessed by the Rkernel in Jupyter Lab.

In the console, within the project directory, type `vim .Renviron`. Enter 'i' to enter the insert mode, and paste in the following line:

```sh
R_LIBS_USER="/cxfs/projects/usgs/water/iidd/data-sci/lake-temp/lake-temperature-out/Rlib_3_6":"/opt/ohpc/pub/usgs/libs/gnu8/R/3.6.3/lib64/R/library"
```
Press 'Esc', then type ':wq' to save and close the file.

Launch Jupyter Lab and open a new Jupyter Notebook with the R kernel. Run the command `.libPaths()`. You should see these 3 paths listed in this order:
```sh
'/cxfs/projects/usgs/water/iidd/data-sci/lake-temp/lake-temperature-out/Rlib_3_6'
'/opt/ohpc/pub/usgs/libs/gnu8/R/3.6.3/lib64/R/library'
'/home/{username}/.conda/envs/jlab/lib/R/library'
```
Now we should be able to load any libraries from our project library folder while in Jupyter Lab, and any necessary dependencies that are not in our project library folder will be loaded from the Yeti R 3.6.3 module library.

#### Creating a conda Jupyter Lab environment (once per user)
```sh
module load legacy
module load python/anaconda3
conda create -n jlab jupyterlab -c conda-forge
```

In order to add an R kernel to the Jupyter Lab IDE (so that we can build and run R notebooks in addition to Python notebooks), we need to run the following series of commands:
```sh
module load legacy
module load python/anaconda3
conda activate jlab
conda install -c r r-irkernel zeromq
```
If you have already launched Jupyter Lab, you will have to re-launch Jupyter Lab to see the R kernel.