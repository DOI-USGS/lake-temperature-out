#!/bin/bash
#SBATCH --job-name=thermalmetrics   	# name that you chose
#SBATCH --partition=cpu         	# the partition you want to use, for this case prod is best
#SBATCH --cpus-per-task=71		# Number of cores (Hayley mentioned that 80 caused issues)
#SBATCH --nodes=1			# Number of nodes
#SBATCH -A iidd                 	# List account
#SBATCH --output=glm3pb0_gcms.out	# file to save output messages to
#SBATCH --time=48:00:00        		# time at which the process will be cancelled if unfinished
#SBATCH --mem=192GB
#SBATCH --mail-type=ALL
#SBATCH --mail-user=lplatt@usgs.gov

module load singularity

srun singularity exec \
    lake-temperature-out_v0.1.sif \
    Rscript glm3pb0_gcms_run.R

