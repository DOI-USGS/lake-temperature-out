#!/bin/bash
#SBATCH --job-name=thermalmetrics   	# name that you chose
#SBATCH --partition=cpu         	# the partition you want to use, for this case prod is best
#SBATCH --cpus-per-task=72		# Number of cores (Hayley mentioned that 80 caused issues)
#SBATCH --nodes=1			# Number of nodes
#SBATCH -A iidd                 	# List account
#SBATCH --output=glm3pb0_sing.out	# file to save output messages to
#SBATCH --time=48:00:00        		# time at which the process will be cancelled if unfinished
#SBATCH --mem=192GB
#SBATCH --mail-type=ALL
#SBATCH --mail-user=lplatt@usgs.gov

module load singularity/3.3.0

ulimit -u 1541404

srun singularity exec \
    lake-temperature-out.sif \
    Rscript glm3pb0_run.R

