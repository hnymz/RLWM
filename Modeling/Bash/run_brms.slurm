#!/bin/bash
#SBATCH --time=8:00:00
#SBATCH --mem=15G
#SBATCH --cpus-per-task=4
#SBATCH --partition=batch

# Activate Conda environment
source ~/miniconda3/etc/profile.d/conda.sh
conda activate myr

# Run the R script
Rscript /home/control/yimzha/Documents/RLWM/Modeling/Regression/run_brms_$1.R
