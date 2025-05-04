#!/bin/bash
#SBATCH --job-name=rl_stan
#SBATCH --output=rl_stan_%j.out
#SBATCH --time=4:00:00
#SBATCH --mem=10G
#SBATCH --cpus-per-task=4
#SBATCH --partition=batch
#SBATCH --output=/home/control/yimzha/Documents/logs/rl_stan_%j.out

# Activate Conda environment
source ~/miniconda3/etc/profile.d/conda.sh
conda activate myr

# Run the R script
Rscript /home/control/yimzha/Documents/RLWM/Modeling/run_rl_model.R
