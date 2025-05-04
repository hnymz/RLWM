#!/bin/bash

for model in {1..5}; do
  echo "Submitting run_brms_$model.R ..."
  sbatch \
    --job-name="brms_${model}" \
    -o "/home/control/yimzha/Documents/logs/brms_${model}_%j.out" \
    run_brms.slurm "$model"
done
