#!/bin/bash

module load gcc/8.2.0 r/4.1.3

mkdir /scratch/apollo_output

Rscript --vanilla ./data-raw/apollo-15.R 10 /scratch/apollo_output

module purge r

now="$(date +'%Y-%m-%d')"

mv /scratch/apollo_output /cluster/work/ivt_vpl/daniehei/batch_run_${now}

