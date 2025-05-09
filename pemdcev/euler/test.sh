#!/bin/bash

module load gcc/8.2.0 r/4.1.3

mkdir /scratch/foo

Rscript --vanilla ./euler/test.R 10 /scratch/foo

module purge r

now="$(date +'%Y-%m-%d')"

mv /scratch/foo /cluster/work/ivt_vpl/daniehei/batch_test_${now}

