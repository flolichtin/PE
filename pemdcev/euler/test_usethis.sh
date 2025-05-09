#!/bin/bash

module load gcc/8.2.0 r/4.1.3

mkdir /scratch/foo

Rscript --vanilla ./euler/test_usethis.R /scratch/foo

module purge r

now="$(date +'%Y-%m-%d')"

mv /scratch/foo /cluster/work/ivt_vpl/daniehei/foo_${now}

