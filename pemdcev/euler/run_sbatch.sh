#!/bin/bash

jobName=$1
runFile=$2
outFile=$3
sbatch -n 10 --output=${outFile} --time 120:00:00 --mem-per-cpu=1G --job-name=${jobName} < ${runFile}
