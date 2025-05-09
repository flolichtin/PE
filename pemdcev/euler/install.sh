#!/bin/bash

module load gcc/8.2.0 r/4.1.3

R CMD INSTALL --preclean --no-multiarch --with-keep.source ../labelr
R CMD INSTALL --preclean --no-multiarch --with-keep.source ../Heimisc
R CMD INSTALL --preclean --no-multiarch --with-keep.source ../PE
R CMD INSTALL --preclean --no-multiarch --with-keep.source ../pemdcev

module purge r
