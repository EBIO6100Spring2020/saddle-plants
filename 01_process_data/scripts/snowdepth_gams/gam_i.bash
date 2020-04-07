#!/bin/bash

infiles=~/poorcast/data_inputs/split_input_filenames.txt

IFS=''
while read var
do
Rscript ~/poorcast/server_bash_gam_i.R $var
done < $infiles

