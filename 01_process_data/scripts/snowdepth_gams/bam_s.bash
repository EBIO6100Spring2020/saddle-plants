#!/bin/bash

infiles=~/poorcast/data_inputs/split_input_filenames.txt

IFS=''
while read var
do
Rscript ~/poorcast/server_bash_bam_s.R $var
done < $infiles

