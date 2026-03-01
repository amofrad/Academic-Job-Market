#!/bin/bash
#$ -cwd
#$ -o /dev/null
#$ -e /dev/null
#$ -l h_rt=24:00:00,h_data=4G
#$ -pe shared 20
#$ -t 1-10

. /u/local/Modules/default/init/modules.sh
module load apptainer/1.2.2

echo "=== SIM BATCH: Task $SGE_TASK_ID ==="
echo "Start: $(date)"
echo "Host: $(hostname)"
echo "Cores: $NSLOTS"

apptainer exec $H2_CONTAINER_LOC/h2-rstudio_4.4.2.sif \
    Rscript sim_batch.R $SGE_TASK_ID

echo "End: $(date)"
