#!/bin/bash
############################
hm=$(pwd)
scr="$SCRDIR/${USER}"
list_jobs=()
slurm_out="slurm_out"
job_name='job.sh'
threads=1
memory=3
echo Script is running now
echo Scracth path: $scr
echo
main="main.r"
#############################################
for job in $(ls -vd JOB_*); do
cd $job
for f1 in $(ls -vd PUN_*); do
	cd $f1
	for f2 in $(ls -vd */); do
		cd $f2
		if [ ! -e "totsum.txt" ]; then
		list_jobs+=($(pwd))
        	fi
		cd ../
	done
	cd ../
done
cd ../
done
#END SCRIPT
###########################################
ll=''
Njobs=0
for j in $(ls -vd ${list_jobs[@]} ); do
	ll="$ll $j"
	Njobs=$(echo $Njobs+1| bc)
done
list_jobs=$ll
###########################################
echo "Submiting the jobs now!"
rm -rf $slurm_out
mkdir $slurm_out
rm -rf $job_name
# creating the job.sh file
#******************************************
#******************************************
echo '#!/bin/bash'                                                                    >> $job_name
echo '#SBATCH -J '$job_name''                                                         >> $job_name
echo '#SBATCH --cpus-per-task='$threads''                                             >> $job_name
echo '#SBATCH --mem-per-cpu='$memory'G'                                               >> $job_name
echo '#SBATCH --array=1-'$Njobs''                                                     >> $job_name
echo '#SBATCH --output='$(pwd)'/'$slurm_out'/slurm_%A_%a.out'                         >> $job_name
echo ''                                                                               >> $job_name
echo 'list_jobs=('$list_jobs')'                                                       >> $job_name
echo 'index=$(echo $SLURM_ARRAY_TASK_ID-1 | bc)'                                      >> $job_name
echo 'job=${list_jobs[$index]}'                                                       >> $job_name
echo 'path='$scr'/${SLURM_ARRAY_JOB_ID}_${SLURM_ARRAY_TASK_ID}'                       >> $job_name
echo 'hostname'                                                                       >> $job_name
echo 'echo $SLURM_ARRAY_TASK_ID $job'                                                 >> $job_name
echo 'rm -rf $path'                                                                   >> $job_name
echo 'mkdir -p $path'                                                                 >> $job_name
echo 'cp -r $job/. $path'                                                             >> $job_name
echo 'cd $path'                                                                       >> $job_name
echo 'Rscript '$main''                                                                >> $job_name
echo 'cp -r . $job'                                                                   >> $job_name
echo 'rm -rf $path'                                                                   >> $job_name
#******************************************
#******************************************
sbatch $job_name

