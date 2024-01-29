hm=$(pwd)
main=main.r

ns=(25)
its=(7000)
bits=(10)
ks=(2)
tols=(1 5 9)
ppuns=(0.0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1.0)
fpuns=(0.0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1.0)
wprods=(0.0)



nens=250
rm -r JOB_*
for n   in ${ns[@]};do
for it  in ${its[@]};do
for bit in ${bits[@]};do
for k   in ${ks[@]}; do
for tol in ${tols[@]}; do
for wprod in ${wprods[@]}; do

fn=JOB_${n}_${it}_${bit}_${k}_${tol}_${wprod}
mkdir $fn
cd $fn

for ppun in ${ppuns[@]}; do
for fpun in ${fpuns[@]}; do
fname=PUN_${ppun}_${fpun}
mkdir $fname
cd $fname
for (( i=1; i<=$nens; i++))
do
	folder_n=$i
	mkdir $folder_n
	cp $hm/$main $folder_n
	sed -i "s|XXX1|$n|g"     $folder_n/$main	
	sed -i "s|XXX2|$it|g"    $folder_n/$main	
	sed -i "s|XXX3|$bit|g"   $folder_n/$main	
	sed -i "s|XXX4|$k|g"     $folder_n/$main	
	sed -i "s|XXX5|$tol|g"   $folder_n/$main	
	sed -i "s|XXX6|$ppun|g"  $folder_n/$main	
	sed -i "s|XXX7|$fpun|g"  $folder_n/$main	
	sed -i "s|XXX8|$wprod|g" $folder_n/$main	
done
cd ../ #cd $fname
done
done


cd ../ #cd $fn
done
done
done
done
done
done
