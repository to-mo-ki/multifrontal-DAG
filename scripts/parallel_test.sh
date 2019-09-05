cd `dirname $0`
sh scripts/init_test.sh
export OMP_NUM_THREADS=2
if [ -e "test.out" ]; then
  rm test.out
fi
for file in `find obj/parallel_test -type d -name "*.dSYM" -prune -o -name "*.out" -print`
# dSYMフォルダは無視する
do
  filename=`basename ${file}`
  echo "====="${filename%.*}"=====" >> test.out
  for i in `seq 1 5000`
  do
    echo "====="${i}"回目=====" >> test.out
    ${file} >> test.out 2>&1
  done
  echo
done

RESULT=(`cat .test`)
echo "Success ${RESULT[0]}/${RESULT[1]}" >> test.out
echo "End of Test" >> test.out
rm .test