cd `dirname $0`
cd ../
sh scripts/init_test.sh
export OMP_NUM_THREADS=2
for file in `find obj/test -type d -name "*.dSYM" -prune -o -name "*.out" -print`
# dSYMフォルダは無視する
do
  filename=`basename ${file}`
  echo "====="${filename%.*}"====="
  ${file}
  echo
done

RESULT=(`cat .test`)
echo "Success ${RESULT[0]}/${RESULT[1]}"
echo "End of Test"
rm .test