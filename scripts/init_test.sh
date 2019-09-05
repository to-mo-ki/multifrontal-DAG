cd `dirname $0`
cd ../
if [ -e ".test" ]; then
  rm .test
fi
echo "0 0" > .test