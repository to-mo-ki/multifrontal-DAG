cd `dirname $0`
cd ../obj/debug
profmerge -a
codecov -dpi pgopti.dpi -spi pgopti.spi
cd ../../
