export currDir=$(cd `dirname $0` && pwd)/..
export QINFRA=$currDir/../qinfra

cd $currDir

$QINFRA/l32/q.sh "$@" 
