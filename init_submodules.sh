#!/usr/bin/env bash
#Init submodules in this dir, if any
DIR="$( cd "$( dirname $0 )" && pwd )"
git submodule sync

#Recursively init submodules
#SUBMODULES="\
#    f2_dsp \
#    f2_cm_serdes_lane \
#    "
#for module in $SUBMODULES; do
#    git submodule update --init ${module}
#    cd ${DIR}/${module}
#    if [ -f "./init_submodules.sh" ]; then
#        ./init_submodules.sh
#    fi
#    sbt publishLocal
#    cd ${DIR}
#done


exit 0
