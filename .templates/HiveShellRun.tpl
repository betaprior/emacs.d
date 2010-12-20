#!/bin/bash
#  -*- mode:shell-script; mode:folding  -*- 
# {{{
# generated from template: ~/.emacs.d/.templates/HiveShellRun.tpl
QUERYLOG=$(dirname $0)/$(basename $0).querylog

GEN_DATE_SCRIPT=/home/hadoop/git-checkouts/datamining-analytics/utils/gen_daterange.py
YESTERDAY=$(${GEN_DATE_SCRIPT} $(date "+%Y%m%d") --subtract 2 --getlast --separator=-)
TODAY=$(date "+%Y-%m-%d")

function build_sed_string() {
    arr=( $(sed -e 's/ \+/\x0/g' <<< "$@" | perl -0e 'print sort {length $b <=> length $a} <>' | tr '\0' '\n') )
    ss='sed -i'
    for a in ${arr[@]}; do
	ss=${ss}' -e "s/\'$a'/'$a'/g"'
    done
    echo "$ss"
}

QUERYFILE=$(mktemp)
trap 'rm -rf "${QUERYFILE}"; exit' INT TERM EXIT

function run_query_with_vars() {
    if [ "$1" ]; then
	QUERYVARS=$1
	eval $(build_sed_string $QUERYVARS) $QUERYFILE
    fi
    if [ "$QUERYLOG" ]; then
	cat $QUERYFILE >> $QUERYLOG
    fi
    hive -S -f $QUERYFILE
}

# }}}

cat > $QUERYFILE <<'EOF'

EOF

VAR=
for ITER in {1}; do
    run_query_with_vars '$VAR $ITER'
done

# Local variables:
# folded-file: t
# end:
