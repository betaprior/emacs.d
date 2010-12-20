#!/bin/bash
#  -*- mode:shell-script; mode:folding  -*- 
# {{{
# generated from template: ~/.emacs.d/.templates/HiveShellRun.tpl
QUERYLOG=$(dirname $0)/$(basename $0).querylog

GEN_DATE_SCRIPT=/home/hadoop/git-checkouts/datamining-analytics/utils/gen_daterange.py
YESTERDAY=$(${GEN_DATE_SCRIPT} $(date "+%Y%m%d") --subtract 2 --getlast --separator=-)
TODAY=$(date "+%Y-%m-%d")

function build_sed_string() {
    for i in "$@" ; do
        case $i in 
                -i) SEDOPT[0]='-i'
                        shift ;;
                --) shift ; break ;;
        esac
    done
    arr=( $(sed -e 's/ \+/\x0/g' <<< "$@" | perl -0e 'print sort {length $b <=> length $a} <>' | tr '\0' '\n') )
    ss="sed ${SEDOPT[@]}"
    for a in ${arr[@]}; do
	ss=${ss}' -e "s#\'$a'#'$a'#g"'
    done
    echo "$ss"
}


QUERYFILE=$(mktemp)
QUERYFILE_SUB=$(mktemp)
trap 'rm -rf "${QUERYFILE}" "${QUERYFILE_SUB}"; exit' INT TERM EXIT

function run_query_with_vars() {
    if [ "$1" ]; then
	QUERYVARS=$1
	eval $(build_sed_string $QUERYVARS) $QUERYFILE >> $QUERYFILE_SUB
    fi
    if [ "$QUERYLOG" ]; then
	cat $QUERYFILE_SUB >> $QUERYLOG
    fi
    hive -S -f $QUERYFILE_SUB
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
