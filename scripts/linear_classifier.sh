#!/bin/bash

RED='\033[0;31m'
NC='\033[0m' # No Color

ntests=0
nbits=8
verbose=""

usage() {
    echo "$0 [options] [thresholds]"
    echo "    -t NUM    run NUM random tests"
    echo "    -h        this message"
    echo "    -b        number of bits [$nbits]"
    echo "    -v        verbose mode"
}

args=()
while [[ $# -gt 0 ]]; do
    case $1 in
        -t)
            ntests=$2
            shift
            shift
            ;;
        -t*)
            ntests=${1#-t}
            shift
            ;;
        -b)
            nbits=$2
            shift
            shift
            ;;
        -b*)
            nbits=${1#-b}
            shift
            ;;
        -v)
            verbose=1
            shift
            ;;
        -h | --help)
            usage
            exit 0
            ;;
        -*)
            echo "error: unkown option $1!"
            usage
            exit 1
            ;;
        *)
            args+=("$1")
            shift
    esac
done
set -- "${args[@]}"

if [[ $# -eq 0 ]]; then
    echo "error: argument required"
    usage
    exit 1
fi

generate() {
    IFS=$'\n'
    for pat in $(./scripts/pattern_generator.hs -n $@); do
        str=$(perl -E "\$a=$pat; for (@\$a) { printf q/%0${nbits}b/, \$_ }")
        [[ $verbose ]] && echo $str >/dev/stderr
        echo $str
    done
    unset IFS
}

# generate wildcard obfuscation
generate $@ | cargo run --release -- multimatch -

# run tests
for ((i = 0; i < ntests; i++)); do 
    inpstr=""
    should_be=1
    for ((j = 0; j < $#; j++)); do
        inp=($(perl -e "print int(rand(1<<($nbits)))"))
        inpstr+=$(perl -e "printf q/%0${nbits}b/, $inp")
        if [[ $inp -ge ${args[$j]} ]]; then
            should_be=0
        fi
    done
    res=$(cargo run --quiet --release -- eval $inpstr)
    if [[ $res -ne $should_be ]]; then
        echo -e "${RED}test failed!${NC} input=$inpstr result=$res should_be=$should_be"
    fi
done
