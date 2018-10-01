#!/bin/bash

set -e

RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m' # No Color

len=8
ntests=16

usage() {
    echo "$(basename $0) [options]"
    echo "    -n NUM    total length of pattern/input [$len]"
    echo "    -T NUM    number of tests to run [$ntests]"
}

args=()
while [[ $# -gt 0 ]]; do
    case $1 in
        -n)  len=$2; shift; shift;;
        -n*) len=${1#-n}; shift;;
        -t)  ntests=$2; shift; shift;;
        -t*) ntests=${1#-t}; shift;;
        -h | --help)
            usage
            exit 0
            ;;
        -*)
            echo "${RED}error: unkown option $1!${NC}"
            usage
            exit 1
            ;;
        *)
            args+=("$1")
            shift
    esac
done
set -- "${args[@]}"

if [[ $# -ne 0 ]]; then
    echo -e "${RED}unknown argument: $@${NC}"
    usage
    exit 1
fi

# generate a random pattern with no wildcards
function rand_pat() {
    for ((i = 0; i < len; i += 1)); do
        echo -n $((RANDOM % 2))
    done
}

function rand_input() {
    for ((i = 0; i < len; i++)); do
        echo -n $((RANDOM % 2))
    done
}

function ms() {
    echo $(($(date +%s%N) / 1000000))
}

# run benchmarks
total_eval_time=0
for ((t=0; t<ntests; t++)); do
    pat=$(rand_pat)
    # echo -e "${GREEN}[test $((t+1))]${NC} pattern=$pat"

    inp=$(rand_input)
    start=$(ms)
    perl -E "print 'match' if /$pat/" <<< $inp
    end=$(ms)
    total_eval_time=$((total_eval_time + (end - start)))
done

echo "eval took $((total_eval_time / ntests))ms on average"
