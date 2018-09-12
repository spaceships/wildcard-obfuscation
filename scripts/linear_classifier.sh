#!/bin/bash

set -e

RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m' # No Color

ntests=0
nbits=8
verbose=""
combine=""
tests_only=""

usage() {
    echo "$0 [options] [thresholds]"
    echo "    -t NUM    run NUM random tests"
    echo "    -T        skip obfuscation, just run tests"
    echo "    -h        this message"
    echo "    -b        number of bits [$nbits]"
    echo "    -c        combine patterns"
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
        -T)
            tests_only=1
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
        -c)
            combine=1
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
    command="./scripts/pattern_generator.hs -b $nbits $@"
    if [[ $combine ]]; then 
        command+=" -c"
    fi
    [[ $verbose ]] && echo "$command" >/dev/stderr
    pats=$($command)
    [[ $verbose ]] && echo "$pats" >/dev/stderr
    echo "$pats"
}

if [[ ! $tests_only ]]; then 
    echo "obfuscating..."
    # generate wildcard obfuscation
    generate $@ | cargo run --release -- multimatch -
fi

# run tests
nfailed=0
total_time=0
for ((i = 0; i < ntests; i++)); do 
    inpstr=""
    should_be=1
    for ((j = 0; j < $#; j++)); do
        inp=($(perl -e "print int(rand(1<<($nbits)))"))
        inpstr+=$(perl -e "printf q/%0${nbits}b/, $inp")
        if [[ $inp -gt ${args[$j]} ]]; then
            should_be=0
        fi
    done
    [[ $verbose ]] && echo "test: $inpstr" >/dev/stderr

    start=$(date +%s)
    res=$(cargo run --quiet --release -- eval $inpstr)
    end=$(date +%s)
    total_time=$((total_time + (end - start)))

    if [[ $res ]] && [[ $res -eq $should_be ]]; then
        [[ $verbose ]] && echo -e "${GREEN}test succeeded!${NC} input=$inpstr result=$res should_be=$should_be"
    else
        nfailed=$(($nfailed + 1))
        echo -e "${RED}test failed!${NC} input=$inpstr result=$res should_be=$should_be"
    fi
done

if [[ $ntests -gt 0 ]]; then 
    if [[ $nfailed -eq 0 ]]; then
        echo -e "${GREEN}passed $ntests tests!$NC"
    else 
        echo -e "${RED}failed $nfailed tests${NC}"
    fi
fi

echo "eval took $((total_time / ntests))s on average"
