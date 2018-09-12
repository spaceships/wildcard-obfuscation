#!/bin/bash

set -e

RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m' # No Color

len=8
alt_len=2
alt_num=2
n_obf_tests=4
n_eval_tests=2

usage() {
    echo "$0 [options]"
    echo "    -n NUM    total length of pattern/input [$len]"
    echo "    -a NUM    length of alternative patterns [$alt_len]"
    echo "    -A NUM    number of alternatives in each bracket [$alt_num]"
    echo "    -t NUM    number of obfuscation tests to run [$n_obf_tests]"
    echo "    -T NUM    number of evaluation tests to run per obfucation [$n_eval_tests]"
}

args=()
while [[ $# -gt 0 ]]; do
    case $1 in
        -n)  len=$2; shift; shift;;
        -n*) len=${1#-t}; shift;;
        -a)  alt_len=$2; shift; shift;;
        -a*) alt_len=${1#-t}; shift;;
        -A)  alt_num=$2; shift; shift;;
        -A*) alt_num=${1#-t}; shift;;
        -t)  n_obf_tests=$2; shift; shift;;
        -t*) n_obf_tests=${1#-t}; shift;;
        -T)  n_eval_tests=$2; shift; shift;;
        -T*) n_eval_tests=${1#-t}; shift;;
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

echo "parameters: len=$len alt_len=$alt_len alt_num=$alt_num n_obf_tests=$n_obf_tests n_eval_tests=$n_eval_tests"

# generate a random disjunction based on the parameters
function rand_pat() {
    for ((i = 0; i < len; i += alt_len)); do
        echo -n "("
        for ((j=0; j < alt_num; j++)); do
            for ((k=0; k < alt_len; k++)); do
                echo -n $((RANDOM % 2))
            done
            [[ $j -lt $((alt_num-1)) ]] && echo -n "|"
        done
        echo -n ")"
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

# make sure it is built
echo "building..."
cargo build --release

# run benchmarks
total_obf_time=0
total_eval_time=0
for ((t=0; t<n_obf_tests; t++)); do
    pat=$(rand_pat)
    echo -e "${GREEN}test $t${NC} $pat"

    start=$(ms)
    cargo run --release --quiet -- multimatch $pat    
    end=$(ms)
    total_obf_time=$((total_obf_time + (end - start)))

    for ((i=0; i<n_eval_tests; i++)); do
        inp=$(rand_input)
        start=$(ms)
        cargo run --release --quiet -- eval $inp >/dev/null
        end=$(ms)
        total_eval_time=$((total_eval_time + (end - start)))
    done

    cargo run --release --quiet -- multimatch $pat    
done

echo "obf  took $((total_obf_time / n_obf_tests))ms on average"
echo "eval took $((total_eval_time / (n_obf_tests * n_eval_tests)))ms on average"
