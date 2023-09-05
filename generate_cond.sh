#!/bin/bash

# check whether it runs in the right directory
if [ ! -f scp-hs.cabal ]; then
    echo "Run this script in the root directory of scp-hs"
    exit 1
fi

# compile cython code
pipenv run python setup.py build_ext --inplace > /dev/null

# check the output argument is provided
# if not, use default output file name
if [ "$#" -ne 1 ]; then
    echo "Usage: $0 /path/to/output_cond_file"
    echo "Using default output file: py-conds.txt"
    set -- "py-conds.txt"
    # use default output file name

fi

# generate intermediate haskell condition file
# and also save the output to a file while printing to stdout
HS_COND='hs-conds.txt'
HS_OUT='hs-out.txt'
cabal run protocol -- $HS_COND | tee $HS_OUT

# generate sympt condition file
pipenv run python -c "from simplify import simplify_condition; simplify_condition('$HS_COND', '$1')"

# show the result
echo "Final conditions:" | tee -a $HS_OUT
cat $1 | tee -a $HS_OUT
