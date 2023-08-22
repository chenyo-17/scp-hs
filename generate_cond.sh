#!/bin/bash

if [ "$#" -ne 1 ]; then
    echo "Usage: $0 /path/to/output_cond_file"
    exit 1
fi

HS_COND="hs-conds"

# pipenv install .
# pipenv shell

python -c "from simplify import simplify_condition; simplify_condition($HS_COND, '$1')"