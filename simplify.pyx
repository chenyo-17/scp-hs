# cython: language_level=3
from sympy import simplify, Or, pretty, parse_expr, to_cnf
from concurrent.futures import ProcessPoolExecutor

def parse_and_simplify(line):
    return simplify(parse_expr(line, evaluate=False))

# read constraint strings from the given file
# simplify each line and write to the output file concurrently
def simplify_condition(input_file, output_file):
    with open(input_file, 'r') as f:
        lines = f.readlines()
    with ProcessPoolExecutor() as executor:
        simplified_exprs = list(executor.map(parse_and_simplify, lines))

    combined_expr = simplify(to_cnf(Or(*simplified_exprs)))

    with open(output_file, 'w') as f:
        f.write(pretty(combined_expr) + '\n')
