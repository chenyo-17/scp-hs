# cython: language_level=3
from sympy.parsing.sympy_parser import parse_expr
from sympy import pretty, to_dnf, to_cnf
# read constraint strings from the given file
# simplify each line and write to the output file concurrently
def simplify_condition(input_file, output_file):
    with open(input_file, 'r') as f:
        lines = f.readlines()
    with open(output_file, 'w') as f:
        for line in lines:
            expr = parse_expr(line, evaluate=False)
            simplified_expr = expr.simplify()
            f.write(pretty(simplified_expr) + '\n')