

nodes_to_optimize = []
tree = ast
pass_n = 0
while len(nodes_to_optimize) > 0:
    nodes_to_optimize = order_by_increasing_magnitude(remove_uniques(count_sub_trees(tree)))
    for term in nodes_to_optimize:
        term_id = make_new_identifier(term)
        factorfunc = lambda tree_: factor(tree_, term, term_id)
        tree = factorfunc(tree)
        map(factorfunc, nodes_to_optimize)