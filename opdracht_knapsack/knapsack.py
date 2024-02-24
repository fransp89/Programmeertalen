"""
Name: Francesco Pavlovic
Study: B.Sc. Informatica
UvAnetID: 13782118
"""

"""
Sources: https://www.geeksforgeeks.org/working-csv-files-python/ (csv read)
https://ioflood.com/blog/python-write-to-csv/ (csv write)
https://www.geeksforgeeks.org/binary-search-tree-set-1-search-and-insertion/ (BST)
https://www.geeksforgeeks.org/dfs-traversal-of-a-tree-using-recursion/ (BST traversal)
"""

import copy
import csv
import random


class Knapsack:
    def __init__(self):
        self.in_sack = []
        self.highest_points = 0

    def append_item(self, item):
        self.in_sack.append(item)

    def pop_item(self):
        self.in_sack.pop()

    def reset_knapsack(self):
        self.in_sack = []

    def retrieve_points(self):
        return self.highest_points

    def get_points(self):
        total_points = 0

        for item in self.in_sack:
            total_points += item[1]

        return total_points

    def change_highest_points(self, highest_points):
        self.highest_points = highest_points

    def save(self, destination_file):
        with open(destination_file, 'w', newline='') as csv_file:
            csv_writer = csv.writer(csv_file)
            csv_writer.writerow([f"points:{self.retrieve_points()}"])
            for item in self.in_sack:
                csv_writer.writerow([item[0]])


class Resources:
    def __init__(self, max_resources):
        self.max_resources = max_resources
        self.resources = list(max_resources)

    def subtract_weight(self, weight):
        self.resources[2] -= weight

    def add_weight(self, weight):
        if (self.resources[2] + weight) > self.max_resources[2]:
            return
        self.resources[2] += weight

    def check_weight(self):
        if self.resources[2] < 0:
            return False

        return True

    def subtract_volume(self, volume):
        self.resources[3] -= volume

    def add_volume(self, volume):
        if (self.resources[3] + volume) > self.max_resources[3]:
            return

        self.resources[3] += volume

    def check_volume(self):
        if self.resources[3] < 0:
            return False

        return True

    def get_weight(self, index):
        return self.resources[index][2]

    def get_volume(self, index):
        return self.resources[index][3]

    def reset_resources(self):
        self.resources = self.max_resources.copy()


class Items:
    def __init__(self, list_of_items):
        self.list_of_items = list_of_items
        self.backup_list = copy.deepcopy(list_of_items)

    def __repr__(self):
        print(self.list_of_items)

    def add_item_to_list(self, unique_item):
        self.list_of_items.append(unique_item)

    def remove_item_from_list(self, index):
        self.list_of_items.pop(index)

    def reset_items_list(self):
        self.list_of_items = self.backup_list.copy()


class Item:
    def __init__(self):
        self.item = ()

    def give_stats(self, stats):
        self.item = stats

    def __repr__(self):
        print(self.item)


class Solver:
    def __init__(self):
        self.knapsack = Knapsack()
        self.best_knapsack = Knapsack()
        self.resources = Resources([])
        self.items = Items([])
        self.item = Item()

    def init_classes(self, max_resources, list_of_items):
        self.knapsack = Knapsack()
        self.best_knapsack = Knapsack()
        self.resources = Resources(max_resources)
        self.items = Items(list_of_items)
        self.item = Item()

    def reset_classes(self):
        self.knapsack.reset_knapsack()
        self.resources.reset_resources()
        self.items.reset_items_list()

    def compare_points(self):
        new_points = self.knapsack.get_points()
        if new_points > self.knapsack.highest_points:
            self.knapsack.change_highest_points(new_points)
            self.best_knapsack = copy.deepcopy(self.knapsack)

    def get_best_knapsack(self):
        return self.best_knapsack


class Solver_Random(Solver):
    def __init__(self, iterations):
        super().__init__()
        self.iterations = iterations

    def randomize_list(self):
        random.shuffle(self.items.list_of_items)

    def solve(self, knapsack_stats, list_of_items):
        knapsack_stats, list_of_items = translate_knapsack(knapsack_stats,
                                                           list_of_items)
        self.init_classes(knapsack_stats, list_of_items)
        for iteration in range(self.iterations):
            while self.resources.check_volume() and self.resources.check_weight():
                self.randomize_list()
                self.knapsack.append_item(self.items.list_of_items[0])
                self.items.remove_item_from_list(0)
                self.resources.subtract_weight(self.knapsack.in_sack[-1][2])
                self.resources.subtract_volume(self.knapsack.in_sack[-1][3])
            self.knapsack.pop_item()
            self.compare_points()
            self.reset_classes()
        self.get_best_knapsack()


class Solver_Optimal_Recursive(Solver):
    def __init__(self):
        super().__init__()

    def solve(self, knapsack_stats, list_of_items):
        pass


class Solver_Optimal_Iterative_Deepcopy(Solver):
    def __init__(self):
        super().__init__()

    def solve(self, knapsack_stats, list_of_items):
        pass


class Solver_Optimal_Iterative(Solver):
    def __init__(self):
        super().__init__()

    def solve(self, knapsack_stats, list_of_items):
        pass


class Solver_Random_Improved(Solver_Random):
    def __init__(self, iterations):
        super().__init__(iterations)

    def solve(self, knapsack_stats, list_of_items):
        pass

def load_knapsack(knapsack_file):
    knapsack, items = [], []
    with open(knapsack_file, "r") as csv_file:
        csv_reader = csv.reader(csv_file)

        next(csv_reader)
        knapsack = next(csv_reader)
        for row in csv_reader:
            items.append(row)

    for i in range(1, len(knapsack)):
        knapsack[i] = int(knapsack[i])

    for row in items:
        for i in range(1, len(row)):
            row[i] = int(row[i])

    return knapsack, items


def translate_knapsack(knapsack_size, items):
    knapsack_size = knapsack_size
    items = [tuple(inner_list) for inner_list in items]

    return knapsack_size, items


def solve(solver, knapsack_file, solution_file):
    """ Uses 'solver' to solve the knapsack problem in file
    'knapsack_file' and writes the best solution to 'solution_file'.
    """
    knapsack, items = load_knapsack(knapsack_file)
    solver.solve(knapsack, items)
    knapsack = solver.get_best_knapsack()
    print(f"saving solution with {knapsack.get_points()} points to '{solution_file}'")
    knapsack.save(solution_file)


def main():
    solver_random = Solver_Random(1000)
    solver_optimal_recursive = Solver_Optimal_Recursive()
    """solver_optimal_iterative_deepcopy = Solver_Optimal_Iterative_Deepcopy()
    solver_optimal_iterative = Solver_Optimal_Iterative()
    solver_random_improved = Solver_Random_Improved(5000)"""

    knapsack_file = "knapsack_small"
    print("=== solving:", knapsack_file)
    solve(solver_random, knapsack_file + ".csv", knapsack_file + "_solution_random.csv")
    solve(solver_optimal_recursive, knapsack_file + ".csv", knapsack_file + "_solution_optimal_recursive.csv")
    """solve(solver_optimal_iterative_deepcopy, knapsack_file + ".csv",
          knapsack_file + "_solution_optimal_iterative_deepcopy.csv")
    solve(solver_optimal_iterative, knapsack_file + ".csv", knapsack_file + "_solution_optimal_iterative.csv")
    solve(solver_random_improved, knapsack_file + ".csv", knapsack_file + "_solution_random_improved.csv")"""

    knapsack_file = "knapsack_medium"
    print("=== solving:", knapsack_file)
    solve(solver_random, knapsack_file + ".csv", knapsack_file + "_solution_random.csv")
    solve(solver_optimal_recursive, knapsack_file + ".csv", knapsack_file + "_solution_optimal_recursive.csv")
    """solve(solver_optimal_iterative_deepcopy, knapsack_file + ".csv",
          knapsack_file + "_solution_optimal_iterative_deepcopy.csv")
    solve(solver_optimal_iterative, knapsack_file + ".csv", knapsack_file + "_solution_optimal_iterative.csv")
    solve(solver_random_improved, knapsack_file + ".csv", knapsack_file + "_solution_random_improved.csv")"""

    knapsack_file = "knapsack_large"
    print("=== solving:", knapsack_file)
    solve(solver_random, knapsack_file + ".csv", knapsack_file + "_solution_random.csv")
    """solve(solver_random_improved, knapsack_file + ".csv", knapsack_file + "_solution_random_improved.csv")"""


if __name__ == "__main__":  # keep this at the bottom of the file
    main()
