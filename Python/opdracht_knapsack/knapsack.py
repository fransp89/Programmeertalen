import copy
import csv
import random

"""
Name: Francesco Pavlovic
Study: B.Sc. Informatica
UvAnetID: 13782118
"""

"""
Sources: https://www.geeksforgeeks.org/working-csv-files-python/ (csv read)
https://ioflood.com/blog/python-write-to-csv/ (csv write)
https://www.geeksforgeeks.org/binary-search-tree-set-1-search-and-insertion/ 
(BST)
https://www.geeksforgeeks.org/dfs-traversal-of-a-tree-using-recursion/
(BST traversal)
"""


class Knapsack:
    """The knapsack that contains the items that count for the total points."""
    """ in_sack: list of items that are in the knapsack
    highest_points: the amount of points that the best knapsack has """
    def __init__(self):
        self.in_sack = []
        self.highest_points = 0

    """ Appends item to the end of in_sack.
    item: tuple that is appended to in_sack """
    def append_item(self, item):
        self.in_sack.append(item)

    """ Removes the last item from in_sack and returns it. """
    def pop_item(self):
        return self.in_sack.pop()

    """ Removes the item at index from in_sack and returns it. 
    index: the spot in in_sack that needs to be removed """
    def remove_item(self, index):
        return self.in_sack.pop(index)

    """ Empties the knapsack. """
    def reset_knapsack(self):
        self.in_sack = []

    """ Calculates the points inside of the knapsack. """
    def get_points(self):
        total_points = 0

        for item in self.in_sack:
            total_points += item[1]

        return total_points

    """ Changes the former stored value of highest_points to a new
    highest_points value.
    highest_points: the new highest_points value """
    def change_highest_points(self, highest_points):
        self.highest_points = highest_points

    """ Writes the best knapsack solution to destination_file. 
    destination_file: the file that is being written to """
    def save(self, destination_file):
        with open(destination_file, 'w', newline='') as csv_file:
            csv_writer = csv.writer(csv_file)
            csv_writer.writerow([f"points:{self.highest_points}"])
            for item in self.in_sack:
                csv_writer.writerow([item[0]])


class Resources:
    """ Holds the maximum resources a knapsack contains and the actual
    resources it contains. """
    """ max_resources: maximum resources that a knapsack has 
    resources: the current resources of a knapsack """
    def __init__(self, max_resources):
        self.max_resources = max_resources
        self.resources = list(max_resources)

    """ Removes weight from the current weight of the knapsack.
    weight: weight that needs to be removed from current weight """
    def subtract_weight(self, weight):
        self.resources[2] -= weight

    """ Removes volume from the current volume of the knapsack.
        volume: volume that needs to be removed from current volume """
    def subtract_volume(self, volume):
        self.resources[3] -= volume

    """ Checks if the resources are inside of their bounds. """
    def check_resources(self):
        if self.resources[2] < 0 or self.resources[3] < 0:
            return False

        return True

    """ Sets the current resources back to the maximum resources of the
    knapsack. """
    def reset_resources(self):
        self.resources = self.max_resources.copy()


class Items:
    """ A class that has lists that hold the items that can be added to the knapsack. """
    """ list_of_items: the list of items that can be added to the knapsack` 
    backup_list: a copy of list_of_items which is used when resetting the
    list """
    def __init__(self, list_of_items):
        self.list_of_items = list_of_items
        self.backup_list = copy.deepcopy(list_of_items)

    """ Removes an item from list_of_items at location index. 
    index: the spot where the item is that gets removed """
    def remove_item_from_list(self, index):
        self.list_of_items.pop(index)

    """ Returns list_of_items back to having all items. """
    def reset_items_list(self):
        self.list_of_items = self.backup_list.copy()


class Item:
    """ A class that makes the items that can be added to the knapsack. """
    """ item: tuple that holds stats for a single item """
    def __init__(self):
        self.item = ()

    """ Assigns stats to an item. 
    data: the stats to be assigned to the item """
    def give_stats(self, data):
        self.item = data


class Solver:
    """ A class that holds the functions to solve the knapsack problem. """
    """ knapsack: a copy of the knapsack object
    best_knapsack: a copy of the knapsack that contains the most points
    resources: a copy of the resources object
    items: a copy of the items object
    item: a copy of the item object """
    def __init__(self):
        self.knapsack = Knapsack()
        self.best_knapsack = Knapsack()
        self.resources = Resources([])
        self.items = Items([])
        self.item = Item()

    """ Inserts parameters into the objects that need them. 
    max_resources: maximum resources that a knapsack has 
    list_of_items: the list of items that can be added to the knapsack """
    def init_classes(self, max_resources, list_of_items):
        self.resources = Resources(max_resources)
        self.items = Items(list_of_items)

    """ Changes the classes back to an empty state. """
    def reset_classes(self):
        self.knapsack.reset_knapsack()
        self.resources.reset_resources()
        self.items.reset_items_list()

    """ Compares the points of the knapsack to the knapsack with the highest
    points. """
    def compare_points(self):
        new_points = self.knapsack.get_points()
        if new_points > self.knapsack.highest_points:
            self.knapsack.change_highest_points(new_points)
            self.best_knapsack = copy.deepcopy(self.knapsack)

    """ Returns the best_knapsack object. """
    def get_best_knapsack(self):
        return self.best_knapsack


class Solver_Random(Solver):
    """ A solver that randomly chooses an item and adds it to the knapsack,
    until an item is added that goes outside the bounds of the knapsack.
    This item is removed before the knapsack is assigned to best_knapsack.
    iterations: how many times the solver will attempt to solve the knapsack
    problem """
    def __init__(self, iterations):
        super().__init__()
        self.iterations = iterations

    """ Randomizes the order of list_of_items. """
    def randomize_list(self):
        random.shuffle(self.items.list_of_items)

    """ Randomly finds the best knapsack that it can and assigns it to best_knapsack. 
    knapsack_stats: the maximum resources that the knapsack can hold, with
    names 
    list_of_items: all items that are given from the start """
    def solve(self, knapsack_stats, list_of_items):
        knapsack_stats, list_of_items = translate_knapsack(knapsack_stats,
                                                           list_of_items)
        self.init_classes(knapsack_stats, list_of_items)
        for _ in range(self.iterations):
            while self.resources.check_resources():
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
    """ A solver that uses a binary search tree and recursive traversal to
    find the knapsack with the most points. """
    def __init__(self):
        super().__init__()

    """ Solves the knapsack problem with recursion. 
    knapsack_stats: the maximum resources that the knapsack can hold, with
    names 
    list_of_items: all items that are given from the start """
    def solve(self, knapsack_stats, list_of_items):
        pass


class Solver_Optimal_Iterative_Deepcopy(Solver):
    """ A solver that creates many copies of a knapsack object on which the
    depth first search algorithm can be applied to.
    list_of_knapsack: a list of knapsacks that are present in the tree """
    def __init__(self):
        super().__init__()
        self.list_of_knapsack = []

    """ Makes a new knapsack object. 
    knapsack_stats: the maximum resources that the knapsack can hold, with
    names 
    list_of_items: all items that are given from the start"""
    def make_knapsack(self, max_resources, list_of_items):
        new_knapsack = Knapsack.__init__(self)
        self.knapsack.no_child_flag = 1
        new_resources = Resources.__init__(self, max_resources)

        return new_knapsack, new_resources

    """ Makes a copy of a knapsack object. 
    knapsack: the knapsack that gets copied 
    max_resources: the maximum resources that the knapsack can have """
    def copy_knapsack(self, knapsack, max_resources):
        copy_resources = copy.deepcopy(max_resources)
        copy_knapsack = copy.deepcopy(knapsack)

        return copy_knapsack, copy_resources

    """ Solves the knapsack problem using depth first search on a tree made of
    knapsacks.
    knapsack_stats: the maximum resources that the knapsack can hold, with
    names 
    list_of_items: all items that are given from the start """
    def solve(self, knapsack_stats, list_of_items):
        pass


class Solver_Optimal_Iterative(Solver):
    """ Solves the knapsack problem by making a binary search tree that
    contains items that are put in the list or not. """
    def __init__(self):
        super().__init__()

    """ Solves the knapsack problem using the depth first search on a tree
    made of items or lack of items. 
    knapsack_stats: the maximum resources that the knapsack can hold, with
    names 
    list_of_items: all items that are given from the start """
    def solve(self, knapsack_stats, list_of_items):
        pass


class Solver_Random_Improved(Solver_Random):
    """ Solves the knapsack problem using the random_solver being assisted by
    the hill climbing algorithm.
    iterations: how many times the solver will attempt to solve the knapsack
    problem """
    def __init__(self, iterations):
        super().__init__(iterations)

    """ Solves the knapsack problem using the hill climbing algorithm to
    increase the chance of a improved result compared to Solver_Random. 
    knapsack_stats: the maximum resources that the knapsack can hold, with
    names 
    list_of_items: all items that are given from the start """
    def solve(self, knapsack_stats, list_of_items):
        super().solve(knapsack_stats, list_of_items)
        for _ in range(self.iterations):
            copy_knapsack = copy.deepcopy(self.best_knapsack)
            copy_items = copy.deepcopy(self.items.list_of_items)

            random_index = random.randint(0, len(copy_knapsack.in_sack) - 1)
            knapsack_pop = copy_knapsack.remove_item(random_index)
            copy_items.append(knapsack_pop)

            random.shuffle(copy_items)
            items_pop = copy_items.pop(random_index)
            copy_knapsack.in_sack.append(items_pop)

            if self.best_knapsack.get_points() > copy_knapsack.get_points():
                continue
            elif self.best_knapsack.get_points() < copy_knapsack.get_points():
                self.best_knapsack = copy.deepcopy(copy_knapsack)


def load_knapsack(knapsack_file):
    """ Reads the knapsack data from knapsack_file.
    knapsack_file: the file that contains all stats for all items and the
    knapsack. """
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
    """ Changes strings that are solely numbers into integers and changes the
    entire statline of the item into a tuple.
    knapsack_size: the maximum resources for the knapsack
    items: a list of the items that are available for use in the knapsack """
    items = [tuple(inner_list) for inner_list in items]

    return knapsack_size, items


def solve(solver, knapsack_file, solution_file):
    """ Uses 'solver' to solve the knapsack problem in file
    'knapsack_file' and writes the best solution to 'solution_file'.
    """
    knapsack, items = load_knapsack(knapsack_file)
    solver.solve(knapsack, items)
    knapsack = solver.get_best_knapsack()
    print(f"saving solution with {knapsack.get_points()} points to"
          f"'{solution_file}'")
    knapsack.save(solution_file)


def main():
    solver_random = Solver_Random(1000)
    solver_optimal_recursive = Solver_Optimal_Recursive()
    solver_optimal_iterative_deepcopy = Solver_Optimal_Iterative_Deepcopy()
    solver_optimal_iterative = Solver_Optimal_Iterative()
    solver_random_improved = Solver_Random_Improved(5000)

    knapsack_file = "knapsack_small"
    print("=== solving:", knapsack_file)
    solve(solver_random, knapsack_file + ".csv", knapsack_file +
          "_solution_random.csv")
    solve(solver_optimal_recursive, knapsack_file + ".csv", knapsack_file +
          "_solution_optimal_recursive.csv")
    solve(solver_optimal_iterative_deepcopy, knapsack_file + ".csv",
          knapsack_file + "_solution_optimal_iterative_deepcopy.csv")
    solve(solver_optimal_iterative, knapsack_file + ".csv", knapsack_file +
          "_solution_optimal_iterative.csv")
    solve(solver_random_improved, knapsack_file + ".csv", knapsack_file +
          "_solution_random_improved.csv")

    knapsack_file = "knapsack_medium"
    print("=== solving:", knapsack_file)
    solve(solver_random, knapsack_file + ".csv", knapsack_file +
          "_solution_random.csv")
    solve(solver_optimal_recursive, knapsack_file + ".csv", knapsack_file +
          "_solution_optimal_recursive.csv")
    solve(solver_optimal_iterative_deepcopy, knapsack_file + ".csv",
          knapsack_file + "_solution_optimal_iterative_deepcopy.csv")
    solve(solver_optimal_iterative, knapsack_file + ".csv", knapsack_file +
          "_solution_optimal_iterative.csv")
    solve(solver_random_improved, knapsack_file + ".csv", knapsack_file +
          "_solution_random_improved.csv")

    knapsack_file = "knapsack_large"
    print("=== solving:", knapsack_file)
    solve(solver_random, knapsack_file + ".csv", knapsack_file +
          "_solution_random.csv")
    solve(solver_random_improved, knapsack_file + ".csv", knapsack_file +
          "_solution_random_improved.csv")


if __name__ == "__main__":  # keep this at the bottom of the file
    main()
