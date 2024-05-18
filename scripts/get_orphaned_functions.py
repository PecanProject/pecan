import os
import re
import csv

# Define a list of color codes for printing. THis proves to be useful for distinction during debugging
color_codes = [
    "\033[30m",  # BLACK-0
    "\033[31m",  # RED-1
    "\033[32m",  # GREEN-2
    "\033[33m",  # YELLOW-3
    "\033[34m",  # BLUE-4
    "\033[35m",  # MAGENTA-5
    "\033[36m",  # CYAN-6
    "\033[37m",  # WHITE-7
    "\033[90m",  # BRIGHT_BLACK-8
    "\033[91m",  # BRIGHT_RED-9
    "\033[92m",  # BRIGHT_GREEN-10
    "\033[93m",  # BRIGHT_YELLOW-11
    "\033[94m",  # BRIGHT_BLUE-12
    "\033[95m",  # BRIGHT_MAGENTA-13
    "\033[96m",  # BRIGHT_CYAN-14
    "\033[97m",  # BRIGHT_WHITE-15
]

# Reset
ENDC = "\033[0m"

# Modules to ignore while checking for orphaned functions
ignored_modules = ["data.hydrology", "DART"]

# Function to get the list of modules
def get_module_list():
    module_dir = os.path.join(main_dir,"modules")
    modules = os.listdir(module_dir)
    return [module for module in modules if module not in ignored_modules]

# List of modules to check for orphaned functions
module_list = get_module_list()

# Initialize two lists to store the orphaned functions
# List_1: Purely orphaned functions
# List_2: Functions that are utilized externally
list_1 = []
list_2 = []

main_dir = os.getcwd()


# Define a function to list all functions in a module
def list_functions_in_module(module_path):
    """
    List all functions defined in the R files of a module.

    Args:
    module_path (str): The path to the module's R directory.

    Returns:
    list: A list of function names defined in the module.
    """
    function_pattern = re.compile(r"\b([a-zA-Z_]\w*(?:\.\w+)*)\s*<-\s*function")
    function_names = []

    # Get all R files in the module_path
    r_files = [f for f in os.listdir(module_path) if f.endswith(".R")]

    # List all R files in the module_path
    # print(color_codes[5] + "R files in the module_path of \t",color_codes[9],module_path," are \n ",color_codes[3],r_files,"\n",ENDC,)

    # Findings all function names in the R files
    for r_file in r_files:
        with open(os.path.join(module_path, r_file), "r") as file:
            content = file.read()
            matches = function_pattern.findall(content)
            function_names.extend(matches)

    function_names = sorted(set(function_names))

    # Print the function names we extracted and their count
    # print(color_codes[13] + "Function names:", function_names, "\n", ENDC)
    # print(color_codes[2] + "Length of function List", len(function_names), "\n", ENDC)

    return function_names


# Define a function to check if a function is utilized within its module
def is_function_utilized_within_module(function_name, module_r_path):
    """
    Check if the function is utilized within its module.

    Args:
    function_name (str): The name of the function to check.
    module_r_path (str): The path to the module's R directory.

    Returns:
    bool: True if the function is utilized within the module, False otherwise.
    """

    # Regex pattern to detect usage of the function
    function_pattern = re.compile(r"\b" + re.escape(function_name) + r"\b\s*\(")

    # Traverse all files in the given module R directory
    for r_file_name in os.listdir(module_r_path):
        # Check if the file is an R file
        if r_file_name.endswith(".R"):
            file_path = os.path.join(module_r_path, r_file_name)
            with open(file_path, "r") as file:
                file_content = file.read()

                # Checks if the function is utilized in the file by searching for the function name
                if function_pattern.search(file_content):
                    # print(f'{color_codes[2]}Function "{function_name}" is utilized in file: {file_path}{ENDC}')
                    return True

    # If no file uses the function
    # print(f'{color_codes[1]}Function "{function_name}" is not utilized in module: {module_r_path}, {ENDC}')
    return False


def is_function_utilized_externally(function_name, module_name):
    """
    Check if the function from a specific module is utilized in any other module.

    Args:
    function_name (str): The name of the function to check.
    module_name (str): The name of the module where the function is originally defined.

    Returns:
    bool: True if the function is utilized externally, False otherwise.
    """
    function_pattern = re.compile(r"\b" + re.escape(function_name) + r"\b")
    func_location_module = os.path.join(main_dir, "modules", module_name)
    search_directories = [
        os.path.join(main_dir, "modules"),
        os.path.join(main_dir, "models"),
        os.path.join(main_dir, "scripts"),
        os.path.join(main_dir, "base"),
        os.path.join(main_dir, "tests"),
        os.path.join(main_dir, "base"),
    ]

    # print(color_codes[2] + "Function name:",function_name," \t Function's Location modules :",func_location_module,ENDC,)

    for directory in search_directories:
        for root, dirs, files in os.walk(directory):
            if (
                root.__contains__(func_location_module)
                or root.__contains__("/man")
                or root.__contains__("/vignettes")
            ):
                # print(f"{color_codes[13]} Skipping the module directory {root}{ENDC}")
                continue
            else:
                # print(f"{color_codes[14]} Searching for the function in the directory {root}{ENDC}")
                for file in files:
                    if file.endswith(".R"):
                        file_path = os.path.join(root, file)
                        with open(file_path, "r") as file:
                            file_content = file.read()
                            if function_pattern.search(file_content):
                                # print(f'{color_codes[12]} Function "{function_name}" is utilized EXTERNALLY in file: {file_path}{ENDC}')
                                return True
    return False


def print_in_batches(list_to_print, label):
    # print(label)
    color_index = 0  # Start with the first color
    for i in range(0, len(list_to_print), 5):
        batch = list_to_print[i : i + 5]
        # Get the current color
        color = color_codes[color_index % len(color_codes)]
        # Print the batch in the current color
        print(color + ", ".join(map(str, batch)) + ENDC)
        # Move to the next color for the next batch
        color_index += 1


def add_to_csv(list_1, list_2, csv_file_path):
    with open(csv_file_path, mode="w", newline="") as file:
        writer = csv.writer(file)

        # Writing the header for list_1
        writer.writerow(["Purely Orphaned functions", "module", "function_name"])

        # Writing data from list_1
        for module, function in list_1:
            writer.writerow(["", module, function])

        # Writing the header for list_2
        writer.writerow(["Externally Utilized Orphaned Functions"])

        # Writing data from list_2
        for func, module in list_2:
            writer.writerow(["", func, module])


def main():
    # Define the path to the modules directory
    script_dir = os.path.dirname(os.path.realpath(__file__))
    modules_dir = os.path.join(script_dir, "..", "modules")

    # Iterate over all modules
    for module_name in module_list:

        # Construct the path to the R directory of the module
        module_r_code_path = os.path.join(modules_dir, module_name, "R")

        # Print the module name and path
        # print(color_codes[1] + "Processing module:", module_name, "\n", ENDC)
        # print(color_codes[6] + "Module path:", module_r_code_path, "\n", ENDC)

        # List all functions in the module
        functions = list_functions_in_module(module_r_code_path)

        # Check if each function is utilized within the module
        for function_name in functions:

            # Check if the function is utilized within the module
            if not is_function_utilized_within_module(
                function_name, module_r_code_path
            ):
                # Function is not utilized within the module. Add to list_1
                list_1.append((module_name, function_name))

                # Check if the function is utilized externally
                if is_function_utilized_externally(function_name, module_name):
                    # Function is utilized externally, Move to list_2. Remove from list_1
                    list_1.remove((module_name, function_name))
                    list_2.append((module_name, function_name))

    # Output

    # print_in_batches(list_1, "Purely Orphaned Functions:")
    # print_in_batches(list_2, "Externally Utilized Functions:")
    add_to_csv(
        list_1, list_2, os.path.join(main_dir, "scripts", "orphaned_functions.csv")
    )


if __name__ == "__main__":
    main()
