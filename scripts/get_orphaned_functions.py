import os
import re
import csv

# THIS SCRIPT IS MEANT TO BE EXECUTED FROM THE ROOT DIRECTORY, i.e., 'pecan'

# Define Working Directory, i.e, '/pecan'
main_dir = os.getcwd()

# Define a list of color codes for printing. Useful for distinction during debugging
color_codes = [
    "\033[30m",  # BLACK
    "\033[31m",  # RED
    "\033[32m",  # GREEN
    "\033[33m",  # YELLOW
    "\033[34m",  # BLUE
    "\033[35m",  # MAGENTA
    "\033[36m",  # CYAN
    "\033[37m",  # WHITE
    "\033[90m",  # BRIGHT_BLACK
    "\033[91m",  # BRIGHT_RED
    "\033[92m",  # BRIGHT_GREEN
    "\033[93m",  # BRIGHT_YELLOW
    "\033[94m",  # BRIGHT_BLUE
    "\033[95m",  # BRIGHT_MAGENTA
    "\033[96m",  # BRIGHT_CYAN
    "\033[97m",  # BRIGHT_WHITE
]

# Reset color
ENDC = "\033[0m"

# Modules to ignore while checking for orphaned functions
MODULES_TO_IGNORE = ["DART", "data.mining"]


# Function to get a list of all module names in the 'modules' directory, excluding ignored modules and hidden files/directories
def get_module_list():
    """
    Get a list of all module names in the 'modules' directory, excluding ignored modules and hidden files/directories

    Args:
    None

    Returns:
    list: A list of module names
    """
    MODULE_DIRECTORY_PATH = os.path.join(main_dir, "modules")
    MODULE_NAMES = os.listdir(MODULE_DIRECTORY_PATH)
    # Filter out ignored modules and hidden files (those starting with '.')
    return [
        module
        for module in MODULE_NAMES
        if module not in MODULES_TO_IGNORE and not module.startswith(".")
    ]


MODULES_LIST = get_module_list()

# Initialize two lists to store the orphaned functions
# purely_orphaned_functions: Purely orphaned functions
# externally_utilized_functions: Functions that are utilized externally
purely_orphaned_functions = []
externally_utilized_functions = []


# Function to list 'functions' within a given module directory
def list_functions_in_module(module_path):
    """
    List all functions within a given module directory

    Args:
    module_path (str): The path to the module directory

    Returns:
    list: A list of function names in the module
    """
    if not os.path.exists(module_path):
        print(
            f"{color_codes[1]}Error: The directory {module_path} does not exist.{ENDC}"
        )
        return []

    # Regular expression pattern to match function definitions in R files
    function_pattern = re.compile(
        r"\b([a-zA-Z_]\w*(?:\.\w+)*)\s*(?:<-\s*function|\s*=\s*function)"
    )
    function_names = []
    R_files = [f for f in os.listdir(module_path) if f.endswith(".R")]

    # List all R files in the module_path
    # print(color_codes[5] + "R files in the module_path of \t",color_codes[9],module_path," are \n ",color_codes[3],R_files,"\n",ENDC,)

    # Extract function names from each R file in the module
    for R_file in R_files:
        with open(os.path.join(module_path, R_file), "r") as file:
            content = file.read()
            matches = function_pattern.findall(content)
            function_names.extend(matches)

    function_names = sorted(set(function_names))

    # Filter Out functions with 'error' in their name, that is, error functions which were accidently added via Regex Recognition
    function_names = [name for name in function_names if "error" not in name.lower()]

    # Print the function names we extracted and their count
    # print(color_codes[13] + "Function names:", function_names, "\n", ENDC)
    # print(color_codes[2] + "Length of functions' List", len(function_names), "\n", ENDC)

    return function_names


# Function to check if a function is utilized within a given module
def is_function_utilized_within_module(function_names, MODULE_DIRECTORY_PATH):
    """
    Check if a function is utilized within a given module

    Args:
    function_names (list): A list of function names to check
    MODULE_DIRECTORY_PATH (str): The path to the module directory

    Returns:
    dict: A bool dictionary of function names and their utilization status
    """
    utilization = {function_name: False for function_name in function_names}
    for Rfile in os.listdir(MODULE_DIRECTORY_PATH):
        if Rfile.endswith(".R"):  # If current fie is an R file
            file_path = os.path.join(MODULE_DIRECTORY_PATH, Rfile)
            with open(file_path, "r") as file:
                file_content = file.read()
                for function_name in function_names:
                    # Regex pattern to detect usage of the function in the R files
                    if re.search(
                        r"\b" + re.escape(function_name) + r"\b\s*\(", file_content
                    ):
                        # print(f'{color_codes[2]}Function "{function_name}" is utilized in file: {file_path}{ENDC}')
                        utilization[function_name] = True
    # If no file uses the function
    return utilization


def is_function_utilized_externally(function_name, module_name, ALL_MODULES):
    """
    Check if the function from a specific module is utilized in any other module.

    Args:
    function_name (str): The name of the function to check
    module_name (str): The name of the module to check
    ALL_MODULES (list): A list of all modules

    Returns:
    bool: True if the function is utilized in any other module, False otherwise
    """

    search_directories = [
        "modules",
        "models",
        "scripts",
        "base",
        "tests",
        "base",
    ]

    for OTHER_MODULE in ALL_MODULES:
        if OTHER_MODULE != module_name:
            for search_directory in search_directories:
                OTHER_MODULE_PATH = os.path.join(
                    main_dir, "modules", OTHER_MODULE, search_directory
                )
                if (
                    os.path.exists(OTHER_MODULE_PATH)
                    and is_function_utilized_within_module(
                        [function_name], OTHER_MODULE_PATH
                    )[function_name]
                ):
                    # print(f'{color_codes[12]} Function "{function_name}" is utilized EXTERNALLY in module: {MODULE_DIRECTORY_PATH}{ENDC}')
                    return True
    return False


def print_in_batches(list_to_print):
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


def add_to_csv(purely_orphaned_functions, externally_utilized_functions, csv_file_path):
    with open(csv_file_path, mode="w", newline="") as file:
        writer = csv.writer(file)
        # Header for purely_orphaned_functions
        writer.writerow(["Purely Orphaned functions", "module", "function_name"])
        # Writing data from purely_orphaned_functions
        for module, function in purely_orphaned_functions:
            writer.writerow(["", module, function])
        # Empty row to separate the two sections
        writer.writerow(["", "", ""])
        # Header for externally_utilized_functions
        writer.writerow(
            ["Externally Utilized Orphaned Functions", "module", "function_name"]
        )
        # Writing data from externally_utilized_functions
        for func, module in externally_utilized_functions:
            writer.writerow(["", func, module])


def main():
    # Define the path to the 'modules' directory
    modules_dir = os.path.join(os.getcwd(), "modules")

    # Iterate over all modules
    for module_name in MODULES_LIST:

        # Construct the path to the R directory of the module
        MODULE_R_DIRECTORY_PATH = os.path.join(modules_dir, module_name, "R")

        # Print the module name and path
        # print(color_codes[1] + "Processing module:", module_name, "\n", ENDC)
        # print(color_codes[6] + "Module path:", MODULE_R_DIRECTORY_PATH, "\n", ENDC)

        # List all functions in the module
        functions = list_functions_in_module(MODULE_R_DIRECTORY_PATH)
        utilization_results = is_function_utilized_within_module(
            functions, MODULE_R_DIRECTORY_PATH
        )

        for function_name, is_utilized in utilization_results.items():
            if not is_utilized:
                if not is_function_utilized_externally(
                    function_name, module_name, MODULES_LIST
                ):
                    purely_orphaned_functions.append((module_name, function_name))
                else:
                    externally_utilized_functions.append((function_name, module_name))

    # Output

    # print_in_batches(purely_orphaned_functions, "Purely Orphaned Functions:")
    # print_in_batches(externally_utilized_functions, "Externally Utilized Functions:")

    add_to_csv(
        purely_orphaned_functions,
        externally_utilized_functions,
        os.path.join(main_dir, "scripts", "orphaned_functions.csv"),
    )


if __name__ == "__main__":
    main()
