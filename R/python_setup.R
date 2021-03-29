# Example taken from: https://github.com/ranikay/shiny-reticulate-app

# Define any Python packages needed for the app here:
PYTHON_DEPENDENCIES = c('pip', 'numpy', 'pandas', 'scipy')

# ------------------ App virtualenv setup (Do not edit) ------------------- #

virtualenv_dir = Sys.getenv('VIRTUALENV_NAME')
python_path = Sys.getenv('PYTHON_PATH')

# Create virtual env and install dependencies. Need to run first 2 only once.
# reticulate::virtualenv_create(envname = virtualenv_dir, python = python_path)
# reticulate::virtualenv_install(virtualenv_dir, packages = PYTHON_DEPENDENCIES, ignore_installed=TRUE)
reticulate::use_virtualenv(virtualenv_dir, required = T)
