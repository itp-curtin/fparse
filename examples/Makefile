
# directories
FPARSE_SRC_DIR = ../src
EXAMPLE_BUILD_DIR = build
OBJ_DIR = $(EXAMPLE_BUILD_DIR)/obj
MOD_DIR = $(EXAMPLE_BUILD_DIR)/mod

# compiler 
FC = gfortran

# compiler flags
FC_MODULE_DIR_FLAG ?= -J$(MOD_DIR)
FC_LINE_LENGTH_FLAG ?= -ffree-line-length-none
FC_optimisation ?= -O3
FCFLAGS = $(FC_MODULE_DIR_FLAG) -I$(MOD_DIR) $(FC_LINE_LENGTH_FLAG) $(FC_optimisation)

# source files
EXAMPLE_SRC = example_main.f90

FPARSE_SOURCES = fparse_types.f90 \
		         fparse_util.f90 \
		         fparse_schema.f90 \
		         fparse_io.f90 \
		         fparse_parser.f90 \
		         fparse_getters.f90 \
		         fparse.f90


# destination of the example executable and the corresponding object and module files
TARGET = $(EXAMPLE_BUILD_DIR)/example.exe
FPARSE_OBJECTS = $(patsubst %.f90, $(OBJ_DIR)/%.o, $(FPARSE_SOURCES))

$(info FPARSE_OBJECTS: $(FPARSE_OBJECTS))

all: $(EXAMPLE_BUILD_DIR) $(OBJ_DIR) $(MOD_DIR) $(TARGET)

$(EXAMPLE_BUILD_DIR):
	mkdir -p $(EXAMPLE_BUILD_DIR)

$(OBJ_DIR):
	mkdir -p $(OBJ_DIR)

$(MOD_DIR):
	mkdir -p $(MOD_DIR)

$(FPARSE_OBJECTS): $(OBJ_DIR)/%.o: $(FPARSE_SRC_DIR)/%.f90
	$(FC) $(FCFLAGS) -c $< -o $@

$(TARGET): $(EXAMPLE_SRC) $(FPARSE_OBJECTS)
	$(FC) $(FCFLAGS) -o $@ $^

clean:
	rm -rf $(EXAMPLE_BUILD_DIR)
