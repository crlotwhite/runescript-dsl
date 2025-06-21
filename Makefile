# RuneScript DSL Makefile
# Provides convenient build targets for development

# Default build type
BUILD_TYPE ?= Debug

# Build directory
BUILD_DIR = build

# Default target
.PHONY: all
all: build

# Create build directory and configure
.PHONY: configure
configure:
	@echo "Configuring RuneScript DSL..."
	@mkdir -p $(BUILD_DIR)
	@cd $(BUILD_DIR) && cmake .. -DCMAKE_BUILD_TYPE=$(BUILD_TYPE) -DBUILD_TESTS=ON

# Build the project
.PHONY: build
build: configure
	@echo "Building RuneScript DSL..."
	@cd $(BUILD_DIR) && cmake --build . --config $(BUILD_TYPE)

# Build in release mode
.PHONY: release
release:
	@$(MAKE) build BUILD_TYPE=Release

# Run tests
.PHONY: test
test: build
	@echo "Running tests..."
	@cd $(BUILD_DIR) && ctest --output-on-failure --build-config $(BUILD_TYPE)

# Build and test examples
.PHONY: test-examples
test-examples: build
	@echo "Testing examples..."
	@cd $(BUILD_DIR) && ./runescript ../examples/basic/hello.rcs || echo "Example test completed"

# Build Haskell components
.PHONY: haskell
haskell:
	@echo "Building Haskell components..."
	@cd haskell/spell-checker && stack build

# Clean build artifacts
.PHONY: clean
clean:
	@echo "Cleaning build artifacts..."
	@rm -rf $(BUILD_DIR)
	@cd haskell/spell-checker && stack clean || true

# Install the project
.PHONY: install
install: release
	@echo "Installing RuneScript DSL..."
	@cd $(BUILD_DIR) && sudo cmake --install .

# Print help
.PHONY: help
help:
	@echo "RuneScript DSL Build System"
	@echo "============================"
	@echo ""
	@echo "Build Targets:"
	@echo "  all          - Build the project (default)"
	@echo "  build        - Build C++ components"
	@echo "  release      - Build in release mode"
	@echo "  haskell      - Build Haskell components"
	@echo ""
	@echo "Test Targets:"
	@echo "  test         - Run all tests"
	@echo "  test-examples- Test example scripts"
	@echo ""
	@echo "Maintenance:"
	@echo "  clean        - Clean build artifacts"
	@echo "  install      - Install the project"
	@echo "  help         - Show this help message"
