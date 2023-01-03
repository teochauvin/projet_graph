#!/bin/bash

# Run the compile.sh file
bash compile.sh

# Test if the compilation was successful
if [ $? -ne 0 ]; then
    # If the compilation failed, print an error message and exit
    echo "Compilation failed. Exiting."
    exit 1
fi

# Prompt the user to choose an option
echo "Please choose one of the following options:"
echo "1. Convert a graph"
echo "2. Color a graph"
echo "3. Quit"
read option

# Handle the user's choice
case $option in
  1)
    # Prompt the user to enter two filenames
    echo "Please enter the first filename (instance.json):"
    read filename1
    echo "Please enter the second filename (to save the graph):"
    read filename2

    # Check if the first filename is a regular file and has the .json extension
    if [ ! -f $filename1 ]; then
      echo "$filename1 is not a regular file. Exiting."
      exit 1
    fi
    if [[ ! $filename1 =~ \.json$ ]]; then
      echo "$filename1 is not a .json file. Exiting."
      exit 1
    fi

    # Check if the two filenames are the same
    if [ $filename1 = $filename2 ]; then
      echo "The two filenames are the same. Exiting."
      exit 1
    fi

    # Execute the main script and measure the time it takes
    time ./convert $filename1 $filename2
    ;;
  2)
    # Prompt the user to enter a filename
    echo "Please enter a filename:"
    read filename
    echo "Please enter a coloring method (dsatur or dsaturbnb):"
    read method

    # Check if the filename is a regular file
    if [ ! -f $filename ]; then
      echo "$filename is not a regular file. Exiting."
      exit 1
    fi

    colored_filename="$filename.$method"
    # Execute the main script
    time ./color $filename $colored_filename $method
    ;;
  3)
    # Quit the app
    echo "Exiting."
    exit 0
    ;;
  *)
    # Handle invalid input
    echo "Invalid option. Exiting."
    exit 1
    ;;
esac
