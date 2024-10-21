# Set the input and output directories
input_dir="shared/sources/"
output_dir="shared/lexed/"

# Create the output directory if it doesn't exist
mkdir -p "$output_dir"

# Loop through all the files in the input directory
for file in "$input_dir"*.tig; do
    # Extract the filename without the extension
    filename=$(basename "$file" .tig)

    # Create the output file
    output_file="$output_dir$filename.lexed"
    touch "$output_file"

    # Generate the output file using ch2_lexer and redirect stdout to the file
    make -s ch2_lexer FILE="$file" > "$output_file"
done
