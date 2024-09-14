library(readxl)

# Read the content of the text file
file_path <- "d:/eechoque/Documents/Edison_INE/CSPro censo/CSPRO_empleo/APP/empleo1.bch.apc"
file_content <- readLines(file_path)

# Read the columns of excel file
excel_data <- read_excel("d:/eechoque/Documents/Edison_INE/Consistencia empleo/CPV2024/MP43_P48.xlsx", sheet = "export", col_names = T)
selected <- excel_data[, "cr_accion"]
write.table(selected, "d:/eechoque/Documents/Edison_INE/Consistencia empleo/CPV2024/MP43_P48.txt", row.names = FALSE, col.names = FALSE, quote = FALSE)
new_text <- readLines("d:/eechoque/Documents/Edison_INE/Consistencia empleo/CPV2024/MP43_P48.txt")
#new_text <- ""

# Find the line numbers where "start_line" and "end_line" occur
start_line <- grep("//START_EDISON1", file_content)
end_line <- grep("//END_EDISON1", file_content)

# Check if both markers are found
if (length(start_line) == 0 || length(end_line) == 0) {
  stop("Markers '//START_EDISON1' and/or '//END_EDISON1' not found in the file.")
}

# Insert the new text at the specified positions
edited_content <- c(
  file_content[1:(start_line - 0)],
  new_text,
  file_content[end_line:length(file_content)]
)
# Write the edited content back to the file
writeLines(edited_content, file_path)



#####################################
library(readxl)

# Read the content of the text file
file_path <- "d:/eechoque/Documents/Edison_INE/CSPro censo/CSPRO_empleo/APP/empleo1.bch.apc"
file_content <- readLines(file_path)

# Read the columns of excel file
excel_data <- read_excel("d:/eechoque/Documents/Edison_INE/Consistencia empleo/CPV2024/MP43_P48.xlsx", sheet = "export", col_names = T)
selected <- excel_data[, "imputacion"]
write.table(selected, "d:/eechoque/Documents/Edison_INE/Consistencia empleo/CPV2024/MP43_P48.txt", row.names = FALSE, col.names = FALSE, quote = FALSE)
new_text <- readLines("d:/eechoque/Documents/Edison_INE/Consistencia empleo/CPV2024/MP43_P48.txt")
#new_text <- ""

# Find the line numbers where "start_line" and "end_line" occur
start_line <- grep("//START_EDISON2", file_content)
end_line <- grep("//END_EDISON2", file_content)

# Check if both markers are found
if (length(start_line) == 0 || length(end_line) == 0) {
  stop("Markers '//START_EDISON2' and/or '//END_EDISON2' not found in the file.")
}

# Insert the new text at the specified positions
edited_content <- c(
  file_content[1:(start_line - 0)],
  new_text,
  file_content[end_line:length(file_content)]
)
# Write the edited content back to the file
writeLines(edited_content, file_path)


