### Read all final pose file, export to csv GNSS and accuracy.
###
library(stringr)
folder= 'C:/Riegl Scans/NagsHead202303.RiSCAN/SCANS'
folder= 'C:/Riegl Scans/NagsHead202212.RiSCAN/SCANS'
# Search for 'final.pose.pose' files recursively
file_list <- list.files(path = folder, pattern = "final.pose.pose", recursive = TRUE, full.names = TRUE)

# Print the list of matching files
print(file_list)

lines <- readLines(file_list[1])
# sp lon lat alt sigma_h sigma_v fixInfo epsg
# get scan position



result  <- data.frame()

for (f in file_list) {
  print(f)
  sp = strsplit(f, '/')[[1]][5]
  lines <- readLines(f)
  if (length(grep("gnssSOCS", lines, value = TRUE)) == 0) {next}
  # Loop through the lines to find the desired patterns
  for (i in 1:(length(lines) - 2)) {
    if (grepl("gnssSOCS", lines[i]) && grepl("latitude", lines[i + 2]) && grepl("longitude", lines[i + 3])) {
      #lines[i + 1])
      alt = str_match_all(lines[i + 1],  ": (\\s*[-+]?\\d+\\.?\\d*)")[[1]][, 2]
      lat = str_match_all(lines[i + 2],  ": (\\s*[-+]?\\d+\\.?\\d*)")[[1]][, 2]
      lon = str_match_all(lines[i + 3],  ": (\\s*[-+]?\\d+\\.?\\d*)")[[1]][, 2]
      # lat_line <- lines[i + 1]
      #  lon_line <- lines[i + 2]
      break  # Exit the loop once the pattern is found
    }
  }

  line = grep("horizontalAccuracy", lines, value = TRUE)
  sigma_h = str_match_all(line,  ": (\\s*[-+]?\\d+\\.?\\d*)")[[1]][, 2]
  line = grep("verticalAccuracy", lines, value = TRUE)
  sigma_v = str_match_all(line,  ": (\\s*[-+]?\\d+\\.?\\d*)")[[1]][, 2]
  line  = grep("fixInfo", lines, value = TRUE)
  line = gsub("[^:A-Za-z ]", "", line)
  fixInfo = str_extract_all(line, "(?<=: )[^,]*")[[1]]
  line = grep("coordinateSystem", lines, value = TRUE)
  epsg = str_match_all(line,  "::(\\s*[-+]?\\d+\\.?\\d*)")[[1]][, 2]

  df =  data.frame(cbind(sp, lat, lon, alt, sigma_h, sigma_v, fixInfo, epsg))
  if (length(result) == 0) {result = df}
  else {result = rbind(result,df)}
}

#write.csv(result, file = 'C:/Users/lxiong/UMD/script/GNSS_sp.csv', row.names = FALSE)
write.table(result, file = 'C:/Users/lxiong/UMD/script/GNSS_sp_202212.csv', sep = "\t", row.names = FALSE, quote = FALSE)
