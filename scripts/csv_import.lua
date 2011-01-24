
csv_name = gk_select_file();

csv_load(csv_name, true);

cols = csv_get_cols();
rows = csv_get_rows();

gk_print("װאיכ: "..csv_name..", סעמכבצמג: "..cols..", סענמך: "..rows);

for r = 0, rows-1 do
  line = "";

  for c = 0, cols-1 do
    line = line..csv_get_cell(c, r).." | ";
  end

  gk_print(line);
end

csv_close();