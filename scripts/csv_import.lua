
csv_name = select_file();

csv_load(csv_name, true);

cols = csv_get_cols();
rows = csv_get_rows();

print("Файл: "..csv_name..", столбцов: "..cols..", строк: "..rows);

for r = 0, rows-1 do
  line = "";

  for c = 0, cols-1 do
    line = line..csv_get_cell(c, r).." | ";
  end

  print(line);
end

csv_close();