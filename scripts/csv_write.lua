
csv_name = select_new_file();

cols = 2;
rows = 10;

csv_create(csv_name, cols, rows);
print("Новый CSV-файл: "..csv_name..", столбцов: "..cols..", строк: "..rows);

for r = 0, rows-1 do
    csv_write_cell("test1");
    csv_write_cell("test2");
end

csv_close();
print("Файл закрыт");
