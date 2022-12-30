
csv_name = select_file();
csv_load(csv_name, true);

cols = csv_get_cols();
rows = csv_get_rows();
print("Файл: "..csv_name..", столбцов: "..cols..", строк: "..rows);

grp = create_group("Президенты США");

for r = 0, rows-1 do
  num = csv_get_cell(0, r);
  line = csv_get_cell(1, r);
  print(line);

  iRec = create_individual("", "", line, "M");
  bind_group_member(grp, iRec);

  evt = create_event(iRec, "FACT");
  set_event_value(evt, "Президент США, "..num.."-й");
end

csv_close();

update_view();