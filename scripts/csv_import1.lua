
csv_name = gk_select_file();
csv_load(csv_name, true);

cols = csv_get_cols();
rows = csv_get_rows();
gk_print("Файл: "..csv_name..", столбцов: "..cols..", строк: "..rows);

grp = gt_create_group("Президенты США");

for r = 0, rows-1 do
  num = csv_get_cell(0, r);
  line = csv_get_cell(1, r);
  gk_print(line);

  iRec = gt_create_person("", "", line, "M");
  gt_bind_group_member(grp, iRec);

  evt = gt_create_event(iRec, "FACT");
  gt_set_event_value(evt, "Президент США, "..num.."-й");
end

csv_close();

gk_update_view();