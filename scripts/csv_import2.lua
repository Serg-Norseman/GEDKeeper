
csv_name = gk_select_file();
csv_load(csv_name, true);

cols = csv_get_cols();
rows = csv_get_rows();
gk_print("Файл: "..csv_name..", столбцов: "..cols..", строк: "..rows);

grp = gt_create_group("Пассажиры Mayflower");

for r = 0, rows-1 do
  num = csv_get_cell(0, r);
  pers = csv_get_cell(1, r);
  age = csv_get_cell(2, r);
  birth = csv_get_cell(3, r);
  rem = csv_get_cell(4, r);

  gk_print(line);

  iRec = gt_create_person("", "", pers, "M");
  gt_bind_group_member(grp, iRec);

  evt = gt_create_event(iRec, "BIRT");
  gt_set_event_place(evt, birth);

  evt = gt_create_event(iRec, "FACT");
  gt_set_event_value(evt, "> "..pers);

  if (age ~= "") then
    evt = gt_create_event(iRec, "FACT");
    gt_set_event_value(evt, "возраст: "..age);
  end

  if (rem ~= "") then
    evt = gt_create_event(iRec, "FACT");
    gt_set_event_value(evt, "> "..rem);
  end
end

csv_close();

gk_update_view();
