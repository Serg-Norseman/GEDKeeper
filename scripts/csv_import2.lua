
csv_name = select_file();
csv_load(csv_name, true);

cols = csv_get_cols();
rows = csv_get_rows();
print("Файл: "..csv_name..", столбцов: "..cols..", строк: "..rows);

grp = create_group("Пассажиры Mayflower");

for r = 0, rows-1 do
  num = csv_get_cell(0, r);
  pers = csv_get_cell(1, r);
  age = csv_get_cell(2, r);
  birth = csv_get_cell(3, r);
  rem = csv_get_cell(4, r);

  print(line);

  iRec = create_individual("", "", pers, "M");
  bind_group_member(grp, iRec);

  evt = create_event(iRec, "BIRT");
  set_event_place(evt, birth);

  evt = create_event(iRec, "FACT");
  set_event_value(evt, "> "..pers);

  if (age ~= "") then
    evt = create_event(iRec, "FACT");
    set_event_value(evt, "возраст: "..age);
  end

  if (rem ~= "") then
    evt = create_event(iRec, "FACT");
    set_event_value(evt, "> "..rem);
  end
end

csv_close();

update_view();
