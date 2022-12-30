
-- выбрать внешний файл
csv_name = select_file();
if (csv_name == "") then
  print("Файл не выбран");
  return
end

-- загрузка файла данных
csv_load(csv_name, true);
cols = csv_get_cols();
rows = csv_get_rows();

print("Файл: "..csv_name..", столбцов: "..cols..", строк: "..rows);

prev_vid = "";
prev_individual = 0;
marr_date = "";
birth_family = null;
key_individual = null;

for r = 0, rows-1 do
  -- получение содержимого ячеек
  page = csv_get_cell(0, r);
  vid = csv_get_cell(1, r);
  link = csv_get_cell(2, r);
  date = csv_get_cell(3, r);
  name = csv_get_cell(4, r);
  patr_name = csv_get_cell(5, r);
  family = csv_get_cell(6, r);
  town = csv_get_cell(7, r);
  source = csv_get_cell(8, r);
  comment = csv_get_cell(10, r);

  -- вывод для отладки
  line = name.." "..patr_name.." "..family..", "..town..", "..source.." + "..page;
  print(line);

  -- т.к. определение полного отчества требует пола, который сам требует отчества - необходимо выбрать
  -- ту операцию первой, которая меньше зависит от другой - это определение пола
  sex = define_sex(name, patr_name); 

  -- определение отчества, третий параметр - разрешение спрашивать пользователя, 
  -- если у программы нет вариантов
  patronymic = define_patronymic(patr_name, sex, true);

  -- создать персону
  p = create_individual(name, patronymic, family, sex);

  if (vid == "Брак") then
    marr_date = date;
    birth_family = null;
  else
    if (vid == "Рождение") then
      evt = create_event(p, "BIRT"); 
      set_event_date(evt, date);

      marr_date = "";

      birth_family = create_family();
      bind_family_child(birth_family, p);
    end

    if (vid == "Смерть") then
      evt = create_event(p, "DEAT"); 
      set_event_date(evt, date);

      marr_date = "";
      birth_family = null;
    end    
  end

  -- создать факт места проживания
  evt = create_event(p, "RESI"); 
  set_event_place(evt, town);

  -- найти, не создан ли уже такой источник
  src = find_source(source);
  if (src == 0) then
    -- создать отсутствующий
    src = create_source(source);
  end

  -- присоединить источник к персоне (3 - высокое качество источника)
  bind_record_source(p, src, page, 3);

  -- проверить, задан ли комментарий, если да - присоединить к персоне
  if not(comment == "") then
    n = create_note();
    bind_record_note(p, n);
    add_note_text(n, comment);
  end

  -- обработка семейных связей
  if (link == "Лицо") then
    key_individual = p;
  end

  if (link == "Жена") then
    f = create_family();
    bind_family_spouse(f, prev_individual); -- предполагаем, что предыдущая персона - муж
    bind_family_spouse(f, p); -- присоедниям к семье жену

    if not(marr_date == "") then
      evt = create_event(f, "MARR"); 
      set_event_date(evt, date);
    end
  end

  if (link == "Отец") and not(birth_family == null) then
    bind_family_spouse(birth_family, p); -- присоединяем к семье ребенка отца
  end

  if (link == "Мать") and not(birth_family == null) then
    bind_family_spouse(birth_family, p); -- присоединяем к семье ребенка мать
  end

  if (link == "Кресный") then
    add_individual_association(key_individual, link, p);
  end

  -- запоминаем состояние текущей строки
  if not(vid == "") then
    prev_vid = vid;
  end
  prev_individual = p;
end

-- закрыли файл данных
csv_close();

-- обновили списки базы данных
update_view();