-- ВНИМАНИЕ: этот скрипт удаляет в древе специфичные факты в персональных записях!!!

-- узнать и вывести количество записей в дереве
x = get_records_count();
print("Записей в древе: "..x);

progress_init(x, "Очистка базы данных");

-- перебрать все записи
for i = 0, x - 1 do
  R = get_record(i); -- получить запись
  rt = get_record_type(R); -- узнать её тип

  if (rt == rtIndividual) then
    at_cnt = get_individual_events_count(R);

    if (at_cnt > 0) then
      print("Запись "..i..", тип - персона, атрибутов: "..at_cnt);

      for at = at_cnt - 1, 0, -1 do
        attr = get_individual_event(R, at);
        val = get_event_value(attr);
        if (strpos("хозяин", val) >= 0) then 
          print("  > Найден факт хозяина двора - "..val);
          delete_individual_event(R, at);
        end
      end
    end
  end

  progress_step();
end

progress_done();