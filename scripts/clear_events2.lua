-- ВНИМАНИЕ: этот скрипт удаляет в древе специфичные факты в персональных записях!!!

-- узнать и вывести количество записей в дереве
x = gt_get_records_count();
gk_print("Записей в древе: "..x);

gk_progress_init(x, "Очистка базы данных");

-- перебрать все записи
for i = 0, x - 1 do
  R = gt_get_record(i); -- получить запись
  rt = gt_get_record_type(R); -- узнать её тип

  if (rt == rtIndividual) then
    at_cnt = gt_get_person_events_count(R);

    if (at_cnt > 0) then
      gk_print("Запись "..i..", тип - персона, атрибутов: "..at_cnt);

      for at = at_cnt - 1, 0, -1 do
        attr = gt_get_person_event(R, at);
        val = gt_get_event_value(attr);
        if (gk_strpos("хозяин", val) > 0) then 
          gk_print("  > Найден факт хозяина двора - "..val);
          gt_delete_person_event(R, at);
        end
      end
    end
  end

  gk_progress_step();
end

gk_progress_done();