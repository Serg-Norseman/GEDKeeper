-- узнать и вывести количество записей в дереве

x = gt_get_records_count();
gk_print("Записей в древе: "..x);

-- перебрать все записи
for i = 0, x - 1 do
  R = gt_get_record(i); -- получить запись
  rt = gt_get_record_type(R); -- узнать её тип
  if (rt == rtIndividual) then
    gk_print("Запись "..i..", тип - персона, имя: "..gt_get_person_name(R)); -- вывод на экран

    at_cnt = gt_get_person_events_count(R);

    if (at_cnt > 0) then
      gk_print("  Фактов "..at_cnt);

      for at = 0, at_cnt - 1 do
        evt = gt_get_person_event(R, at);
        gk_print("    > факт "..at..", место "..gt_get_event_place(evt));
      end
    end
  end
end
