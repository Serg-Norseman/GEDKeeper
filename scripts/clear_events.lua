-- ВНИМАНИЕ: этот скрипт удаляет в древе все события в персональных записях!!!

-- узнать и вывести количество записей в дереве
x = gt_get_records_count();
gk_print("Записей в древе: "..x);

gk_progress_init(x, "Очистка базы данных");

-- перебрать все записи
for i = 0, x - 1 do
  R = gt_get_record(i); -- получить запись
  rt = gt_get_record_type(R); -- узнать её тип

  if (rt == rtIndividual) then
    gk_print("Запись "..i..", тип - персональная запись"); -- вывод на экран

    cnt = gt_get_person_events_count(R);
    gk_print("Количество событий: "..cnt);
    for ev = 1, cnt do
      gt_delete_person_event(R, 0);
    end
  end

  gk_progress_step();
end

gk_progress_done();

gk_update_view();
