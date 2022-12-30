-- ВНИМАНИЕ: этот скрипт удаляет в древе все события в персональных записях!!!

-- узнать и вывести количество записей в дереве
x = get_records_count();
print("Записей в древе: "..x);

progress_init(x, "Очистка базы данных");

-- перебрать все записи
for i = 0, x - 1 do
  R = get_record(i); -- получить запись
  rt = get_record_type(R); -- узнать её тип

  if (rt == rtIndividual) then
    print("Запись "..i..", тип - персональная запись"); -- вывод на экран

    cnt = get_individual_events_count(R);
    print("Количество событий: "..cnt);
    for ev = 1, cnt do
      delete_individual_event(R, 0);
    end
  end

  progress_step();
end

progress_done();

update_view();
