-- узнать и вывести количество записей в дереве

x = get_records_count();
print("Записей в древе: "..x);

-- перебрать все записи
for i = 0, x - 1 do
  R = get_record(i); -- получить запись
  rt = get_record_type(R); -- узнать её тип
  if (rt == rtIndividual) then
    print("Запись "..i..", тип - персона, имя: "..get_individual_name(R)); -- вывод на экран
  end
end
