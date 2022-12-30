-- узнать и вывести количество записей в дереве

x = get_records_count();
print("Записей в древе: "..x);

-- перебрать все записи
for i = 0, x - 1 do
  R = get_record(i); -- получить запись
  rt = get_record_type(R); -- узнать её тип
  rt_name = get_record_type_name(rt); -- получить имя типа

  print("Запись "..i..", тип "..rt_name); -- вывод на экран
end
