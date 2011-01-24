-- узнать и вывести количество записей в дереве

x = gt_get_records_count();
gk_print("«аписей в древе: "..x);

-- перебрать все записи
for i = 0, x - 1 do
  R = gt_get_record(i); -- получить запись
  rt = gt_get_record_type(R); -- узнать еЄ тип
  rt_name = gt_get_record_type_name(rt); -- получить им€ типа

  gk_print("«апись "..i..", тип "..rt_name); -- вывод на экран
end
