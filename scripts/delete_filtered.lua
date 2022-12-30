-- Удалить все персональные записи, 
-- которые сейчас отфильтрованы в главном списке

local del_count = 0;
for i = get_records_count() - 1, 0, -1 do
  R = get_record(i);
  rt = get_record_type(R);
  if ((rt == rtIndividual) and (record_is_filtered(R))) then
    del_count = del_count + 1;
    print("Удалена запись: "..get_individual_name(R));
    delete_record(R);
  end
end

print("Удалено: "..del_count);

update_view();