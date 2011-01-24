-- Удалить все персональные записи, 
-- которые сейчас отфильтрованы в главном списке

local del_count = 0;
for i = gt_get_records_count() - 1, 0, -1 do
  R = gt_get_record(i);
  rt = gt_get_record_type(R);
  if ((rt == rtIndividual) and (gt_record_is_filtered(R))) then
    del_count = del_count + 1;
    gk_print("Удалена запись: "..gt_get_person_name(R));
    gt_delete_record(R);
  end
end

gk_print("Удалено: "..del_count);

gk_update_view();