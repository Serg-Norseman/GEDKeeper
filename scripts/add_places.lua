-- Установить всем персональным записям, 
-- заданное место

for i = gt_get_records_count() - 1, 0, -1 do
  R = gt_get_record(i);
  rt = gt_get_record_type(R);
  if ((rt == rtIndividual) and (gt_record_is_filtered(R))) then
    evt = gt_create_event(R, "RESI");
    gt_set_event_place(evt, "Пермская губ., Красноуфимский уезд, Шайтанский завод");
  end
end

gk_update_view();