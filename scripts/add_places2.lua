-- Установить всем персональным записям заданное место
-- Set given place to all personal records

for i = gt_get_records_count() - 1, 0, -1 do
  R = gt_get_record(i);
  rt = gt_get_record_type(R);
  if ((rt == rtIndividual) and (gt_record_is_filtered(R))) then
    evt = gt_get_person_event_ex(R, "BIRT");

    name = gt_get_person_name(R);
    year = gt_get_event_year(evt);
    place = gt_get_event_place(evt);
    idx_br = gk_strpos(" (", name);
    ex = (gk_strpos("Балам", name) == 0) or (gk_strpos("Макур", name) == 0) or (gk_strpos("Мал", name) == 0);

    if ((year > 1729) and (place == "") and (ex)) then
      gt_set_event_place(evt, "Пермская губ., Красноуфимский уезд, Шайтанский завод");
    end
  end
end

gk_update_view();