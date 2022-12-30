-- Установить всем персональным записям заданное место
-- Set given place to all individualal records

for i = get_records_count() - 1, 0, -1 do
  R = get_record(i);
  rt = get_record_type(R);
  if ((rt == rtIndividual) and (record_is_filtered(R))) then
    evt = get_individual_event_ex(R, "BIRT");

    name = get_individual_name(R);
    year = get_event_year(evt);
    place = get_event_place(evt);
    idx_br = strpos(" (", name);
    ex = (strpos("Балам", name) == 0) or (strpos("Макур", name) == 0) or (strpos("Мал", name) == 0);

    if ((year > 1729) and (place == "") and (ex)) then
      set_event_place(evt, "Пермская губ., Красноуфимский уезд, Шайтанский завод");
    end
  end
end

update_view();