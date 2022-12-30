-- Установить всем персональным записям заданное место
-- Set given place to all individualal records

for i = get_records_count() - 1, 0, -1 do
  R = get_record(i);
  rt = get_record_type(R);
  if ((rt == rtIndividual) and (record_is_filtered(R))) then
    evt = create_event(R, "RESI");
    set_event_place(evt, "Пермская губ., Красноуфимский уезд, Шайтанский завод");
  end
end

update_view();