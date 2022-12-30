-- Установить всем персональным записям выбранный источник
-- Set given source to all individualal records

src = select_record(rtSource);

for i = get_records_count() - 1, 0, -1 do
  R = get_record(i);
  rt = get_record_type(R);
  if ((rt == rtIndividual) and (record_is_filtered(R))) then
    bind_record_source(R, src, "-", 3);
  end
end

update_view();