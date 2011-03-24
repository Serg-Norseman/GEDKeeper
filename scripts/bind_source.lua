-- Установить всем персональным записям выбранный источник

src = gt_select_record(rtSource);

for i = gt_get_records_count() - 1, 0, -1 do
  R = gt_get_record(i);
  rt = gt_get_record_type(R);
  if ((rt == rtIndividual) and (gt_record_is_filtered(R))) then
    gt_bind_record_source(R, src, "-", 3);
  end
end

gk_update_view();