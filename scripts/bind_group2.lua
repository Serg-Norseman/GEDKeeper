
-- grp = gt_select_record(rtGroup);
grp = gt_create_group("Греки");

for i = gt_get_records_count() - 1, 0, -1 do
  R = gt_get_record(i);
  rt = gt_get_record_type(R);
  if ((rt == rtIndividual) and (gt_record_is_filtered(R))) then
    gt_bind_group_member(grp, R);
  end
end

gk_update_view();