
for i = gt_get_records_count() - 1, 0, -1 do
  R = gt_get_record(i);
  rt = gt_get_record_type(R);
  if ((rt == rtIndividual) and (gt_record_is_filtered(R))) then
    sex = gt_get_person_sex(R);
    if ((sex ~= "F") and (sex ~= "M")) then
      gt_set_person_sex(R, "M");
    end
  end
end

gk_update_view();