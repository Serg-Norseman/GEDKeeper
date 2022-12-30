
for i = get_records_count() - 1, 0, -1 do
  R = get_record(i);
  rt = get_record_type(R);
  if ((rt == rtIndividual) and (record_is_filtered(R))) then
    sex = get_individual_sex(R);
    if ((sex ~= "F") and (sex ~= "M")) then
      set_individual_sex(R, "M");
    end
  end
end

update_view();