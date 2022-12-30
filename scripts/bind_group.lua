
-- src = select_record(rtSource);
grp = create_group("Греки");

for i = get_records_count() - 1, 0, -1 do
  R = get_record(i);
  rt = get_record_type(R);
  if ((rt == rtIndividual) and (record_is_filtered(R))) then
    bind_record_source(R, src, "-", 3);
  end
end

update_view();
print("ready");
