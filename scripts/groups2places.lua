
for i = get_records_count() - 1, 0, -1 do
  R = get_record(i);
  rt = get_record_type(R);
  if ((rt == rtIndividual) and (record_is_filtered(R))) then
    gr_count = get_individual_groups_count(R);
    if (gr_count > 0) then
      gr = get_individual_group(R, 0);
      gr_name = get_group_name(gr);

      evt = create_event(R, "RESI");
      set_event_place(evt, gr_name);
    end
  end
end

update_view();