
for i = gt_get_records_count() - 1, 0, -1 do
  R = gt_get_record(i);
  rt = gt_get_record_type(R);
  if ((rt == rtIndividual) and (gt_record_is_filtered(R))) then
    gr_count = gt_get_person_groups_count(R);
    if (gr_count > 0) then
      gr = gt_get_person_group(R, 0);
      gr_name = gt_get_group_name(gr);

      evt = gt_create_event(R, "RESI");
      gt_set_event_place(evt, gr_name);
    end
  end
end

gk_update_view();