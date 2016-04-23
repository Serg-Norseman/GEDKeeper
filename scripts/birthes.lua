-- Определить варианты событий рождения

for i = 0, gt_get_records_count() - 1 do
  R = gt_get_record(i); -- получить запись
  rt = gt_get_record_type(R); -- узнать её тип
  if (rt == rtIndividual) then
    local b_count = 0;
    local birthes = {};
    
    for at = 0, gt_get_person_events_count(R) - 1 do
      evt = gt_get_person_event(R, at);
      ev_name = gt_get_event_name(evt);
      ev_date = gt_get_event_date(evt);

      if (ev_name == "BIRT") then
        b_count = b_count + 1;
        birthes[b_count] = ev_date;
      end
    end

    if (b_count > 1) then
      -- вывод на экран
      gk_print("Персона "..gt_get_record_xref(R)..", имя: "..gt_get_person_name(R)..", событий рождения "..b_count);

      for k = 1, #birthes do
        gk_print("    > факт "..birthes[k]);
      end
    end
  end
end
