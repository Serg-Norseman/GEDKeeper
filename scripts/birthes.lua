-- Определить варианты событий рождения

for i = 0, get_records_count() - 1 do
  R = get_record(i); -- получить запись
  rt = get_record_type(R); -- узнать её тип
  if (rt == rtIndividual) then
    local b_count = 0;
    local birthes = {};
    
    for at = 0, get_individual_events_count(R) - 1 do
      evt = get_individual_event(R, at);
      ev_name = get_event_name(evt);
      ev_date = get_event_date(evt);

      if (ev_name == "BIRT") then
        b_count = b_count + 1;
        birthes[b_count] = ev_date;
      end
    end

    if (b_count > 1) then
      -- вывод на экран
      print("Персона "..get_record_xref(R)..", имя: "..get_individual_name(R)..", событий рождения "..b_count);

      for k = 1, #birthes do
        print("    > факт "..birthes[k]);
      end
    end
  end
end
