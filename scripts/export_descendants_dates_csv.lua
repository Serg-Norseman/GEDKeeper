function do_descendants(individual, level, ix)
  if (individual ~= nil) then
    local dbirth = "";
    local ddeath = "";
    for at = 0, get_individual_events_count(individual) - 1 do
      local evt = get_individual_event(individual, at);
      local ev_name = get_event_name(evt);
      local ev_date = get_event_date(evt);

      if (ev_date ~= nil) then
	    if ((ev_name == "BIRT") and (dbirth == "")) then
		  dbirth = ""..ev_date;
	    end

	    if ((ev_name == "DEAT") and (ddeath == "")) then
		  ddeath = ""..ev_date;
	    end
	  end
    end
    
    ix = ix + 1;
    csv_write_cell(""..ix);
    csv_write_cell(""..level);
    csv_write_cell(get_individual_name(individual));
    csv_write_cell(dbirth);
    csv_write_cell(ddeath);

    local spouses = get_individual_spouses_count(individual);
    for i = 0, spouses-1 do
      local spouse_family = get_individual_spouse_family(individual, i);
      local childs = get_family_childs_count(spouse_family);

      for k = 0, childs-1 do
        local child = get_family_child(spouse_family, k);
        do_descendants(child, level + 1, ix);
      end
    end
  end
end


prs = select_record(rtIndividual);
csv_name = select_new_file();
cols = 5;
rows = 1000;
csv_create(csv_name, cols, rows);
csv_write_cell("Номер"); csv_write_cell("Уровень"); csv_write_cell("Имя"); csv_write_cell("Дата рождения"); csv_write_cell("Дата смерти");

do_descendants(prs, 0, 0);

print("Новый CSV-файл: "..csv_name..", столбцов: "..cols..", строк: "..rows);
csv_close();
