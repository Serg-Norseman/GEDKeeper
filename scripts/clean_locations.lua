-- Удалить все не использующиеся места

function is_empty(loc)
  local res = false;

  if (loc ~= nil) then
    local usages = get_location_usages(loc);
    local notes = get_record_notes_count(loc);

    res = (usages == 0) and (notes == 0);
  end

  return res;
end

local del_count = 0;
for i = get_records_count() - 1, 0, -1 do
  R = get_record(i);
  rt = get_record_type(R);
  if (rt == rtLocation) and is_empty(R) then
    del_count = del_count + 1;
    print("Удалена запись: "..get_record_xref(R));
    delete_record(R);
  end
end

print("Удалено: "..del_count);
update_view();
