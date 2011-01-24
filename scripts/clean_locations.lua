-- Удалить все не использующиеся места

function is_empty(loc)
  local res = false;

  if (loc ~= nil) then
    local usages = gt_get_location_usages(loc);
    local notes = gt_get_record_notes_count(loc);

    res = (usages == 0) and (notes == 0);
  end

  return res;
end

local del_count = 0;
for i = gt_get_records_count() - 1, 0, -1 do
  R = gt_get_record(i);
  rt = gt_get_record_type(R);
  if (rt == rtLocation) and is_empty(R) then
    del_count = del_count + 1;
    gk_print("Удалена запись: "..gt_get_record_xref(R));
    gt_delete_record(R);
  end
end

gk_print("Удалено: "..del_count);
gk_update_view();
