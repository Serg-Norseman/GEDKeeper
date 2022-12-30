-- Удалить все пустые записи семей

function is_empty(family)
  local res = false;

  if (family ~= nil) then
    local husb = get_family_husband(family);
    local wife = get_family_wife(family);
    local childs = get_family_childs_count(family);

    res = (husb == nil) and (wife == nil) and (childs == 0);
  end

  return res;
end

local del_count = 0;
for i = get_records_count() - 1, 0, -1 do
  R = get_record(i);
  rt = get_record_type(R);
  if (rt == rtFamily) and is_empty(R) then
    del_count = del_count + 1;
    print("Удалена запись: "..get_record_xref(R));
    delete_record(R);
  end
end

print("Удалено: "..del_count);
update_view();
