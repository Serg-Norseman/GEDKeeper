-- Удалить все пустые записи семей

function is_empty(family)
  local res = false;

  if (family ~= nil) then
    local husb = gt_get_family_husband(family);
    local wife = gt_get_family_wife(family);
    local childs = gt_get_family_childs_count(family);

    res = (husb == nil) and (wife == nil) and (childs == 0);
  end

  return res;
end

local del_count = 0;
for i = gt_get_records_count() - 1, 0, -1 do
  R = gt_get_record(i);
  rt = gt_get_record_type(R);
  if (rt == rtFamily) and is_empty(R) then
    del_count = del_count + 1;
    gk_print("Удалена запись: "..gt_get_record_xref(R));
    gt_delete_record(R);
  end
end

gk_print("Удалено: "..del_count);
gk_update_view();
