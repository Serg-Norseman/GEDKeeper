
function do_descendants(person, level)
  if (person ~= nil) then
    gk_print(level..": "..gt_get_person_name(person));

    local spouses = gt_get_person_spouses_count(person);
    for i = 0, spouses-1 do
      local spouse_family = gt_get_person_spouse_family(person, i);
      local childs = gt_get_family_childs_count(spouse_family);

      for k = 0, childs-1 do
        local child = gt_get_family_child(spouse_family, k);
        do_descendants(child, level + 1);
      end
    end
  end
end

local p = gt_select_record(rtIndividual);
do_descendants(p, 0);
