
function do_ancestors(person, level)
  if (person ~= nil) then
    gk_print(level..": "..gt_get_person_name(person));

    local parents_family = gt_get_person_parents_family(person);
    if (parents_family ~= nil) then
      local father = gt_get_family_husband(parents_family);
      local mother = gt_get_family_wife(parents_family);

      do_ancestors(father, level + 1);
      do_ancestors(mother, level + 1);
    end
  end
end

local p = gt_select_record(rtIndividual);
do_ancestors(p, 0);
