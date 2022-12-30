
function do_ancestors(individual, level)
  if (individual ~= nil) then
    print(level..": "..get_individual_name(individual));

    local parents_family = get_individual_parents_family(individual);
    if (parents_family ~= nil) then
      local father = get_family_husband(parents_family);
      local mother = get_family_wife(parents_family);

      do_ancestors(father, level + 1);
      do_ancestors(mother, level + 1);
    end
  end
end

local p = select_record(rtIndividual);
do_ancestors(p, 0);
