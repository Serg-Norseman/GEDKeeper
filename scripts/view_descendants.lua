
function do_descendants(individual, level)
  if (individual ~= nil) then
    print(level..": "..get_individual_name(individual));

    local spouses = get_individual_spouses_count(individual);
    for i = 0, spouses-1 do
      local spouse_family = get_individual_spouse_family(individual, i);
      local childs = get_family_childs_count(spouse_family);

      for k = 0, childs-1 do
        local child = get_family_child(spouse_family, k);
        do_descendants(child, level + 1);
      end
    end
  end
end

local p = select_record(rtIndividual);
do_descendants(p, 0);
