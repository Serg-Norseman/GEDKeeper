
list = {};

function add_list(level, person)
  for k = 1, #list do
    if (list[k][2] == person) then
      return;
    end
  end

  list[#list+1] = { level, person };
end

function print_list()
  gk_print("Братья и сестры ["..#list.."]:");

  for k = 1, #list do
    level = list[k][1];
    p = list[k][2];

    gk_print("["..level.."] "..get_desc(level, p)..": "..gt_get_person_name(p));
  end
end

function get_desc(level, person)
  sx = gt_get_person_sex(person);
  local simple = false;
  local x = "";

  if (level == 1) then
    simple = true;
  elseif (level == 2) then
    x = "двою";
  elseif (level == 3) then
    x = "трою";
  elseif (level == 4) then
    x = "четверою";
  elseif (level == 5) then
    x = "пятию";
  elseif (level == 6) then
    x = "шестию";
  end

  if (sx == "M") then
    if (simple) then
      kin = "родной брат";
    else
      kin = "родный брат";
    end
  elseif (sx == "F") then
    kin = "родная сестра";
  end
  
  return x..kin;
end

function do_descendants(source, person, level, sub_level)
  if (person ~= nil) then
    local spouses = gt_get_person_spouses_count(person);
    for i = 0, spouses-1 do
      local spouse_family = gt_get_person_spouse_family(person, i);
      local childs = gt_get_family_childs_count(spouse_family);

      for k = 0, childs-1 do
        local child = gt_get_family_child(spouse_family, k);

        local y = sub_level - 1;

        if (child ~= source) then
          if (y == 0) then
            add_list(level, child);
          end

          if (y > 0) then
            do_descendants(person, child, level, sub_level - 1);
          end
        end
      end
    end
  end
end

function do_ancestors(person, level)
  if (person ~= nil) then
    --gk_print(">>>"..level..": "..gt_get_person_name(person));

    local parents_family = gt_get_person_parents_family(person);
    if (parents_family ~= nil) then
      local father = gt_get_family_husband(parents_family);
      local mother = gt_get_family_wife(parents_family);

      local x = level + 1;

      do_descendants(person, father, x, x);
      do_descendants(person, mother, x, x);

      do_ancestors(father, x);
      do_ancestors(mother, x);
    end
  end
end

local p = gt_select_record(rtIndividual);
do_ancestors(p, 0);
print_list();
