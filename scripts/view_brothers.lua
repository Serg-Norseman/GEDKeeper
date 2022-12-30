
list = {};

function add_list(level, individual)
  for k = 1, #list do
    if (list[k][2] == individual) then
      return;
    end
  end

  list[#list+1] = { level, individual };
end

function print_list()
  print("Братья и сестры ["..#list.."]:");

  for k = 1, #list do
    level = list[k][1];
    p = list[k][2];

    print("["..level.."] "..get_desc(level, p)..": "..get_individual_name(p));
  end
end

function get_desc(level, individual)
  sx = get_individual_sex(individual);
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

function do_descendants(source, individual, level, sub_level)
  if (individual ~= nil) then
    local spouses = get_individual_spouses_count(individual);
    for i = 0, spouses-1 do
      local spouse_family = get_individual_spouse_family(individual, i);
      local childs = get_family_childs_count(spouse_family);

      for k = 0, childs-1 do
        local child = get_family_child(spouse_family, k);

        local y = sub_level - 1;

        if (child ~= source) then
          if (y == 0) then
            add_list(level, child);
          end

          if (y > 0) then
            do_descendants(individual, child, level, sub_level - 1);
          end
        end
      end
    end
  end
end

function do_ancestors(individual, level)
  if (individual ~= nil) then
    --print(">>>"..level..": "..get_individual_name(individual));

    local parents_family = get_individual_parents_family(individual);
    if (parents_family ~= nil) then
      local father = get_family_husband(parents_family);
      local mother = get_family_wife(parents_family);

      local x = level + 1;

      do_descendants(individual, father, x, x);
      do_descendants(individual, mother, x, x);

      do_ancestors(father, x);
      do_ancestors(mother, x);
    end
  end
end

local p = select_record(rtIndividual);
do_ancestors(p, 0);
print_list();
