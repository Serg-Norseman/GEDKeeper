-- установить PRIM признак всем первым мультимедиа-ссылкам в персональных записях

local set_count = 0;

for i = get_records_count() - 1, 0, -1 do
  R = get_record(i);
  rt = get_record_type(R);
  if (rt == rtIndividual) then
    --exists_prim = get_individual_primary_medialink(R);
    --exists_prim == nil and 

    if (get_record_medialinks_count(R) > 0) then
      media = get_record_medialink(R, 0);
      set_medialink_primary(media, true);
      print("Записи `"..get_individual_name(R).."` установлен PRIM");
      set_count = set_count + 1;
    end
  end
end

update_view();

print("Установлено: "..set_count);
