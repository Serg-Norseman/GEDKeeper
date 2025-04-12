gpts = search_location_geopoints("Madrid", 10)
num = get_geopoints_count(gpts)
print(num)
for k = 0, num-1 do
    print(get_geopoint_latitude(gpts, k))
    print(get_geopoint_longitude(gpts, k))
    print(get_geopoint_name(gpts, k))
end

gpts = search_location_geopoints("Memphis", 10)
num = get_geopoints_count(gpts)
print(num)
for k = 0, num-1 do
    print(get_geopoint_latitude(gpts, k))
    print(get_geopoint_longitude(gpts, k))
    print(get_geopoint_name(gpts, k))
end
