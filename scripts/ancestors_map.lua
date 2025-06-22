--
--    Ancestors_map.lua: A lua script for GEDKeeper that generates an ancestors map in html
--
--    Copyright (C) 2025 Miguel A. Pérez Valdenebro
--
--    This program is free software: you can redistribute it and/or modify
--    it under the terms of the GNU General Public License as published by
--    the Free Software Foundation, either version 3 of the License, or
--    (at your option) any later version.
--
--    This program is distributed in the hope that it will be useful,
--    but WITHOUT ANY WARRANTY; without even the implied warranty of
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--    GNU General Public License for more details.
--
--    You should have received a copy of the GNU General Public License
--    along with this program.  If not, see <https://www.gnu.org/licenses/>.
--
--	DESCRIPTION
--
--	This lua script generates an html file that allows to display on an ‘openstreetmap’ map,
--	the locations where the ancestors of a selected person were born, provided that they are
--	defined in the GEDkeeper database with their coordinates. If not found, try to find them
--	through the geolocator.
--
--	It makes use of the facilities offered by the javascript leaflet library to draw on 
--	OpenStreetMap maps.
--
--	A circle is drawn over each location, the area of which is proportional to the number of 
--	people who were born in that location.
--
--	The colour of the circle is related to the branch of the family. It is used:
--		- red for the paternal grandfather's branch,
--		- fucsia for the paternal grandmother's branch, 
--		- green for the maternal grandfather's branch
--		- cyan for the maternal grandmother's branch.
--
--	The colours gradually darken as the generations pass.
--
--	In the case of two people coinciding in the same locality, their colours merge with each 
--	other.
--
--	Positioning the cursor over each circle shows the name of the location and the people who
--	were born there.
--
--	HISTORY
--
--	17/04/2025	Miguel A. Pérez Valdenebro		Written.
--	28/04/2025	Sergey V. Zhdanovskikh			Added Russian translation.
--	01/05/2025	Miguel A. Pérez Valdenebro		Ensure that the decimal separator in the html
--												file is the dot regardless of the locale used.
--												Shading parameters adjusted.
--
-----------------------------------------------------------------------------------------------

-- GLOBAL VARIABLES  --------------------------------------------------------------------------
--

-- for localization
--
locales = {}

-- Supported languages coded following ISO 639-1
--
supported_languages = { "es", "en", "fr", "pt", "ca", "ru" }

-- Messages in the different supported languages.
--

-- English - en
--
locales.en = {
["In 0"]="Info: Scanning %s ancestors tree.",
["In 1"]="Info: Done!.",
["In 2"]="Info: Writing html output in %s ...",
["Er 0"]="Error: No valid location has been found. Unable to generate map.",
["Er 1"]="Error: Cannot open file %s.",
["Wr 0"]="Warning: Location %s not found !",
["Wr 1"]="Warning: Language %s not supported. Using english."
}

-- Spanish - es
--
locales.es = {
["In 0"]="Info: Rastreando el arbol genealógico de %s.",
["In 1"]="Info: Hecho.",
["In 2"]="Info: Escribiendo la salida html en %s ...",
["Er 0"]="Error: No se ha encontrado ninguna ubicación válida. No se puede generar el mapa.",
["Er 1"]="Error: No se puede abrir el archivo %s.",
["Wr 0"]="Aviso: La ubicación %s no se ha encontrado.",
["Wr 1"]="Aviso: El idioma %s no está soportado. Usando inglés."  -- It is never shown in spanish. English is always used for this message.
}

-- French - fr
--
locales.fr = {
["In 0"]="Info: Analyse de l'arbre des ancêtres de %s.",
["In 1"]="Info: Terminé.",
["In 2"]="Info: Ecriture de la sortie html dans %s ...",
["Er 0"]="Erreur : Aucun emplacement valide n'a été trouvé. Impossible de générer une carte.",
["Er 1"]="Erreur : Impossible d'ouvrir le fichier",
["Wr 0"]="Avertissement : L'emplacement du %s n'a pas été trouvé.",
["Wr 1"]="Avertissement : La langue %s n'est pas prise en charge. Utiliser l'anglais."  -- It is never shown in french. English is always used for this message.
}

-- Portuguese - pt 
--
locales.pt = {
["In 0"]="Info: Traçando a árvore genealógica de %s.",
["In 1"]="Info: Feito.",
["In 2"]="Info: Escrever a saída html em %s ...",
["Er 0"]="Erro: Não foi encontrada nenhuma localização válida. Não foi possível gerar o mapa.",
["Er 1"]="Erro: Não é possível abrir o ficheiro %s.",
["Wr 0"]="Aviso: Localização %s não encontrada.",
["Wr 1"]="Aviso: O idioma %s não é suportado. Utilizamos o inglês."  -- It is never shown in portuguese. English is always used for this message.
}

-- Catalan - ca
--
locales.ca = {
["In 0"]="Info: Rastrejant l'arbre genealògic de %s.",
["In 1"]="Info: Fet.",
["In 2"]="Info: Escrivint la sortida html a %s ...",
["Er 0"]="Error: No s'ha trobat cap ubicació vàlida. No es pot generar el mapa.",
["Er 1"]="Error: No es pot obrir l'arxiu %s.",
["Wr 0"]="Avís: La ubicació %s no s'ha trobat.",
["Wr 1"]="Avís: L'idioma %s no és compatible. Utilitzant l'anglès."  -- It is never shown in catalan. English is always used for this message.
}

-- Russian - ru
--
locales.ru = {
["In 0"]="Info: Сканирование дерева предков %s.",
["In 1"]="Info: Завершено!",
["In 2"]="Info: Запись html-вывода в %s...",
["Er 0"]="Error: Не найдено допустимого местоположения. Невозможно создать карту.",
["Er 1"]="Error: Невозможно открыть файл %s.",
["Wr 0"]="Warning: Местоположение %s не найдено!",
["Wr 1"]="Warning: Язык %s не поддерживается. Используется английский."
}

locale = "en"	-- default language. You can set your preferred language here or set it in the environment variable LANG.

-- FUNCTIONS  ----------------------------------------------------------------------------------
--

--Returns the message in the set language.
--
function translate(s)
	return locales[locale][s]
end

-- Set the language. If not available, English is assigned.
--
function set_locale( id )

	if string.len( id ) > 2 then
		id = string.sub( id, 1, 2 ) 
	end
	
	id = string.lower( id )
	
	for i = 1,#supported_languages do
		if (supported_languages[i] == id) then
			locale = id
			return
		end
	end
  
  locale = "en"
  
  -- Warning: Language %s not supported. Using english.
  print( string.format( translate("Wr 1") , id ) )
end

-- shade the reference colour
-- We darken the colour by a dec amount multiplied by the depth level of the family tree.
--
function shade_colour( col, dec, level ) 

	if level * dec >= 0xFF then
		return 0
	end

	red = col & 0xff0000
	green = col & 0x00ff00
	blue = col & 0x0000ff
	
	if red ~= 0 then
		red = 0xff - ( dec * level )
	end

	if green ~= 0 then
		green = 0xff - ( dec * level )
	end

	if blue ~= 0 then
		blue = 0xff - ( dec * level )
	end

	return ( (red << 16) | (green << 8) | blue )
end

-- Function to mix two colours
--
function mix_colour( old_colour, people, new_colour )
	
	-- we break down the colours into their fundamental components 
	
	old_red = ( ( old_colour & 0xff0000 ) >> 16)
	new_red = ( ( new_colour & 0xff0000 ) >> 16)
	
	old_green = ( ( old_colour & 0x00ff00 ) >> 8)
	new_green = ( ( new_colour & 0x00ff00 ) >> 8)

	old_blue = ( old_colour & 0x0000ff )
	new_blue = ( new_colour & 0x0000ff )
	
	-- average weighted -by the number of persons- in each component
	
	red = math.floor( ( old_red * people + new_red ) / (people + 1) )
	green = math.floor( ( old_green * people + new_green ) / (people + 1) )
	blue = math.floor( ( old_blue * people + new_blue ) / (people + 1) )

	-- We return the recomposed colour

	return ( (red << 16) | (green << 8) | blue )
end

-- Function to add a location to the site list
--
function add_place( location, place, year, level, name, xref, col )

	local list = place_list
	
	local place_name = place;

	if location ~= nil then
		place_name = get_location_name( location )
		if place_name == nil then
			place_name = place
		end
	end

	-- We look for the place in the list of places

	while list do	
		if list.location == place_name then

			local xlist = list.id_list
			while xlist do
				if xlist.id == xref then
					return
				end
				xlist = xlist.next
			end

			-- If found, we update their values ...
			aux_list = { id = xref, next = list.id_list }
			list.id_list = aux_list
			list.text = list.text.."<br>[<b>"..level.."</b>] "..name.." ("..year..")"
			list.colour = mix_colour( list.colour, list.people, col )
			list.people = list.people + 1
			
			-- ... and we return
			return
		end
		
		list = list.next
	end
	
	-- If it is not in the list of values ...
	-- ... we look for its coordinates
	
	if location ~= nil then
		latitude = get_location_latitude( location )
		longitude = get_location_longitude( location )
	end
	
	if ( location == nil or latitude == nil or longitude == nil ) then 	
		-- If we have not found the location or its coordinates in the GEDKeeper database,
		-- we try to find them through the geolocator.
		gpts = search_location_geopoints( place_name, 10 )
		num = get_geopoints_count(gpts)

		if num > 0 then
			latitude = get_geopoint_latitude(gpts, 0)
			longitude = get_geopoint_longitude(gpts, 0)
		else -- not found ... either.
		
			-- Warning: Location not found !
			print( string.format( translate("Wr 0") , place_name ) )

			latitude=nil
			longitude=nil
			
			return
		end
	end
	
	-- We update, if necessary, maximum and minimum longitudes and latitudes ...
		
	if max_lat == nil then
		max_lat = latitude
	elseif latitude > max_lat then
		max_lat = latitude
	end
		
	if min_lat == nil then
		min_lat = latitude
	elseif latitude < min_lat then
		min_lat = latitude
	end
		
	if max_long == nil then
		max_long = longitude
	elseif longitude > max_long then
		max_long = longitude
	end
		
	if min_long == nil then
		min_long = longitude
	elseif longitude < min_long then
		min_long = longitude
	end
	
	-- ... and insert the locality in the list
	
	xref_list = { id = xref, next = nil }
	list = { location = place_name, text = "<b>"..place_name.."</b><br><br>[<b>"..level.."</b>] "..name.." ("..year..")", colour = col, people = 1, id_list = xref_list, lat = latitude, long = longitude, next = place_list };
	place_list = list;
end

-- Function to extract the year from the date. Returns " -- " if not found.
--
function get_year( date )

	if ( date == nil or date == "" ) then
		return " -- "
	end

	local year = "%d%d%d%d"
	local c = string.find( date, year)

	return string.sub(date, c)
end

-- Function to navigate through the family tree searching for birthplaces
--
function do_ancestors(individual, level, colour)

	local ind_name=get_individual_name(individual)
	local ind_xref = get_record_xref(individual)
	local colour_f = shade_colour( colour, 20, level )
	local colour_m = colour_f;

	-- colours

    if level == 0 then
		colour = 0xffff00 			  -- colour for main character
		colour_f = 0xff0000			  -- colour for father
		colour_m = 0x00ff00			  -- colour for mother
	elseif level == 1 then
		if colour & 0xff0000 ~= 0 then -- colours for the father's branch
			colour_f = 0xf00000 	  -- red shades for paternal grandfather branch
			colour_m = 0xf000f0 	  -- fuchsia shades for paternal grandmother branch
		else						  -- colours for the mother's branch
			colour_f = 0x00f000 	  -- green shades for maternal grandfather branch
			colour_m = 0x00f0f0 	  -- aqua shades for maternal grandfather branch
		end
	end

	-- Search for place of birth

	cnt = get_individual_events_count(individual)

    for t = 0, cnt - 1 do
      ev = get_individual_event(individual, t)
      ev_name = get_event_name(ev)
	  
	  if ev_name == "BIRT" then
		ev_location = get_event_location( ev )
        ev_place = get_event_place(ev)
		ev_date = get_event_date(ev)
		ev_year = get_year(ev_date)

		-- If we find the place of birth, we add it to the list of places.    
		if ev_place ~= "" then
		  add_place( ev_location, ev_place, ev_year, level, ind_name, ind_xref, colour )
		end
	  end
    end

	-- Recursive descent through family branches

    local parents_family = get_individual_parents_family(individual)
    if parents_family ~= nil then
		local father = get_family_husband(parents_family)
		local mother = get_family_wife(parents_family)

		if father ~= nil then
			do_ancestors(father, level + 1, colour_f)
		end
		if mother ~= nil then
			do_ancestors(mother, level + 1, colour_m)
		end
    end
end

-- Function to write the html document header
--
function write_html_head( f, n )

	local head = [[
<head>
<base target="_top">
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
]]
	local title = "<title>"..n.." ancestry map</title>\n"
	
	local headend = [[
<link rel="stylesheet" href="https://unpkg.com/leaflet@1.9.4/dist/leaflet.css" integrity="sha256-p4NxAoJBhIIN+hmNHrzRCf9tD/miZyoHS5obTRR9BMY=" crossorigin=""/>
<script src="https://unpkg.com/leaflet@1.9.4/dist/leaflet.js" integrity="sha256-20nQCchB9co0qIjJZRGuk2/Z9VM+kNiyxNV1lvTlZBo=" crossorigin=""></script>
<style>
	html, body { height: 100%; margin: 0; }
	.leaflet-container { height: 100%; width: 100%; max-width: 100%; max-height: 100%; }
</style>
</head>
]]

	f:write( head )
	f:write( title )
	f:write( headend )
end

-- Function to validate coordinates
-- If no place found with valid coordinates
-- we assign value 0, as we cannot perform 
-- arithmetic functions with value nil
--
function check_coordinates ()

	if (min_lat == nil ) then 
		min_lat = 0
		max_lat = 0
		min_long = 0
		max_long = 0
	end
end

-- Function to estimate scale
--
function calculate_scale()

	local degrees = { 230, 170, 120, 77, 50, 30, 15, 6, 3, 1.5, 1.2, 1, 0.8, 0.65, 0.50, 0.35, 0.20, 0.10, 0 }

	local x = 0;

	local lat = max_lat-min_lat
	if( lat > 180 ) then
		lat = lat - 180
	end
	
	local long = max_long-min_long;

	if lat > long then
		x = lat
	else
		x = long
	end

	local s = 1;
	
	while x < degrees[s] do
		s = s + 1
	end

	return s
end

-- Function to write the body of the html document
--
function write_html_body( f , lt )

	local ct = 0 -- counter

	-- Calculation of mean longitudes and latitudes, necessary to centre the map
	check_coordinates()
	local clat = min_lat + (max_lat-min_lat)/2
	local clong = min_long + (max_long-min_long)/2

	local scala = calculate_scale()

	-- write the body of the document
	f:write("\n<body>\n")
	f:write("<div id=\"map\"></div>\n")
	f:write("\n<script>\n")
	f:write("const map = L.map('map').setView(["..clat..","..clong.."], "..scala..");\n")
	f:write("const tiles = L.tileLayer('https://tile.openstreetmap.org/{z}/{x}/{y}.png', {\n")
	f:write("  maxZoom: 19,\n  attribution: '&copy; <a href=\"http://www.openstreetmap.org/copyright\">OpenStreetMap</a> | &copy <a href=\"https://gedkeeper.net/\"</a>GedKeeper</a>'\n");
	f:write("}).addTo(map);\n")

	-- We go through the list of locations to generate the leaflet components needed to draw on the map.
	while lt do
		if ( lt.lat ~= nil and lt.long ~= nil ) then
			local area = math.floor(300*math.sqrt( lt.people ));		
			f:write("\nconst circle_"..ct.." = L.circle( ["..string.format("%03.6f",lt.lat)..","..string.format("%03.6f",lt.long).."], {\n  color: '#"..string.format("%06x",lt.colour).."',\n")
			f:write("  weight: 2,\n  fillOpacity: 0.333,\n  radius: "..area.."\n}).addTo(map).bindPopup('"..lt.text.."');\n")
		end
		
		lt = lt.next
		ct = ct + 1
	end

	f:write("\n</script>\n\n")
	f:write("</body>\n")
end

-- Function to write the output html file
--
function write_html_output( list, name )

	-- Select filename
	local fn = select_new_file()

	-- We open the file
	local fd=io.open( fn, "w" )
	
	if fd == nil then
		-- Error: Cannot open file
		print( string.format( translate("Er 1") , fn ) )
		return
	end
	
	-- Info: Writing html output ...
	print( string.format( translate("In 2") , fn ) )
	
	-- Ensure that the decimal separator in the html file is the dot 
	local sl = os.setlocale( nil )
	os.setlocale( 'C' )

	-- start the document
	fd:write("<!DOCTYPE html>\n")
	fd:write("<html lang=\""..locale.."\">\n")
	
	-- write the document header
	write_html_head( fd, name )
	
	-- write the body of the document
	write_html_body( fd, list )
	
	-- finalise the document
	fd:write("</html>\n")

	-- close the file
	fd:close()

	-- restores the previous locale
	if sl ~= nil then
		os.setlocale( sl )
	end

end

-- GLOBAL VARIABLES  ---------------------------------------------------------
--
place_list = nil
max_long = nil
min_long = nil
max_lat = nil
min_lat = nil

-- MAIN PROGRAM ---------------------------------------------------------------
--

local lang = os.getenv("LANG")
if  lang ~= nil then
	set_locale( lang )
end

local p = select_record(rtIndividual)
local name = get_individual_name(p)

-- Info: Scanning ancestors tree
print( string.format( translate("In 0") , name ) )

do_ancestors(p, 0, 0)

if place_list == nil then
	-- Error: No valid location has been found. Unable to generate map.
	print(translate("Er 0"))
else
	write_html_output( place_list, name )
end

-- Info: Done!
print( translate("In 1") )
