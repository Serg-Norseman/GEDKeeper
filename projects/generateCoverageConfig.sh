ASSEMBLIES=`find GKTests/bin/debug -type f -name GK*.dll | grep -v /obj/ | grep -v tsts.dll | perl -e '@in=grep(s/\n$//, <>); print "[\"GKTests/bin/debug/GEDKeeper2.exe\", \"".join("\", \"", @in)."\"],\n";'`
echo "{"
echo "  \"assemblies\": ${ASSEMBLIES}" 
echo "  \"typeInclude\": \".*\"" 
#echo "  ,\"typeExclude\": \"Codecov.Example.*Tests*\""
echo "}"
