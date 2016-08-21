# C# Compiler settings for Windows.

windowscompilerdosany := /c/Program\ Files\ \(x86\)/MSBuild/14.0/bin/csc.exe
windowscompilerdosx86-64 := $(windowscompilerdosany)
windowscompilerdosx86 := $(windowscompilerdosany)
windowsrcdosdotnet46 := /c/Program\ Files\ \(x86\)/Microsoft\ SDKs/Windows/v10.0A/bin/NETFX\ 4.6\ Tools/ResGen.exe
windowsrcdosdotnet20 := /c/Program\ Files\ \(x86\)/Microsoft\ SDKs/Windows/v7.0A/bin/Resgen.exe

msftexcelinterop := "C:\\WINDOWS\\assembly\\GAC_MSIL\\Microsoft.Office.Interop.Excel\\15.0.0.0__71e9bce111e9429c\\Microsoft.Office.Interop.Excel.dll"
msftwordinterop := "C:\\WINDOWS\\assembly\\GAC_MSIL\\Microsoft.Office.Interop.Word\\15.0.0.0__71e9bce111e9429c\\Microsoft.Office.Interop.Word.dll"

#msftexcelinterop := "C:\\WINDOWS\\assembly\\GAC_MSIL\\Microsoft.Office.Interop.Excel\\14.0.0.0__71e9bce111e9429c\\Microsoft.Office.Interop.Excel.dll"
#msftwordinterop := "C:\\WINDOWS\\assembly\\GAC_MSIL\\Microsoft.Office.Interop.Word\\14.0.0.0__71e9bce111e9429c\\Microsoft.Office.Interop.Word.dll"

refsdosdotnet46 := C:\\Program Files (x86)\\Reference Assemblies\\Microsoft\\Framework\\.NETFramework\\v4.5.2\\
refsdosdotnet20 := C:\\Windows\\Microsoft.NET\\Framework\\v2.0.50727\\
refsdos := $(refsdos$(dotnet))
coreresgenrefdotnet46 := \
//r:"$(refsdos)mscorlib.dll" \
//r:"$(refsdos)System.Data.dll" \
//r:"$(refsdos)System.dll" \
//r:"$(refsdos)System.Drawing.dll" \
//r:"$(refsdos)System.Runtime.Remoting.dll" \
//r:"$(refsdos)System.Windows.Forms.dll" \
//r:"$(refsdos)System.Xml.dll" \
//r:"$(refsdos)System.Core.dll" \
//r:"$(refsdos)System.Management.dll"
coreresgenrefdotnet20 := \
//r:"$(refsdos)mscorlib.dll" \
//r:"$(refsdos)System.Data.dll" \
//r:"$(refsdos)System.dll" \
//r:"$(refsdos)System.Drawing.dll" \
//r:"$(refsdos)System.Runtime.Remoting.dll" \
//r:"$(refsdos)System.Windows.Forms.dll" \
//r:"$(refsdos)System.Xml.dll"

#analysisreleaseruleset := "C:\\Program Files (x86)\\Microsoft Visual Studio 14.0\\Team Tools\\Static Analysis Tools\\Rule Sets\\MinimumRecommendedRules.ruleset"
analysisreleaseruleset := ..\\gk21.ruleset
analysisdebugruleset := ..\\gk21.ruleset