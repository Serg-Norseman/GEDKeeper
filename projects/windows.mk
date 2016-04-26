# C# Compiler settings for Windows.

windowscompilerdosany := C:\Program Files (x86)\MSBuild\14.0\bin\csc.exe
windowscompilerdosx86-64 := $(windowscompilerdosany)
windowscompilerdosx86 := $(windowscompilerdosany)
windowsrcdosany := C:\Program Files (x86)\Microsoft SDKs\Windows\v10.0A\bin\NETFX 4.6 Tools\resgen.exe
windowsrcdosx86-64 := $(windowsrcdosany)
windowsrcdosx86 := $(windowsrcdosany)

refsdos := C:\\Program Files (x86)\\Reference Assemblies\\Microsoft\\Framework\\.NETFramework\\v4.5.2\\

coreresgenref := \
//r:"$(refsdos)mscorlib.dll" \
//r:"$(refsdos)System.Core.dll" \
//r:"$(refsdos)System.Data.dll" \
//r:"$(refsdos)System.dll" \
//r:"$(refsdos)System.Drawing.dll" \
//r:"$(refsdos)System.Management.dll" \
//r:"$(refsdos)System.Runtime.Remoting.dll" \
//r:"$(refsdos)System.Windows.Forms.dll" \
//r:"$(refsdos)System.Xml.dll"
