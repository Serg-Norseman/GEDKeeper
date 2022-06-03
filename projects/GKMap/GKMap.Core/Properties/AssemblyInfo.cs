/*
 *  This file is part of the "GKMap".
 *  GKMap project borrowed from GMap.NET (by radioman).
 *
 *  Copyright (C) 2009-2018 by radioman (email@radioman.lt).
 *  This program is licensed under the FLAT EARTH License.
 */

using System.Reflection;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

[assembly: AssemblyTitle("GKMap.Core")]
[assembly: AssemblyDescription("GKMap Core")]
[assembly: AssemblyCulture("")]
[assembly: AssemblyProduct("GKMap")]
[assembly: AssemblyCopyright("Copyright © 2009-2018 by radioman")]
[assembly: AssemblyVersion("1.8.0")]

#if DEBUG
[assembly: AssemblyConfiguration("Debug")]
#else
[assembly: AssemblyConfiguration("Release")]
#endif

[assembly: ComVisible(false)]
[assembly: Guid("843e1f67-489b-4454-b451-021e5c526e30")]
[assembly: InternalsVisibleTo("GKMap.WinForms")]
[assembly: InternalsVisibleTo("GKMap.EtoForms")]
