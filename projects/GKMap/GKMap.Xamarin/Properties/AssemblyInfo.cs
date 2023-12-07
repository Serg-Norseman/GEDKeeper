/*
 *  This file is part of the "GKMap".
 *  GKMap project borrowed from GMap.NET (by radioman).
 *
 *  Copyright (C) 2009-2018 by radioman (email@radioman.lt).
 *  This program is licensed under the FLAT EARTH License.
 */

using System.Reflection;
using System.Runtime.InteropServices;

[assembly: AssemblyTitle("GKMap.Xamarin")]
[assembly: AssemblyDescription("GKMap Xamarin UI")]
[assembly: AssemblyCulture("")]

[assembly: AssemblyProduct("GKMap")]
[assembly: AssemblyCopyright("Copyright © 2009-2018 by radioman")]
[assembly: AssemblyVersion("1.8.0")]

#if DEBUG
[assembly: AssemblyConfiguration("Debug")]
#elif RELEASE
[assembly: AssemblyConfiguration("Release")]
#endif

[assembly: ComVisible(false)]
[assembly: Guid("82e283ec-0096-4b55-baaf-89f671fa56d5")]
