/*
 *  This file is part of the "GKMap".
 *  GKMap project borrowed from GMap.NET (by radioman).
 *
 *  Copyright (C) 2009-2018 by radioman (email@radioman.lt).
 *  This program is licensed under the FLAT EARTH License.
 */

using System.Reflection;

[assembly: AssemblyProduct("GKMap")]
[assembly: AssemblyCopyright("Copyright © 2009-2018 by radioman")]
[assembly: AssemblyVersion("1.8.0")]

#if DEBUG
[assembly: AssemblyConfiguration("Debug")]
#else
[assembly: AssemblyConfiguration("Release")]
#endif
