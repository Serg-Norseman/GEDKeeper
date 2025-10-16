/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2025 by Sergey V. Zhdanovskih.
 *
 *  This file is part of "GEDKeeper".
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

using System;
using System.Collections.Generic;
using System.IO;
using System.Reflection;
using System.Threading.Tasks;
using GDModel;
using GKCore.Design;
using GKCore.Filters;
using GKCore.Lists;
using GKCore.Locales;
using GKCore.Options;

namespace GKCore.Plugins
{
    /// <summary>
    /// 
    /// </summary>
    public class PluginsMan
    {
        private readonly List<IPlugin> fPlugins;

        public int Count
        {
            get { return fPlugins.Count; }
        }

        public IPlugin this[int index]
        {
            get { return fPlugins[index]; }
        }

        public IList<IPlugin> List
        {
            get { return fPlugins; }
        }

        public PluginsMan()
        {
            fPlugins = new List<IPlugin>();
        }

        public void Load(IHost host, Assembly asm)
        {
            if (host == null || asm == null) {
                return;
            }

            Type pluginType = typeof(IPlugin);

            Type[] types = asm.GetTypes();
            foreach (Type type in types) {
                if (type.IsInterface || type.IsAbstract) continue;
                if (type.GetInterface(pluginType.FullName) == null) continue;

                var plugin = (IPlugin)Activator.CreateInstance(type);
                plugin.Startup(host);
                fPlugins.Add(plugin);
            }
        }

        public void Load(IHost host, string path)
        {
            if (!Directory.Exists(path)) return;
            Logger.WriteInfo("Plugins load path: " + path);

            try {
#if !NETCOREAPP && !NETSTANDARD2_0
                AppDomain.CurrentDomain.SetupInformation.PrivateBinPath = path;
#else
#endif

                string[] pluginFiles = Directory.GetFiles(path, "*.dll");
                foreach (string pfn in pluginFiles) {
                    try {
#if !NETCOREAPP && !NETSTANDARD2_0
                        AssemblyName assemblyName = AssemblyName.GetAssemblyName(pfn);
                        Assembly asm = Assembly.Load(assemblyName);
#else
                        Assembly asm = Assembly.LoadFrom(pfn);
#endif

                        if (asm != null) {
                            Load(host, asm);
                        }
                    } catch {
                        // block exceptions for bad or non-dotnet assemblies
                    }
                }
            } catch (Exception ex) {
                Logger.WriteError("PluginsMan.Load(" + path + ")", ex);
            }
        }

        public void Unload()
        {
            try {
                for (int i = 0, count = fPlugins.Count; i < count; i++) {
                    var plugin = fPlugins[i];
                    plugin.Shutdown();
                }
            } catch (Exception ex) {
                Logger.WriteError("PluginsMan.Unload()", ex);
            }
        }

        public void OnLanguageChange()
        {
            try {
                for (int i = 0, count = fPlugins.Count; i < count; i++) {
                    var plugin = fPlugins[i];
                    plugin.OnLanguageChange();
                }
            } catch (Exception ex) {
                Logger.WriteError("PluginsMan.OnLanguageChange()", ex);
            }
        }

        public async void NotifyRecord(IBaseWindow baseWin, object record, RecordAction action)
        {
            if (baseWin == null || record == null) return;

            await Task.Run(() => {
                for (int i = 0, count = fPlugins.Count; i < count; i++) {
                    try {
                        if (fPlugins[i] is ISubscriber subscriber) {
                            subscriber.NotifyRecord(baseWin, record, action);
                        }
                    } catch (Exception ex) {
                        Logger.WriteError("PluginsMan.NotifyRecord()", ex);
                    }
                }
            });
        }

        public void NotifyFilter(IBaseWindow baseWin, GDMRecordType recType, IListSource listSource, ListFilter filter)
        {
            if (baseWin == null || filter == null) return;

            for (int i = 0, count = fPlugins.Count; i < count; i++) {
                try {
                    ISubscriber subscriber = (fPlugins[i] as ISubscriber);
                    if (subscriber != null) {
                        subscriber.NotifyFilter(baseWin, recType, listSource, filter);
                    }
                } catch (Exception ex) {
                    Logger.WriteError("PluginsMan.NotifyFilter()", ex);
                }
            }
        }

        public ILangMan CreateLangMan(object sender)
        {
            if (sender == null)
                return null;

            return CreateLangMan(sender.GetType().Assembly);
        }

        public static ILangMan CreateLangMan(Assembly asm)
        {
            if (asm == null)
                return null;

            string lngSign = GlobalOptions.Instance.GetLanguageSign();

            Module[] mods = asm.GetModules();
            string asmFile = mods[0].FullyQualifiedName;

            string langFile = Path.ChangeExtension(asmFile, "." + lngSign);
            if (!File.Exists(langFile)) {
                langFile = Path.ChangeExtension(asmFile, "." + LangMan.LS_DEF_SIGN);
            }

            LangManager langMan = new LangManager();
            bool res = langMan.LoadFromFile(langFile, null);
            return (res) ? langMan : null;
        }


        /// <summary>
        /// Core 3.0, Core 3.1, 5, 6, 7, 8.
        ///
        /// PluginLoadContext loadContext = new PluginLoadContext(pluginLocation);
        /// return loadContext.LoadFromAssemblyName(new AssemblyName(Path.GetFileNameWithoutExtension(pluginLocation)));
        /// </summary>
        /*private class PluginLoadContext : AssemblyLoadContext
        {
            private AssemblyDependencyResolver _resolver;

            public PluginLoadContext(string pluginPath)
            {
                _resolver = new AssemblyDependencyResolver(pluginPath);
            }

            protected override Assembly Load(AssemblyName assemblyName)
            {
                string assemblyPath = _resolver.ResolveAssemblyToPath(assemblyName);
                if (assemblyPath != null) {
                    return LoadFromAssemblyPath(assemblyPath);
                }
                return null;
            }

            protected override IntPtr LoadUnmanagedDll(string unmanagedDllName)
            {
                string libraryPath = _resolver.ResolveUnmanagedDllToPath(unmanagedDllName);
                if (libraryPath != null) {
                    return LoadUnmanagedDllFromPath(libraryPath);
                }
                return IntPtr.Zero;
            }
        }*/
    }
}
