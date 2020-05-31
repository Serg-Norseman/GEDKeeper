/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2017 by Sergey V. Zhdanovskih.
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

using GKCore.Interfaces;
using GKCore.Options;
using GKCore.Types;

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

                IPlugin plugin = (IPlugin)Activator.CreateInstance(type);
                plugin.Startup(host);
                fPlugins.Add(plugin);
            }
        }

        public void Load(IHost host, string path)
        {
            if (!Directory.Exists(path)) return;
            Logger.LogWrite("Plugins load path: " + path);

            try {
                #if !NETSTANDARD
                AppDomain.CurrentDomain.SetupInformation.PrivateBinPath = path;
                #else
                #endif

                string[] pluginFiles = Directory.GetFiles(path, "*.dll");
                foreach (string pfn in pluginFiles) {
                    try {
                        AssemblyName assemblyName = AssemblyName.GetAssemblyName(pfn);
                        Assembly asm = Assembly.Load(assemblyName);

                        if (asm != null) {
                            Load(host, asm);
                        }
                    } catch {
                        // block exceptions for bad or non-dotnet assemblies
                    }
                }
            } catch (Exception ex) {
                Logger.LogException(ex);
            }
        }

        public void Unload()
        {
            try {
                foreach (IPlugin plugin in fPlugins) {
                    plugin.Shutdown();
                }
            } catch (Exception ex) {
                Logger.LogException(ex);
            }
        }

        public void OnLanguageChange()
        {
            try {
                foreach (IPlugin plugin in fPlugins) {
                    plugin.OnLanguageChange();
                }
            } catch (Exception ex) {
                Logger.LogException(ex);
            }
        }

        public void NotifyRecord(IBaseWindow baseWin, object record, RecordAction action)
        {
            if (baseWin == null || record == null) return;

            foreach (IPlugin plugin in fPlugins) {
                ISubscriber subscriber = (plugin as ISubscriber);
                if (subscriber == null) continue;

                try {
                    subscriber.NotifyRecord(baseWin, record, action);
                } catch (Exception ex) {
                    Logger.LogException(ex);
                }
            }
        }

        public ILangMan CreateLangMan(object sender)
        {
            if (sender == null)
                return null;

            string lngSign = GlobalOptions.Instance.GetLanguageSign();

            Assembly asm = sender.GetType().Assembly;
            Module[] mods = asm.GetModules();
            string asmFile = mods[0].FullyQualifiedName;

            string langFile = Path.ChangeExtension(asmFile, "." + lngSign);
            if (!File.Exists(langFile)) {
                langFile = Path.ChangeExtension(asmFile, "." + LangMan.LS_DEF_SIGN);
            }

            LangManager langMan = new LangManager();
            bool res = langMan.LoadFromFile(langFile);
            return (res) ? langMan : null;
        }
    }
}
