/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.IO;
using System.Reflection;
using BSLib;
using GKCore.Design.Graphics;
using GKCore.Locales;

namespace GKCore.Plugins
{
    /// <summary>
    /// 
    /// </summary>
    public abstract class OrdinaryPlugin : BaseObject, IPlugin
    {
        private IHost fHost;

        public IHost Host { get { return fHost; } }

        public abstract string DisplayName { get; }
        public abstract ILangMan LangMan { get; }
        public abstract IImage Icon { get; }
        public abstract PluginCategory Category { get; }

        public abstract void Execute();

        public virtual void OnHostClosing(HostClosingEventArgs eventArgs)
        {
            // dummy
        }

        public virtual void OnHostActivate()
        {
            // dummy
        }

        public virtual void OnHostDeactivate()
        {
            // dummy
        }

        public virtual void OnLanguageChange()
        {
            // dummy
        }

        public virtual bool Startup(IHost host)
        {
            fHost = host;
            return true;
        }

        public virtual bool Shutdown()
        {
            return true;
        }

        public Stream LoadResourceStream(string resName)
        {
            Type thisType = this.GetType();
            Assembly assembly = thisType.Assembly;
            string thisNamespace = thisType.Namespace;

            resName = string.Concat("Resources.", resName);
            if (!string.IsNullOrEmpty(thisNamespace)) {
                resName = string.Concat(thisNamespace, ".", resName);
            }

            return assembly.GetManifestResourceStream(resName);
        }

        public virtual void LoadOptions(IniFile ini)
        {
        }

        public virtual void SaveOptions(IniFile ini)
        {
        }
    }
}
