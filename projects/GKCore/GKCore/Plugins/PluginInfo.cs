/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Reflection;
using GKCore.Utilities;

namespace GKCore.Plugins
{
    /// <summary>
    /// 
    /// </summary>
    public struct PluginInfo
    {
        public string Title;
        public string Description;
        public string Copyright;
        public string Version;


        public static PluginInfo GetPluginInfo(IPlugin plugin)
        {
            if (plugin == null)
                throw new ArgumentNullException(nameof(plugin));

            PluginInfo info = new PluginInfo();

            Assembly asm = plugin.GetType().Assembly;

            var attr1 = SysUtils.GetAssemblyAttribute<AssemblyTitleAttribute>(asm);
            if (attr1 != null) info.Title = attr1.Title;

            var attr2 = SysUtils.GetAssemblyAttribute<AssemblyDescriptionAttribute>(asm);
            if (attr2 != null) info.Description = attr2.Description;

            var attr3 = SysUtils.GetAssemblyAttribute<AssemblyCopyrightAttribute>(asm);
            if (attr3 != null) info.Copyright = attr3.Copyright;

            var attr4 = asm.GetName().Version;
            if (attr4 != null) info.Version = attr4.ToString();

            return info;
        }
    }
}
