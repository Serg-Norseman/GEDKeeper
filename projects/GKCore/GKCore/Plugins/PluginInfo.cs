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
using System.Reflection;
using GKCore.Interfaces;

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
                throw new ArgumentNullException("plugin");

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
