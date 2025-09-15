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
using GKCore.Design;
using GKCore.Locales;

namespace GKCore.Plugins
{
    [Flags]
    public enum WidgetLocation : uint
    {
        None = 0,

        HLeft = 1,
        HCenter = 2,
        HRight = 4,

        VTop = 8,
        VCenter = 16,
        VBottom = 32
    }


    public interface IWidgetForm : ILocalizable
    {
    }


    public interface IWidgetPlugin
    {
        /// <summary>
        /// Pointer to the main form of application.
        /// </summary>
        IHost Host { get; }

        void BaseChanged(IBaseWindow baseWin);
        void BaseClosed(IBaseWindow baseWin);
        void BaseRenamed(IBaseWindow baseWin, string oldName, string newName);
        void BaseSaved(IBaseWindow baseWin, string fileName);

        void SelectedIndexChanged(IBaseWindow baseWin);
        void TabChanged(IBaseWindow baseWin);

        void WidgetInit(IHost host);
        void WidgetEnable();
    }
}
