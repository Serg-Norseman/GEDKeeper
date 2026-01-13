/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
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
