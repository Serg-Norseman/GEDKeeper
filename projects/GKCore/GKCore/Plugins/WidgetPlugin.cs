/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using GKCore.Design;

namespace GKCore.Plugins
{
    /// <summary>
    /// 
    /// </summary>
    public abstract class WidgetPlugin : OrdinaryPlugin, IWidgetPlugin
    {
        public virtual void WidgetInit(IHost host)
        {
            // dummy
        }

        public virtual void BaseChanged(IBaseWindow baseWin)
        {
            // dummy
        }

        public virtual void BaseClosed(IBaseWindow baseWin)
        {
            // dummy
        }

        public virtual void BaseRenamed(IBaseWindow baseWin, string oldName, string newName)
        {
            // dummy
        }

        public virtual void BaseSaved(IBaseWindow baseWin, string fileName)
        {
            // dummy
        }

        public virtual void SelectedIndexChanged(IBaseWindow baseWin)
        {
            // dummy
        }

        public virtual void TabChanged(IBaseWindow baseWin)
        {
            // dummy
        }

        public virtual void WidgetEnable()
        {
            // dummy
        }
    }
}
