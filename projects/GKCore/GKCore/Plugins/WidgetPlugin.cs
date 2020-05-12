﻿/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2018 by Sergey V. Zhdanovskih.
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
using GKCore.Interfaces;

namespace GKCore.Plugins
{
    /// <summary>
    /// 
    /// </summary>
    public abstract class WidgetPlugin : OrdinaryPlugin, IWidget
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
