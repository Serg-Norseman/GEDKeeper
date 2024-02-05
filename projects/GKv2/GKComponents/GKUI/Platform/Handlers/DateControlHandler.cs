/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2024 by Sergey V. Zhdanovskih.
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
using GDModel;
using GKCore.Design.Controls;
using GKUI.Components;

namespace GKUI.Platform.Handlers
{
    public sealed class DateControlHandler : BaseControlHandler<GKDateControl, DateControlHandler>, IDateControl
    {
        public GDMCustomDate Date
        {
            get { return Control.Date; }
            set { Control.Date = value; }
        }

        public GDMDateType FixedDateType
        {
            get { return Control.FixedDateType; }
            set { Control.FixedDateType = value; }
        }

        public event EventHandler DateChanged
        {
            add { Control.DateChanged += value; }
            remove { Control.DateChanged -= value; }
        }

        public DateControlHandler(GKDateControl control) : base(control)
        {
        }

        public void PasteValue(string value)
        {
            Control.PasteValue(value);
        }
    }
}
