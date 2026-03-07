/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
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
