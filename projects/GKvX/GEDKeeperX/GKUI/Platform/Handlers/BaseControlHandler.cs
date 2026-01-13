/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using GKCore.Design;
using Xamarin.Forms;

namespace GKUI.Platform
{
    public abstract class BaseControlHandler<T, TThis> : ControlHandler<T, TThis>, IBaseControl
        where T : VisualElement
        where TThis : ControlHandler<T, TThis>
    {
        protected BaseControlHandler(T control) : base(control)
        {
        }

        public bool Enabled
        {
            get { return Control.IsEnabled; }
            set { Control.IsEnabled = value; }
        }

        public bool Visible
        {
            get { return Control.IsVisible; }
            set { Control.IsVisible = value; }
        }

        public void Activate()
        {
            Control.Focus();
        }
    }
}
