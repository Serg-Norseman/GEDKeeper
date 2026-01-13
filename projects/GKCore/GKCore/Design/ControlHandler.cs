/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

namespace GKCore.Design
{
    public abstract class ControlHandler<T, TThis> : IControl where TThis : ControlHandler<T, TThis>
    {
        protected T fControl;

        public virtual T Control
        {
            get { return fControl; }
        }

        protected ControlHandler(T control)
        {
            fControl = control;
        }
    }
}
