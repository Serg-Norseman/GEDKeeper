﻿/*
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

using BSLib;

namespace GKCore.Design
{
    /// <summary>
    /// 
    /// </summary>
    public abstract class FormController<TView> : BaseObject where TView : IView
    {
        protected IBaseWindow fBase;
        protected readonly TView fView;


        public IBaseWindow Base
        {
            get { return fBase; }
        }


        protected FormController(TView view)
        {
            fView = view;
            SetLocale();
        }

        public virtual void Init(IBaseWindow baseWin)
        {
            fBase = baseWin;
        }

        public virtual void Done()
        {
        }

        protected T GetControl<T>(string controlName) where T : class, IControl
        {
            return fView.GetCoreControl<T>(controlName);
        }

        public virtual void UpdateView()
        {
            // dummy
        }

        public virtual void SetLocale()
        {
            // dummy
        }

        public virtual void ApplyTheme()
        {
            // dummy
        }

        protected virtual void SetToolTip(string componentName, string toolTip)
        {
            var ctl = fView.GetControl(componentName);
            fView.SetToolTip(ctl, toolTip);
        }
    }
}
