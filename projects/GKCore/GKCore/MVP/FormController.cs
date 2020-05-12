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

using BSLib.Design.MVP;
using GKCore.Interfaces;

namespace GKCore.MVP
{
    /// <summary>
    /// 
    /// </summary>
    public abstract class FormController<TView> : Controller<TView> where TView : IView
    {
        protected IBaseWindow fBase;

        // TODO: remove IBaseWindow to IBaseContext, everywhere!
        public IBaseWindow Base
        {
            get { return fBase; }
        }


        protected FormController(TView view) : base(view)
        {
        }

        public virtual void Init(IBaseWindow baseWin)
        {
            fBase = baseWin;
        }
    }
}
