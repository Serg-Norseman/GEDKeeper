/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2023 by Sergey V. Zhdanovskih.
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

namespace GKCore.Design
{
    /// <summary>
    /// 
    /// </summary>
    public abstract class EditorController<TModel, TView> : DialogController<TView>, IController<TModel, TView>
        where TModel : class
        where TView : IView
    {
        protected TModel fModel;

        public TModel Model
        {
            get { return fModel; }
            set {
                if (fModel != value) {
                    SetModel(value);
                }
            }
        }

        protected EditorController(TView view) : base(view)
        {
        }

        protected virtual void SetModel(TModel value)
        {
            fModel = value;
            UpdateView();
        }
    }
}
