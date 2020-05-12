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
using GKCore.Operations;

namespace GKCore.MVP
{
    /// <summary>
    /// 
    /// </summary>
    public abstract class DialogController<TView> : FormController<TView> where TView : IView
    {
        protected ChangeTracker fLocalUndoman;

        public ChangeTracker LocalUndoman
        {
            get { return fLocalUndoman; }
        }

        protected DialogController(TView view) : base(view)
        {
        }

        public virtual bool Accept()
        {
            return true;
        }

        public virtual void Cancel()
        {
            RollbackChanges();
        }

        protected void CommitChanges()
        {
            fLocalUndoman.Commit();
        }

        protected void RollbackChanges()
        {
            fLocalUndoman.Rollback();
        }

        public override void Init(IBaseWindow baseWin)
        {
            base.Init(baseWin);
            if (fBase != null) {
                fLocalUndoman = new ChangeTracker(fBase.Context.Tree);
            }
        }
    }
}
