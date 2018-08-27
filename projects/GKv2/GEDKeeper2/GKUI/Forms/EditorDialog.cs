/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2017 by Sergey V. Zhdanovskih.
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

using System.Windows.Forms;
using GKCore.Controllers;
using GKCore.Interfaces;
using GKCore.Operations;

namespace GKUI.Forms
{
    /// <summary>
    /// 
    /// </summary>
    public class EditorDialog : Form, ICommonDialog, IBaseEditor
    {
        protected IBaseWindow fBase;
        protected readonly ControlsManager fControlsManager;
        protected ChangeTracker fLocalUndoman;

        public IBaseWindow Base
        {
            get { return fBase; }
        }

        /// <summary>
        /// For the working state of the form designer,
        /// it is essential that existed a simple constructor.
        /// </summary>
        public EditorDialog()
        {
            fControlsManager = new ControlsManager();
        }

        protected void CommitChanges()
        {
            fLocalUndoman.Commit();
        }

        protected void RollbackChanges()
        {
            fLocalUndoman.Rollback();
        }

        public virtual void UpdateView()
        {
        }

        public virtual void InitDialog(IBaseWindow baseWin)
        {
            fBase = baseWin;
            if (fBase != null) {
                fLocalUndoman = new ChangeTracker(fBase.Context.Tree);
            }
        }

        public virtual bool ShowModalX(object owner)
        {
            return (ShowDialog() == DialogResult.OK);
        }
    }
}
