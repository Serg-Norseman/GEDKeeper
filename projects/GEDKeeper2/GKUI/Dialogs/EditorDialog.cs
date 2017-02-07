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
using GKCore.Interfaces;
using GKCore.Operations;

namespace GKUI.Dialogs
{
    /// <summary>
    /// 
    /// </summary>
    public class EditorDialog : Form, IBaseEditor
    {
        protected readonly IBaseWindow fBase;
        protected readonly ChangeTracker fLocalUndoman;

        public IBaseWindow Base
        {
            get { return this.fBase; }
        }

        /// <summary>
        /// For the working state of the form designer, 
        /// it is essential that existed a simple constructor.
        /// </summary>
        public EditorDialog()
        {
        }

        public EditorDialog(IBaseWindow baseWin)
        {
            this.fBase = baseWin;
            this.fLocalUndoman = new ChangeTracker(this.fBase.Tree);
        }

        protected void CommitChanges()
        {
            this.fLocalUndoman.Commit();
        }

        protected void RollbackChanges()
        {
            this.fLocalUndoman.Rollback();
        }
    }
}
