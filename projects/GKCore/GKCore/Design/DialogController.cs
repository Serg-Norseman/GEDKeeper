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
using System.Threading.Tasks;
using GDModel;
using GKCore.Interfaces;
using GKCore.Operations;
using GKCore.Options;

namespace GKCore.Design
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

        public virtual async Task<bool> Cancel()
        {
            if (await CheckChangesPersistence()) {
                return false;
            }

            try {
                RollbackChanges();
                return true;
            } catch (Exception ex) {
                Logger.WriteError("DialogController.Cancel()", ex);
                return false;
            }
        }

        protected void CommitChanges()
        {
            if (fLocalUndoman != null)
                fLocalUndoman.Commit();
        }

        protected void RollbackChanges()
        {
            if (fLocalUndoman != null)
                fLocalUndoman.Rollback();
        }

        /// <summary>
        /// Check the persistence of changes, the need to save or cancel them.
        /// </summary>
        /// <returns>if `true`, discard dialog closing events</returns>
        public async Task<bool> CheckChangesPersistence()
        {
            bool result;
            if (GlobalOptions.Instance.DialogClosingWarn && fLocalUndoman != null && fLocalUndoman.HasChanges()) {
                result = (await AppHost.StdDialogs.ShowQuestion(LangMan.LS(LSID.WarningOfDialogUnsavedChanges)));
            } else {
                result = false;
            }

            if (!result) {
                Done();
            }

            return result;
        }

        public override void Init(IBaseWindow baseWin)
        {
            base.Init(baseWin);
            if (fBase != null) {
                fLocalUndoman = new ChangeTracker(fBase.Context);
            }
        }

        public void JumpToRecord(IGDMPointerHost pointer)
        {
            if (pointer != null && Accept()) {
                fBase.SelectRecordByXRef(pointer.XRef, true);
                fView.Close();
            }
        }
    }
}
