/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Threading.Tasks;
using GDModel;
using GKCore.Locales;
using GKCore.Operations;
using GKCore.Options;
using GKCore.Validation;

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

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                if (fLocalUndoman != null) fLocalUndoman.Clear();
            }
            base.Dispose(disposing);
        }

        protected bool Validate<T>(T obj)
        {
            if (GlobalOptions.Instance.EnableStdValidation) {
                var results = ValidationFactory.Validate(obj);
                if (!results.Valid) {
                    AppHost.StdDialogs.ShowError(results.Messages[0].Message);
                    return false;
                }
            }
            return true;
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
