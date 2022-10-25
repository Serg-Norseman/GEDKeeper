/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2022 by Sergey V. Zhdanovskih.
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
using BSLib;
using GDModel;
using GKCore.Interfaces;
using GKCore.MVP.Views;
using GKCore.Operations;
using GKCore.Types;

namespace GKCore.Lists
{
    /// <summary>
    /// 
    /// </summary>
    public class AssociationsListModel : SheetModel<GDMAssociation>
    {
        private GDMIndividualRecord fRelIndi;

        public AssociationsListModel(IBaseWindow baseWin, ChangeTracker undoman) : base(baseWin, undoman)
        {
            AllowedActions = EnumSet<RecordAction>.Create(
                RecordAction.raAdd, RecordAction.raEdit, RecordAction.raDelete,
                RecordAction.raJump,
                RecordAction.raCopy, RecordAction.raPaste);

            fListColumns.AddColumn(LSID.LSID_Relation, 300, false);
            fListColumns.AddColumn(LSID.LSID_Person, 200, false);
            fListColumns.ResetDefaults();
        }

        public override void Fetch(GDMAssociation aRec)
        {
            base.Fetch(aRec);
            fRelIndi = fBaseContext.Tree.GetPtrValue(fFetchedRec);
        }

        protected override object GetColumnValueEx(int colType, int colSubtype, bool isVisible)
        {
            object result = null;
            switch (colType) {
                case 0:
                    result = fFetchedRec.Relation;
                    break;
                case 1:
                    result = ((fRelIndi == null) ? string.Empty : GKUtils.GetNameString(fRelIndi, true, false));
                    break;
            }
            return result;
        }

        public override void UpdateContents()
        {
            var person = fDataOwner as GDMIndividualRecord;
            if (person == null) return;

            try {
                UpdateStructList(person.Associations);
            } catch (Exception ex) {
                Logger.WriteError("AssociationsListModel.UpdateContents()", ex);
            }
        }

        public override void Modify(object sender, ModifyEventArgs eArgs)
        {
            var person = fDataOwner as GDMIndividualRecord;
            if (fBaseWin == null || person == null) return;

            bool result = false;

            GDMAssociation ast = eArgs.ItemData as GDMAssociation;

            switch (eArgs.Action) {
                case RecordAction.raAdd:
                case RecordAction.raEdit:
                    using (var dlg = AppHost.ResolveDialog<IAssociationEditDlg>(fBaseWin)) {
                        bool exists = (ast != null);
                        if (!exists) {
                            ast = new GDMAssociation();
                        }

                        dlg.Association = ast;
                        result = AppHost.Instance.ShowModalX(dlg, false);

                        if (!exists) {
                            if (result) {
                                result = fUndoman.DoOrdinaryOperation(OperationType.otIndividualAssociationAdd, person, ast);
                            } else {
                                ast.Dispose();
                            }
                        }
                    }
                    break;

                case RecordAction.raDelete:
                    if (AppHost.StdDialogs.ShowQuestionYN(LangMan.LS(LSID.LSID_RemoveAssociationQuery))) {
                        result = fUndoman.DoOrdinaryOperation(OperationType.otIndividualAssociationRemove, person, ast);
                    }
                    break;

                case RecordAction.raCopy:
                    AppHost.Instance.SetClipboardObj(ast);
                    break;

                case RecordAction.raCut:
                    break;

                case RecordAction.raPaste:
                    ast = AppHost.Instance.GetClipboardObj<GDMAssociation>();
                    if (ast != null) {
                        ast = ast.Clone();
                        result = fUndoman.DoOrdinaryOperation(OperationType.otIndividualAssociationAdd, person, ast);
                    }
                    break;
            }

            if (result) {
                if (eArgs.Action == RecordAction.raAdd || eArgs.Action == RecordAction.raPaste) {
                    eArgs.ItemData = ast;
                }

                fBaseWin.Context.Modified = true;
                eArgs.IsChanged = true;
            }
        }
    }
}
