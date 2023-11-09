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

using System;
using System.Threading.Tasks;
using BSLib;
using GDModel;
using GKCore.Design;
using GKCore.Design.Views;
using GKCore.Interfaces;
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

        public AssociationsListModel(IView owner, IBaseWindow baseWin, ChangeTracker undoman) : base(owner, baseWin, undoman)
        {
            AllowedActions = EnumSet<RecordAction>.Create(
                RecordAction.raAdd, RecordAction.raEdit, RecordAction.raDelete,
                RecordAction.raJump,
                RecordAction.raCopy, RecordAction.raPaste);

            fListColumns.AddColumn(LSID.Relation, 300, false);
            fListColumns.AddColumn(LSID.Person, 200, false);
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
                    result = ((fRelIndi == null) ? string.Empty : GKUtils.GetNameString(fRelIndi, false));
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

        public override async Task Modify(object sender, ModifyEventArgs eArgs)
        {
            var person = fDataOwner as GDMIndividualRecord;
            if (fBaseWin == null || person == null) return;

            bool result = false;

            GDMAssociation ast = eArgs.ItemData as GDMAssociation;

            switch (eArgs.Action) {
                case RecordAction.raAdd:
                case RecordAction.raEdit: {
                        bool exists = (ast != null);
                        if (!exists) {
                            ast = new GDMAssociation();
                        }

                        using (var dlg = AppHost.ResolveDialog<IAssociationEditDlg>(fBaseWin)) {
                            dlg.Association = ast;
                            result = await AppHost.Instance.ShowModalAsync(dlg, fOwner, false);
                        }

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
                    if (AppHost.StdDialogs.ShowQuestion(LangMan.LS(LSID.RemoveAssociationQuery))) {
                        result = fUndoman.DoOrdinaryOperation(OperationType.otIndividualAssociationRemove, person, ast);
                    }
                    break;

                case RecordAction.raCopy:
                    AppHost.Instance.SetClipboardObj<GDMAssociation>(ast);
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
