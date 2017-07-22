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

using System;
using GKCommon;
using GKCommon.GEDCOM;
using GKCore.Interfaces;
using GKCore.Operations;
using GKCore.Types;
using GKCore.UIContracts;

namespace GKCore.Lists
{
    /// <summary>
    /// 
    /// </summary>
    public class AssociationsListModel : ListModel
    {
        public AssociationsListModel(IBaseWindow baseWin, ChangeTracker undoman) : base(baseWin, undoman)
        {
            AllowedActions = EnumSet<RecordAction>.Create(
                RecordAction.raAdd, RecordAction.raEdit, RecordAction.raDelete,
                RecordAction.raJump);

            fListColumns.AddColumn(LSID.LSID_Relation, 300, false);
            fListColumns.AddColumn(LSID.LSID_Person, 200, false);
            fListColumns.ResetDefaults();
        }

        public override void UpdateContents()
        {
            var person = fDataOwner as GEDCOMIndividualRecord;
            if (fSheetList == null || person == null) return;

            try
            {
                fSheetList.ClearItems();

                foreach (GEDCOMAssociation ast in person.Associations) {
                    string nm = ((ast.Individual == null) ? "" : GKUtils.GetNameString(ast.Individual, true, false));

                    fSheetList.AddItem(ast, new object[] { ast.Relation, nm });
                }
            }
            catch (Exception ex)
            {
                Logger.LogWrite("AssociationsListModel.UpdateContents(): " + ex.Message);
            }
        }

        public override void Modify(object sender, ModifyEventArgs eArgs)
        {
            var person = fDataOwner as GEDCOMIndividualRecord;
            if (fBaseWin == null || fSheetList == null || person == null) return;

            bool result = false;

            GEDCOMAssociation ast = eArgs.ItemData as GEDCOMAssociation;

            switch (eArgs.Action)
            {
                case RecordAction.raAdd:
                case RecordAction.raEdit:
                    using (var dlg = AppHost.Container.Resolve<IAssociationEditDlg>())
                    {
                        dlg.InitDialog(fBaseWin);

                        bool exists = (ast != null);
                        if (!exists) {
                            ast = new GEDCOMAssociation(fBaseWin.Context.Tree, person, "", "");
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
                    if (AppHost.StdDialogs.ShowQuestionYN(LangMan.LS(LSID.LSID_RemoveAssociationQuery)))
                    {
                        result = fUndoman.DoOrdinaryOperation(OperationType.otIndividualAssociationRemove, person, ast);
                    }
                    break;
            }

            if (result) {
                fBaseWin.Context.Modified = true;
                eArgs.IsChanged = true;
            }
        }
    }
}
