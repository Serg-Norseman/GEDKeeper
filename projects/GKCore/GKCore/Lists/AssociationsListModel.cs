/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System.Threading.Tasks;
using BSLib;
using GDModel;
using GKCore.Design;
using GKCore.Design.Views;
using GKCore.Locales;
using GKCore.Operations;
using GKCore.Options;

namespace GKCore.Lists
{
    /// <summary>
    /// 
    /// </summary>
    public class AssociationsListModel : SheetModel<GDMAssociation>
    {
        private GDMIndividualRecord fRelIndi;

        public AssociationsListModel(IView owner, IBaseWindow baseWin, ChangeTracker undoman) : base(owner, baseWin, undoman, CreateListColumns())
        {
            AllowedActions = EnumSet<RecordAction>.Create(
                RecordAction.raAdd, RecordAction.raEdit, RecordAction.raDelete,
                RecordAction.raJump,
                RecordAction.raCopy, RecordAction.raPaste, RecordAction.raDetails);
        }

        public static ListColumns CreateListColumns()
        {
            var result = new ListColumns(GKListType.stIndividualAssociations);
            result.AddColumn(LSID.Relation, 300, false);
            result.AddColumn(LSID.Person, 200, false);
            return result;
        }

        protected override GDMRecord GetReferenceRecord(object itemData)
        {
            var ast = itemData as GDMAssociation;
            return (ast == null) ? null : fBaseContext.Tree.GetPtrValue<GDMIndividualRecord>(ast);
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
            if (person != null)
                UpdateStructList(person.Associations);
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
                    if (await AppHost.StdDialogs.ShowQuestion(LangMan.LS(LSID.RemoveAssociationQuery))) {
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
