/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using GDModel;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKCore.Lists;
using GKCore.Locales;
using GKCore.Options;

namespace GKCore.Controllers
{
    public class StdPersonEditDlgController : PersonEditDlgController<IStdPersonEditDlg>
    {
        public StdPersonEditDlgController(IStdPersonEditDlg view) : base(view)
        {
            fView.ChildrenList.OnModify += ModifyChildrenSheet;
            fView.ChildrenList.OnItemValidating += PersonEditDlg_ItemValidating;
        }

        private void PersonEditDlg_ItemValidating(object sender, ItemValidatingEventArgs e)
        {
            var record = e.Item as GDMRecord;
            e.IsAvailable = record == null || fBase.Context.IsAvailableRecord(record);
        }

        private void ModifyChildrenSheet(object sender, ModifyEventArgs eArgs)
        {
            if (eArgs.Action == RecordAction.raJump) {
                JumpToRecord(eArgs.ItemData as GDMChildLink);
            }
        }

        public override void Init(IBaseWindow baseWin)
        {
            base.Init(baseWin);

            fView.ChildrenList.ListModel = new IndividualChildrenListModel(fView, baseWin, fLocalUndoman);
            fView.DNATestsList.ListModel = new DNATestsListModel(fView, baseWin, fLocalUndoman);
        }

        public override void Done()
        {
            base.Done();

            fView.ChildrenList.ListModel.SaveSettings();
            fView.DNATestsList.ListModel.SaveSettings();
        }

        protected override void UpdateListModels(GDMIndividualRecord indiRec)
        {
            base.UpdateListModels(indiRec);

            fView.ChildrenList.ListModel.DataOwner = indiRec;
            fView.DNATestsList.ListModel.DataOwner = indiRec;
        }

        protected override void UpdateLocked(bool locked)
        {
            base.UpdateLocked(locked);

            fView.ChildrenList.ReadOnly = locked;
            fView.DNATestsList.ReadOnly = locked;
        }

        public override void UpdateNameControls(GDMPersonalName np)
        {
            base.UpdateNameControls(np);
        }

        protected override void AcceptNameParts(GDMPersonalName persName)
        {
            base.AcceptNameParts(persName);
        }

        public override void SetLocale()
        {
            base.SetLocale();

            GetControl<ITabPage>("pageChilds").Text = LangMan.LS(LSID.Childs);
            GetControl<ITabPage>("pageDNATests").Text = LangMan.LS(LSID.DNATests);
        }

        public override void ApplyTheme()
        {
            base.ApplyTheme();
        }

        public override void UpdateView()
        {
            base.UpdateView();

            try {
                bool disNoStd = GlobalOptions.Instance.DisableNonStdFeatures;
                var tabs = GetControl<ITabControl>("tabsOther");
                var tabDNA = GetControl<ITabPage>("pageDNATests");
                tabs.SetTabVisible(tabDNA, !disNoStd);
            } catch (Exception ex) {
                Logger.WriteError("StdPersonEditDlgController.UpdateView()", ex);
            }
        }
    }
}
