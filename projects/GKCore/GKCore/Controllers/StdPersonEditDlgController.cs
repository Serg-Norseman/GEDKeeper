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
        #region Design components
#pragma warning disable CS0649, IDE0044
        private ITabPage _pageChilds;
        private ITabPage _pageDNATests;
        private ISheetList _fChildrenList;
        private ISheetList _fDNATestsList;
#pragma warning restore CS0649, IDE0044
        #endregion

        public StdPersonEditDlgController(IStdPersonEditDlg view) : base(view)
        {
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

        public override void InitView(IBaseWindow baseWin)
        {
            base.InitView(baseWin);

            _fChildrenList.OnModify += ModifyChildrenSheet;
            _fChildrenList.OnItemValidating += PersonEditDlg_ItemValidating;

            _fChildrenList.ListModel = new IndividualChildrenListModel(fView, baseWin, fLocalUndoman);
            _fDNATestsList.ListModel = new DNATestsListModel(fView, baseWin, fLocalUndoman);
        }

        public override void DoneView()
        {
            base.DoneView();

            _fChildrenList.ListModel.SaveSettings();
            _fDNATestsList.ListModel.SaveSettings();

            _fChildrenList.OnModify -= ModifyChildrenSheet;
            _fChildrenList.OnItemValidating -= PersonEditDlg_ItemValidating;
        }

        protected override void UpdateListModels(GDMIndividualRecord indiRec)
        {
            base.UpdateListModels(indiRec);

            _fChildrenList.ListModel.DataOwner = indiRec;
            _fDNATestsList.ListModel.DataOwner = indiRec;
        }

        protected override void UpdateLocked(bool locked)
        {
            base.UpdateLocked(locked);

            _fChildrenList.ReadOnly = locked;
            _fDNATestsList.ReadOnly = locked;
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

            _pageChilds.Text = LangMan.LS(LSID.Childs);
            _pageDNATests.Text = LangMan.LS(LSID.DNATests);
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
                tabs.SetTabVisible(_pageDNATests, !disNoStd);
            } catch (Exception ex) {
                Logger.WriteError("StdPersonEditDlgController.UpdateView()", ex);
            }
        }
    }
}
