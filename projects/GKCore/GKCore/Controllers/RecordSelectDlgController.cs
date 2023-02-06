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

using GDModel;
using GKCore.Design.MVP.Controls;
using GKCore.Interfaces;
using GKCore.Lists;
using GKCore.MVP;
using GKCore.MVP.Views;
using GKCore.Types;

namespace GKCore.Controllers
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class RecordSelectDlgController : DialogController<IRecordSelectDialog>
    {
        private GDMRecordType fRecType;
        private readonly Target fTarget;


        public GDMRecordType RecType
        {
            get { return fRecType; }
            set { fRecType = value; }
        }

        public Target Target
        {
            get { return fTarget; }
        }


        public RecordSelectDlgController(IRecordSelectDialog view) : base(view)
        {
            fTarget = new Target();
        }

        public void SetTarget(TargetMode mode, GDMIndividualRecord target, GDMSex needSex)
        {
            fTarget.TargetMode = mode;
            fTarget.TargetIndividual = target;
            fTarget.NeedSex = needSex;
            UpdateView();
        }

        private static bool ChildSelectorHandler(GDMRecord record)
        {
            GDMIndividualRecord iRec = record as GDMIndividualRecord;
            return (iRec != null && iRec.ChildToFamilyLinks.Count == 0);
        }

        private bool SpouseSelectorHandler(GDMRecord record)
        {
            GDMFamilyRecord famRec = record as GDMFamilyRecord;
            return (famRec != null && famRec.HasSpouse(fTarget.TargetIndividual));
        }

        public override void UpdateView()
        {
            string flt = fView.FilterBox.Text;
            if (string.IsNullOrEmpty(flt)) {
                flt = "*";
            } else if (flt != "*") {
                flt = "*" + flt + "*";
            }

            IListViewEx recordsList = fView.RecordsList;
            recordsList.ListMan.Filter.Clear();
            recordsList.ListMan.QuickFilter = flt;

            switch (fRecType) {
                case GDMRecordType.rtIndividual: {
                        IndividualListFilter iFilter = (IndividualListFilter)recordsList.ListMan.Filter;
                        iFilter.Sex = fTarget.NeedSex;
                        if (fTarget.TargetMode == TargetMode.tmParent) {
                            recordsList.ListMan.ExternalFilter = ChildSelectorHandler;
                        }
                        break;
                    }

                case GDMRecordType.rtFamily:
                    if (fTarget.TargetMode == TargetMode.tmFamilySpouse) {
                        recordsList.ListMan.ExternalFilter = SpouseSelectorHandler;
                    }
                    break;
            }

            recordsList.UpdateContents();
        }

        public override void SetLocale()
        {
            fView.Title = LangMan.LS(LSID.LSID_WinRecordSelect);

            GetControl<IButton>("btnCreate").Text = LangMan.LS(LSID.LSID_DlgAppend);
            GetControl<IButton>("btnSelect").Text = LangMan.LS(LSID.LSID_DlgSelect);
            GetControl<IButton>("btnCancel").Text = LangMan.LS(LSID.LSID_DlgCancel);
        }
    }
}
