/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2018 by Sergey V. Zhdanovskih.
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
using GKCommon.GEDCOM;
using GKCore.Interfaces;
using GKCore.Lists;
using GKCore.Types;
using GKCore.UIContracts;

namespace GKCore.Controllers
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class RecordSelectDlgController : DialogController<IRecordSelectDialog>
    {
        private string fFilter;
        private GEDCOMRecordType fRecType;
        private Target fTarget;


        public string Filter
        {
            get { return fFilter; }
            set {
                string flt = value;
                if (flt == "") {
                    flt = "*";
                } else if (flt != "*") {
                    flt = "*" + flt + "*";
                }
                fFilter = flt;
                UpdateFilter();
            }
        }

        public GEDCOMRecordType RecType
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

        private void UpdateFilter()
        {
            IListView recordsList = fView.RecordsList;
            recordsList.ListMan.Filter.Clear();
            recordsList.ListMan.QuickFilter = fFilter;

            if (fRecType == GEDCOMRecordType.rtIndividual) {
                IndividualListFilter iFilter = (IndividualListFilter)recordsList.ListMan.Filter;
                iFilter.Sex = fTarget.NeedSex;

                if (fTarget.TargetMode == TargetMode.tmParent) {
                    recordsList.ListMan.ExternalFilter = ChildSelectorHandler;
                }
            }

            recordsList.UpdateContents();
        }

        private static bool ChildSelectorHandler(GEDCOMRecord record)
        {
            GEDCOMIndividualRecord iRec = record as GEDCOMIndividualRecord;
            return (iRec != null && iRec.ChildToFamilyLinks.Count == 0);
        }

        public override void UpdateView()
        {
        }
    }
}
