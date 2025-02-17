/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2025 by Sergey V. Zhdanovskih.
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
using GKCore.Design;
using GKCore.Design.Views;
using GKCore.Interfaces;

namespace GKCore.Controllers
{
    /// <summary>
    /// 
    /// </summary>
    public class RecordInfoDlgController : DialogController<IRecordInfoDlg>
    {
        private GDMRecord fRecord;

        public GDMRecord Record
        {
            get { return fRecord; }
            set {
                if (fRecord != value) {
                    fRecord = value;
                    UpdateView();
                }
            }
        }


        public RecordInfoDlgController(IRecordInfoDlg view) : base(view)
        {
        }

        public override void UpdateView()
        {
            fView.HyperView.Lines.Assign(fBase.GetRecordContent(fRecord, RecordContentType.Quick));
        }

        public void SelectLink(string linkName)
        {
            if (linkName.StartsWith(GKData.INFO_HTTP_PREFIX)) {
                GKUtils.LoadExtFile(linkName);
                return;
            }

            if (linkName.StartsWith(GKData.INFO_HREF_VIEW)) {
                string xref = linkName.Remove(0, GKData.INFO_HREF_VIEW.Length);
                var mmRec = fBase.Context.Tree.FindXRef<GDMMultimediaRecord>(xref);
                if (mmRec != null) {
                    fBase.ShowMedia(mmRec, false);
                }
            } else {
                GDMRecord record = Base.Context.Tree.XRefIndex_Find(linkName);
                fView.HyperView.Lines.Assign(fBase.GetRecordContent(record, RecordContentType.Quick));
            }
        }

        public override void SetLocale()
        {
            fView.Title = LangMan.LS(LSID.MIFileProperties);
        }

        public override void ApplyTheme()
        {
            // dummy
        }
    }
}
