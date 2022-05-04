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

using GDModel;
using GKCore.MVP;
using GKCore.MVP.Views;

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
            fView.HyperView.Lines.Assign(fBase.GetRecordContent(fRecord));
        }

        public void SelectLink(string linkName)
        {
            if (linkName.StartsWith("http")) {
                GKUtils.LoadExtFile(linkName);
                return;
            }

            if (linkName.StartsWith("view_")) {
                string xref = linkName.Remove(0, 5);
                GDMMultimediaRecord mmRec = fBase.Context.Tree.XRefIndex_Find(xref) as GDMMultimediaRecord;
                if (mmRec != null) {
                    fBase.ShowMedia(mmRec, false);
                }
            } else {
                GDMRecord record = Base.Context.Tree.XRefIndex_Find(linkName);
                fView.HyperView.Lines.Assign(fBase.GetRecordContent(record));
            }
        }

        public override void SetLocale()
        {
            fView.Title = LangMan.LS(LSID.LSID_MIFileProperties);
        }
    }
}
