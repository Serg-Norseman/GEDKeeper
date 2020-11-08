/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2020 by Sergey V. Zhdanovskih.
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
using GKCore.MVP;
using GKCore.MVP.Views;

namespace GKCore.Controllers
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class SourceCitEditDlgController : DialogController<ISourceCitEditDlg>
    {
        private GDMSourceCitation fSourceCitation;
        private readonly StringList fSourcesList;

        public GDMSourceCitation SourceCitation
        {
            get { return fSourceCitation; }
            set {
                if (fSourceCitation != value) {
                    fSourceCitation = value;
                    UpdateView();
                }
            }
        }


        public SourceCitEditDlgController(ISourceCitEditDlg view) : base(view)
        {
            fSourcesList = new StringList();

            for (int i = 0; i < GKData.CertaintyAssessments.Length; i++) {
                fView.Certainty.Add(LangMan.LS(GKData.CertaintyAssessments[i]));
            }
        }

        public override bool Accept()
        {
            try {
                int idx = fSourcesList.IndexOf(fView.Source.Text);
                GDMSourceRecord src = ((idx < 0) ? null : (fSourcesList.GetObject(idx) as GDMSourceRecord));

                if (src == null) {
                    AppHost.StdDialogs.ShowError(LangMan.LS(LSID.LSID_DoNotSetSource));

                    return false;
                } else {
                    fSourceCitation.Value = src;
                    fSourceCitation.Page = fView.Page.Text;
                    fSourceCitation.CertaintyAssessment = fView.Certainty.SelectedIndex;

                    return true;
                }
            } catch (Exception ex) {
                Logger.WriteError("SourceCitEditDlgController.Accept()", ex);
                return false;
            }
        }

        public override void UpdateView()
        {
            GDMSourceRecord src = (fSourceCitation.Value as GDMSourceRecord);
            if (src != null) fView.Source.Text = src.ShortTitle;

            fView.Page.Text = fSourceCitation.Page;
            fView.Certainty.SelectedIndex = fSourceCitation.CertaintyAssessment;
        }

        public void AddSource()
        {
            object[] anArgs = new object[0];
            GDMSourceRecord src = fBase.Context.SelectRecord(GDMRecordType.rtSource, anArgs) as GDMSourceRecord;
            if (src == null) return;

            fBase.Context.GetSourcesList(fSourcesList);
            RefreshSourcesList("");
            fView.Source.Text = src.ShortTitle;
        }

        public override void Init(IBaseWindow baseWin)
        {
            base.Init(baseWin);

            fBase.Context.GetSourcesList(fSourcesList);
            RefreshSourcesList("");
        }

        public void RefreshSourcesList(string filter)
        {
            fView.Source.BeginUpdate();
            try {
                fView.Source.Clear();

                string flt = "*" + filter + "*";

                int num = fSourcesList.Count;
                for (int i = 0; i < num; i++) {
                    string st = fSourcesList[i];

                    if (filter == "" || GKUtils.MatchesMask(st, flt)) {
                        fView.Source.AddItem(st, fSourcesList.GetObject(i));
                    }
                }
            } finally {
                fView.Source.EndUpdate();
            }
        }
    }
}
