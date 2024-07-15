/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2024 by Sergey V. Zhdanovskih.
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

using System.Collections.Generic;
using System.Windows.Forms;
using BSLib;
using GDModel;
using GKCore;
using GKCore.Controllers;
using GKCore.Design;
using GKCore.Interfaces;
using GKCore.Operations;
using GKCore.Options;

namespace GKLocExpertPlugin
{
    public partial class LEForm : Form, IView, ILocalizable
    {
        private readonly IBaseWindow fBase;
        private readonly LocExpertController fController;
        private List<LocEntry> fEntries;
        private readonly StringList<GDMLocationRecord> fLocationsList;


        public LEForm()
        {
            InitializeComponent();
            SetLocale();
            chkReverseOrder.Checked = GlobalOptions.Instance.ReversePlaceEntitiesOrder;
        }

        public LEForm(Plugin plugin, IBaseWindow curBase) : this()
        {
            fBase = curBase;
            fController = new LocExpertController(fBase);
            fEntries = new List<LocEntry>();
            fLocationsList = new StringList<GDMLocationRecord>();

            // debug data
            var date = new GDMDate();
            date.ParseString("1816");
            dtlPlaceDate.Date = date;
            txtPlace.Text = "Пермская губерния, Екатеринбургский уезд, Шайтанский завод"; // direct order!
        }

        public void SetLocale()
        {
            Text = "Location Expert";
            lblDate.Text = LangMan.LS(LSID.Date);
            lblPlace.Text = LangMan.LS(LSID.Place);
            chkReverseOrder.Text = LangMan.LS(LSID.ReversePlacesOrder);
            lblGeneratedName.Text = "Сгенерированное место";
            btnAnalysis.Text = LangMan.LS(LSID.Analyze);
            lblLocName.Text = "Имя места:";
            btnLocNameAdd.Text = "Добавить";
            btnLocNameEdit.Text = LangMan.LS(LSID.DoEdit);
            lblTopLink.Text = "Связь с верхним:";
            btnTopLinkAdd.Text = "Добавить";
            btnTopLinkEdit.Text = LangMan.LS(LSID.DoEdit);

            lvEntries.AddColumn("Entry", 230); // Пункт/Элемент?
            lvEntries.AddColumn("LocName", 190); // Запись места?
            lvEntries.AddColumn("LocName date intersects", 90); // Пересечение/Перекрытие даты места?
            lvEntries.AddColumn("Has TopLink", 90); // Ссылка верхний уровень?
            lvEntries.AddColumn("TopLink date instersects", 150); // Пересечение/Перекрытие даты ссылки?
        }

        private void UpdateSections(GDMCustomDate date)
        {
            GDMLocationRecord targetLoc = null;

            lvEntries.BeginUpdate();
            lvEntries.ClearItems();

            for (int i = 0; i < fEntries.Count; i++) {
                var entry = fEntries[i];
                var entryPair = entry.Pair;

                var locNameStr = (entryPair != null) ? entry.Pair.Name.StringValue : GKData.CROSS_MARK;
                var hasNameDateIntersects = entry.HasDateIntersects ? GKData.CHECK_MARK : GKData.CROSS_MARK;
                var hasTopLink = (entry.TopLevelLink != null) ? GKData.CHECK_MARK : GKData.CROSS_MARK;
                var hasTopLevelDateIntersects = entry.TopLevelIntersects ? GKData.CHECK_MARK : GKData.CROSS_MARK;

                lvEntries.AddItem(entry, entry.Text, locNameStr, hasNameDateIntersects, hasTopLink, hasTopLevelDateIntersects);

                if (targetLoc == null && entryPair != null) {
                    targetLoc = entryPair.Location;
                }
            }

            lvEntries.EndUpdate();

            if (targetLoc != null) {
                txtGeneratedName.Text = targetLoc.GetNameByDate(date, true);
            }
        }

        private void btnAnalysis_Click(object sender, System.EventArgs e)
        {
            bool reverseOrder = chkReverseOrder.Checked;
            string place = txtPlace.Text;
            fEntries = fController.SplitPlaceStr(place, reverseOrder);

            var date = dtlPlaceDate.Date;
            fController.Analyze(fEntries, date);
            UpdateSections(date);
        }

        private LocEntry GetSelectedEntry()
        {
            return lvEntries.GetSelectedData() as LocEntry;
        }

        private void lvEntries_SelectedIndexChanged(object sender, System.EventArgs e)
        {
            var entry = GetSelectedEntry();
            if (entry == null) return;

            btnLocNameAdd.Enabled = entry.Pair == null;
            btnLocNameEdit.Enabled = entry.Pair != null;

            btnTopLinkAdd.Enabled = entry.Pair != null && entry.TopLevelLink == null;
            btnTopLinkEdit.Enabled = entry.Pair != null && entry.TopLevelLink != null;
        }

        private async void ModifyLocation(GDMLocationRecord locRec, string proposedName = "")
        {
            var locRes = await BaseController.ModifyLocation(this, fBase, locRec, proposedName);
            if (locRes.Result)
                btnAnalysis_Click(null, null);

            /*
            var locNameRes = await BaseController.ModifyLocationName(fOwner, fBaseWin, fUndoman, dataOwner, locName);
            locName = locNameRes.Record;
            result = locNameRes.Result;

            if (result) {
                if (!dataOwner.ValidateNames()) {
                    AppHost.StdDialogs.ShowError(LangMan.LS(LSID.PeriodsOverlap));
                }
                dataOwner.SortNames();
            }
            */
        }

        private async void btnLocNameAdd_Click(object sender, System.EventArgs e)
        {
            var entry = GetSelectedEntry();
            if (entry == null) return;

            ModifyLocation(null, entry.Text);
        }

        private async void btnLocNameEdit_Click(object sender, System.EventArgs e)
        {
            var entry = GetSelectedEntry();
            if (entry == null || entry.Pair == null) return;

            ModifyLocation(entry.Pair.Location, string.Empty);
        }

        private async void ModifyLocationLink(GDMLocationRecord locRec, GDMLocationLink topLevelLink, GDMLocationRecord proposedTopLocation = null)
        {
            ChangeTracker undoman = new ChangeTracker(fBase.Context);
            try {
                var locLinkRes = await BaseController.ModifyLocationLink(this, fBase, undoman, locRec, topLevelLink, proposedTopLocation);
                //locLink = locLinkRes.Record;

                if (locLinkRes.Result) {
                    if (!locRec.ValidateLinks()) {
                        AppHost.StdDialogs.ShowError(LangMan.LS(LSID.PeriodsOverlap));
                    }
                    locRec.SortTopLevels();

                    undoman.Commit();

                    btnAnalysis_Click(null, null);
                }
            } catch {
                undoman.Rollback();
            }
        }

        private void btnTopLinkAdd_Click(object sender, System.EventArgs e)
        {
            var entry = GetSelectedEntry();
            if (entry == null) return;

            GDMLocationRecord proposedTopLocation = null;
            var idx = fEntries.IndexOf(entry);
            if (idx < fEntries.Count - 1) {
                var pair = fEntries[idx + 1].Pair;
                if (pair != null) {
                    proposedTopLocation = pair.Location;
                }
            }

            ModifyLocationLink(entry.Pair.Location, null, proposedTopLocation);
        }

        private void btnTopLinkEdit_Click(object sender, System.EventArgs e)
        {
            var entry = GetSelectedEntry();
            if (entry == null || entry.Pair == null || entry.TopLevelLink == null) return;

            ModifyLocationLink(entry.Pair.Location, entry.TopLevelLink, null);
        }

        private void dtlPlaceDate_DateChanged(object sender, System.EventArgs e)
        {
            btnAnalysis_Click(null, null);
        }

        private void cmbLocationSearch_KeyUp(object sender, KeyEventArgs e)
        {
            fController.GetLocationsList(fLocationsList, cmbLocationSearch.Text, false);
            fController.RefreshCombo(cmbLocationSearch, fLocationsList);
        }

        #region View

        public string Title
        {
            get { return string.Empty; }
            set { }
        }

        public object GetControl(string controlName)
        {
            return null;
        }

        public void SetToolTip(object component, string toolTip)
        {
        }

        #endregion
    }
}
