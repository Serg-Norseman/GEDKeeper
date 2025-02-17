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

using System;
using System.Collections.Generic;
using BSLib;
using GDModel;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKCore.Operations;
using GKCore.Types;
using GKUI.Themes;

namespace GKCore.Controllers
{
    public sealed class LocEntry
    {
        public string Text;

        public LocPair Pair;
        public bool HasDateIntersects;
        public GDMLocationLink TopLevelLink;
        public bool TopLevelIntersects;

        public LocEntry(string text)
        {
            Text = text;
        }
    }


    public sealed class LocPair
    {
        public GDMLocationRecord Location;
        public GDMLocationName Name;

        public LocPair(GDMLocationRecord location, GDMLocationName name)
        {
            Location = location;
            Name = name;
        }
    }


    public class LocExpertDlgController: DialogController<ILocExpertDlg>
    {
        private const float FIND_LOC_THRESHOLD = 0.85f;
        private const float FILTER_LOC_THRESHOLD = 0.85f;


        private List<LocEntry> fEntries;


        public LocExpertDlgController(ILocExpertDlg view) : base(view)
        {
            fEntries = new List<LocEntry>();
        }

        public override void SetLocale()
        {
            fView.Title = LangMan.LS(LSID.LocExpert);
            GetControl<IButton>("btnClose").Text = LangMan.LS(LSID.DlgClose);
            GetControl<ILabel>("lblEventDates").Text = LangMan.LS(LSID.EventDates);
            GetControl<ILabel>("lblDate").Text = LangMan.LS(LSID.Date);
            GetControl<ILabel>("lblPlace").Text = LangMan.LS(LSID.Place);
            GetControl<ICheckBox>("chkReverseOrder").Text = LangMan.LS(LSID.ReversePlacesOrder);
            GetControl<ILabel>("lblGeneratedName").Text = "Generated Name";
            GetControl<IButton>("btnAnalysis").Text = LangMan.LS(LSID.Analyze);
            GetControl<ILabel>("lblLocName").Text = "Location name:";
            GetControl<ILabel>("lblTopLink").Text = "Top link:";

#if NETCORE
            GetControl<IButton>("btnLocNameAdd").Text = LangMan.LS(LSID.DoAdd);
            GetControl<IButton>("btnLocNameEdit").Text = LangMan.LS(LSID.DoEdit);
            GetControl<IButton>("btnTopLinkAdd").Text = LangMan.LS(LSID.DoAdd);
            GetControl<IButton>("btnTopLinkEdit").Text = LangMan.LS(LSID.DoEdit);
#else
            GetControl<IButtonToolItem>("btnLocNameAdd").Text = LangMan.LS(LSID.DoAdd);
            GetControl<IButtonToolItem>("btnLocNameEdit").Text = LangMan.LS(LSID.DoEdit);
            GetControl<IButtonToolItem>("btnTopLinkAdd").Text = LangMan.LS(LSID.DoAdd);
            GetControl<IButtonToolItem>("btnTopLinkEdit").Text = LangMan.LS(LSID.DoEdit);
#endif

            var lvEntries = GetControl<IListView>("lvEntries");
            lvEntries.AddColumn(LangMan.LS(LSID.Entry), 180, false);
            lvEntries.AddColumn("LocName", 180, false); // Запись места?
            lvEntries.AddColumn("LocName date intersects", 120, false); // Пересечение/Перекрытие даты места?
            lvEntries.AddColumn("Has TopLink", 120, false); // Ссылка верхний уровень?
            lvEntries.AddColumn("TopLink date instersects", 120, false); // Пересечение/Перекрытие даты ссылки?
        }

        public override void ApplyTheme()
        {
            if (!AppHost.Instance.HasFeatureSupport(Feature.Themes)) return;

            GetControl<IButton>("btnClose").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Cancel);
        }

        public void InitView(SortedSet<GDMCustomDate> eventDates, string placeName)
        {
            var cmbEventDates = GetControl<IComboBox>("cmbEventDates");
            foreach (var cd in eventDates) {
                cmbEventDates.AddItem(cd.GetDisplayStringExt(DateFormat.dfDD_MM_YYYY, true, true), cd);
            }

            GetControl<IDateControl>("dtlPlaceDate").Date = new GDMDate();
            GetControl<ITextBox>("txtPlace").Text = placeName;
        }

        public void SelectEntry()
        {
            var entry = GetSelectedEntry();
            if (entry == null) return;

            GetControl<IButtonToolItem>("btnLocNameAdd").Enabled = entry.Pair == null;
            GetControl<IButtonToolItem>("btnLocNameEdit").Enabled = entry.Pair != null;
            GetControl<IButtonToolItem>("btnTopLinkAdd").Enabled = entry.Pair != null && entry.TopLevelLink == null;
            GetControl<IButtonToolItem>("btnTopLinkEdit").Enabled = entry.Pair != null && entry.TopLevelLink != null;
        }

        public void Analyze()
        {
            bool reverseOrder = GetControl<ICheckBox>("chkReverseOrder").Checked;
            string place = GetControl<ITextBox>("txtPlace").Text;
            var date = GetControl<IDateControl>("dtlPlaceDate").Date;

            fEntries = SplitPlaceStr(place, reverseOrder);

            // first pass, get location and their names by text
            for (int i = 0; i < fEntries.Count; i++) {
                var entry = fEntries[i];
                entry.Pair = FindLocation(entry.Text, true);
                entry.HasDateIntersects = (entry.Pair == null || entry.Pair.Name == null) ? false : !GDMCustomDate.GetIntersection(date, entry.Pair.Name.Date).IsEmpty();
            }

            // second pass, check links to top levels
            for (int i = 0, num = fEntries.Count; i < num; i++) {
                var entry = fEntries[i];
                if (entry.Pair == null) continue;

                var nextEntry = (i < num - 1) ? fEntries[i + 1] : null;
                if (nextEntry == null || nextEntry.Pair == null) continue;

                var topLevels = entry.Pair.Location.TopLevels;
                for (int j = 0; j < topLevels.Count; j++) {
                    var topLink = topLevels[j];

                    if (topLink.XRef == nextEntry.Pair.Location.XRef) {
                        entry.TopLevelLink = topLink;

                        var interDate = GDMCustomDate.GetIntersection(date, topLink.Date.Value);
                        if (!interDate.IsEmpty()) {
                            entry.TopLevelIntersects = true;
                        }
                    }
                }
            }

            UpdateView();
        }

        public LocPair FindLocation(string value, bool indistinct = false)
        {
            if (!indistinct) {
                value = "*" + value + "*";
            }

            var tree = fBase.Context.Tree;
            for (int i = 0, num = tree.RecordsCount; i < num; i++) {
                var locRec = tree[i] as GDMLocationRecord;
                if (locRec == null) continue;

                var names = locRec.Names;
                for (int j = 0; j < names.Count; j++) {
                    var locName = names[j];
                    if (MatchLocName(value, locName, indistinct, FIND_LOC_THRESHOLD)) {
                        return new LocPair(locRec, locName);
                    }
                }
            }

            return null;
        }

        private static bool MatchLocName(string pattern, GDMLocationName locName, bool indistinct, float threshold)
        {
            bool res;

            if (indistinct) {
                res = (IndistinctMatching.GetSimilarity(locName.StringValue, pattern) >= threshold);
            } else {
                res = SysUtils.MatchPattern(pattern, locName.StringValue, false);
            }

            var abbr = locName.Abbreviation;
            if (!res && !string.IsNullOrEmpty(abbr)) {
                if (indistinct) {
                    res = (IndistinctMatching.GetSimilarity(abbr, pattern) >= threshold);
                } else {
                    res = SysUtils.MatchPattern(pattern, abbr, false);
                }
            }

            return res;
        }

        public void GetLocationsList(StringList<GDMLocationRecord> list, string filter, bool indistinct = false)
        {
            if (list == null) return;

            if (!indistinct) {
                filter = "*" + filter + "*";
            }

            var tree = fBase.Context.Tree;
            int num = tree.RecordsCount;
            for (int i = 0; i < num; i++) {
                var locRec = tree[i] as GDMLocationRecord;
                if (locRec == null) continue;

                var names = locRec.Names;
                for (int j = 0; j < names.Count; j++) {
                    var locName = names[j];
                    if (MatchLocName(filter, locName, indistinct, FILTER_LOC_THRESHOLD)) {
                        list.AddObject(locName.StringValue, locRec);
                    }
                }
            }
        }

        public void RefreshLocCombo()
        {
            var comboBox = GetControl<IComboBox>("cmbLocationSearch");
            var list = new StringList<GDMLocationRecord>();

            GetLocationsList(list, comboBox.Text, false);

            comboBox.BeginUpdate();
            try {
                comboBox.Clear();

                for (int i = 0, num = list.Count; i < num; i++) {
                    string st = list[i];
                    comboBox.AddItem(st, list.GetObject(i));
                }
            } finally {
                comboBox.EndUpdate();
            }

            //comboBox.SelectionStart = comboBox.Text.Length;
        }

        public List<LocEntry> SplitPlaceStr(string place, bool reverseOrder)
        {
            var result = new List<LocEntry>();

            string[] placeParts = place.Split(GKUtils.PLACE_DELIMITERS, StringSplitOptions.None);
            if (placeParts.Length > 1) {
                if (reverseOrder) {
                    for (int i = 0; i < placeParts.Length; i++) {
                        result.Add(new LocEntry(placeParts[i].Trim()));
                    }
                } else {
                    for (int i = placeParts.Length - 1; i >= 0; i--) {
                        result.Add(new LocEntry(placeParts[i].Trim()));
                    }
                }
            }

            return result;
        }

        public override void UpdateView()
        {
            var lvEntries = GetControl<IListView>("lvEntries");
            var txtGeneratedName = GetControl<ITextBox>("txtGeneratedName");
            var date = GetControl<IDateControl>("dtlPlaceDate").Date;

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

        public async void ModifyLocation(GDMLocationRecord locRec, string proposedName = "")
        {
            var locRes = await BaseController.ModifyLocation(fView, fBase, locRec, proposedName);
            if (locRes.Result)
                Analyze();

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

        public async void ModifyLocationLink(GDMLocationRecord locRec, GDMLocationLink topLevelLink, GDMLocationRecord proposedTopLocation = null)
        {
            ChangeTracker undoman = new ChangeTracker(fBase.Context);
            try {
                var locLinkRes = await BaseController.ModifyLocationLink(fView, fBase, undoman, locRec, topLevelLink, proposedTopLocation);
                //locLink = locLinkRes.Record;

                if (locLinkRes.Result) {
                    if (!locRec.ValidateLinks()) {
                        AppHost.StdDialogs.ShowError(LangMan.LS(LSID.PeriodsOverlap));
                    }
                    locRec.SortTopLevels();

                    undoman.Commit();

                    Analyze();
                }
            } catch {
                undoman.Rollback();
            }
        }

        public LocEntry GetSelectedEntry()
        {
            var lvEntries = GetControl<IListView>("lvEntries");
            return lvEntries.GetSelectedData() as LocEntry;
        }

        public void AddLocName()
        {
            var entry = GetSelectedEntry();
            if (entry == null) return;

            ModifyLocation(null, entry.Text);
        }

        public void EditLocName()
        {
            var entry = GetSelectedEntry();
            if (entry == null || entry.Pair == null) return;

            ModifyLocation(entry.Pair.Location, string.Empty);
        }

        public void AddTopLink()
        {
            var entry = GetSelectedEntry();
            if (entry == null) return;

            GDMLocationRecord proposedTopLocation = null;
            var entries = fEntries;
            var idx = entries.IndexOf(entry);
            if (idx < entries.Count - 1) {
                var pair = entries[idx + 1].Pair;
                if (pair != null) {
                    proposedTopLocation = pair.Location;
                }
            }

            ModifyLocationLink(entry.Pair.Location, null, proposedTopLocation);
        }

        public void EditTopLink()
        {
            var entry = GetSelectedEntry();
            if (entry == null || entry.Pair == null || entry.TopLevelLink == null) return;

            ModifyLocationLink(entry.Pair.Location, entry.TopLevelLink, null);
        }
    }
}
