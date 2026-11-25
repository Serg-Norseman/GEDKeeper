/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Collections.Generic;
using BSLib;
using GDModel;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKCore.Lists;
using GKCore.Locales;
using GKCore.Operations;
using GKCore.Options;
using GKCore.Utilities;
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
        #region Design components
#pragma warning disable CS0649, IDE0044
        private IButton _btnClose;
        private ILabel _lblDate;
        private IButton _btnAnalysis;
        private ICheckBox _chkReverseOrder;
        private ITextBox _txtPlace;
        private ILabel _lblPlace;
        private ILabel _lblLocName;
        private IComboBox _cmbLocationSearch;
        private IButton _btnLocNameAdd;
        private IButton _btnLocNameEdit;
        private ILabel _lblTopLink;
        private IButton _btnTopLinkAdd;
        private IButton _btnTopLinkEdit;
        private ITextBox _txtGeneratedName;
        private ILabel _lblGeneratedName;
        private IComboBox _cmbEventDates;
        private ILabel _lblEventDates;
        private IDateControl _dtlPlaceDate;
        private IListView _lvEntries;
#pragma warning restore CS0649, IDE0044
        #endregion

        private const float FIND_LOC_THRESHOLD = 0.85f;
        private const float FILTER_LOC_THRESHOLD = 0.85f;

        private readonly LocEntriesListModel fEntriesListModel;
        private List<LocEntry> fEntries;


        public LocExpertDlgController(ILocExpertDlg view) : base(view)
        {
            fEntries = new List<LocEntry>();
            fEntriesListModel = new LocEntriesListModel();
            fEntriesListModel.DataSource = fEntries;
        }

        public override void InitView(IBaseWindow baseWin)
        {
            base.InitView(baseWin);

            _lvEntries.ListMan = fEntriesListModel;
            _lvEntries.UpdateContents();
        }

        public override void SetLocale()
        {
            fView.SetTitle(LangMan.LS(LSID.LocExpert));
            _btnClose.Text = LangMan.LS(LSID.DlgClose);
            _lblEventDates.Text = LangMan.LS(LSID.EventDates);
            _lblDate.Text = LangMan.LS(LSID.Date);
            _lblPlace.Text = LangMan.LS(LSID.Place);
            _chkReverseOrder.Text = LangMan.LS(LSID.ReversePlacesOrder);
            _lblGeneratedName.Text = "Generated Name";
            _btnAnalysis.Text = LangMan.LS(LSID.Analyze);
            _lblLocName.Text = "Location name:";
            _lblTopLink.Text = "Top link:";

#if NETCOREAPP
            _btnLocNameAdd.Text = LangMan.LS(LSID.DoAdd);
            _btnLocNameEdit.Text = LangMan.LS(LSID.DoEdit);
            _btnTopLinkAdd.Text = LangMan.LS(LSID.DoAdd);
            _btnTopLinkEdit.Text = LangMan.LS(LSID.DoEdit);
#else
            GetControl<IToolItem>("btnLocNameAdd").Text = LangMan.LS(LSID.DoAdd);
            GetControl<IToolItem>("btnLocNameEdit").Text = LangMan.LS(LSID.DoEdit);
            GetControl<IToolItem>("btnTopLinkAdd").Text = LangMan.LS(LSID.DoAdd);
            GetControl<IToolItem>("btnTopLinkEdit").Text = LangMan.LS(LSID.DoEdit);
#endif
        }

        public override void ApplyTheme()
        {
            if (!AppHost.Instance.HasFeatureSupport(Feature.Themes)) return;

            _btnClose.Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Cancel);
        }

        public void InitView(SortedSet<GDMCustomDate> eventDates, string placeName)
        {
            foreach (var cd in eventDates) {
                _cmbEventDates.AddItem(cd.GetDisplayString(DateFormat.dfDD_MM_YYYY, true, true), cd);
            }

            _dtlPlaceDate.Date = new GDMDate();
            _txtPlace.Text = placeName;
        }

        public void SelectEntry()
        {
            var entry = GetSelectedEntry();
            if (entry == null) return;

#if NETCOREAPP
            _btnLocNameAdd.Enabled = entry.Pair == null;
            _btnLocNameEdit.Enabled = entry.Pair != null;
            _btnTopLinkAdd.Enabled = entry.Pair != null && entry.TopLevelLink == null;
            _btnTopLinkEdit.Enabled = entry.Pair != null && entry.TopLevelLink != null;
#else
            GetControl<IToolItem>("btnLocNameAdd").Enabled = entry.Pair == null;
            GetControl<IToolItem>("btnLocNameEdit").Enabled = entry.Pair != null;
            GetControl<IToolItem>("btnTopLinkAdd").Enabled = entry.Pair != null && entry.TopLevelLink == null;
            GetControl<IToolItem>("btnTopLinkEdit").Enabled = entry.Pair != null && entry.TopLevelLink != null;
#endif
        }

        public void Analyze()
        {
            bool reverseOrder = _chkReverseOrder.Checked;
            string place = _txtPlace.Text;
            var date = _dtlPlaceDate.Date;

            fEntries = SplitPlaceStr(place, reverseOrder);

            // first pass, get location and their names by text
            for (int i = 0; i < fEntries.Count; i++) {
                var entry = fEntries[i];
                entry.Pair = FindLocation(entry.Text, true);
                entry.HasDateIntersects = entry.Pair != null && entry.Pair.Name != null && !GDMCustomDate.GetIntersection(date, entry.Pair.Name.Date).IsEmpty();
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
            var list = new StringList<GDMLocationRecord>();

            GetLocationsList(list, _cmbLocationSearch.Text, false);

            _cmbLocationSearch.BeginUpdate();
            try {
                _cmbLocationSearch.Clear();

                for (int i = 0, num = list.Count; i < num; i++) {
                    string st = list[i];
                    _cmbLocationSearch.AddItem(st, list.GetObject(i));
                }
            } finally {
                _cmbLocationSearch.EndUpdate();
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
            var date = _dtlPlaceDate.Date;

            GDMLocationRecord targetLoc = null;
            for (int i = 0; i < fEntries.Count; i++) {
                var entry = fEntries[i];
                if (targetLoc == null && entry.Pair != null) {
                    targetLoc = entry.Pair.Location;
                }
            }

            fEntriesListModel.DataSource = fEntries;
            _lvEntries.SortOrder = GKSortOrder.None;
            _lvEntries.UpdateContents();

            if (targetLoc != null) {
                _txtGeneratedName.Text = targetLoc.GetNameByDate(date, true);
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
                        await AppHost.StdDialogs.ShowError(LangMan.LS(LSID.PeriodsOverlap));
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
            return _lvEntries.GetSelectedData() as LocEntry;
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


        private sealed class LocEntriesListModel : SimpleListModel<LocEntry>
        {
            public LocEntriesListModel() : base(null, CreateListColumns())
            {
            }

            public static ListColumns CreateListColumns()
            {
                var result = new ListColumns(GKListType.ltNone);
                result.AddColumn(LangMan.LS(LSID.Entry), 180, false);
                result.AddColumn("LocName", 180, false); // Запись места?
                result.AddColumn("LocName date intersects", 120, false); // Пересечение/Перекрытие даты места?
                result.AddColumn("Has TopLink", 120, false); // Ссылка верхний уровень?
                result.AddColumn("TopLink date instersects", 120, false); // Пересечение/Перекрытие даты ссылки?
                return result;
            }

            protected override object GetColumnValueEx(int colType, int colSubtype, bool isVisible)
            {
                object result = null;
                switch (colType) {
                    case 0:
                        result = fFetchedRec.Text;
                        break;
                    case 1:
                        result = (fFetchedRec.Pair != null) ? fFetchedRec.Pair.Name.StringValue : GKData.CROSS_MARK;
                        break;
                    case 2:
                        result = fFetchedRec.HasDateIntersects ? GKData.CHECK_MARK : GKData.CROSS_MARK;
                        break;
                    case 3:
                        result = (fFetchedRec.TopLevelLink != null) ? GKData.CHECK_MARK : GKData.CROSS_MARK;
                        break;
                    case 4:
                        result = fFetchedRec.TopLevelIntersects ? GKData.CHECK_MARK : GKData.CROSS_MARK;
                        break;
                }
                return result;
            }
        }
    }
}
