/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2017 by Sergey V. Zhdanovskih.
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
using System.Windows.Forms;

using GKCommon.GEDCOM;
using GKCore.Interfaces;
using GKCore.Stats;
using GKUI.Components;
using WordCloud;

namespace GKWordsCloudPlugin
{
    /// <summary>
    /// 
    /// </summary>
    public partial class WordsCloudWidget : Form, ILocalization
    {
        private class CloudType
        {
            public readonly string Name;
            public readonly StatsMode Mode;

            public CloudType(string name, StatsMode mode)
            {
                Name = name;
                Mode = mode;
            }
        }

        private static readonly CloudType[] CloudTypes = new CloudType[] {
            new CloudType("Surnames", StatsMode.smSurnames),
            new CloudType("Names", StatsMode.smNames),
            new CloudType("Occupation", StatsMode.smOccupation),
            new CloudType("Religious", StatsMode.smReligious),
            new CloudType("National", StatsMode.smNational),
            new CloudType("Education", StatsMode.smEducation),
            new CloudType("Caste", StatsMode.smCaste),
            new CloudType("Hobby", StatsMode.smHobby),
        };

        private readonly Plugin fPlugin;
        private IBaseWindow fBase;
        private StatsMode fMode;
        private List<Word> fWords;

        public WordsCloudWidget(Plugin plugin)
        {
            InitializeComponent();

            fPlugin = plugin;
            fWords = new List<Word>();

            foreach (CloudType cloudType in CloudTypes) {
                cbType.Items.Add(new GKComboItem(cloudType.Name, cloudType.Mode));
            }
            fMode = StatsMode.smNames;

            //Screen scr = Screen.PrimaryScreen;
            //Location = new Point(scr.WorkingArea.Width - Width - 10, scr.WorkingArea.Height - Height - 10);

            SetLang();
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                fPlugin.CloseForm();
            }
            base.Dispose(disposing);
        }

        private void CalcWidget_Load(object sender, EventArgs e)
        {
            fPlugin.Host.WidgetShow(fPlugin);
            BaseChanged(fPlugin.Host.GetCurrentFile());
        }

        private void CalcWidget_Closed(object sender, EventArgs e)
        {
            BaseChanged(null);
            fPlugin.Host.WidgetClose(fPlugin);
        }

        public void BaseChanged(IBaseWindow baseWin)
        {
            if (fBase != baseWin) {
                fBase = baseWin;
                UpdateCloud();
            }
        }

        private void UpdateCloud()
        {
            try {
                fWords.Clear();

                if (fBase != null) {
                    List<StatsItem> vals = new List<StatsItem>();
                    TreeStats treeStats = new TreeStats(fBase.Context, fBase.GetContentList(GEDCOMRecordType.rtIndividual));
                    treeStats.GetSpecStats(fMode, vals);

                    fWords.Capacity = vals.Count;
                    foreach (var statsItem in vals) {
                        string word = statsItem.Caption;
                        if (word != "?") {
                            fWords.Add(new Word(statsItem.Caption, statsItem.Value));
                        }
                    }
                    fWords.Sort(CompareWords);
                }

                cloudViewer.WeightedWords = fWords;
            } catch (Exception ex) {
                MessageBox.Show(ex.Message);
            }
        }

        private static int CompareWords(Word item1, Word item2)
        {
            return item2.Occurrences - item1.Occurrences;
        }

        private void cbType_SelectedIndexChanged(object sender, EventArgs e)
        {
            GKComboItem item = (GKComboItem)cbType.SelectedItem;
            if (item != null) {
                fMode = (StatsMode)item.Tag;
                UpdateCloud();
            }
        }

        #region ILocalization support

        public void SetLang()
        {
            Text = fPlugin.LangMan.LS(CLS.LSID_Title);
        }

        #endregion
    }
}
