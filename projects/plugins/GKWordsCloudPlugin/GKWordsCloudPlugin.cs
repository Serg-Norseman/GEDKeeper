/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Collections.Generic;
using System.Reflection;
using GDModel;
using GKCore;
using GKCore.Design;
using GKCore.Design.Graphics;
using GKCore.Locales;
using GKCore.Plugins;
using GKCore.Stats;
using GKWordsCloudPlugin.WordsCloud;

[assembly: AssemblyTitle("GKWordsCloudPlugin")]
[assembly: AssemblyDescription("GEDKeeper WordsCloud plugin")]
[assembly: AssemblyProduct("GEDKeeper")]
[assembly: AssemblyCopyright("Copyright © 2017-2023 by Sergey V. Zhdanovskih")]
[assembly: AssemblyVersion("1.1.0.0")]
[assembly: AssemblyCulture("")]

#if DEBUG
[assembly: AssemblyConfiguration("Debug")]
#elif RELEASE
[assembly: AssemblyConfiguration("Release")]
#endif

namespace GKWordsCloudPlugin
{
    public enum PLS
    {
        WordsCloud = 1,
    }

    public sealed class Plugin : WidgetPlugin
    {
        private string fDisplayName = "GKWordsCloudPlugin";
        private ILangMan fLangMan;

        public override string DisplayName { get { return fDisplayName; } }
        public override ILangMan LangMan { get { return fLangMan; } }
        public override IImage Icon { get { return null; } }
        public override PluginCategory Category { get { return PluginCategory.Common; } }

        private WordsCloudWidget fForm;

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                CloseForm();
            }
            base.Dispose(disposing);
        }

        internal void CloseForm()
        {
            if (fForm != null) {
                fForm = null;
            }
        }

        public override void Execute()
        {
            if (!Host.IsWidgetActive(this)) {
                fForm = new WordsCloudWidget(this);
                fForm.Show();
            } else {
                fForm.Close();
            }
        }

        public override void OnLanguageChange()
        {
            try {
                fLangMan = Host.CreateLangMan(this);
                fDisplayName = fLangMan.LS(PLS.WordsCloud);

                if (fForm != null)
                    fForm.SetLocale();
            } catch (Exception ex) {
                Logger.WriteError("GKWordsCloudPlugin.OnLanguageChange()", ex);
            }
        }

        public override bool Shutdown()
        {
            bool result = true;
            try {
                CloseForm();
            } catch (Exception ex) {
                Logger.WriteError("GKWordsCloudPlugin.Shutdown()", ex);
                result = false;
            }
            return result;
        }

        public override void BaseChanged(IBaseWindow baseWin)
        {
            if (fForm != null) {
                fForm.BaseChanged(baseWin);
            }
        }

        public override void BaseClosed(IBaseWindow baseWin)
        {
            if (fForm != null) {
                fForm.BaseChanged(null);
            }
        }

        internal class CloudType
        {
            public readonly LSID Name;
            public readonly StatsMode Mode;

            public CloudType(LSID name, StatsMode mode)
            {
                Name = name;
                Mode = mode;
            }
        }

        internal static readonly CloudType[] CloudTypes = new CloudType[] {
            new CloudType(LSID.Surname, StatsMode.smSurnames),
            new CloudType(LSID.GivenName, StatsMode.smNames),
            new CloudType(LSID.Occupation, StatsMode.smOccupation),
            new CloudType(LSID.Religion, StatsMode.smReligious),
            new CloudType(LSID.Nationality, StatsMode.smNational),
            new CloudType(LSID.Education, StatsMode.smEducation),
            new CloudType(LSID.Caste, StatsMode.smCaste),
            new CloudType(LSID.Hobby, StatsMode.smHobby),
        };

        internal void CollectData(IBaseWindow baseWin, StatsMode mode, List<Word> words)
        {
            words.Clear();

            if (baseWin != null) {
                var vals = new List<StatsItem>();
                var treeStats = new TreeStats(baseWin.Context, baseWin.GetContentList(GDMRecordType.rtIndividual));
                treeStats.GetSpecStats(mode, vals);

                words.Capacity = vals.Count;
                foreach (var statsItem in vals) {
                    string word = statsItem.Caption;
                    if (word != "?") {
                        words.Add(new Word(statsItem.Caption, statsItem.Value));
                    }
                }
                words.Sort();
            }
        }
    }
}
